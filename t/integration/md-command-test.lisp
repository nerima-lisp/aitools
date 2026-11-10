(in-package #:cl-user)

(defpackage #:aitools.integration.md-command-test
            (:use #:cl)
            (:shadowing-import-from #:cl-weave #:describe)
            (:import-from #:cl-weave #:it #:expect))

(in-package #:aitools.integration.md-command-test)

(defun %md-ok (root arguments command)
  (multiple-value-bind (code envelope stream) (aitools.test.support:run-aitools root arguments)
    (expect code :to-be 0)
    (expect stream :to-be :stdout)
    (expect envelope :to-match-envelope "command" command "status" "ok")
    envelope))

(defun %md-error (root arguments command error-code)
  (multiple-value-bind (code envelope stream) (aitools.test.support:run-aitools root arguments)
    (expect code :to-be (aitools.protocol.domain:error-code-exit-code error-code))
    (expect stream :to-be :stderr)
    (expect envelope :to-match-envelope "command" command "status" "error")
    (expect (aitools.test.support:envelope-value envelope "error" "code") :to-equal error-code)
    envelope))
(defun %md-flow-result (flow &rest arguments)
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&rest continuations)
     (apply flow (append arguments continuations)))))
(defun %make-md-test-boundaries (&key workspace-host text-source open-store
                                            read-stdin-octets parse-markdown splice-section)
  (let ((boundaries (cl-boundary-kit:make-boundary-context)))
    (dolist (capability
             (list (cons :md-workspace-host workspace-host)
                   (cons :md-text-source text-source)
                   (cons :md-open-store open-store)
                   (cons :md-read-stdin-octets read-stdin-octets)
                   (cons :md-parse-markdown parse-markdown)
                   (cons :md-splice-section splice-section)))
      (setf boundaries
            (cl-boundary-kit:boundary-context-with
             boundaries (car capability) (cdr capability))))
    boundaries))
(defun %md-read-boundaries (root &key (outcome :text) parser)
  (let* ((text (format nil "# One~%body~%## Two~%nested~%"))
         (bytes (if (eq outcome :binary)
                    (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)
                    (sb-ext:string-to-octets text :external-format :utf-8))))
    (flet ((unused (&rest arguments)
             (declare (ignore arguments))
             nil))
      (%make-md-test-boundaries :workspace-host
       (aitools.workspace.infrastructure:make-host-workspace-boundaries
        :current-directory (lambda () root))
       :text-source
       (aitools.text.application:make-text-source
        :file-size #'unused :read-prefix #'unused :read-octets #'unused
        :call-with-chunks #'unused
        :read-sniffed
        (lambda (path count continue-p &key max-bytes)
          (declare (ignore path max-bytes))
          (case outcome
            (:missing (values nil nil))
            (:unreadable (values nil :unreadable))
            (otherwise
             (let* ((size (if (eq outcome :too-large)
                              (1+ aitools.md.application::+md-max-input-bytes+)
                              (length bytes)))
                    (prefix (subseq bytes 0 (min count (length bytes))))
                    (continue (funcall continue-p prefix size)))
               (values (if continue bytes prefix) size))))))
       :parse-markdown (or parser #'aitools.md.infrastructure:parse-md-source)))))

(describe "Markdown commands through the loaded dispatcher"
          (it "lists duplicate headings and reads each selected section completely"
              (aitools.test.support:with-workspace (root state :chdir t)
                                                   (declare (ignore state))
                                                   (let ((source
                                                          (format nil
                                                                  "---~%title: demo~%---~%# Same~%first~%## Child~%nested~%# Same~%second~%")))
                                                     (aitools.test.support:write-text
                                                      (concatenate 'string root "/guide.md")
                                                      source)
                                                     (let* ((listed
                                                             (%md-ok root '("md" "headings" "guide.md") "md headings"))
                                                            (entries
                                                             (coerce
                                                              (aitools.test.support:envelope-value listed "headings")
                                                              'list)))
                                                       (expect
                                                        (mapcar
                                                         (lambda (entry)
                                                           (list (aitools.test.support:envelope-value entry "section")
                                                                 (aitools.test.support:envelope-value entry "heading")
                                                                 (aitools.test.support:envelope-value entry "level")
                                                                 (aitools.test.support:envelope-value entry "line")))
                                                         entries)
                                                        :to-equal
                                                        '((1 "Same" 1 4) (2 "Child" 2 6) (3 "Same" 1 8)))
                                                       (let* ((start (search "# Same" source))
                                                              (end (search "# Same" source :start2 (1+ start)))
                                                              (read-first
                                                               (%md-ok root '("md" "read" "guide.md" "1") "md read")))
                                                         (expect
                                                          (aitools.test.support:envelope-value read-first "content")
                                                          :to-equal
                                                          (subseq source start end)))
                                                       (let ((read-last
                                                              (%md-ok root '("md" "read" "guide.md" "3") "md read")))
                                                         (expect
                                                          (aitools.test.support:envelope-value read-last "content")
                                                          :to-equal
                                                          (subseq source
                                                                  (search "# Same"
                                                                          source
                                                                          :start2
                                                                          (1+ (search "# Same" source))))))))
                                                   (%md-error root
                                                              '("md" "read" "guide.md" "99")
                                                              "md read"
                                                              "selection.no-match")
                                                   (%md-error root
                                                              '("md" "read" "guide.md" "zero")
                                                              "md read"
                                                              "argument.invalid"))))
(describe "Markdown application read flows"
  (it "maps headings, selected sections, and ordinal inputs"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((boundaries (%md-read-boundaries root))
             (listed (%md-flow-result #'aitools.md.application:md-headings/k boundaries "guide.md" :root root))
             (headings (cdr (assoc "headings" (aitools.protocol.application:command-result-fields listed)
                                  :test #'string=))))
        (expect (aitools.protocol.application:command-result-kind listed) :to-be :ok)
        (let ((boundaries (%make-md-test-boundaries)))
          (flet ((error-code (path section hash stdin)
                   (getf (aitools.protocol.application:command-result-fields
                          (%md-flow-result (function aitools.md.application:md-replace/k)
                                           boundaries path section :expect-hash hash :stdin stdin))
                         :code)))
            (expect (error-code nil 1 "hash" t) :to-equal "argument.invalid")
            (expect (error-code "guide.md" 1 "hash" nil) :to-equal "argument.invalid")
            (expect (error-code "guide.md" 0 "hash" t) :to-equal "argument.invalid")
            (expect (error-code "guide.md" 1 "" t) :to-equal "argument.invalid")
            (expect (error-code "guide.md" 1 nil t) :to-equal "argument.invalid")
            (expect (error-code "guide.md" 1 "hash" t) :to-equal "internal.unexpected")))
        (expect (mapcar (lambda (heading)
                          (mapcar (lambda (key)
                                    (cdr (assoc key
                                                (aitools.protocol.domain:json-object-members heading)
                                                :test (function string=))))
                                  (list "section" "heading" "level" "line")))
                        headings)
                :to-equal '((1 "One" 1 1) (2 "Two" 2 3)))
        (expect (stringp (cdr (assoc "hash" (aitools.protocol.application:command-result-fields listed)
                                    :test #'string=)))
                :to-be t)
        (let* ((read (%md-flow-result #'aitools.md.application:md-read/k boundaries "guide.md" "2" :root root))
               (fields (aitools.protocol.application:command-result-fields read)))
          (expect (mapcar (lambda (key) (cdr (assoc key fields :test #'string=)))
                          '("section" "heading" "level" "line" "content"))
                  :to-equal (list 2 "Two" 2 3 (format nil "## Two~%nested~%"))))
        (dolist (case '((1 1) ("2" 2) (0 nil) (-1 nil) ("" nil) ("x" nil) ("３" nil) (nil nil)))
          (expect (aitools.md.application::%md-section-number (first case)) :to-be (second case)))
        (expect (getf (aitools.protocol.application:command-result-fields
                       (%md-flow-result #'aitools.md.application:md-read/k boundaries "guide.md" "zero" :root root))
                      :code)
                :to-equal "argument.invalid")
        (expect (getf (aitools.protocol.application:command-result-fields
                       (%md-flow-result #'aitools.md.application:md-read/k boundaries "guide.md" 99 :root root))
                      :code)
                :to-equal "selection.no-match")
        (expect (getf (aitools.protocol.application:command-result-fields
                       (%md-flow-result #'aitools.md.application:md-read/k
                                         (%md-read-boundaries root :outcome :missing) "guide.md" 1 :root root))
                      :code)
                :to-equal "input.not-found"))))
  (it "preserves document errors for paths, ports, roots, sources, and parser offsets"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((boundaries (%md-read-boundaries root))
             (source (cl-boundary-kit:boundary-context-require boundaries :md-text-source))
             (host (cl-boundary-kit:boundary-context-require boundaries :md-workspace-host))
             (parser (cl-boundary-kit:boundary-context-require boundaries :md-parse-markdown)))
        (dolist (path (list nil ""))
          (expect (getf (aitools.protocol.application:command-result-fields
                         (%md-flow-result #'aitools.md.application:md-headings/k boundaries path :root root))
                        :code)
                  :to-equal "argument.invalid"))
        (dolist (incomplete (list (%make-md-test-boundaries :text-source source :parse-markdown parser)
                                  (%make-md-test-boundaries :workspace-host host :parse-markdown parser)
                                  (%make-md-test-boundaries :workspace-host host :text-source source)))
          (expect (getf (aitools.protocol.application:command-result-fields
                         (%md-flow-result #'aitools.md.application:md-headings/k incomplete "guide.md" :root root))
                        :code)
                  :to-equal "internal.unexpected"))
        (dolist (case '((:binary "input.unsupported-format") (:missing "input.not-found")
                        (:unreadable "environment.io") (:too-large "refusal.too-large")))
          (expect (getf (aitools.protocol.application:command-result-fields
                         (%md-flow-result #'aitools.md.application:md-headings/k
                                           (%md-read-boundaries root :outcome (first case)) "guide.md" :root root))
                        :code)
                  :to-equal (second case)))
        (expect (getf (aitools.protocol.application:command-result-fields
                       (%md-flow-result #'aitools.md.application:md-headings/k boundaries "guide.md"
                                         :root (format nil "~A/missing" root)))
                      :code)
                :to-equal "input.not-found")
        (let ((not-directory (format nil "~A/not-directory" root)))
          (aitools.test.support:write-text not-directory "file")
          (expect (getf (aitools.protocol.application:command-result-fields
                         (%md-flow-result #'aitools.md.application:md-headings/k boundaries "guide.md"
                                           :root not-directory))
                        :code)
                  :to-equal "argument.invalid"))
        (dolist (offset (list 4 nil))
          (let* ((bad-parser (lambda (source octets) (declare (ignore source octets)) (values nil "input.unparsable" "invalid Markdown" offset)))
                 (result (%md-flow-result #'aitools.md.application:md-headings/k
                                          (%md-read-boundaries root :parser bad-parser) "guide.md" :root root))
                 (fields (aitools.protocol.application:command-result-fields result)))
            (expect (getf fields :code) :to-equal "input.unparsable")
            (expect (not (null (getf fields :diagnostics))) :to-be (not (null offset))))))))
  (it "dispatches replace dry-run and write options in process"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((source (format nil "# One~%old~%"))
             (replacement (format nil "# One~%new~%"))
             (path (concatenate 'string root "/guide.md"))
             (stdin-path (concatenate 'string root "/replacement.md")))
        (aitools.test.support:write-text path source)
        (aitools.test.support:write-text stdin-path replacement)
        (let* ((listed (%md-ok root '("md" "headings" "guide.md") "md headings"))
               (hash (aitools.test.support:envelope-value listed "hash"))
               (base (list "md" "replace" "guide.md" "1" "--expect-hash" hash "--stdin")))
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:with-stdin-from (stdin-path)
                (aitools.test.support:run-aitools root (append base (list "--dry-run"))))
            (expect (list code stream) :to-equal '(0 :stdout))
            (expect (aitools.test.support:envelope-value envelope "status") :to-equal "ok")
            (expect (aitools.test.support:read-text path) :to-equal source))
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:with-stdin-from (stdin-path)
                (aitools.test.support:run-aitools root base))
            (expect (list code stream) :to-equal '(0 :stdout))
            (expect (aitools.test.support:envelope-value envelope "status") :to-equal "ok")
            (expect (aitools.test.support:read-text path) :to-equal replacement)))))))

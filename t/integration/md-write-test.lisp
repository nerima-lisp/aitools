(in-package #:cl-user)

(defpackage #:aitools.integration.md-write-test
            (:use #:cl)
            (:shadowing-import-from #:cl-weave #:describe)
            (:import-from #:cl-weave #:it #:expect))

(in-package #:aitools.integration.md-write-test)

(defun %md-write-ports (replacement)
  (let ((replacement-octets (sb-ext:string-to-octets replacement :external-format :utf-8)))
    (aitools.md.application:make-md-ports :workspace-host
                                          (aitools.workspace.infrastructure:make-host-workspace-boundaries)
                                          :text-source
                                          (aitools.text.infrastructure:make-host-text-source)
                                          :open-store
                                          #'aitools.store.infrastructure:make-posix-store
                                          :read-stdin-octets
                                          (lambda (limit)
                                            (if (> (length replacement-octets) limit)
                                                (list :too-large)
                                                (list :octets replacement-octets)))
                                          :parse-markdown
                                          #'aitools.md.infrastructure:parse-md-octets
                                          :splice-section
                                          #'aitools.md.infrastructure::splice-section-bytes)))

(defun %md-flow-result (flow &rest arguments)
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&rest continuations)
     (apply flow (append arguments continuations)))))

(defun %field (fields name)
  (cdr (assoc name fields :test #'string=)))

(describe "Markdown replacement flow"
          (it "journals a byte-exact replacement while preserving CRLF and UTF-8"
              (aitools.test.support:with-workspace (root state :chdir t)
                                                   (declare (ignore state))
                                                   (let* ((source
                                                           (format nil
                                                                   "before~C~C# A~C~Cé body~C~C## Child~C~Cinside~C~C# Next~C~Ctail~C~C"
                                                                   #\Return
                                                                   #\Newline
                                                                   #\Return
                                                                   #\Newline
                                                                   #\Return
                                                                   #\Newline
                                                                   #\Return
                                                                   #\Newline
                                                                   #\Return
                                                                   #\Newline
                                                                   #\Return
                                                                   #\Newline
                                                                   #\Return
                                                                   #\Newline))
                                                          (replacement
                                                           (format nil
                                                                   "# A revised~C~Creplacement 漢字~C~C"
                                                                   #\Return
                                                                   #\Newline
                                                                   #\Return
                                                                   #\Newline))
                                                          (path (concatenate 'string root "/notes.md"))
                                                          (ports (%md-write-ports replacement)))
                                                     (aitools.test.support:write-text path source)
                                                     (let* ((original (aitools.test.support:read-bytes path))
                                                            (hash (aitools.kernel.domain:content-hash original))
                                                            (start (search "# A" source))
                                                            (end (search "# Next" source))
                                                            (expected
                                                             (sb-ext:string-to-octets
                                                              (concatenate 'string
                                                                           (subseq source 0 start)
                                                                           replacement
                                                                           (subseq source end))
                                                              :external-format
                                                              :utf-8))
                                                            (result
                                                             (%md-flow-result #'aitools.md.application:md-replace/k
                                                                              ports
                                                                              "notes.md"
                                                                              1
                                                                              :root
                                                                              root
                                                                              :expect-hash
                                                                              hash
                                                                              :stdin
                                                                              t))
                                                            (fields
                                                             (aitools.protocol.application:command-result-fields result))
                                                            (op-id (%field fields "op_id")))
                                                       (expect (aitools.protocol.application:command-result-kind result)
                                                               :to-be
                                                               :ok)
                                                       (expect (%field fields "section") :to-be 1)
                                                       (expect (%field fields "heading") :to-equal "A")
                                                       (expect (%field fields "level") :to-be 1)
                                                       (expect (%field fields "line") :to-be 2)
                                                       (expect (stringp op-id) :to-be t)
                                                       (expect (aitools.test.support:read-bytes path)
                                                               :to-equalp
                                                               expected)
                                                       (multiple-value-bind (undo-code undo-envelope stream)
                                                           (aitools.test.support:run-aitools root (list "undo" op-id))
                                                         (expect undo-code :to-be 0)
                                                         (expect stream :to-be :stdout)
                                                         (expect
                                                          (aitools.test.support:envelope-value undo-envelope "status")
                                                          :to-equal
                                                          "ok")
                                                         (expect (aitools.test.support:read-bytes path)
                                                                 :to-equalp
                                                                 original)))))))

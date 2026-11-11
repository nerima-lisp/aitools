(in-package #:aitools.edit.test)
(describe "aitools.edit.application pipeline helpers"
  (it "inspects static write options without reading workspace state"
    (let ((ok-calls 0)
          (error-calls 0)
          (result nil))
      (aitools.edit.application::%inspect-write-plan/k
       (aitools.edit.application:make-write-plan
        :command "edit"
        :targets (list (aitools.edit.application:make-write-target "a.txt"))
        :expect-hashes (quote ("a.txt=00"))
        :expect-count "2"
        :plan (lambda (context &key on-committed on-rejected)
                (declare (ignore context on-committed on-rejected))))
       nil nil (quote ("edit" "a.txt")) "a.txt"
       :on-ok (lambda (lock-timeout-ms hash-entries expect-count)
                (incf ok-calls)
                (setf result (list lock-timeout-ms hash-entries expect-count)))
       :on-error (lambda (code message repairs)
                   (incf error-calls)
                   (setf result (list code message repairs))))
      (expect ok-calls :to-be 1)
      (expect error-calls :to-be 0)
      (expect (numberp (first result)) :to-be t)
      (expect (length (second result)) :to-be 1)
      (expect (third result) :to-be 2)))

  (it "returns the redacted-input refusal from static inspection"
    (let ((ok-calls 0)
          (error-calls 0)
          (result nil))
      (aitools.edit.application::%inspect-write-plan/k
       (aitools.edit.application:make-write-plan
        :command "edit"
        :inputs (list aitools.edit.application::+redaction-placeholder+)
        :plan (lambda (context &key on-committed on-rejected)
                (declare (ignore context on-committed on-rejected))))
       nil nil (quote ("edit")) nil
       :on-ok (lambda (lock-timeout-ms hash-entries expect-count)
                (declare (ignore lock-timeout-ms hash-entries expect-count))
                (incf ok-calls))
       :on-error (lambda (code message repairs)
                   (incf error-calls)
                   (setf result (list code message repairs))))
      (expect ok-calls :to-be 0)
      (expect error-calls :to-be 1)
      (expect result :to-equal
              (list "refusal.redacted-input"
                    (format nil "the input contains ~A, an output mask rather than real content"
                            aitools.edit.application::+redaction-placeholder+)
                    nil))))

  (it "resolves bare, target-named and additional hash paths by their area"
    (call-with-edit-workspace (lambda () (sb-posix:mkdir (disk "tmp") #o700)
      (let* ((targets (list (aitools.edit.application:make-write-target "a.txt")))
             (paths (list "a.txt"))
             (host (aitools.workspace.infrastructure:make-host-workspace-boundaries))
             (root (aitools.workspace.domain:make-workspace-root
                    :path *root* :real *root* :source :cwd))
             (temporary-root (disk "tmp")))
        (multiple-value-bind (path area)
            (aitools.edit.application::%resolve-hash-path
             (aitools.kernel.domain:parse-expect-hash-argument "00")
             targets paths :inside host root temporary-root)
          (expect path :to-equal "a.txt")
          (expect area :to-be :inside))
        (multiple-value-bind (path area)
            (aitools.edit.application::%resolve-hash-path
             (aitools.kernel.domain:parse-expect-hash-argument "a.txt=00")
             targets paths :inside host root temporary-root)
          (expect path :to-equal "a.txt")
          (expect area :to-be :inside))
        (multiple-value-bind (path area)
            (aitools.edit.application::%resolve-hash-path
             (aitools.kernel.domain:parse-expect-hash-argument "other=00")
             targets paths :inside host root temporary-root)
          (expect path :to-equal "other")
          (expect area :to-be :inside))
        (multiple-value-bind (path area)
            (aitools.edit.application::%resolve-hash-path
             (aitools.kernel.domain:parse-expect-hash-argument
              (format nil "~A=00" (disk "tmp/other")))
             targets paths :inside host root temporary-root)
          (expect path :to-equal "other")
          (expect area :to-be :temporary))))))
  (it "keeps write-plan defaults and dry-run operands stable"
    (let* ((target (aitools.edit.application:make-write-target "a.md"))
           (root-target
             (aitools.edit.application:make-write-target
              "a.md" :follow nil :base :root))
           (plan
             (aitools.edit.application:make-write-plan
              :command "edit"
              :plan (lambda (&rest arguments)
                      (declare (ignore arguments)))))
           (context (aitools.edit.application::make-write-context)))
      (expect (aitools.edit.application::write-target-follow target) :to-be t)
      (expect (aitools.edit.application::write-target-base target) :to-be :cwd)
      (expect (aitools.edit.application::write-target-follow root-target) :to-be nil)
      (expect (aitools.edit.application::write-target-base root-target) :to-be :root)
      (expect (aitools.edit.application::write-plan-targets plan) :to-equal (quote ()))
      (expect (aitools.edit.application::write-plan-inputs plan) :to-equal (quote ()))
      (expect (aitools.edit.application::write-plan-guard-requirements plan) :to-equal (quote ()))
      (expect (aitools.edit.application::write-plan-expect-hashes plan) :to-equal (quote ()))
      (expect (aitools.edit.application::write-plan-max-target-bytes plan) :to-be nil)
      (expect (aitools.edit.application::write-plan-record-options plan) :to-equal (quote ()))
      (expect (aitools.edit.application::write-context-paths context) :to-equal (quote ()))
      (expect (aitools.edit.application::write-context-hash-paths context) :to-equal (quote ()))
      (expect
       (aitools.edit.application::dry-run-command-line
        (quote ("edit" "--expect-count" "3" "a.md")))
       :to-equal "aitools edit a.md --dry-run")
      (expect
       (aitools.edit.application::dry-run-command-line
        (quote ("edit" "replace" "a.md" "--" "--expect-count" "literal")))
       :to-equal "aitools edit replace a.md --dry-run -- --expect-count literal")))
  (it "resolves discovered paths inside the workspace and rejects temporary or outside paths"
    (call-with-edit-workspace
     (lambda ()
       (sb-posix:mkdir (disk "tmp") #o700)
       (let* ((host
                (aitools.workspace.infrastructure:make-host-workspace-boundaries))
              (root
                (aitools.workspace.domain:make-workspace-root
                 :path *root* :real *root* :source :cwd))
              (temporary-root (disk "tmp"))
              (context
                (aitools.edit.application::make-write-context
                 :host host :root root :temporary-root temporary-root))
              (outside (aitools.workspace.domain:path-parent *root*))
              (result nil))
         (aitools.kernel.domain:flow-call
          (aitools.edit.application::resolve-extra-path/k context "inside.md")
          (:on-inside (relative)
            (setf result (list :inside relative)))
          (:on-outside (message)
            (setf result (list :outside message))))
         (expect result :to-equal (list :inside "inside.md"))
         (setf result nil)
         (aitools.kernel.domain:flow-call
          (aitools.edit.application::resolve-extra-path/k
           context "tmp/child.md" :base :root)
          (:on-inside (relative)
            (setf result (list :inside relative)))
          (:on-outside (message)
            (setf result (list :outside message))))
         (expect result :to-equal
                 (list :outside
                       "tmp/child.md is in the mktemp area, not the workspace"))
         (setf result nil)
         (aitools.kernel.domain:flow-call
          (aitools.edit.application::resolve-extra-path/k context outside :base :cwd)
          (:on-inside (relative)
            (setf result (list :inside relative)))
          (:on-outside (message)
            (setf result (list :outside message))))
         (expect (first result) :to-be :outside)
         (expect (search "outside the root" (second result)) :to-be-truthy)))))
  (it "forwards read errors from edit plans to rejection callbacks"
    (call-with-edit-workspace
     (lambda ()
       (let* ((view (aitools.store.application:disk-view (open-store *root*)))
              (rejected-code
                (lambda (plan paths)
                  (let ((code nil))
                    (funcall plan
                             (aitools.edit.application::make-write-context :view view :paths paths)
                             :on-committed (lambda (&rest values) (declare (ignore values)))
                             :on-rejected (lambda (value &rest values)
                                            (declare (ignore values))
                                            (setf code value)))
                    code)))
              (patch (aitools.edit.application::%make-file-patch "missing.txt" "output.txt" nil)))
         (dolist (case (list
                        (list (aitools.edit.application::%edit-plan nil) '("missing.txt"))
                        (list (aitools.edit.application::%insert-plan "x" :end nil) '("missing.txt"))
                        (list (aitools.edit.application::%apply-plan (list (cons t patch)) 0 nil)
                              '("missing.txt" "output.txt"))
                        (list (aitools.edit.application::%move-lines-plan nil :start nil nil)
                              '("missing.txt" "output.txt"))
                        (list (aitools.edit.application::%replace-plan nil nil :range '("missing.txt") nil)
                              '("missing.txt"))))
           (expect (funcall rejected-code (first case) (second case))
                   :to-equal "input.not-found"))
         (put "source.txt" "source")
         (put "broken-destination.txt" (octet-vector 97 #xFF))
         (expect
          (funcall rejected-code
                   (aitools.edit.application::%move-lines-plan
                    (aitools.kernel.domain:make-range-selector "1") :start nil nil)
                   (list "source.txt" "broken-destination.txt"))
          :to-equal "input.not-utf8")))))
  (it "checks rm file byte limits against the view"
    (call-with-edit-workspace
     (lambda ()
       (put "data.bin" "x")
       (put "empty.bin" "")
       (let* ((host (aitools.workspace.infrastructure:make-host-workspace-boundaries))
              (root (aitools.workspace.domain:make-workspace-root
                     :path *root* :real *root* :source :cwd))
              (view (aitools.store.application:disk-view (open-store *root*))))
         (labels ((run-rm (path max-bytes max-entries)
                    (let ((result nil))
                      (aitools.kernel.domain:flow-call
                       (aitools.edit.application::%rm-requests/k
                        (aitools.edit.application::make-write-context
                         :view view :host host :root root :paths (list path))
                        nil max-bytes max-entries)
                       (:on-error (code message &rest keys)
                         (declare (ignore message keys))
                         (setf result (list :error code)))
                       (:on-committed (requests &optional extra)
                         (declare (ignore extra))
                         (setf result (list :ok requests))))
                      result)))
           (expect (first (run-rm "data.bin" 5 5)) :to-be :ok)
           (expect (length (second (run-rm "data.bin" 5 5))) :to-be 1)
           (expect (first (run-rm "empty.bin" 5 5)) :to-be :ok)
           (expect (length (second (run-rm "empty.bin" 5 5))) :to-be 1)
           (expect (second (run-rm "data.bin" 0 5)) :to-equal "refusal.too-large"))))))
  (it "forwards symbol candidates from transform plan errors"
    (let ((result nil))
      (cl-weave:with-mocked-functions
          (((symbol-function 'aitools.edit.application::read-document/k)
            (lambda (context path &key prefilter max-bytes on-error on-document on-filtered)
              (declare (ignore context path prefilter max-bytes on-error on-filtered))
              (funcall on-document
                       (aitools.edit.domain:make-text-document "(defun present () 1)")))))
        (aitools.edit.application::%run-transform-plan/k
         (aitools.edit.application::make-write-context :paths '("a.lisp"))
         "a.lisp" '("sort") (aitools.kernel.domain:make-symbol-selector "presnt") nil
         :on-committed (lambda (&rest values) (declare (ignore values)))
         :on-rejected (lambda (code message &rest keys)
                        (declare (ignore message))
                        (setf result (list code keys)))))
      (expect (first result) :to-equal "selection.no-match")
      (expect (getf (second result) :candidates) :to-be-truthy))))

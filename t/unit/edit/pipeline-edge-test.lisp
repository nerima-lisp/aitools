(in-package #:aitools.edit.test)
(defun %make-write-plan-with-precommit-check (precommit-check)
  (aitools.edit.application:make-write-plan
   :command "archive"
   :plan (lambda (&rest arguments)
           (declare (ignore arguments)))
   :precommit-check precommit-check))

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
  (it "retains a precommit-check function on a write plan"
    (let* ((check (lambda () :checked))
           (plan (%make-write-plan-with-precommit-check check)))
      (expect (eq (aitools.edit.application::write-plan-precommit-check plan) check)
              :to-be t)
      (expect
       (funcall (aitools.edit.application::write-plan-precommit-check plan))
       :to-be :checked)))
  (it "rejects a non-function precommit-check"
    (signals type-error
      (%make-write-plan-with-precommit-check :not-a-function)))
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
         (expect (search "outside the root" (second result)) :to-be-truthy))))))

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
    (with-edit-workspace ()
      (sb-posix:mkdir (disk "tmp") #o700)
      (let* ((targets (list (aitools.edit.application:make-write-target "a.txt")))
             (paths (list "a.txt"))
             (host (aitools.workspace.infrastructure:make-host-workspace-host))
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

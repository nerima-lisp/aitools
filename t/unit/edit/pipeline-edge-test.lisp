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
        :expect-hashes '("a.txt=00")
        :expect-count "2"
        :plan (lambda (context commit reject)
                (declare (ignore context commit reject))))
       nil nil '("edit" "a.txt") "a.txt"
       (lambda (lock-timeout-ms hash-entries expect-count)
         (incf ok-calls)
         (setf result (list lock-timeout-ms hash-entries expect-count)))
       (lambda (code message repairs)
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
        :plan (lambda (context commit reject)
                (declare (ignore context commit reject))))
       nil nil '("edit") nil
       (lambda (lock-timeout-ms hash-entries expect-count)
         (declare (ignore lock-timeout-ms hash-entries expect-count))
         (incf ok-calls))
       (lambda (code message repairs)
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
    (let* ((targets (list (aitools.edit.application:make-write-target "a.txt")))
           (paths '("a.txt"))
           (calls 0)
           (resolve (lambda (path)
                      (incf calls)
                      (values (concatenate 'string "resolved/" path) :temporary))))
      (multiple-value-bind (path area)
          (aitools.edit.application::%resolve-hash-path
           (aitools.kernel.domain:parse-expect-hash-argument "00") targets paths :inside resolve)
        (expect path :to-equal "a.txt")
        (expect area :to-be :inside))
      (multiple-value-bind (path area)
          (aitools.edit.application::%resolve-hash-path
           (aitools.kernel.domain:parse-expect-hash-argument "a.txt=00") targets paths :inside resolve)
        (expect path :to-equal "a.txt")
        (expect area :to-be :inside))
      (multiple-value-bind (path area)
          (aitools.edit.application::%resolve-hash-path
           (aitools.kernel.domain:parse-expect-hash-argument "other=00") targets paths :inside resolve)
        (expect path :to-equal "resolved/other")
        (expect area :to-be :temporary))
      (expect calls :to-be 1))))

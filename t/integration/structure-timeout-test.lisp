(in-package #:aitools.integration.structure-test)

(defparameter *detached-spawn-exception*
  (list "packages/feature/process/src/infrastructure/bg-launcher.lisp"
        "The supervisor must outlive aitools and record the detached target exit status."))
(defun timeout-source-violations (root &optional require-detached-exception)
  (let ((violations '())
        (detached-count 0))
    (dolist (pathname (directory (merge-pathnames "packages/**/src/**/*.lisp" root)))
      (let* ((text (uiop:read-file-string pathname))
             (stripped (%strip-prose text))
             (relative (enough-namestring pathname root)))
        (dolist (call '(("process-kit:run" 3)
                        ("process-kit:communicate" 2)))
          (dolist (start (%form-positions stripped (first call)))
            (let ((form (%read-form-at text start)))
              (unless
                  (and (consp form)
                       (loop for (key value) on (nthcdr (second call) form) by #'cddr
                             thereis (and (eq key :timeout) value)))
                (push
                 (format nil "~A:~D: ~A requires a direct :TIMEOUT argument"
                         relative (%line-at text start) (string-upcase (first call)))
                 violations)))))
        (dolist (start (%form-positions stripped "process-kit:spawn-native"))
          (if (string= relative (first *detached-spawn-exception*))
              (incf detached-count)
              (push
               (format nil
                       "~A:~D: PROCESS-KIT:SPAWN-NATIVE is not the approved detached exception"
                       relative (%line-at text start))
               violations)))))
    (when require-detached-exception
      (unless (and (string= (first *detached-spawn-exception*)
                            "packages/feature/process/src/infrastructure/bg-launcher.lisp")
                   (stringp (second *detached-spawn-exception*))
                   (plusp (length (second *detached-spawn-exception*)))
                   (= detached-count 1))
        (push
         "detached spawn exception must have one occurrence and a nonempty reason"
         violations)))
    (sort violations #'string<)))
(defun timeout-fixture-violations (name)
  (timeout-source-violations
   (merge-pathnames (format nil "t/integration/fixtures/~A/" name)
                    (asdf:system-source-directory "aitools/test"))))
(describe "process timeout structure"
  (it "rejects missing direct timeouts"
    (expect
     (timeout-fixture-violations "violation-missing-timeout")
     :to-equal
     '("packages/feature/process/src/infrastructure/bad.lisp:4: PROCESS-KIT:RUN requires a direct :TIMEOUT argument"
       "packages/feature/process/src/infrastructure/bad.lisp:6: PROCESS-KIT:COMMUNICATE requires a direct :TIMEOUT argument")))

  (it "rejects nested timeout values"
    (expect
     (timeout-fixture-violations "violation-nested-timeout")
     :to-equal
     '("packages/feature/process/src/infrastructure/bad.lisp:4: PROCESS-KIT:RUN requires a direct :TIMEOUT argument"
       "packages/feature/process/src/infrastructure/bad.lisp:6: PROCESS-KIT:COMMUNICATE requires a direct :TIMEOUT argument")))

  (it "accepts direct timeouts for run and communicate"
    (expect (timeout-fixture-violations "valid-direct-timeout") :to-equal nil))

  (it "rejects spawn-native outside the detached exception"
    (expect
     (timeout-fixture-violations "violation-unapproved-spawn")
     :to-equal
     '("packages/feature/process/src/infrastructure/bad.lisp:4: PROCESS-KIT:SPAWN-NATIVE is not the approved detached exception")))

  (it "allows exactly one detached spawn with a nonempty reason"
    (expect
     (first *detached-spawn-exception*)
     :to-equal
     "packages/feature/process/src/infrastructure/bg-launcher.lisp")
    (expect (plusp (length (second *detached-spawn-exception*))) :to-be t)
    (expect (timeout-source-violations (repository-root) t) :to-equal nil)))

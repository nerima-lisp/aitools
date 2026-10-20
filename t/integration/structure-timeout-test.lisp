(in-package #:aitools.integration.structure-test)

(defparameter *detached-spawn-exception*
  (list "packages/feature/process/src/infrastructure/bg-launcher.lisp"
        "The supervisor must outlive aitools and record the detached target exit status."))

(defun timeout-source-violations (root &optional require-detached-exception)
  (setf root (uiop:ensure-directory-pathname root))
  (let ((violations '())
        (detached-count 0)
        (pathnames
         (sort
          (remove-duplicates
           (append (funcall (symbol-function '%budget-pathnames)
                            root "src/**/*.lisp")
                   (funcall (symbol-function '%budget-pathnames)
                            root "packages/**/src/**/*.lisp"))
           :test #'equal)
          #'string<
          :key (lambda (pathname) (enough-namestring pathname root)))))
    (labels ((operator-named-p (form name)
               (and (consp form)
                    (symbolp (first form))
                    (string= (symbol-name (first form)) name)))
             (process-kit-call-p (form name)
               (and (operator-named-p form name)
                    (let ((package (symbol-package (first form))))
                      (and package
                           (string= (package-name package) "PROCESS-KIT")))))
             (direct-timeout-p (form start)
               (loop for (key value) on (nthcdr start form) by #'cddr
                     thereis (and (eq key :timeout) value)))
             (timed-process-collector-p (form process)
               (and (or (process-kit-call-p form "COMMUNICATE")
                        (process-kit-call-p form "PROCESS-WAIT"))
                    (eq (second form) process)
                    (direct-timeout-p form 2)))
             (binding-variable (binding)
               (if (consp binding) (first binding) binding))
             (binding-shadows-p (bindings process)
               (some (lambda (binding)
                       (eq (binding-variable binding) process))
                     bindings))
             (contains-timed-collector-p (form process)
               (cond
                 ((atom form) nil)
                 ((or (operator-named-p form "QUOTE")
                      (operator-named-p form "FUNCTION"))
                  nil)
                 ((and (or (operator-named-p form "LET")
                           (operator-named-p form "LET*")
                           (operator-named-p form "SYMBOL-MACROLET"))
                       (binding-shadows-p (second form) process))
                  nil)
                 ((and (operator-named-p form "LAMBDA")
                       (member process (second form) :test #'eq))
                  nil)
                 ((timed-process-collector-p form process) t)
                 (t (or
                      (contains-timed-collector-p (car form) process)
                      (contains-timed-collector-p (cdr form) process)))))
             (invalid-spawn-forms (form)
               (cond
                 ((atom form) nil)
                 ((or (operator-named-p form "QUOTE")
                      (operator-named-p form "FUNCTION"))
                  nil)
                 ((or (operator-named-p form "LET")
                      (operator-named-p form "LET*"))
                  (append
                   (loop for binding in (second form)
                         when (consp binding)
                           append
                           (let ((process (binding-variable binding))
                                 (initializer (second binding)))
                             (if (process-kit-call-p initializer "SPAWN")
                                 (append
                                  (unless
                                      (and (symbolp process)
                                           (not (binding-shadows-p
                                                 (remove binding (second form)
                                                         :test #'eq)
                                                 process))
                                           (contains-timed-collector-p
                                            (rest (rest form)) process))
                                    (list initializer))
                                  (invalid-spawn-forms (rest initializer)))
                                 (invalid-spawn-forms initializer))))
                   (loop for part in (rest (rest form))
                         append (invalid-spawn-forms part))))
                 ((process-kit-call-p form "SPAWN") (list form))
                 (t (append
                      (invalid-spawn-forms (car form))
                      (invalid-spawn-forms (cdr form)))))))
      (dolist (pathname pathnames)
        (let* ((text (uiop:read-file-string pathname))
               (stripped (%strip-prose text))
               (relative (enough-namestring pathname root)))
          (dolist (call '(("process-kit:run" 3)
                          ("process-kit:communicate" 2)
                          ("process-kit:process-wait" 2)))
            (dolist (start (%form-positions stripped (first call)))
              (let ((form (%read-form-at text start)))
                (unless
                    (and (consp form)
                         (direct-timeout-p form (second call)))
                  (push
                   (format nil "~A:~D: ~A requires a direct :TIMEOUT argument"
                           relative (%line-at text start)
                           (string-upcase (first call)))
                   violations)))))
          (dolist (start (%form-positions stripped "process-kit:spawn-native"))
            (if (string= relative (first *detached-spawn-exception*))
                (incf detached-count)
                (push
                 (format nil
                         "~A:~D: PROCESS-KIT:SPAWN-NATIVE is not the approved detached exception"
                         relative (%line-at text start))
                 violations)))
          (let ((invalid-spawn-p nil)
                (reader (symbol-function '%budget-read-source-forms)))
            (dolist (platform '(:darwin :linux))
              (dolist (form (first (multiple-value-list
                                    (funcall reader pathname platform))))
                (when (invalid-spawn-forms form)
                  (setf invalid-spawn-p t))))
            (when invalid-spawn-p
              (push
               (format nil
                       "~A: PROCESS-KIT:SPAWN must be bound directly and collected by timed COMMUNICATE or PROCESS-WAIT"
                       relative)
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
      (sort violations #'string<))))

(progn
  (defun %call-with-timeout-sources (sources function)
    (aitools.test.support:call-with-workspace
     (lambda (root state-directory)
       (declare (ignore state-directory))
       (dolist (entry sources)
         (let* ((pathname
                 (merge-pathnames (first entry)
                                  (uiop:ensure-directory-pathname root)))
                (text (second entry)))
           (ensure-directories-exist pathname)
           (with-open-file
               (stream pathname :direction :output :if-exists :supersede)
             (write-string text stream))))
       (funcall function root))))
  (defun timeout-fixture-violations (name)
    (timeout-source-violations
     (merge-pathnames (format nil "t/integration/fixtures/~A/" name)
                      (asdf:system-source-directory "aitools")))))

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

  (it "scans top-level src and package source trees"
  (let ((violations
          (%call-with-timeout-sources
           '(("src/bad.lisp"
              "(defun run-it () (process-kit:run \"program\" '()))")
             ("packages/feature/process/src/infrastructure/bad.lisp"
              "(defun communicate-it (process) (process-kit:communicate process) (process-kit:process-wait process))"))
           (lambda (root) (timeout-source-violations root)))))
    (expect
     violations
     :to-equal
     '("packages/feature/process/src/infrastructure/bad.lisp:1: PROCESS-KIT:COMMUNICATE requires a direct :TIMEOUT argument"
       "packages/feature/process/src/infrastructure/bad.lisp:1: PROCESS-KIT:PROCESS-WAIT requires a direct :TIMEOUT argument"
       "src/bad.lisp:1: PROCESS-KIT:RUN requires a direct :TIMEOUT argument"))))

  (it "accepts a directly bound spawn with either timed collector"
    ;; Require a direct binding and timed collector: COMMUNICATE drains output; PROCESS-WAIT bounds waiting.
    (let ((violations
           (%call-with-timeout-sources
            (list
             (list
              "src/good.lisp"
              (format nil
                      "(defun run-it ()~%  (let ((process~%               (process-kit:spawn \"program\" '())))~%    (unwind-protect~%      (process-kit:communicate process :timeout 1)~%      (process-kit:close-process process))))"))
             (list
              "src/wait-good.lisp"
              "(defun wait-it () (let ((process (process-kit:spawn \"program\" '()))) (process-kit:process-wait process :timeout 1)))"))
            (lambda (root) (timeout-source-violations root)))))
      (expect violations :to-equal nil)))

  (it "rejects spawns without a direct binding and timed collection"
  (let ((violations
          (%call-with-timeout-sources
           '(("src/unbound.lisp"
              "(defun run-it () (process-kit:spawn \"program\" '()))")
             ("packages/feature/process/src/infrastructure/uncollected.lisp"
              "(defun run-it () (let ((process (process-kit:spawn \"program\" '()))) process))"))
           (lambda (root) (timeout-source-violations root)))))
    (expect
     violations
     :to-equal
     '("packages/feature/process/src/infrastructure/uncollected.lisp: PROCESS-KIT:SPAWN must be bound directly and collected by timed COMMUNICATE or PROCESS-WAIT"
       "src/unbound.lisp: PROCESS-KIT:SPAWN must be bound directly and collected by timed COMMUNICATE or PROCESS-WAIT"))))

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

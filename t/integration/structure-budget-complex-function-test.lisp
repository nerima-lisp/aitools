(in-package #:aitools.integration.structure-test)

(defun %complex-package-name (form)
  (let ((head (%complex-form-head-name form)))
    (when (and head
               (string-equal head "IN-PACKAGE")
               (consp (cdr form)))
      (let ((package (second form)))
        (cond
          ((symbolp package) (symbol-name package))
          ((stringp package) (string-upcase package))
          (t nil))))))

(defun complex-function-complete-inventory (root)
  (let ((depths (make-hash-table :test (function equal))))
    (dolist (pathname
              (append (%budget-pathnames root "src/**/*.lisp")
                      (%budget-pathnames root "packages/**/src/**/*.lisp")))
      (dolist (platform (list :darwin :linux))
        (let ((package-name nil))
          (dolist (form (%budget-read-source-forms pathname platform))
            (let ((declared-package (%complex-package-name form)))
              (when declared-package
                (setf package-name declared-package)))
            (multiple-value-bind (name body)
                (%complex-definition-name-and-body form)
              (when (and package-name name)
                (let* ((path (%budget-relative-path pathname root))
                       (key (list package-name path name))
                       (depth
                         (loop for body-form in body
                               maximize (%complex-control-depth body-form 0)
                                 into maximum
                               finally (return (or maximum 0)))))
                  (setf (gethash key depths)
                        (max depth (gethash key depths 0))))))))))
    (sort
     (loop for key being the hash-keys of depths using (hash-value depth)
           collect (append key (list depth)))
     (lambda (left right)
       (or (string< (first left) (first right))
           (and (string= (first left) (first right))
                (or (string< (second left) (second right))
                    (and (string= (second left) (second right))
                         (string< (third left) (third right))))))))))

(defun complex-function-baseline-violations (root)
  (let ((inventory (complex-function-complete-inventory root))
        (violations '()))
    (unless (and (listp *complex-function-baseline*)
                 *complex-function-baseline*)
      (push "complex-function baseline is empty" violations))
    (dolist (entry *complex-function-baseline*)
      (if (and (listp entry)
               (= (length entry) 5)
               (stringp (first entry))
               (stringp (second entry))
               (integerp (third entry))
               (> (third entry) *complex-function-depth-limit*)
               (stringp (fourth entry))
               (listp (fifth entry)))
          (let ((package (first entry))
                (name (second entry))
                (location (fourth entry))
                (targets (fifth entry)))
            (unless targets
              (push (format nil "~A:~A at ~A has no current successor mapping"
                            package name location)
                    violations))
            (dolist (target targets)
              (if (and (listp target)
                       (= (length target) 3)
                       (every (lambda (part) (stringp part)) target))
                  (let ((actual
                          (find target inventory
                                :key (lambda (item) (subseq item 0 3))
                                :test (function equal))))
                    (if actual
                        (when (> (fourth actual) *complex-function-depth-limit*)
                          (push (format nil "~A:~A has current depth ~D (limit ~D)"
                                        (first target) (third target)
                                        (fourth actual)
                                        *complex-function-depth-limit*)
                                violations))
                        (push (format nil "~A:~A from baseline at ~A has missing current successor ~A:~A"
                                      package name location
                                      (first target) (third target))
                              violations)))
                  (push (format nil "~A:~A from baseline at ~A has invalid successor mapping ~S"
                                package name location target)
                        violations))))
          (push (format nil "invalid complex-function baseline entry ~S" entry)
                violations)))
    (sort violations (function string<))))

(defun %complex-function-baseline-fixture-violations (depth package name target-name)
  (aitools.test.support:call-with-workspace
   (lambda (root state)
     (declare (ignore state))
     (let* ((root-pathname (uiop:ensure-directory-pathname root))
            (source-path (merge-pathnames "src/composition.lisp" root-pathname))
            (form "nil"))
       (loop repeat depth
             do (setf form (format nil "(if t ~A nil)" form)))
       (ensure-directories-exist source-path)
       (with-open-file (output source-path
                               :direction :output
                               :if-exists :supersede)
         (format output
                 "(in-package #:aitools.integration.structure-test)~%(defun fixture () ~A)~%"
                 form))
       (let ((*complex-function-baseline*
               (list
                (list package name 5 "complex-function-baseline.lisp:1"
                      (list
                       (list package "src/composition.lisp" target-name))))))
         (complex-function-baseline-violations root-pathname))))
   :prefix "aitools-complex-function-baseline"))

(describe "aitools complex-function baseline gate"
  (it "pins the 19-entry complex-function baseline"
    (expect *complex-function-baseline* :to-be-truthy)
    (expect (length *complex-function-baseline*) :to-equal 19))
  (it "requires every mapped current function to exist and stay within the depth limit"
    (expect (complex-function-baseline-violations (repository-root)) :to-equal nil))
  (it "rejects a missing current successor"
    (expect
     (some (lambda (violation)
             (search "missing current successor" violation))
           (%complex-function-baseline-fixture-violations
            0 "AITOOLS.INTEGRATION.STRUCTURE-TEST" "COMPLEX-FUNCTION-FIXTURE" "MISSING-FIXTURE"))
     :to-be-truthy))
  (it "rejects mapped current functions deeper than four"
    (expect
     (some (lambda (violation)
             (search "current depth 5 (limit 4)" violation))
           (%complex-function-baseline-fixture-violations
            5 "AITOOLS.INTEGRATION.STRUCTURE-TEST" "COMPLEX-FUNCTION-FIXTURE" "FIXTURE"))
     :to-be-truthy)))

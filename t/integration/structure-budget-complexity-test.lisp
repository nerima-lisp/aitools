(in-package #:aitools.integration.structure-test)

(defparameter *complex-function-depth-limit*
  4)

(defparameter *complex-control-forms*
  '("IF" "WHEN"
         "UNLESS"
         "COND"
         "CASE"
         "CCASE"
         "ECASE"
         "TYPECASE"
         "ETYPECASE"
         "DOLIST"
         "DOTIMES"
         "DO"
         "DO*"
         "LOOP"
         "HANDLER-CASE"
         "HANDLER-BIND"
         "RESTART-CASE"
         "UNWIND-PROTECT"))

(defun %complex-form-head-name (form)
  (and (consp form) (symbolp (first form)) (symbol-name (first form))))

(defun %complex-control-depth (form depth)
  (if (atom form)
      depth
      (let* ((head (%complex-form-head-name form))
             (quoted
              (and head
                   (member head '("QUOTE" "BACKQUOTE") :test #'string-equal)))
             (next
              (if (and head
                       (member head *complex-control-forms*
                               :test #'string-equal))
                  (1+ depth)
                  depth)))
        (if quoted
            depth
            (loop with maximum = next
                  for tail = form then (cdr tail)
                  while (consp tail)
                  do (setf maximum (max maximum
                                        (%complex-control-depth (car tail) next)))
                  finally (return maximum))))))

(defun %complex-definition-name-and-body (form)
  (let ((head (%complex-form-head-name form)))
    (cond
      ((and head (string-equal head "DEFUN"))
       (values
        (if (symbolp (second form))
            (symbol-name (second form))
            (string-upcase (prin1-to-string (second form))))
        (cdddr form)))
      ((and head (string-equal head "DEFMETHOD"))
       (let* ((tail (cddr form))
              (lambda-list-position (position-if #'consp tail)))
         (when lambda-list-position
           (values
            (if (symbolp (second form))
                (symbol-name (second form))
                (string-upcase (prin1-to-string (second form))))
            (nthcdr (1+ lambda-list-position) tail)))))
      ((and head
            (string-equal head "DEFINE-FLOW")
            (consp (second form)))
       (let* ((signature (second form))
              (name (first signature))
              (body (cddr form)))
         (values
          (if (symbolp name)
              (symbol-name name)
              (string-upcase (prin1-to-string name)))
          (if (stringp (first body))
              (rest body)
              body))))
      ((and head
            (string-equal head "DEFINE-WRITE-COMMAND")
            (stringp (second form)))
       (let* ((command-name (second form))
              (body (cdddr form)))
         (values
          (format nil
                  "PREPARE-~A/K"
                  (substitute #\- #\. (string-upcase command-name)))
          (if (stringp (first body))
              (rest body)
              body)))))))

(defun complex-function-inventory (root)
  (let ((depths (make-hash-table :test #'equal)))
    (dolist (pathname
              (append (%budget-pathnames root "src/**/*.lisp")
                      (%budget-pathnames root "packages/**/src/**/*.lisp")))
      (dolist (platform (list :darwin :linux))
        (dolist (form (%budget-read-source-forms pathname platform))
          (multiple-value-bind (name body)
              (%complex-definition-name-and-body form)
            (when name
              (let* ((key (list (%budget-relative-path pathname root) name))
                     (depth
                      (loop for body-form in body
                            maximize (%complex-control-depth body-form 0)
                              into maximum
                            finally (return (or maximum 0)))))
                (setf (gethash key depths)
                      (max depth (gethash key depths 0)))))))))
    (sort
     (loop for key being the hash-keys of depths using (hash-value depth)
           when (> depth *complex-function-depth-limit*)
             collect (list (first key) (second key) depth))
     (lambda (left right)
       (or (string< (first left) (first right))
           (and (string= (first left) (first right))
                (string< (second left) (second right))))))))

(defun complex-function-violations (root)
  (let ((remaining (copy-tree *complex-function-ratchet*))
        (violations '()))
    (dolist (actual (complex-function-inventory root))
      (let* ((path (first actual))
             (name (second actual))
             (depth (third actual))
             (match (position-if
                     (lambda (entry)
                       (and (equal path (first entry))
                            (equal name (second entry))))
                     remaining)))
        (if match
            (let ((maximum (third (nth match remaining))))
              (setf (nth match remaining) nil)
              (cond
                ((> depth maximum)
                 (push
                  (format nil
                          "~A:~A nesting depth ~D exceeds its ratchet of ~D"
                          path name depth maximum)
                  violations))
                ((< depth maximum)
                 (push
                  (format nil
                          "~A:~A nesting depth ~D is below its ratchet of ~D; lower the ratchet"
                          path name depth maximum)
                  violations))))
            (push
             (format nil
                     "~A:~A has new control nesting depth ~D (limit ~D)"
                     path name depth *complex-function-depth-limit*)
             violations))))
    (dolist (entry remaining)
      (when entry
        (push
         (format nil "~A:~A is a stale complex-function ratchet entry"
                 (first entry) (second entry))
         violations)))
    (sort violations #'string<)))

(defun %complex-budget-fixture-violations (depth ratchet
                                           &optional
                                             (definition-template
                                               "(defun fixture () ~A)~%"))
  (aitools.test.support:call-with-workspace
   (lambda (root state)
     (declare (ignore state))
     (let* ((root-pathname (uiop:ensure-directory-pathname root))
            (source-path
              (merge-pathnames "src/composition.lisp" root-pathname))
            (form "nil"))
       (loop repeat depth
             do (setf form (format nil "(if t ~A nil)" form)))
       (ensure-directories-exist source-path)
       (with-open-file (output source-path
                               :direction :output
                               :if-exists :supersede)
         (format output definition-template form))
       (let ((*complex-function-ratchet* ratchet))
         (complex-function-violations root-pathname))))
   :prefix "aitools-complex-budget"))

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

(defun complex-function-f0-violations (root)
  (let ((inventory (complex-function-complete-inventory root))
        (violations '()))
    (unless (and (listp *complex-function-f0-baseline*)
                 *complex-function-f0-baseline*)
      (push "F0 complex-function baseline is empty" violations))
    (dolist (entry *complex-function-f0-baseline*)
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
                        (push (format nil "~A:~A from F0 ~A has missing current successor ~A:~A"
                                      package name location
                                      (first target) (third target))
                              violations)))
                  (push (format nil "~A:~A from F0 ~A has invalid successor mapping ~S"
                                package name location target)
                        violations))))
          (push (format nil "invalid F0 complex-function entry ~S" entry)
                violations)))
    (sort violations (function string<))))

(defun %complex-f0-budget-fixture-violations (depth package name target-name)
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
       (let ((*complex-function-f0-baseline*
               (list
                (list package name 5 "f0-fixture.lisp:1"
                      (list
                       (list package "src/composition.lisp" target-name))))))
         (complex-function-f0-violations root-pathname))))
   :prefix "aitools-complex-f0-budget"))
(describe "aitools structure budgets (control nesting)"
  (it "keeps the fixed inventory of deeply nested functions at or below measured depths"
    (expect *complex-function-depth-limit* :to-equal 4)
    (expect (length *complex-function-ratchet*) :to-equal 0)
    (expect
     (notany (lambda (entry)
               (search "packages/feature/process/" (first entry)))
             *complex-function-ratchet*)
     :to-be-truthy)
    (expect
     (notany (lambda (entry)
               (search "packages/feature/process/" (first entry)))
             (complex-function-inventory (repository-root)))
     :to-be-truthy)
    (expect (complex-function-violations (repository-root)) :to-equal nil))
  (it "passes a clean top-level src fixture with a matching depth ratchet"
    (expect
     (%complex-budget-fixture-violations
      5
      '(("src/composition.lisp" "FIXTURE" 5)))
     :to-equal nil))
  (it "rejects new deeply nested unratcheted definitions in top-level src"
    (expect
     (some (lambda (violation)
             (search "new control nesting depth" violation))
           (%complex-budget-fixture-violations 5 nil))
     :to-be-truthy)
    (dolist (definition-template
              '("(define-flow (fixture &key on-ok) \"fixture doc\" ~A)~%"
                "(define-flow (fixture &key on-ok) ~A)~%"))
      (expect
       (some (lambda (violation)
               (and (search "new control nesting depth" violation)
                    (search "depth 5 (limit 4)" violation)
                    (search "FIXTURE" violation)))
             (%complex-budget-fixture-violations
              5 nil definition-template))
       :to-be-truthy)))
  (it "rejects nesting depths above their ratchets"
    (expect
     (some (lambda (violation)
             (search "exceeds its ratchet" violation))
           (%complex-budget-fixture-violations
            6
            '(("src/composition.lisp" "FIXTURE" 5))))
     :to-be-truthy))
  (it "rejects nesting ratchets above the measured depths"
    (expect
     (some (lambda (violation)
             (search "below its ratchet" violation))
           (%complex-budget-fixture-violations
            5
            '(("src/composition.lisp" "FIXTURE" 6))))
     :to-be-truthy))
  (it "rejects stale complex-function ratchets"
    (expect
     (some (lambda (violation)
             (search "stale complex-function ratchet entry" violation))
           (%complex-budget-fixture-violations
            4
            '(("src/composition.lisp" "FIXTURE" 5))))
     :to-be-truthy)))

(describe "aitools F0 complex-function gate"
  (it "pins all 19 F0 definitions"
    (expect *complex-function-f0-baseline* :to-be-truthy)
    (expect (length *complex-function-f0-baseline*) :to-equal 19))
  (it "requires every mapped current function to exist and stay within the depth limit"
    (expect (complex-function-f0-violations (repository-root)) :to-equal nil))
  (it "rejects a missing current successor"
    (expect
     (some (lambda (violation)
             (search "missing current successor" violation))
           (%complex-f0-budget-fixture-violations
            0 "AITOOLS.INTEGRATION.STRUCTURE-TEST" "F0-FIXTURE" "MISSING-FIXTURE"))
     :to-be-truthy))
  (it "rejects mapped current functions deeper than four"
    (expect
     (some (lambda (violation)
             (search "current depth 5 (limit 4)" violation))
           (%complex-f0-budget-fixture-violations
            5 "AITOOLS.INTEGRATION.STRUCTURE-TEST" "F0-FIXTURE" "FIXTURE"))
     :to-be-truthy)))

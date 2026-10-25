(in-package #:aitools.integration.structure-test)

(defun %literal-form-head-name (form)
  (and (consp form) (symbolp (first form)) (symbol-name (first form))))

(defun %quoted-collection-size (value)
  (cond
    ((consp value)
     (handler-case
         (list-length value)
       (type-error () nil)))
    ((and (vectorp value) (not (stringp value))) (length value))))

(defun %quoted-table-sizes (form)
  (cond
    ((and (vectorp form) (not (stringp form)))
     (let ((size (length form)))
       (if (> size *literal-table-limit*) (list size) nil)))
    ((consp form)
     (let ((head (%literal-form-head-name form)))
       (if (and head (string-equal head "QUOTE"))
           (let ((size (%quoted-collection-size (second form))))
             (if (and size (> size *literal-table-limit*))
                 (list size)
                 nil))
           (loop for tail = form then (cdr tail)
                 while (consp tail)
                 append (%quoted-table-sizes (car tail))))))
    (t nil)))

(defun literal-table-inventory (root)
  (loop for pathname in
          (append (%budget-pathnames root "src/**/*.lisp")
                  (%budget-pathnames root "packages/**/src/**/*.lisp"))
        for platform-candidates =
          (loop for platform in (list :darwin :linux)
                collect
                (sort
                 (loop for form in (%budget-read-source-forms pathname platform)
                       append (%quoted-table-sizes form))
                 #'>))
        for candidate-count =
          (loop for candidates in platform-candidates
                maximize (length candidates))
        for sizes =
          (loop for index below candidate-count
                collect
                (loop for candidates in platform-candidates
                      maximize (or (nth index candidates) 0)))
        when sizes
          collect (list (%budget-relative-path pathname root) sizes)))

(defun literal-table-violations (root)
  (let ((ratchet (copy-tree *literal-table-ratchet*))
        (inventory (literal-table-inventory root))
        (violations '()))
    (dolist (actual inventory)
      (let* ((path (first actual))
             (sizes (second actual))
             (entry (assoc path ratchet :test #'equal)))
        (if (null entry)
            (push
             (format nil "~A has unapproved quoted tables of sizes ~S"
                     path sizes)
             violations)
            (let ((maximum-sizes (sort (copy-list (second entry)) #'>)))
              (cond
                ((< (length sizes) (length maximum-sizes))
                 (push
                  (format nil
                          "~A: quoted-table ratchet is stale; expected ~D candidates, found ~D"
                          path (length maximum-sizes) (length sizes))
                  violations))
                ((> (length sizes) (length maximum-sizes))
                 (push
                  (format nil
                          "~A has ~D quoted tables, above its ratchet of ~D"
                          path (length sizes) (length maximum-sizes))
                  violations))
                (t
                 (loop for size in sizes
                       for maximum in maximum-sizes
                       do (cond
                            ((> size maximum)
                             (push
                              (format nil
                                      "~A has a quoted table of size ~D, above its ratchet of ~D"
                                      path size maximum)
                              violations))
                            ((< size maximum)
                             (push
                              (format nil
                                      "~A has a quoted table of size ~D, below its ratchet of ~D; lower the ratchet"
                                      path size maximum)
                              violations))))))))))
    (dolist (entry ratchet)
      (unless (assoc (first entry) inventory :test #'equal)
        (push
         (format nil "~A: quoted-table ratchet has no remaining candidates"
                 (first entry))
         violations)))
    (sort violations #'string<)))

(defun %literal-budget-fixture-violations (size ratchet)
  (aitools.test.support:call-with-workspace
   (lambda (root state)
     (declare (ignore state))
     (let* ((root-pathname (uiop:ensure-directory-pathname root))
            (source-path
              (merge-pathnames "src/composition.lisp" root-pathname)))
       (ensure-directories-exist source-path)
       (with-open-file (output source-path
                               :direction :output
                               :if-exists :supersede)
         (format output "(defparameter +fixture+ '~S)~%"
                 (loop for number below size collect number)))
       (let ((*literal-table-ratchet* ratchet))
         (literal-table-violations root-pathname))))
   :prefix "aitools-literal-budget"))
(describe "aitools structure budgets (quoted tables)"
  (it "ratchets quoted list and vector literals by source file"
    (expect *literal-table-limit* :to-equal 16)
    (expect
     (and
      (search "Baseline 1b20c3abdea1d95a049cdb0481afcfd72072b2a4"
              *literal-table-limit-rationale*)
      (search "289 source files" *literal-table-limit-rationale*)
      (search "13 is the largest quoted collection below threshold 16"
              *literal-table-limit-rationale*))
     :to-be-truthy)
    (expect (search "19 is the smallest above" *literal-table-limit-rationale*)
            :to-be-truthy)
    (expect (length *literal-table-ratchet*) :to-equal 0)
    (expect (literal-table-violations (repository-root)) :to-equal nil))
  (it "passes a clean top-level src fixture with matching ratchets"
    (expect
     (%literal-budget-fixture-violations
      17
      '(("src/composition.lisp" (17))))
     :to-equal nil))
  (it "rejects new quoted-table violations in top-level src"
    (expect
     (some (lambda (violation)
             (search "unapproved quoted tables" violation))
           (%literal-budget-fixture-violations 17 nil))
     :to-be-truthy))
  (it "rejects quoted-table values above their ratchets"
    (expect
     (some (lambda (violation)
             (search "above its ratchet" violation))
           (%literal-budget-fixture-violations
            18
            '(("src/composition.lisp" (17)))))
     :to-be-truthy))
  (it "rejects quoted-table ratchets above the measured values"
    (expect
     (some (lambda (violation)
             (search "below its ratchet" violation))
           (%literal-budget-fixture-violations
            17
            '(("src/composition.lisp" (18)))))
     :to-be-truthy))
  (it "rejects stale quoted-table ratchets"
    (expect
     (some (lambda (violation)
             (search "no remaining candidates" violation))
           (%literal-budget-fixture-violations
            16
            '(("src/composition.lisp" (17)))))
     :to-be-truthy)))

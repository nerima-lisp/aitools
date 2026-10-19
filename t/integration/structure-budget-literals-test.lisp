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
  (loop for pathname in (%budget-pathnames root "packages/**/src/**/*.lisp")
        for sizes = (sort
                     (loop for form in (%budget-read-source-forms pathname)
                           append (%quoted-table-sizes form))
                     #'>)
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
                       when (> size maximum)
                         do (push
                             (format nil
                                     "~A has a quoted table of size ~D, above its ratchet of ~D"
                                     path size maximum)
                             violations))))))))
    (dolist (entry ratchet)
      (unless (assoc (first entry) inventory :test #'equal)
        (push
         (format nil "~A: quoted-table ratchet has no remaining candidates"
                 (first entry))
         violations)))
    (sort violations #'string<)))

(describe "aitools structure budgets (quoted tables)"
          (it "ratchets quoted list and vector literals by source file"
              (expect *literal-table-limit* :to-equal 16)
              (expect
               (search "15 is the largest below"
                       *literal-table-limit-rationale*)
               :to-be-truthy)
              (expect
               (search "19 is the smallest above"
                       *literal-table-limit-rationale*)
               :to-be-truthy)
              (expect (length *literal-table-ratchet*) :to-equal 4)
              (expect (literal-table-violations (repository-root))
                      :to-equal
                      nil)))

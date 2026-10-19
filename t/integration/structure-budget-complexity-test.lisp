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
            (nthcdr (1+ lambda-list-position) tail))))))))

(defun complex-function-inventory (root)
  (let ((inventory '()))
    (dolist (pathname (%budget-pathnames root "packages/**/src/**/*.lisp"))
      (dolist (form (%budget-read-source-forms pathname))
        (multiple-value-bind (name body)
            (%complex-definition-name-and-body form)
          (when name
            (let ((depth
                   (loop for body-form in body
                         maximize (%complex-control-depth body-form 0) into maximum
                         finally (return (or maximum 0)))))
              (when (> depth *complex-function-depth-limit*)
                (push (list (%budget-relative-path pathname root) name depth)
                      inventory)))))))
    (sort inventory
          (lambda (left right)
            (or (string< (first left) (first right))
                (and (string= (first left) (first right))
                     (or (string< (second left) (second right))
                         (and (string= (second left) (second right))
                              (< (third left) (third right))))))))))

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
              (when (> depth maximum)
                (push
                 (format nil
                         "~A:~A nesting depth ~D exceeds its ratchet of ~D"
                         path name depth maximum)
                 violations)))
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

(describe "aitools structure budgets (control nesting)"
          (it
           "keeps the fixed inventory of deeply nested functions at or below its measured depths"
           (expect *complex-function-depth-limit* :to-equal 4)
           (expect (length *complex-function-ratchet*) :to-equal 18)
           (expect (complex-function-violations (repository-root))
                   :to-equal
                   nil)))

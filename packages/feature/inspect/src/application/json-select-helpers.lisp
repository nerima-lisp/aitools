(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (%where-comparisons texts &key on-comparisons on-error)
 "COMPARISONs keyed by relative pointer tokens for each --where text."
 (declare (type function on-comparisons on-error))
 (let ((comparisons '())
       (failure nil))
   (dolist (text texts)
     (unless failure
       (multiple-value-bind (left operator right) (split-comparison text)
         (let ((tokens (and left (parse-json-pointer left))))
           (if (or (null left) (eq tokens :invalid))
               (setf failure
                     (list
                      "argument.invalid"
                      (format nil
                              "--where ~S is not <rel-pointer><op><json-value> (op: = != < <= > >= ~~)"
                              text)
                      (list
                       (repair "where-example"
                               "Compare a member of each element, e.g. /status=\"open\"."
                               "aitools schema json select"))))
               (aitools.kernel.domain:flow-call
                (make-comparison/k tokens operator right)
                (:on-comparison (comparison)
                  (push comparison comparisons))
                (:on-error (message)
                  (setf failure
                        (list
                         "input.syntax-error"
                         message
                         (list
                          (repair "fix-pattern"
                                  "Correct the regular expression."
                                  "aitools schema json select"))))))))))
   (if failure
       (aitools.kernel.domain:flow-continue
        :on-error (first failure) (second failure)
        :repairs (third failure))
       (aitools.kernel.domain:flow-continue
        :on-comparisons (nreverse comparisons))))))

(defun %relative-value (element tokens)
  "(VALUES value present-p) of TOKENS below ELEMENT."
  (aitools.kernel.domain:flow-call
   (resolve-json-pointer/k element tokens)
   (:on-found (value)
     (values value t))
   (:on-missing (parent-tokens parent token)
     (declare (ignore parent-tokens parent token))
     (values nil nil))))

(defun %element-matches-p (element comparisons)
  (every
   (lambda (comparison)
     (multiple-value-bind (value present)
         (%relative-value element (comparison-key comparison))
       (comparison-holds-p comparison value present)))
   comparisons))

(defun %picked (element picks)
  (json-object-from-pairs
   (mapcar
    (lambda (pick)
      (cons pick
            (multiple-value-bind (value present)
                (%relative-value element (parse-json-pointer pick))
              (if present
                  value
                  (json-null)))))
    picks)))

(defun %select-indexes (array comparisons sort-tokens desc)
  "Indexes of ARRAY's elements that satisfy COMPARISONS, sorted by the
value under SORT-TOKENS when given."
  (let ((indexes
         (loop for index below (length array)
               when (%element-matches-p (aref array index) comparisons)
                 collect index)))
    (if sort-tokens
        (flet ((key (index)
                 (values (%relative-value (aref array index) sort-tokens))))
          (stable-sort indexes
                       (if desc
                           (lambda (a b)
                             (json-value-less-p (key b) (key a)))
                           (lambda (a b)
                             (json-value-less-p (key a) (key b))))))
        (if desc
            (nreverse indexes)
            indexes))))

(defun %select-fields (context target
                               pointer
                               array
                               indexes
                               picks
                               output
                               limit
                               &key
                               where)
  "(VALUES fields truncated)."
  (if (string= output "count")
      (values
       (list (cons "path" (file-target-argument target))
             (cons "pointer" pointer)
             (cons "mode" "count")
             (cons "count" (length indexes)))
       nil)
      (let* ((shown (subseq indexes 0 (min limit (length indexes))))
             (truncated (< (length shown) (length indexes))))
        (values
         (append
          (list (cons "path" (file-target-argument target))
                (cons "pointer" pointer)
                (cons "mode" "items")
                (cons "items"
                      (mapcar
                       (lambda (index)
                         (json-object "pointer"
                                      (format nil "~A/~D" pointer index)
                                      "value"
                                      (if picks
                                          (%picked (aref array index) picks)
                                          (aref array index))))
                       shown))
                (cons "total" (length indexes))
                (cons "truncated" (json-bool truncated)))
          (when truncated
            (list
             (cons "next_commands"
                   (list
                    (command-line context
                                  (append
                                   (list "json"
                                         "select"
                                         (file-target-argument target)
                                         pointer)
                                   (loop for condition in where
                                         append (list "--where" condition))
                                   (list "--output" "count"))))))))
         truncated))))

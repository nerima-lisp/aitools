;;;; TOML root, null, and integer constraints for JSON write values.
(in-package #:aitools.edit.domain)

(defun %toml-write-refusal (reason pointer)
  (refuse "input.unsupported-value"
          (format nil "TOML value at JSON Pointer ~A uses unsupported ~A."
                  (or pointer "") reason)))

(defun %toml-null-pointer (value tokens)
  (cond
    ((json-kit:json-null-p value) (format-json-pointer tokens))
    ((json-obj-p value)
     (loop for (key . item) in (json-obj-members value)
           for pointer = (%toml-null-pointer item (append tokens (list key)))
           when pointer return pointer))
    ((and (vectorp value) (not (stringp value)))
     (loop for item across value
           for index from 0
           for pointer = (%toml-null-pointer
                          item (append tokens (list (princ-to-string index))))
           when pointer return pointer))))

(defun validate-toml-write-value (value)
  (unless (json-obj-p value)
    (%toml-write-refusal "non-table-root" nil))
  (let ((pointer (%toml-null-pointer value nil)))
    (when pointer
      (%toml-write-refusal "toml-null" pointer)))
  (labels ((validate (item)
             (cond
               ((json-num-p item)
                (let ((number (edit-json-number item)))
                  (when (integerp number)
                    (unless (<= (- (expt 2 63))
                                number
                                (1- (expt 2 63)))
                      (refuse "argument.invalid"
                              "TOML integer must fit signed 64-bit.")))))
               ((json-obj-p item)
                (dolist (member (json-obj-members item))
                  (validate (cdr member))))
               ((and (vectorp item) (not (stringp item)))
                (loop for child across item do (validate child))))))
    (validate value))
  value)

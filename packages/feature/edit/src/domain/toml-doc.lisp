;;;; packages/feature/edit/src/domain/toml-doc.lisp
;;;; TOML write validation and conversion through the inspect JSON value model.
(in-package #:aitools.edit.domain)

(defun %toml-write-refusal (reason pointer)
  (refuse "input.unsupported-value"
          (format nil "TOML value at JSON Pointer ~A uses unsupported ~A."
                  pointer reason)))

(defun %toml-finite-float-p (value)
  (handler-case
      (and (= value value)
           (<= (- most-positive-double-float) value most-positive-double-float))
    (floating-point-invalid-operation () nil)))

(defun %toml-check-value (value tokens)
  (cond
    ((cl-date-kit:offset-date-time-p value)
     (%toml-write-refusal "offset-date-time" (format-json-pointer tokens)))
    ((cl-date-kit:local-date-time-p value)
     (%toml-write-refusal "local-date-time" (format-json-pointer tokens)))
    ((cl-date-kit:local-date-p value)
     (%toml-write-refusal "local-date" (format-json-pointer tokens)))
    ((cl-date-kit:local-time-p value)
     (%toml-write-refusal "local-time" (format-json-pointer tokens)))
    ((floatp value)
     (unless (%toml-finite-float-p value)
       (%toml-write-refusal "non-finite-float" (format-json-pointer tokens))))
    ((hash-table-p value)
     (maphash (lambda (key item)
                (%toml-check-value item (append tokens (list key))))
              value))
    ((and (vectorp value) (not (stringp value)))
     (loop for item across value
           for index from 0
           do (%toml-check-value item
                                 (append tokens (list (princ-to-string index))))))))

(defun parse-toml-write-document (text)
  "Return an edit JSON value and the input's TOML comment count."
  (handler-case
      (let ((native (cl-toml-kit:parse text)))
        (%toml-check-value native nil)
        (let* ((value (aitools.inspect.domain:parse-toml-document text))
               (edit-value (parse-json-text
                            (aitools.inspect.domain:render-json value))))
          (values edit-value
                  (length (cl-toml-kit:toml-comment-spans text)))))
    (edit-refusal (condition) (error condition))
    (aitools.inspect.domain:toml-unsupported-value (condition)
      (%toml-write-refusal
       (aitools.inspect.domain:toml-unsupported-reason condition)
       (aitools.inspect.domain:toml-unsupported-pointer condition)))
    (aitools.inspect.domain:toml-syntax-error ()
      (refuse "input.syntax-error" "TOML syntax error."))
    (cl-toml-kit:toml-parse-error ()
      (refuse "input.syntax-error" "TOML syntax error."))))

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

(defun %toml-native-value (value)
  (cond
    ((json-obj-p value)
     (let ((table (make-hash-table :test #'equal)))
       (dolist (member (json-obj-members value) table)
         (setf (gethash (car member) table)
               (%toml-native-value (cdr member))))))
    ((and (vectorp value) (not (stringp value)))
     (map 'simple-vector #'%toml-native-value value))
    ((json-num-p value)
     (let ((number (%edit-json-number value)))
       (when (and (integerp number)
                  (not (<= (- (expt 2 63)) number (1- (expt 2 63)))))
         (refuse "argument.invalid"
                 "TOML integer must fit signed 64-bit."))
       number))
    ((json-kit:json-false-p value) cl-toml-kit:+toml-false+)
    (t value)))

(defun serialize-toml-document (value)
  "Validate TOML's post-edit root/null rules and encode its native table."
  (unless (json-obj-p value)
    (%toml-write-refusal "non-table-root" ""))
  (let ((pointer (%toml-null-pointer value nil)))
    (when pointer (%toml-write-refusal "toml-null" pointer)))
  (cl-toml-kit:encode (%toml-native-value value)))

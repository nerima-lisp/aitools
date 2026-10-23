;;;; packages/feature/edit/src/domain/yaml-doc.lisp
;;;; YAML write validation and conversion through the inspect JSON value model.
(in-package #:aitools.edit.domain)

(defun %yaml-write-refusal (reason pointer)
  (refuse "input.unsupported-value"
          (format nil "YAML value at JSON Pointer ~A uses unsupported ~A."
                  pointer reason)))

(defun %yaml-explicit-tag (node tokens)
  (let ((tag (yaml-kit:node-tag node)))
    (when (and tag (not (string= tag "?")))
      (%yaml-write-refusal "explicit-tag" (format-json-pointer tokens)))))

(defun %yaml-anchor (node tokens)
  (when (yaml-kit:node-anchor node)
    (%yaml-write-refusal "anchor-or-alias" (format-json-pointer tokens))))

(defun %yaml-key-value (node tokens)
  (%yaml-explicit-tag node tokens)
  (%yaml-anchor node tokens)
  (unless (yaml-kit:scalar-node-p node)
    (%yaml-write-refusal "non-string-key" (format-json-pointer tokens)))
  (let ((value (yaml-kit::construct node
                                    :schema :core
                                    :mapping-type :yaml-mapping
                                    :sequence-type :vector)))
    (unless (stringp value)
      (%yaml-write-refusal "non-string-key" (format-json-pointer tokens)))
    value))

(defun %yaml-finite-float-p (value)
  (handler-case
      (and (= value value)
           (<= (- most-positive-double-float) value most-positive-double-float))
    (floating-point-invalid-operation () nil)))

(defun %yaml-check-node (node tokens visited)
  (unless (gethash node visited)
    (setf (gethash node visited) t)
    (%yaml-explicit-tag node tokens)
    (%yaml-anchor node tokens)
    (cond
      ((yaml-kit:scalar-node-p node)
       (let ((value (yaml-kit::construct node
                                         :schema :core
                                         :mapping-type :yaml-mapping
                                         :sequence-type :vector)))
         (when (floatp value)
           (unless (%yaml-finite-float-p value)
             (%yaml-write-refusal "non-finite-float"
                                  (format-json-pointer tokens))))))
      ((yaml-kit:sequence-node-p node)
       (loop for child in (yaml-kit:sequence-node-items node)
             for index from 0
             do (%yaml-check-node child
                                  (append tokens (list (princ-to-string index)))
                                  visited)))
      ((yaml-kit:mapping-node-p node)
       (let ((seen (make-hash-table :test (function equal))))
         (dolist (pair (yaml-kit:mapping-node-pairs node))
           (let ((key (%yaml-key-value (car pair) tokens)))
             (when (gethash key seen)
               (%yaml-write-refusal "duplicate-key"
                                    (format-json-pointer tokens)))
             (setf (gethash key seen) t)
             (%yaml-check-node (cdr pair)
                               (append tokens (list key))
                               visited))))))))

(defun parse-yaml-write-document (text)
  "Return the edit JSON value and YAML comment count."
  (handler-case
      (let ((nodes (yaml-kit:compose-all text)))
        (unless (= (length nodes) 1)
          (%yaml-write-refusal "document-count" ""))
        (%yaml-check-node (first nodes) nil (make-hash-table :test (function eq)))
        (let* ((value (aitools.inspect.domain:parse-yaml-document text))
               (edit-value (parse-json-text
                            (aitools.inspect.domain:render-json value))))
          (values edit-value (length (yaml-kit:comment-spans text)))))
    (edit-refusal (condition)
      (error condition))
    (aitools.inspect.domain:yaml-unsupported-value (condition)
      (%yaml-write-refusal
       (aitools.inspect.domain:yaml-unsupported-reason condition)
       (aitools.inspect.domain:yaml-unsupported-pointer condition)))
    (aitools.inspect.domain:yaml-syntax-error ()
      (refuse "input.syntax-error" "YAML syntax error."))
    (yaml-kit:yaml-kit-error ()
      (refuse "input.syntax-error" "YAML syntax error."))))

(defun %edit-json-number (value)
  (let ((text (json-num-text value)))
    (labels ((mantissa-nonzero-p ()
               (loop for character across text
                     until (find character "eE")
                     thereis (find character "123456789"))))
      (handler-case
          (let ((*read-eval* nil)
                (*read-default-float-format* 'double-float))
            (multiple-value-bind (number end)
                (read-from-string text nil nil)
              (unless (and number (= end (length text)))
                (refuse "argument.invalid" "JSON number cannot be represented."))
              (when (and (floatp number)
                         (zerop number)
                         (mantissa-nonzero-p))
                (refuse "argument.invalid" "JSON number cannot be represented."))
              (when (and (floatp number)
                         (not (%yaml-finite-float-p number)))
                (refuse "argument.invalid" "JSON number cannot be represented."))
              number))
        (floating-point-overflow ()
          (refuse "argument.invalid" "JSON number cannot be represented."))
        (floating-point-underflow ()
          (if (mantissa-nonzero-p)
              (refuse "argument.invalid" "JSON number cannot be represented.")
              0.0d0))
        (floating-point-invalid-operation ()
          (refuse "argument.invalid" "JSON number cannot be represented."))))))

(defun %yaml-native-value (value)
  (cond
    ((json-obj-p value)
     (yaml-kit:make-yaml-mapping
      (mapcar (lambda (member)
                (cons (car member) (%yaml-native-value (cdr member))))
              (json-obj-members value))))
    ((and (vectorp value) (not (stringp value)))
     (map 'simple-vector #'%yaml-native-value value))
    ((json-num-p value) (%edit-json-number value))
    ((json-kit:json-null-p value) yaml-kit:+yaml-null+)
    ((json-kit:json-false-p value) yaml-kit:+yaml-false+)
    (t value)))

(defun serialize-yaml-document (value)
  "Serialize an edit JSON value with YAML's ordered mapping representation."
  (yaml-kit:emit (%yaml-native-value value)))

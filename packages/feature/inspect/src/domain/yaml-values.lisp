(in-package #:aitools.inspect.domain)

(define-condition yaml-unsupported-value (error)
  ((reason :initarg :reason :reader yaml-unsupported-reason)
   (pointer :initarg :pointer :reader yaml-unsupported-pointer)))
(define-condition yaml-syntax-error (error) ())

(defun %yaml-unsupported (reason tokens)
  (error 'yaml-unsupported-value
         :reason reason
         :pointer (format-json-pointer tokens)))

(defun %yaml-standard-tag-p (tag)
  (member tag
          '("tag:yaml.org,2002:str" "tag:yaml.org,2002:int"
            "tag:yaml.org,2002:float" "tag:yaml.org,2002:bool"
            "tag:yaml.org,2002:null" "tag:yaml.org,2002:seq"
            "tag:yaml.org,2002:map" "!!str" "!!int" "!!float"
            "!!bool" "!!null" "!!seq" "!!map")
          :test #'string=))

(defun %yaml-node-pointer (tokens key)
  (if (and (yaml-kit:scalar-node-p key)
           (stringp (yaml-kit:scalar-node-value key)))
      (append tokens (list (yaml-kit:scalar-node-value key)))
      tokens))

(defun %yaml-validate-node (node tokens active)
  (when (gethash node active)
    (%yaml-unsupported "anchor-or-alias" tokens))
  (setf (gethash node active) t)
  (unwind-protect
       (let ((tag (yaml-kit:node-tag node)))
         (when (and tag
                    (not (member tag '("?" "!") :test #'string=))
                    (not (%yaml-standard-tag-p tag)))
           (%yaml-unsupported "explicit-tag" tokens))
         (cond
           ((yaml-kit:sequence-node-p node)
            (loop for child in (yaml-kit:sequence-node-items node)
                  for index from 0
                  do (%yaml-validate-node
                      child (append tokens (list (princ-to-string index))) active)))
           ((yaml-kit:mapping-node-p node)
            (dolist (pair (yaml-kit:mapping-node-pairs node))
              (%yaml-validate-node (car pair) tokens active)
              (%yaml-validate-node
               (cdr pair) (%yaml-node-pointer tokens (car pair)) active)))))
    (remhash node active)))

(defun %yaml-to-json (value tokens)
  (cond
    ((yaml-kit:yaml-null-p value) (json-null))
    ((yaml-kit:yaml-false-p value) (json-false))
    ((eq value t) t)
    ((stringp value) value)
    ((integerp value) value)
    ((floatp value)
     (handler-case
         (if (and (= value value)
                  (<= (- most-positive-double-float) value most-positive-double-float))
             value
             (%yaml-unsupported "non-finite-float" tokens))
       (floating-point-invalid-operation ()
         (%yaml-unsupported "non-finite-float" tokens))))
    ((yaml-kit:yaml-mapping-p value)
     (let ((seen (make-hash-table :test #'equal))
           (pairs nil))
       (dolist (pair (yaml-kit:yaml-mapping-entries value))
         (let ((key (car pair)))
           (unless (stringp key)
             (%yaml-unsupported "non-string-key" tokens))
           (when (gethash key seen)
             (%yaml-unsupported "duplicate-key" tokens))
           (setf (gethash key seen) t)
           (push (cons key (%yaml-to-json (cdr pair) (append tokens (list key))))
                 pairs)))
       (json-object-from-pairs (nreverse pairs))))
    ((and (vectorp value) (not (stringp value)))
     (coerce
      (loop for item across value
            for index from 0
            collect (%yaml-to-json item (append tokens (list (princ-to-string index)))))
      'vector))
    (t (error "unexpected YAML parser value: ~S" value))))

(defun parse-yaml-document (text)
  (handler-case
      (let ((nodes (yaml-kit:compose-all text)))
        (unless (= (length nodes) 1)
          (%yaml-unsupported "document-count" nil))
        (%yaml-validate-node (first nodes) nil (make-hash-table :test #'eq))
        (let ((documents (yaml-kit:parse-all text
                                             :schema :core
                                             :mapping-type :yaml-mapping
                                             :sequence-type :vector
                                             :duplicate-key-policy :error)))
          (%yaml-to-json (first documents) nil)))
    (yaml-kit:yaml-kit-error ()
      (error 'yaml-syntax-error))))

(defun %yaml-write-finite-float-p (value)
  (handler-case
      (and (= value value)
           (<= (- most-positive-double-float) value most-positive-double-float))
    (floating-point-invalid-operation () nil)))

(defun %yaml-write-explicit-tag (node tokens)
  (let ((tag (yaml-kit:node-tag node)))
    (when (and tag (not (string= tag "?")))
      (%yaml-unsupported "explicit-tag" tokens))))

(defun %yaml-write-anchor (node tokens)
  (when (yaml-kit:node-anchor node)
    (%yaml-unsupported "anchor-or-alias" tokens)))

(defun %yaml-write-key-value (node tokens)
  (%yaml-write-explicit-tag node tokens)
  (%yaml-write-anchor node tokens)
  (unless (yaml-kit:scalar-node-p node)
    (%yaml-unsupported "non-string-key" tokens))
  (let ((value (yaml-kit::construct node
                                    :schema :core
                                    :mapping-type :yaml-mapping
                                    :sequence-type :vector)))
    (unless (stringp value)
      (%yaml-unsupported "non-string-key" tokens))
    value))

(defun %yaml-write-check-node (node tokens visited depth max-depth)
  (let* ((container-p (or (yaml-kit:sequence-node-p node)
                          (yaml-kit:mapping-node-p node)))
         (depth (if container-p (1+ depth) depth)))
    (when (and container-p max-depth (> depth max-depth))
      (error 'yaml-syntax-error))
    (unless (gethash node visited)
      (setf (gethash node visited) t)
      (%yaml-write-explicit-tag node tokens)
      (%yaml-write-anchor node tokens)
      (cond
        ((yaml-kit:scalar-node-p node)
         (let ((value (yaml-kit::construct node
                                           :schema :core
                                           :mapping-type :yaml-mapping
                                           :sequence-type :vector)))
           (when (and (floatp value)
                      (not (%yaml-write-finite-float-p value)))
             (%yaml-unsupported "non-finite-float" tokens))))
        ((yaml-kit:sequence-node-p node)
         (loop for child in (yaml-kit:sequence-node-items node)
               for index from 0
               do (%yaml-write-check-node
                   child
                   (append tokens (list (princ-to-string index)))
                   visited depth max-depth)))
        ((yaml-kit:mapping-node-p node)
         (let ((seen (make-hash-table :test (function equal))))
           (dolist (pair (yaml-kit:mapping-node-pairs node))
             (let ((key (%yaml-write-key-value (car pair) tokens)))
               (when (gethash key seen)
                 (%yaml-unsupported "duplicate-key" tokens))
               (setf (gethash key seen) t)
               (%yaml-write-check-node
                (cdr pair)
                (append tokens (list key))
                visited depth max-depth)))))))))

(defun parse-yaml-write-document (text &key max-input-length max-depth)
  (handler-case
      (let* ((options
               (append (when max-input-length
                         (list :max-input-length max-input-length))
                       (when max-depth
                         (list :max-depth max-depth))))
             (nodes (apply #'yaml-kit:compose-all text options)))
        (unless (= (length nodes) 1)
          (%yaml-unsupported "document-count" nil))
        (%yaml-write-check-node
         (first nodes) nil (make-hash-table :test (function eq)) 0 max-depth)
        (values (parse-yaml-document text)
                (length
                 (if max-input-length
                     (yaml-kit:comment-spans
                      text :max-input-length max-input-length)
                     (yaml-kit:comment-spans text)))))
    (yaml-kit:yaml-kit-error ()
      (error 'yaml-syntax-error))))

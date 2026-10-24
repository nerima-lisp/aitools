;;;; packages/core/kernel/src/domain/json-model.lisp
;;;;
;;;; JSON value-model operations and JSON Pointer traversal.
;;;; The model supplies only type predicates, ordered object members, array
;;;; elements, constructors, null detection, string handling, and equality.
(in-package #:aitools.kernel.domain)

(defstruct (json-value-model
            (:constructor make-json-value-model
                (&key object-p object-members object-from-members
                      array-p array-elements array-from-elements array-insert array-remove
                      null-p string-p string-text string-length equal))
            (:copier nil))
  (object-p nil :type function :read-only t)
  ;; OBJECT-MEMBERS returns ordered (KEY . VALUE) pairs as the model's
  ;; traversal view; the value itself need not be an alist.
  (object-members nil :type function :read-only t)
  (object-from-members nil :type function :read-only t)
  (array-p nil :type function :read-only t)
  (array-elements nil :type function :read-only t)
  (array-from-elements nil :type function :read-only t)
  ;; These callbacks preserve the model's array representation during edits.
  (array-insert nil :type function :read-only t)
  (array-remove nil :type function :read-only t)
  (null-p nil :type function :read-only t)
  ;; String values and JSON Patch pointer members are CL strings in the JSON,
  ;; YAML, and TOML adapters. Other representations provide their predicate,
  ;; pointer-text conversion, and length at this boundary.
  (string-p nil :type function :read-only t)
  (string-text nil :type function :read-only t)
  (string-length nil :type function :read-only t)
  (equal nil :type function :read-only t))

(define-condition json-model-error (error)
  ((code :initarg :code :reader json-model-error-code)
   (message :initarg :message :reader json-model-error-message))
  (:report (lambda (condition stream)
             (write-string (json-model-error-message condition) stream))))

(defun %json-model-error (code control &rest arguments)
  (error 'json-model-error :code code :message (apply #'format nil control arguments)))

(defun %json-model-object-get (model object key)
  (let ((member (assoc key (reverse (funcall (json-value-model-object-members model) object))
                     :test #'string=)))
    (if member
        (values (cdr member) t)
        (values nil nil))))

(defun %json-model-object-put (model object key value)
  (let ((members (funcall (json-value-model-object-members model) object)))
    (funcall (json-value-model-object-from-members model)
             (if (assoc key members :test #'string=)
                 (mapcar (lambda (member)
                           (if (string= (car member) key)
                               (cons key value)
                               member))
                         members)
                 (append members (list (cons key value)))))))

(defun %json-model-object-remove (model object key)
  (funcall (json-value-model-object-from-members model)
           (remove key (funcall (json-value-model-object-members model) object)
                   :key #'car :test #'string=)))

(defun %json-model-array-elements (model array)
  (funcall (json-value-model-array-elements model) array))

(defun %json-model-array-from-elements (model elements)
  (funcall (json-value-model-array-from-elements model) elements))

(defun %json-model-array-insert (model array index value)
  (funcall (json-value-model-array-insert model) array index value))

(defun %json-model-array-remove (model array index)
  (funcall (json-value-model-array-remove model) array index))

(defun %json-model-missing (tokens)
  (%json-model-error "input.not-found" "no value at JSON pointer ~S"
                     (format-json-pointer tokens)))

(defun json-model-pointer-get (model value tokens)
  (let ((current value)
        (seen '()))
    (dolist (token tokens current)
      (push token seen)
      (multiple-value-bind (child present)
          (json-model-child model current token)
        (unless present
          (%json-model-missing (nreverse seen)))
        (setf current child)))))

(defun %json-model-update-at (model value tokens function &optional (path '()))
  (if (null (rest tokens))
      (funcall function value (first tokens))
      (let ((token (first tokens)))
        (cond
          ((funcall (json-value-model-object-p model) value)
           (multiple-value-bind (child present)
               (%json-model-object-get model value token)
             (unless present
               (%json-model-missing (append path (list token))))
             (funcall (json-value-model-object-from-members model)
                      (mapcar (lambda (member)
                                (if (string= (car member) token)
                                    (cons (car member)
                                          (%json-model-update-at model child (rest tokens) function
                                                                  (append path (list token))))
                                    member))
                              (funcall (json-value-model-object-members model) value)))))
          ((funcall (json-value-model-array-p model) value)
           (let* ((elements (%json-model-array-elements model value))
                  (index (json-pointer-array-index token (length elements))))
             (unless index
               (%json-model-missing (append path (list token))))
             (setf elements (copy-seq elements)
                   (elt elements index)
                   (%json-model-update-at model (elt elements index) (rest tokens) function
                                           (append path (list token))))
             (%json-model-array-from-elements model elements)))
          (t (%json-model-missing (append path (list token))))))))

(defun json-model-add (model value tokens new &key (array-mode :insert))
  (if (null tokens)
      new
      (%json-model-update-at
       model value tokens
       (lambda (container token)
         (cond
           ((funcall (json-value-model-object-p model) container)
            (%json-model-object-put model container token new))
           ((funcall (json-value-model-array-p model) container)
            (let* ((elements (%json-model-array-elements model container))
                   (index (json-pointer-array-index token (length elements) :allow-end t)))
              (unless index
                (%json-model-missing (list token)))
              (if (and (eq array-mode :replace)
                       (< index (length elements))
                       (string/= token "-"))
                  (progn
                    (setf elements (copy-seq elements)
                          (elt elements index) new)
                    (%json-model-array-from-elements model elements))
                  (%json-model-array-insert model container index new))))
           (t (%json-model-missing (list token))))))))

(defun json-model-remove (model value tokens)
  (when (null tokens)
    (%json-model-error "argument.invalid" "the root value cannot be removed"))
  (%json-model-update-at
   model value tokens
   (lambda (container token)
     (cond
       ((funcall (json-value-model-object-p model) container)
        (multiple-value-bind (ignored present)
            (%json-model-object-get model container token)
          (declare (ignore ignored))
          (unless present
            (%json-model-missing (list token)))
          (%json-model-object-remove model container token)))
       ((funcall (json-value-model-array-p model) container)
        (let* ((elements (%json-model-array-elements model container))
               (index (json-pointer-array-index token (length elements))))
          (unless index
            (%json-model-missing (list token)))
          (%json-model-array-remove model container index)))
       (t (%json-model-missing (list token)))))))

(defun json-model-replace (model value tokens new)
  (json-model-pointer-get model value tokens)
  (if (null tokens)
      new
      (%json-model-update-at
       model value tokens
       (lambda (container token)
         (cond
           ((funcall (json-value-model-object-p model) container)
            (%json-model-object-put model container token new))
           ((funcall (json-value-model-array-p model) container)
            (let* ((elements (%json-model-array-elements model container))
                   (index (json-pointer-array-index token (length elements))))
              (setf elements (copy-seq elements)
                    (elt elements index) new)
              (%json-model-array-from-elements model elements)))
           (t (%json-model-missing (list token))))))))

(defun json-model-equal (model left right)
  (funcall (json-value-model-equal model) left right))

(defun json-model-child (model value token)
  (cond
    ((funcall (json-value-model-object-p model) value)
     (%json-model-object-get model value token))
    ((funcall (json-value-model-array-p model) value)
     (let* ((elements (%json-model-array-elements model value))
            (index (json-pointer-array-index token (length elements))))
       (if index
           (values (elt elements index) t)
           (values nil nil))))
    (t (values nil nil))))

(defun json-model-child-names (model value)
  (cond
    ((funcall (json-value-model-object-p model) value)
     (mapcar #'car (funcall (json-value-model-object-members model) value)))
    ((funcall (json-value-model-array-p model) value)
     (loop for index below (length (%json-model-array-elements model value))
           collect (princ-to-string index)))
    (t '())))

(defun json-model-value-length (model value)
  (cond
    ((funcall (json-value-model-object-p model) value)
     (length (funcall (json-value-model-object-members model) value)))
    ((funcall (json-value-model-array-p model) value)
     (length (%json-model-array-elements model value)))
    ((funcall (json-value-model-string-p model) value)
     (funcall (json-value-model-string-length model) value))
    (t nil)))

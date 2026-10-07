;;;; packages/core/kernel/src/domain/json-model.lisp
;;;;
;;;; JSON Pointer traversal and RFC 6902/7386 operations over a value model.
;;;; The model supplies only type predicates, ordered object members, array
;;;; elements, constructors, null detection, and equality classification.
(in-package #:aitools.kernel.domain)

(defstruct (json-value-model
            (:constructor make-json-value-model
                (&key object-p object-members object-from-members
                      array-p array-elements array-from-elements null-p classify))
            (:copier nil))
  (object-p nil :type function :read-only t)
  (object-members nil :type function :read-only t)
  (object-from-members nil :type function :read-only t)
  (array-p nil :type function :read-only t)
  (array-elements nil :type function :read-only t)
  (array-from-elements nil :type function :read-only t)
  (null-p nil :type function :read-only t)
  (classify nil :type function :read-only t))

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
                  (%json-model-array-from-elements
                   model
                   (concatenate 'list (subseq elements 0 index) (list new) (subseq elements index))))))
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
          (%json-model-array-from-elements
           model (concatenate 'list (subseq elements 0 index) (subseq elements (1+ index))))))
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
  (json-equal left right (json-value-model-classify model)))

(defun json-model-merge-patch (model target patch)
  (if (not (funcall (json-value-model-object-p model) patch))
      patch
      (let ((result (if (funcall (json-value-model-object-p model) target)
                        target
                        (funcall (json-value-model-object-from-members model) '()))))
        (dolist (member (funcall (json-value-model-object-members model) patch) result)
          (let ((key (car member))
                (value (cdr member)))
            (setf result
                  (if (funcall (json-value-model-null-p model) value)
                      (%json-model-object-remove model result key)
                      (%json-model-object-put
                       model result key
                       (json-model-merge-patch
                        model
                        (multiple-value-bind (existing present)
                            (%json-model-object-get model result key)
                          (and present existing))
                        value)))))))))

(defun %json-model-operation-field (model operation name &key required)
  (multiple-value-bind (value present)
      (%json-model-object-get model operation name)
    (when (and required (not present))
      (%json-model-error "argument.invalid" "JSON Patch operation lacks ~S" name))
    (values value present)))

(defun %json-model-operation-pointer (model operation name)
  (multiple-value-bind (text ignored)
      (%json-model-operation-field model operation name :required t)
    (declare (ignore ignored))
    (unless (stringp text)
      (%json-model-error "argument.invalid" "JSON Patch member ~S must be a string" name))
    (let ((tokens (parse-json-pointer text)))
      (when (eq tokens :invalid)
        (%json-model-error "argument.invalid" "JSON pointer ~S is malformed" text))
      tokens)))

(defun json-model-apply-patch (model value operations)
  (unless (funcall (json-value-model-array-p model) operations)
    (%json-model-error "argument.invalid" "a JSON Patch document must be an array"))
  (loop with result = value
        for operation in (coerce (%json-model-array-elements model operations) 'list)
        for index from 0
        do (progn
             (unless (funcall (json-value-model-object-p model) operation)
               (%json-model-error "argument.invalid"
                                  "JSON Patch operation ~D is not an object" index))
             (multiple-value-bind (op ignored)
                 (%json-model-operation-field model operation "op" :required t)
               (declare (ignore ignored))
               (setf result
                     (cond
                     ((equal op "add")
                      (multiple-value-bind (new present)
                          (%json-model-operation-field model operation "value" :required t)
                        (declare (ignore present))
                        (json-model-add model result
                                        (%json-model-operation-pointer model operation "path") new)))
                     ((equal op "remove")
                      (json-model-remove model result
                                         (%json-model-operation-pointer model operation "path")))
                     ((equal op "replace")
                      (multiple-value-bind (new present)
                          (%json-model-operation-field model operation "value" :required t)
                        (declare (ignore present))
                        (json-model-replace model result
                                            (%json-model-operation-pointer model operation "path") new)))
                     ((equal op "move")
                      (let ((from (%json-model-operation-pointer model operation "from"))
                            (path (%json-model-operation-pointer model operation "path")))
                        (cond
                          ((equal from path) result)
                          ((and (< (length from) (length path))
                                (equal from (subseq path 0 (length from))))
                           (%json-model-error "argument.invalid"
                                              "JSON Patch cannot move a value into itself"))
                          (t (let ((moved (json-model-pointer-get model result from)))
                               (json-model-add model (json-model-remove model result from) path moved))))))
                     ((equal op "copy")
                      (json-model-add model result
                                      (%json-model-operation-pointer model operation "path")
                                      (json-model-pointer-get
                                       model result
                                       (%json-model-operation-pointer model operation "from"))))
                     ((equal op "test")
                      (let ((path (%json-model-operation-pointer model operation "path")))
                        (multiple-value-bind (expected present)
                            (%json-model-operation-field model operation "value" :required t)
                          (declare (ignore present))
                          (unless (json-model-equal model
                                                    (json-model-pointer-get model result path)
                                                    expected)
                            (%json-model-error
                             "selection.no-match"
                             "JSON Patch test failed at ~S (operation ~D)"
                             (format-json-pointer path) index)))
                        result))
                       (t (%json-model-error "argument.invalid" "unknown JSON Patch op ~S" op))))))
        finally (return result)))

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

(defun json-model-resolve/k (model document tokens &key on-found on-missing)
  (declare (type function on-found on-missing))
  (let ((value document))
    (loop for token in tokens
          for depth from 0
          do
             (multiple-value-bind (child present) (json-model-child model value token)
               (unless present
                 (return-from json-model-resolve/k
                   (funcall on-missing (subseq tokens 0 depth) value token)))
               (setf value child)))
    (funcall on-found value)))

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
    ((stringp value) (length value))
    (t nil)))

(defun json-model-diff-ops (model left right &optional (path '()))
  (cond
    ((and (funcall (json-value-model-object-p model) left)
          (funcall (json-value-model-object-p model) right))
     (let ((keys-left (mapcar #'car (funcall (json-value-model-object-members model) left)))
           (keys-right (mapcar #'car (funcall (json-value-model-object-members model) right))))
       (append
        (loop for key in keys-left
              for (value-right present) = (multiple-value-list (%json-model-object-get model right key))
              append (if present
                         (json-model-diff-ops
                          model
                          (nth-value 0 (%json-model-object-get model left key))
                          value-right
                          (append path (list key)))
                         (list (list :remove (append path (list key))
                                     (nth-value 0 (%json-model-object-get model left key)) nil))))
        (loop for key in keys-right
              unless (member key keys-left :test #'string=)
                collect (list :add (append path (list key)) nil
                              (nth-value 0 (%json-model-object-get model right key)))))))
    ((and (funcall (json-value-model-array-p model) left)
          (funcall (json-value-model-array-p model) right))
     (let ((elements-left (%json-model-array-elements model left))
           (elements-right (%json-model-array-elements model right)))
       (append
        (loop for index below (min (length elements-left) (length elements-right))
              append (json-model-diff-ops model (elt elements-left index) (elt elements-right index)
                                          (append path (list (princ-to-string index)))))
        (loop for index from (1- (length elements-left)) downto (length elements-right)
              collect (list :remove (append path (list (princ-to-string index)))
                            (elt elements-left index) nil))
        (loop for index from (length elements-left) below (length elements-right)
              collect (list :add (append path (list (princ-to-string index))) nil
                            (elt elements-right index))))))
    ((json-model-equal model left right) '())
    (t (list (list :replace path left right)))))

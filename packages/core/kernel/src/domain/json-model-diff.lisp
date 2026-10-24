(in-package #:aitools.kernel.domain)

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

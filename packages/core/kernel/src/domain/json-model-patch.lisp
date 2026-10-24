(in-package #:aitools.kernel.domain)

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
  (multiple-value-bind (value ignored)
      (%json-model-operation-field model operation name :required t)
    (declare (ignore ignored))
    (unless (funcall (json-value-model-string-p model) value)
      (%json-model-error "argument.invalid" "JSON Patch member ~S must be a string" name))
    (let* ((text (funcall (json-value-model-string-text model) value))
           (tokens (parse-json-pointer text)))
      (when (eq tokens :invalid)
        (%json-model-error
         "argument.invalid"
         "JSON pointer ~S must be empty or start with /, with valid ~~ escapes"
         text))
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
                      (let ((path (%json-model-operation-pointer model operation "path")))
                        (multiple-value-bind (new present)
                            (%json-model-operation-field model operation "value" :required t)
                          (declare (ignore present))
                          (json-model-add model result path new))))
                     ((equal op "remove")
                      (json-model-remove model result
                                         (%json-model-operation-pointer model operation "path")))
                     ((equal op "replace")
                      (let ((path (%json-model-operation-pointer model operation "path")))
                        (multiple-value-bind (new present)
                            (%json-model-operation-field model operation "value" :required t)
                          (declare (ignore present))
                          (json-model-replace model result path new))))
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

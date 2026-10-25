"(in-package #:aitools.kernel.domain)

( setf ( fdefinition 'json-model-equal) #'identity)
(setf (symbol-function 'json-model-pointer-get) #'identity)
(defun handwritten-stub-fixture () nil)
(cl:setf (fdefinition 'json-model-pointer-put) #'identity)
(cl:defun handwritten-cl-defun-fixture () nil)
#; \"this reader-commented string must not hide the next form\" (setf (symbol-function 'handwritten-after-reader-string) #'identity)
`(,(setf (fdefinition '(setf json-model-equal)) #'identity))
``(,(setf (symbol-function '(setf json-model-pointer-get)) #'identity))
``(,,(setf (symbol-function '(setf json-model-pointer-put)) #'identity))
(defun (setf handwritten-defun-pointer) (value object) (declare (ignore value object)))
(funcall #'(lambda () (setf (symbol-function 'handwritten-function-quoted-target) #'identity)))
"

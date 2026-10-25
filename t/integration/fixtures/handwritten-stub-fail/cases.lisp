"(in-package #:aitools.kernel.domain)

( setf ( fdefinition 'json-model-equal) #'identity)
(setf (symbol-function 'json-model-pointer-get) #'identity)
(defun handwritten-stub-fixture () nil)
(cl:setf (fdefinition 'json-model-pointer-put) #'identity)
(cl:defun handwritten-cl-defun-fixture () nil)
#; \"this reader-commented string must not hide the next form\" (setf (symbol-function 'handwritten-after-reader-string) #'identity)
"

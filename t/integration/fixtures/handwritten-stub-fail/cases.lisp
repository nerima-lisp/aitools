"(in-package #:aitools.kernel.domain)
(setf (fdefinition 'json-model-equal) #'identity)
(setf (symbol-function 'json-model-pointer-get) #'identity)
(setf (macro-function 'json-model-pointer-put) #'identity)
(defun handwritten-stub-fixture () nil)
(cl:setf (fdefinition 'handwritten-cl-setf-fixture) #'identity)
(setf (fdefinition dynamic-target) #'identity)
(defun aitools/cli::json-model-equal () nil)"

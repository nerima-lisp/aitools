(in-package #:cl-user)
; (setf (symbol-function 'aitools.kernel.domain::json-model-equal) #'identity)
(defparameter *quoted-example*
  "(defun AITOOLS.KERNEL.DOMAIN::JSON-MODEL-EQUAL () nil)")
#; (setf (fdefinition 'aitools.kernel.domain::json-model-equal) #'identity)
'(setf (symbol-function 'aitools.kernel.domain::json-model-equal) #'identity)
'(defun aitools.kernel.domain::json-model-equal () nil)
`(setf (fdefinition 'aitools.kernel.domain::json-model-pointer-get) #'identity)
`(defun aitools.kernel.domain::json-model-pointer-get () nil)
(aitools.workspace.application:make-workspace-host
 :read-octets (lambda (path) (declare (ignore path)) #()))
(defun instrument-by-variable (target replacement)
  (setf (symbol-function target) replacement))

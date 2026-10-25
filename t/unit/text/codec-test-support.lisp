(in-package #:aitools.text.test)

(defun only-archive-errors-p (function input)
  "True when FUNCTION on INPUT returns or signals ARCHIVE-ERROR, never any
other condition (an index error would mean an unchecked read)."
  (handler-case (progn (funcall function input) t)
    (archive-error () t)
    (error () nil)))

(defparameter *dynamic-vector*
  "edcea901c0400c03b059edc479f7e73dde15428504768ccad2179cdc682f8136891c75d07c0bcf27cd85666d6cb98263fdbc1469ebc3422bcaa1b5e6449633e0639bd56250eba89eb0a420f66c7a5940368b527b326c709deb5ce73ad7b9ce75ae739deb5ce7dff900")

(defun dynamic-vector-input ()
  "bytes(97 + (i**3 + i//13) % 11 for i in range(3000))"
  (let ((result (make-array 3000 :element-type '(unsigned-byte 8))))
    (dotimes (i 3000 result)
      (setf (aref result i) (+ 97 (mod (+ (* i i i) (floor i 13)) 11))))))

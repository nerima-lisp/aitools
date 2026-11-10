(in-package #:aitools.md.infrastructure)

(defun %read-bounded (stream limit)
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (chunk (make-array 4096 :element-type '(unsigned-byte 8)))
        (done nil)
        (too-large nil))
    (loop until (or done too-large)
          do (let ((count (read-sequence chunk stream :end (min (length chunk) (- (1+ limit) (fill-pointer buffer))))))
               (if (zerop count)
                   (setf done t)
                   (loop for index below
                         count
                         do (vector-push-extend (aref chunk index) buffer)))
               (when (> (fill-pointer buffer) limit)
                 (setf too-large t)))
          finally (return
                   (if too-large
                       (list :too-large)
                       (list :octets (coerce buffer '(simple-array (unsigned-byte 8) (*)))))))))

(defun read-stdin-octets (limit)
  (handler-case (let ((stream (sb-sys:make-fd-stream 0 :input t :element-type '(unsigned-byte 8) :buffering :full)))
                  (%read-bounded stream limit))
    (error (condition)
      (list :failure (princ-to-string condition)))))


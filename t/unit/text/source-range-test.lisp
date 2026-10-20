(in-package #:aitools.text.test)

(describe "aitools.text.application source-read-range errors"
  (it "preserves an unreadable FILE-SIZE result"
    (let ((source
           (make-text-source
            :file-size (lambda (path) (declare (ignore path)) (values nil :unreadable))
            :read-prefix (lambda (path count) (declare (ignore path count)) nil)
            :read-octets (lambda (path) (declare (ignore path)) nil)
            :call-with-chunks (lambda (path size function)
                                (declare (ignore path size function))
                                t))))
      (expect (multiple-value-list (source-read-range source "/locked" 0 2))
              :to-equal (quote (nil :unreadable))))))

(describe "aitools.text.application source-read-range"
  (it "returns an empty range without reading chunks"
    (let* ((chunk-reads 0)
           (source
             (make-text-source
              :file-size (lambda (path) (declare (ignore path)) 4)
              :read-prefix (lambda (path count)
                             (declare (ignore path count)) nil)
              :read-octets (lambda (path) (declare (ignore path)) nil)
              :call-with-chunks (lambda (path size function)
                                  (declare (ignore path size function))
                                  (incf chunk-reads)
                                  t))))
      (multiple-value-bind (range size) (source-read-range source "/range" 2 2)
        (expect range :to-equalp (octets))
        (expect size :to-be 4))
      (multiple-value-bind (range size) (source-read-range source "/range" 10 20)
        (expect range :to-equalp (octets))
        (expect size :to-be 4))
      (expect chunk-reads :to-be 0)))
  (it "reports a missing file when its size is unavailable"
    (multiple-value-bind (range size)
        (source-read-range (fake-source (quote ())) "/missing" 0 1)
      (expect range :to-be nil)
      (expect size :to-be nil)))
  (it "copies a range across chunks and stops at its end"
    (let* ((bytes (make-array (* 3 65536)
                              :element-type (quote (unsigned-byte 8))
                              :initial-element 0))
           (chunk-reads 0)
           (source
             (make-text-source
              :file-size (lambda (path)
                           (declare (ignore path))
                           (length bytes))
              :read-prefix (lambda (path count)
                             (declare (ignore path count))
                             nil)
              :read-octets (lambda (path)
                             (declare (ignore path))
                             nil)
              :call-with-chunks
              (lambda (path chunk-size function)
                (declare (ignore path))
                (loop for start from 0 below (length bytes) by chunk-size
                      for end = (min (length bytes) (+ start chunk-size))
                      do (incf chunk-reads)
                         (when (eq (funcall function (subseq bytes start end)) :stop)
                           (return t))
                      finally (return t))))))
      (loop for offset from 65534 below 65539
            for value from 1
            do (setf (aref bytes offset) value))
      (multiple-value-bind (range size)
          (source-read-range source "/large" 65534 65539)
        (expect range :to-equalp (octets 1 2 3 4 5))
        (expect size :to-be (* 3 65536))
        (expect chunk-reads :to-be 2))))
  (it "reports an unavailable range when chunk reading fails"
    (let ((source (make-text-source
                   :file-size (lambda (path) (declare (ignore path)) 4)
                   :read-prefix (lambda (path count)
                                  (declare (ignore path count)) nil)
                   :read-octets (lambda (path) (declare (ignore path)) nil)
                   :call-with-chunks (lambda (path size function)
                                       (declare (ignore path size function)) nil))))
      (multiple-value-bind (range size)
          (source-read-range source "/unreadable" 1 3)
        (expect range :to-be nil)
        (expect size :to-be nil)))))

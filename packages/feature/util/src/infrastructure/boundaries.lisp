;;;; Host readers and production capabilities for the util composition context.
(in-package #:aitools.util.infrastructure)

(defun %unix-ms ()
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ (* seconds 1000) (floor microseconds 1000))))

(defun %read-bounded (stream limit)
  "Read at most LIMIT plus one octet to distinguish a complete input from an oversized one."
  (let ((buffer (make-array (min (1+ limit) 65536) :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0))
        (chunk (make-array 65536 :element-type '(unsigned-byte 8))))
    (loop for count = (read-sequence chunk stream
                                     :end (min (length chunk)
                                               (- (1+ limit) (fill-pointer buffer))))
          until (zerop count)
          do (loop for index below count
                   do (vector-push-extend (aref chunk index) buffer)))
    (if (> (fill-pointer buffer) limit)
        (values :too-large nil)
        (values :octets (coerce buffer '(simple-array (unsigned-byte 8) (*)))))))

(defun read-file-octets (path limit)
  "Read PATH literally, relative to the process's current directory."
  (let ((outcome
          (handler-case
              (let ((stream (open (uiop:parse-native-namestring path)
                                  :element-type '(unsigned-byte 8)
                                  :if-does-not-exist nil)))
                (if stream
                    (unwind-protect
                         (multiple-value-list (%read-bounded stream limit))
                      (close stream))
                    (list :missing nil)))
            (error (condition)
              (list :failure (princ-to-string condition))))))
    (values-list outcome)))

(defun read-stdin-octets (limit)
  "Read file descriptor 0 as raw octets, independent of the character external format."
  (let ((outcome
          (handler-case
              (multiple-value-list
               (%read-bounded
                (sb-sys:make-fd-stream 0 :input t :element-type '(unsigned-byte 8)
                                       :buffering :full)
                limit))
            (error (condition)
              (list :failure (princ-to-string condition))))))
    (values-list outcome)))

(defun make-production-util-capabilities ()
  (let ((random-source (make-os-random-source)))
    (list
     (cons :util-random-octets
           (lambda (count)
             (cl-boundary-kit:random-source-bytes random-source count)))
     (cons :util-uuid-v4
           (let ((uuid-source
                   (cl-boundary-kit:make-uuid-source
                    :generate-fn
                    (lambda ()
                      (aitools.util.domain:uuid-v4-from-octets
                       (cl-boundary-kit:random-source-bytes random-source 16))))))
             (lambda () (cl-boundary-kit:uuid-generate uuid-source))))
     (cons :util-unix-ms
           (let ((clock (cl-boundary-kit:make-clock :now-fn #'%unix-ms)))
             (lambda () (cl-boundary-kit:clock-now clock))))
     (cons :util-read-file-octets #'read-file-octets)
     (cons :util-read-stdin-octets #'read-stdin-octets))))

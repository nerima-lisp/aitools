(in-package #:aitools.text.integration-test)

(describe "aitools text production source ranged read"
  (it "reads sparse ranges from zero, the middle, and the end of a file"
    (call-with-scratch-directory (lambda (scratch)
      (let ((path (concatenate 'string scratch "/sparse.bin"))
            (size (* 64 1024 1024))
            (offset (* 40 1024 1024)))
        (write-file path "")
        (sb-posix:truncate path size)
        (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output
                             :element-type '(unsigned-byte 8) :if-exists :overwrite)
          (file-position out offset)
          (write-sequence (sb-ext:string-to-octets "window" :external-format :utf-8) out))
        (multiple-value-bind (octets reported)
            (funcall (cl-boundary-kit:boundary-context-require (aitools.text.infrastructure:make-host-text-source) :text-read-range) path offset (+ offset 6))
          (expect (text octets) :to-equal "window")
          (expect reported :to-be size))
        (multiple-value-bind (octets reported)
            (funcall (cl-boundary-kit:boundary-context-require (aitools.text.infrastructure:make-host-text-source) :text-read-range) path 0 6)
          (expect (length octets) :to-be 6)
          (expect (every (function zerop) octets) :to-be t)
          (expect reported :to-be size))
        (multiple-value-bind (octets reported)
            (funcall (cl-boundary-kit:boundary-context-require (aitools.text.infrastructure:make-host-text-source) :text-read-range) path (- size 2) (+ size 100))
          (expect (length octets) :to-be 2)
          (expect reported :to-be size)))))))
(describe "aitools text host source bounded reads"
  (it "bounds a sniffed read when the file grows after fstat"
    (call-with-scratch-directory
     (lambda (scratch)
       (let* ((path (concatenate 'string scratch "/changing"))
              (bytes (make-array 200000 :element-type '(unsigned-byte 8)))
              (head nil))
         (dotimes (i (length bytes))
           (setf (aref bytes i) (mod (* i 7) 256)))
         (setf head (subseq bytes 0 100))
         (flet ((read-bounded (size prefix max-bytes)
                  (let ((fd (sb-posix:open path sb-posix:o-rdonly)))
                    (unwind-protect
                         (progn
                           (when prefix (sb-posix:lseek fd (length prefix) sb-posix:seek-set))
                           (multiple-value-bind (octets problem)
                               (aitools.text.infrastructure::%read-all fd size prefix :max-bytes max-bytes)
                             (values octets problem (sb-posix:lseek fd 0 sb-posix:seek-cur))))
                      (sb-posix:close fd)))))
           (write-file path (subseq bytes 0 120))
           (multiple-value-bind (octets problem offset) (read-bounded 100 head 150)
             (expect octets :to-equalp (subseq bytes 0 120))
             (expect problem :to-be-falsy)
             (expect offset :to-be 120))
           (write-file path bytes)
           (expect (multiple-value-list (read-bounded 100 head 150))
        :to-equal '(nil :too-large 151)))))))
  (it "bounds the boundary octet read before a whole-file allocation"
    (call-with-scratch-directory
     (lambda (scratch)
       (let* ((path (concatenate 'string scratch "/bounded"))
              (read-octets
                (cl-boundary-kit:boundary-context-require
                 (aitools.text.infrastructure:make-host-text-source)
                 :text-read-octets)))
         (write-file path (make-array 5 :element-type '(unsigned-byte 8) :initial-element 65))
         (expect (multiple-value-list (funcall read-octets path :max-bytes 4))
        :to-equal '(nil :too-large))))))
  (it "bounds the direct sniffed read at the first excess byte"
    (call-with-scratch-directory
     (lambda (scratch)
       (let* ((path (concatenate 'string scratch "/sniffed"))
              (read-sniffed
                (cl-boundary-kit:boundary-context-require
                 (aitools.text.infrastructure:make-host-text-source)
                 :text-read-sniffed)))
         (write-file path (make-array 5 :element-type '(unsigned-byte 8) :initial-element 65))
         (expect (multiple-value-list (funcall read-sniffed path 8192 (constantly t) :max-bytes 4))
        :to-equal '(nil :too-large))))))
  (it "stops after the first byte beyond the fstat size"
    (call-with-scratch-directory
     (lambda (scratch)
       (let* ((path (concatenate 'string scratch "/grown"))
              (bytes (make-array 151 :element-type '(unsigned-byte 8) :initial-element 65))
              (fd nil))
         (write-file path bytes)
         (setf fd (sb-posix:open path sb-posix:o-rdonly))
         (unwind-protect
              (multiple-value-bind (octets problem)
                  (aitools.text.infrastructure::%read-all fd 150 nil :max-bytes 150)
                (expect octets :to-be-falsy)
                (expect problem :to-be :too-large)
                (expect (sb-posix:lseek fd 0 sb-posix:seek-cur) :to-be 151))
           (sb-posix:close fd))))))
  (it "does not read after the first excess byte in the grown helper"
    (call-with-scratch-directory
     (lambda (scratch)
       (let* ((path (concatenate 'string scratch "/grown"))
              (bytes (make-array 200 :element-type '(unsigned-byte 8) :initial-element 65))
              (fd nil))
         (write-file path bytes)
         (setf fd (sb-posix:open path sb-posix:o-rdonly))
         (unwind-protect
              (progn
                (expect (sb-posix:lseek fd 151 sb-posix:seek-set) :to-be 151)
                (multiple-value-bind (octets problem)
                    (aitools.text.infrastructure::%read-grown (subseq bytes 0 150) fd (aref bytes 150) 150)
                  (expect octets :to-be-falsy)
                  (expect problem :to-be :too-large)
                  (expect (sb-posix:lseek fd 0 sb-posix:seek-cur) :to-be 151)))
           (sb-posix:close fd)))))))


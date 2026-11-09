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
            (aitools.text.application:source-read-range
             (aitools.text.infrastructure:make-host-text-source) path offset (+ offset 6))
          (expect (text octets) :to-equal "window")
          (expect reported :to-be size))
        (multiple-value-bind (octets reported)
            (aitools.text.application:source-read-range
             (aitools.text.infrastructure:make-host-text-source) path 0 6)
          (expect (length octets) :to-be 6)
          (expect (every (function zerop) octets) :to-be t)
          (expect reported :to-be size))
        (multiple-value-bind (octets reported)
            (aitools.text.application:source-read-range
             (aitools.text.infrastructure:make-host-text-source) path (- size 2) (+ size 100))
          (expect (length octets) :to-be 2)
          (expect reported :to-be size)))))))

(describe "aitools text host source bounded reads"
(it "bounds growth reads at MAX-BYTES plus one byte"
  (call-with-scratch-directory (lambda (scratch)
    (let* ((path (concatenate 'string scratch "/changing"))
           (bytes (make-array 200000 :element-type '(unsigned-byte 8)))
           (head (make-array 100 :element-type '(unsigned-byte 8))))
      (dotimes (i (length bytes)) (setf (aref bytes i) (mod (* i 7) 256)))
      (setf head (subseq bytes 0 100))
      (flet ((read-bounded (size head max-bytes)
               (let ((fd (sb-posix:open path sb-posix:o-rdonly)))
                 (unwind-protect
                      (progn
                        (when head (sb-posix:lseek fd (length head) sb-posix:seek-set))
                        (multiple-value-bind (octets problem)
                            (aitools.text.infrastructure::%read-all fd size head :max-bytes max-bytes)
                          (values octets problem (sb-posix:lseek fd 0 sb-posix:seek-cur))))
                   (sb-posix:close fd)))))
        (write-file path (subseq bytes 0 120))
        (multiple-value-bind (octets problem offset) (read-bounded 100 head 150)
          (expect octets :to-equalp (subseq bytes 0 120))
          (expect problem :to-be-falsy)
          (expect offset :to-be 120))
        (write-file path bytes)
        (expect (multiple-value-list (read-bounded 100 head 150))
                :to-equal '(nil :too-large 151))
        (write-file path (subseq bytes 0 5))
        (expect (multiple-value-list
                 (aitools.text.infrastructure::%read-sniffed path 8192 (constantly t) :max-bytes 4))
                :to-equal '(nil :too-large))))))
))

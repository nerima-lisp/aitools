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


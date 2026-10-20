;;;; t/support/files.lisp
(in-package #:aitools.test.support)

(defun read-bytes (path)
  "Read PATH as a simple unsigned-byte vector."
  (with-open-file (stream (sb-ext:parse-native-namestring path)
                          :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream)
                             :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun write-bytes (path bytes)
  "Write octet vector BYTES to PATH, creating parent directories."
  (ensure-directories-exist (sb-ext:parse-native-namestring path))
  (with-open-file (stream (sb-ext:parse-native-namestring path)
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence bytes stream))
  path)

(defun read-text (path)
  (sb-ext:octets-to-string (read-bytes path) :external-format :utf-8))

(defun write-text (path text)
  (write-bytes path (sb-ext:string-to-octets text :external-format :utf-8)))

(defmacro with-stdin-from ((path) &body body)
  "Run BODY with file descriptor 0 reading PATH, restoring the original
standard input afterwards."
  (let ((saved (gensym "SAVED")) (fd (gensym "FD")))
    `(let ((,saved (sb-posix:dup 0)))
       (unwind-protect
           (let ((,fd (sb-posix:open ,path sb-posix:o-rdonly)))
             (unwind-protect (progn (sb-posix:dup2 ,fd 0) ,@body)
               (unwind-protect (sb-posix:dup2 ,saved 0)
                 (sb-posix:close ,fd))))
         (sb-posix:close ,saved)))))


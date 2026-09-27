;;;; t/integration/text-host-source-test.lisp
;;;;
;;;; The production TEXT-SOURCE against a real filesystem, and the text read
;;;; flow over it.
(in-package #:aitools.text.integration-test)

(defun source () (aitools.text.infrastructure:make-host-text-source))

(describe "aitools text production source"
  (it "reads sizes, prefixes, whole files, and chunks of oddly named files"
    (with-scratch-directory (scratch)
      (let ((path (concatenate 'string scratch "/a*[b].txt")))
        (write-file path "0123456789")
        (expect (aitools.text.application:source-file-size (source) path) :to-be 10)
        (expect (text (aitools.text.application:source-read-prefix (source) path 4)) :to-equal "0123")
        (expect (text (aitools.text.application:source-read-octets (source) path)) :to-equal "0123456789")
        (let ((chunks '()))
          (expect (aitools.text.application:source-call-with-chunks
                   (source) path 4 (lambda (chunk) (push (text chunk) chunks) nil))
                  :to-be-truthy)
          (expect (reverse chunks) :to-equal '("0123" "4567" "89"))))))

  (it "follows a symlink to a file and refuses directories, FIFOs, and missing paths"
    (with-scratch-directory (scratch)
      (write-file (concatenate 'string scratch "/target") "x")
      (make-symlink "target" (concatenate 'string scratch "/link"))
      (sb-posix:mkfifo (concatenate 'string scratch "/pipe") #o600)
      (expect (aitools.text.application:source-file-size (source) (concatenate 'string scratch "/link")) :to-be 1)
      (expect (aitools.text.application:source-read-octets (source) scratch) :to-be-falsy)
      (expect (aitools.text.application:source-read-octets (source) (concatenate 'string scratch "/pipe")) :to-be-falsy)
      (expect (aitools.text.application:source-file-size (source) (concatenate 'string scratch "/none")) :to-be-falsy)))

  (it "tells a file it may not open apart from a missing one"
    (with-scratch-directory (scratch)
      (let ((path (concatenate 'string scratch "/locked")))
        (write-file path "secret")
        (sb-posix:chmod path 0)
        (unwind-protect
             (progn
               (expect (multiple-value-list (aitools.text.application:source-file-size (source) path))
                       :to-equal '(nil :unreadable))
               (expect (multiple-value-list (aitools.text.application:source-read-octets (source) path))
                       :to-equal '(nil :unreadable))
               (expect (multiple-value-list (aitools.text.application:source-read-prefix (source) path 4))
                       :to-equal '(nil :unreadable))
               (expect (multiple-value-list (aitools.text.application:source-call-with-chunks
                                             (source) path 4 (lambda (chunk) (declare (ignore chunk)) nil)))
                       :to-equal '(nil :unreadable))
               (dolist (missing (list (concatenate 'string scratch "/none")
                                      (concatenate 'string scratch "/locked/below")))
                 (expect (multiple-value-list (aitools.text.application:source-file-size (source) missing))
                         :to-equal '(nil))
                 (expect (multiple-value-list (aitools.text.application:source-read-prefix (source) missing 4))
                         :to-equal '(nil))
                 (expect (multiple-value-list (aitools.text.application:source-read-octets (source) missing))
                         :to-equal '(nil)))
               (expect (aitools.text.application:call-with-text-file/k
                        (source) path
                        :on-text (lambda (octets layout) (declare (ignore octets layout)) :text)
                        :on-binary (lambda (prefix size) (declare (ignore prefix size)) :binary)
                        :on-missing (lambda (path) (declare (ignore path)) :missing)
                        :on-unreadable (lambda (path) (declare (ignore path)) :unreadable))
                       :to-be :unreadable))
          (sb-posix:chmod path #o600)))))

  (it "makes write --content-file of a file it may not open environment.io, not input.not-found"
    (with-scratch-directory (scratch)
      (let ((root (concatenate 'string scratch "/work"))
            (locked (concatenate 'string scratch "/locked"))
            (previous (sb-posix:getenv "XDG_STATE_HOME")))
        (make-directories root)
        (write-file locked "secret")
        (sb-posix:chmod locked 0)
        (sb-posix:setenv "XDG_STATE_HOME" (concatenate 'string scratch "/state") 1)
        (unwind-protect
             (multiple-value-bind (app registry) (aitools/cli:build-app)
               (let* ((out (make-string-output-stream))
                      (err (make-string-output-stream))
                      (code (aitools/cli:dispatch app registry
                                                  (list "aitools" "--root" root "write" "x" "--content-file" locked)
                                                  :stdout out :stderr err))
                      (output (get-output-stream-string out))
                      (envelope (json-kit:parse (if (plusp (length output)) output (get-output-stream-string err))))
                      (error-object (gethash "error" envelope)))
                 (expect (gethash "code" error-object) :to-equal "environment.io")
                 (expect (gethash "message" error-object) :to-equal (format nil "cannot read ~A" locked))
                 (expect code :to-be (gethash "exit_code" error-object))
                 (expect (probe-file (sb-ext:parse-native-namestring (concatenate 'string root "/x"))) :to-be-falsy)))
          (if previous
              (sb-posix:setenv "XDG_STATE_HOME" previous 1)
              (sb-posix:unsetenv "XDG_STATE_HOME"))
          (sb-posix:chmod locked #o600)))))

  (it "reads a file larger than one buffer completely"
    (with-scratch-directory (scratch)
      (let ((path (concatenate 'string scratch "/big"))
            (bytes (make-array 200000 :element-type '(unsigned-byte 8))))
        (dotimes (i (length bytes)) (setf (aref bytes i) (mod (* i 31) 256)))
        (write-file path bytes)
        (expect (aitools.text.application:source-read-octets (source) path) :to-equalp bytes))))

  (it "reads a prefix, then the rest on the same descriptor only when asked"
    (with-scratch-directory (scratch)
      (let ((path (concatenate 'string scratch "/big"))
            (bytes (make-array 200000 :element-type '(unsigned-byte 8)))
            (seen '()))
        (dotimes (i (length bytes)) (setf (aref bytes i) (mod (* i 31) 256)))
        (write-file path bytes)
        (flet ((sniff (count continue)
                 (aitools.text.application:source-read-sniffed
                  (source) path count (lambda (prefix size) (push (list (length prefix) size) seen) continue))))
          (expect (sniff 8192 nil) :to-equalp (subseq bytes 0 8192))
          (expect (sniff 8192 t) :to-equalp bytes)
          (expect (sniff 300000 t) :to-equalp bytes)
          (expect (reverse seen) :to-equal '((8192 200000) (8192 200000) (200000 200000))))
        (write-file path "")
        (expect (aitools.text.application:source-read-sniffed (source) path 8192 (constantly t)) :to-equalp #())
        (expect (multiple-value-list (aitools.text.application:source-read-sniffed
                                      (source) scratch 8192 (constantly t)))
                :to-equal '(nil))
        (sb-posix:chmod path 0)
        (unwind-protect
             (expect (multiple-value-list (aitools.text.application:source-read-sniffed
                                           (source) path 8192 (constantly t)))
                     :to-equal '(nil :unreadable))
          (sb-posix:chmod path #o600)))))

  (it "names no file with a path holding NUL or a character the file system cannot encode"
    (with-scratch-directory (scratch)
      (let ((target (concatenate 'string scratch "/target")))
        (write-file target "bytes")
        (dolist (path (list (format nil "~A~Csuffix" target (code-char 0))
                            (format nil "~A~C" target (code-char #xD800))))
          (expect (multiple-value-list (aitools.text.application:source-file-size (source) path)) :to-equal '(nil))
          (expect (multiple-value-list (aitools.text.application:source-read-prefix (source) path 2)) :to-equal '(nil))
          (expect (multiple-value-list (aitools.text.application:source-read-octets (source) path)) :to-equal '(nil))
          (expect (multiple-value-list (aitools.text.application:source-read-sniffed (source) path 2 (constantly t)))
                  :to-equal '(nil))))))

  (it "ends chunked reads at end of file and at :STOP"
    (with-scratch-directory (scratch)
      (let ((empty (concatenate 'string scratch "/empty"))
            (full (concatenate 'string scratch "/full"))
            (chunks '()))
        (write-file empty "")
        (write-file full "abcdefgh")
        (expect (aitools.text.application:source-call-with-chunks
                 (source) empty 4 (lambda (chunk) (push chunk chunks) nil))
                :to-be t)
        (expect chunks :to-equal '())
        (expect (aitools.text.application:source-call-with-chunks
                 (source) full 4 (lambda (chunk) (push (text chunk) chunks) :stop))
                :to-be t)
        (expect chunks :to-equal '("abcd"))
        (setf chunks '())
        (aitools.text.application:source-call-with-chunks
         (source) full 4 (lambda (chunk) (push (text chunk) chunks) nil))
        (expect (reverse chunks) :to-equal '("abcd" "efgh")))))

  (it "reads a file that grew or shrank after fstat to its current end"
    (with-scratch-directory (scratch)
      (let* ((path (concatenate 'string scratch "/changing"))
             (bytes (make-array 200000 :element-type '(unsigned-byte 8))))
        (dotimes (i (length bytes)) (setf (aref bytes i) (mod (* i 7) 256)))
        (write-file path bytes)
        ;; A stale fstat size stands in for a file appended to or truncated
        ;; between fstat and read(2).
        (flet ((read-all (size &optional head)
                 (let ((fd (sb-posix:open path sb-posix:o-rdonly)))
                   (unwind-protect
                        (progn
                          (when head (sb-posix:lseek fd (length head) sb-posix:seek-set))
                          (aitools.text.infrastructure::%read-all fd size head))
                     (sb-posix:close fd)))))
          (expect (read-all 3) :to-equalp bytes)
          (expect (read-all 300000) :to-equalp bytes)
          (expect (read-all 100 (subseq bytes 0 150)) :to-equalp bytes)
          (expect (read-all 200000 (subseq bytes 0 150)) :to-equalp bytes))
        ;; Growth of exactly one chunk past the stale size and probe byte.
        (write-file path (subseq bytes 0 65540))
        (let ((fd (sb-posix:open path sb-posix:o-rdonly)))
          (unwind-protect (expect (aitools.text.infrastructure::%read-all fd 3) :to-equalp (subseq bytes 0 65540))
            (sb-posix:close fd))))))

  (it "retries nothing but EINTR: another read(2) error reaches the caller"
    (let ((buffer (make-array 4 :element-type '(unsigned-byte 8))))
      (expect (handler-case (sb-sys:with-pinned-objects (buffer)
                              (aitools.text.infrastructure::%posix-read -1 (sb-sys:vector-sap buffer) 4))
                (sb-posix:syscall-error (condition) (sb-posix:syscall-errno condition)))
              :to-be sb-posix:ebadf)))

  (it "classifies text and binary files through call-with-text-file/k"
    (with-scratch-directory (scratch)
      (write-file (concatenate 'string scratch "/t.txt") (format nil "a~C~%" #\Return))
      (write-file (concatenate 'string scratch "/b.bin") (coerce #(1 0 2) '(simple-array (unsigned-byte 8) (*))))
      (flet ((outcome (name)
               (aitools.text.application:call-with-text-file/k
                (source) (concatenate 'string scratch "/" name)
                :on-text (lambda (bytes layout) (list :text (length bytes) (text-layout-line-ending layout)))
                :on-binary (lambda (prefix size) (list :binary (guess-mime prefix) size))
                :on-missing (lambda (path) (declare (ignore path)) :missing))))
        (expect (outcome "t.txt") :to-equal '(:text 3 :crlf))
        (expect (outcome "b.bin") :to-equal '(:binary "application/octet-stream" 3))
        (expect (outcome "nope") :to-be :missing))))

  (it "registers the source in a cl-boundary-kit boundary context"
    (let* ((source (source))
           (context (aitools.text.infrastructure:with-text-boundaries
                     (cl-boundary-kit:make-boundary-context) :source source)))
      (expect (aitools.text.infrastructure:text-source-from-context context) :to-be source))))

(describe "aitools text production source ranged read"
  (it "reads a window from the middle of a sparse file and reports the whole size"
    (with-scratch-directory (scratch)
      (let ((path (concatenate 'string scratch "/sparse.bin"))
            (size (* 64 1024 1024))
            (offset (* 40 1024 1024)))
        (write-file path "")
        (sb-posix:truncate path size)
        (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output
                                                                  :element-type '(unsigned-byte 8) :if-exists :overwrite)
          (file-position out offset)
          (write-sequence (sb-ext:string-to-octets "window" :external-format :utf-8) out))
        (multiple-value-bind (octets reported) (aitools.text.application:source-read-range (source) path offset (+ offset 6))
          (expect (text octets) :to-equal "window")
          (expect reported :to-be size))
        (multiple-value-bind (octets reported)
            (aitools.text.application:source-read-range (source) path (- size 2) (+ size 100))
          (expect (length octets) :to-be 2)
          (expect reported :to-be size))))))

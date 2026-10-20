;;;; These tests exercise production PROCESS-PORTS adapters one port at a time
;;;; on the real filesystem, signals, and sockets. They check failures exposed
;;;; to flows as PROCESS-PORT-ERROR. Package and directory helpers are in
;;;; t/support/process-test-support.lisp; run-program and launcher tests are
;;;; in t/integration/process-ports-launch-test.lisp and use port helpers here.
(in-package #:aitools.process.integration-test)

(defun production-ports (&optional state-directory)
  (aitools.process.infrastructure:make-production-process-ports
   :state-directory-function (and state-directory (lambda () state-directory))))

(defun call-port (ports accessor &rest arguments)
  (apply (funcall accessor ports) arguments))

(defun port-error-message (function)
  "Run FUNCTION and return a printed PROCESS-PORT-ERROR, or :NO-ERROR."
  (handler-case (progn (funcall function) :no-error)
    (aitools.process.application:process-port-error (condition)
      (princ-to-string condition))))

(defun starts-with-p (prefix text)
  (and (stringp text) (eql 0 (search prefix text))))

(defun write-file (path text)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-string text out))
  path)

(defun file-mode (path)
  (logand #o777 (sb-posix:stat-mode (sb-posix:lstat (uiop:native-namestring path)))))

(defun make-executable (path text)
  (write-file path text)
  (sb-posix:chmod (uiop:native-namestring path) #o755)
  path)

(defun call-with-environment (bindings function)
  "Call FUNCTION with each (NAME . VALUE) of BINDINGS set in the process
environment (VALUE NIL unsets NAME), restoring the previous values after."
  (let ((saved (mapcar (lambda (binding) (cons (car binding) (sb-posix:getenv (car binding)))) bindings)))
    (flet ((apply-bindings (pairs)
             (dolist (pair pairs)
               (if (cdr pair)
                   (sb-posix:setenv (car pair) (cdr pair) 1)
                   (sb-posix:unsetenv (car pair))))))
      (unwind-protect (progn (apply-bindings bindings) (funcall function))
        (apply-bindings saved)))))

(describe "aitools process production ports: files (integration)"
  (it "creates a private file exclusively and refuses an existing file or a symlink"
    (with-temporary-directory (directory)
      (let ((ports (production-ports))
            (path (merge-pathnames "bg-1.log" directory))
            (link (merge-pathnames "bg-2.log" directory))
            (target (merge-pathnames "target" directory)))
        (expect (call-port ports 'aitools.process.application::process-ports-create-file-exclusive path "one")
                :to-be t)
        (expect (uiop:read-file-string path) :to-equal "one")
        (expect (file-mode path) :to-be #o600)
        (expect (call-port ports 'aitools.process.application::process-ports-create-file-exclusive path "two")
                :to-be nil)
        (expect (uiop:read-file-string path) :to-equal "one")
        (sb-posix:symlink (uiop:native-namestring target) (uiop:native-namestring link))
        (expect (call-port ports 'aitools.process.application::process-ports-create-file-exclusive link "x")
                :to-be nil)
        (expect (probe-file target) :to-be nil))))

  (it "reports a failed exclusive create other than an existing path as a port error"
    (with-temporary-directory (directory)
      (let ((path (merge-pathnames "missing/bg-1.log" directory)))
        (expect (starts-with-p (format nil "creating ~A failed: " path)
                               (port-error-message
                                (lambda ()
                                  (call-port (production-ports)
                                             'aitools.process.application::process-ports-create-file-exclusive
                                             path ""))))
                :to-be t))))

  (it "replaces a record atomically, private, and leaves no temporary file behind"
    (with-temporary-directory (directory)
      (let ((ports (production-ports))
            (path (merge-pathnames "bg-1.json" directory)))
        (write-file path "old")
        (call-port ports 'aitools.process.application::process-ports-replace-file path "new")
        (expect (uiop:read-file-string path) :to-equal "new")
        (expect (file-mode path) :to-be #o600)
        (expect (call-port ports 'aitools.process.application::process-ports-list-directory directory)
                :to-equal '("bg-1.json")))))

  (it "retries after two temporary-name collisions and errors when every name collides"
    (with-temporary-directory (directory)
      (let* ((ports (production-ports))
             (path (merge-pathnames "bg-1.json" directory))
             (victim (merge-pathnames "victim" directory))
             (planted (merge-pathnames ".bg-1.tmp-PLANTED.json" directory))
             (suffixes (list "PLANTED" "PLANTED" "FRESH")))
        (sb-posix:symlink (uiop:native-namestring victim) (uiop:native-namestring planted))
        ;; The first two names collide with the planted symlink.
        (cl-weave:with-mocked-functions
            (((symbol-function 'aitools.process.infrastructure::%random-temp-suffix)
              (lambda () (or (pop suffixes) "FRESH"))))
          (call-port ports 'aitools.process.application::process-ports-replace-file path "v1")
          (expect (uiop:read-file-string path) :to-equal "v1")
          (expect suffixes :to-equal '()))
        (cl-weave:with-mocked-functions
            (((symbol-function 'aitools.process.infrastructure::%random-temp-suffix)
              (lambda () "PLANTED")))
          (expect (starts-with-p (format nil "writing ~A failed: " path)
                                 (port-error-message
                                  (lambda ()
                                    (call-port ports 'aitools.process.application::process-ports-replace-file
                                               path "v2"))))
                  :to-be t))
        (expect (uiop:read-file-string path) :to-equal "v1")
        (expect (probe-file victim) :to-be nil))))

  (it "reports a replace into a missing directory as a port error"
    (with-temporary-directory (directory)
      (let ((path (merge-pathnames "missing/bg-1.json" directory)))
        (expect (starts-with-p (format nil "writing ~A failed: " path)
                               (port-error-message
                                (lambda ()
                                  (call-port (production-ports)
                                             'aitools.process.application::process-ports-replace-file path "x"))))
                :to-be t))))

  (it "removes a file and ignores one that is already gone"
    (with-temporary-directory (directory)
      (let ((ports (production-ports))
            (path (write-file (merge-pathnames "bg-1.log" directory) "x")))
        (call-port ports 'aitools.process.application::process-ports-remove-file path)
        (expect (probe-file path) :to-be nil)
        (expect (port-error-message
                 (lambda () (call-port ports 'aitools.process.application::process-ports-remove-file path)))
                :to-be :no-error))))

  (it "lists only file names, leaving subdirectories out"
    (with-temporary-directory (directory)
      (write-file (merge-pathnames "bg-1.json" directory) "{}")
      (write-file (merge-pathnames "bg-1.log" directory) "")
      (ensure-directories-exist (merge-pathnames "bg-2.json/" directory))
      (expect (sort (call-port (production-ports) 'aitools.process.application::process-ports-list-directory
                               directory)
                    #'string<)
              :to-equal '("bg-1.json" "bg-1.log"))))

  (it "reads a record from one descriptor despite unlink or replacement after open"
    (with-temporary-directory (directory)
      (dolist (replacement '(nil "different replacement"))
        (let* ((path (write-file (merge-pathnames "bg-1.json" directory) "original"))
               (original (symbol-function 'aitools.process.infrastructure::%open-input-file))
               (opens 0)
               (opened nil))
          (cl-weave:with-mocked-functions
              (((symbol-function 'aitools.process.infrastructure::%open-input-file)
                (lambda (input &rest arguments)
                  (let ((stream (apply original input arguments)))
                    (incf opens)
                    (when (= opens 1)
                      (setf opened stream)
                      (delete-file path)
                      (when replacement (write-file path replacement)))
                    stream))))
            (unwind-protect
                 (progn
                   (expect (call-port (production-ports)
                                      'aitools.process.application::process-ports-read-file-text path)
                           :to-equal "original")
                   (expect opens :to-be 1)
                   (expect (open-stream-p opened) :to-be nil))
              (when (and opened (open-stream-p opened)) (close opened))))))))

  (it "reads byte ranges, sizes, and text with replacement, and NIL for a missing file"
    (with-temporary-directory (directory)
      (let ((ports (production-ports))
            (path (merge-pathnames "bg-1.log" directory))
            (missing (merge-pathnames "bg-9.log" directory)))
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
          (write-sequence (coerce '(97 98 99 255 100) '(vector (unsigned-byte 8))) out))
        (expect (coerce (call-port ports 'aitools.process.application::process-ports-read-file-octets path 1 3) 'list)
                :to-equal '(98 99))
        (expect (coerce (call-port ports 'aitools.process.application::process-ports-read-file-octets path 3 99) 'list)
                :to-equal '(255 100))
        (expect (call-port ports 'aitools.process.application::process-ports-file-size path) :to-be 5)
        (expect (call-port ports 'aitools.process.application::process-ports-read-file-text path)
                :to-equal (coerce (list #\a #\b #\c (code-char #xfffd) #\d) 'string))
        (expect (call-port ports 'aitools.process.application::process-ports-file-size missing) :to-be nil)
        (expect (call-port ports 'aitools.process.application::process-ports-read-file-text missing) :to-be nil)
        (expect (starts-with-p (format nil "reading ~A failed: " missing)
                               (port-error-message
                                (lambda ()
                                  (call-port ports 'aitools.process.application::process-ports-read-file-octets
                                             missing 0 1))))
                :to-be t)))))


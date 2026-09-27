;;;; t/integration/workspace-test-support.lisp
;;;;
;;;; Scratch directories and external commands for the workspace and text
;;;; integration tests. Paths are native strings end to end (no CL pathname
;;;; parsing), because the fixtures deliberately use names containing `*`,
;;;; `[`, and non-ASCII characters. External programs (git, zip, tar, gzip)
;;;; are test oracles only; production code never starts them.
(in-package #:cl-user)

(defpackage #:aitools.workspace.integration-support
  (:use #:cl)
  (:export
   #:hex-octets
   #:program-path
   #:run-command
   #:call-with-scratch-directory
   #:with-scratch-directory
   #:make-directories
   #:write-file
   #:read-file
   #:make-symlink))

(in-package #:aitools.workspace.integration-support)

(defun hex-octets (hex)
  (let ((result (make-array (floor (length hex) 2) :element-type '(unsigned-byte 8))))
    (dotimes (i (length result) result)
      (setf (aref result i) (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun program-path (name)
  "The absolute path of executable NAME on PATH, or NIL."
  (loop for directory in (uiop:split-string (or (sb-posix:getenv "PATH") "") :separator ":")
        for candidate = (concatenate 'string directory "/" name)
        when (and (plusp (length directory))
                  (handler-case (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:stat candidate)))
                    (error () nil))
                  (zerop (sb-posix:access candidate sb-posix:x-ok)))
          return candidate))

(defun run-command (directory program arguments &key environment (input nil))
  "Run PROGRAM with ARGUMENTS in DIRECTORY. ENVIRONMENT is an alist added to
the inherited environment. Returns (VALUES exit-code stdout-octets
stderr-string); signals when PROGRAM is not on PATH."
  (let* ((path (or (program-path program) (error "~A is not on PATH" program)))
         (overridden (mapcar #'car environment))
         (env (append (loop for (name . value) in environment collect (format nil "~A=~A" name value))
                      (remove-if (lambda (entry)
                                   (let ((equals (position #\= entry)))
                                     (and equals (member (subseq entry 0 equals) overridden :test #'string=))))
                                 (sb-ext:posix-environ))))
         (stdout (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (stderr (make-string-output-stream)))
    (let* ((process (sb-ext:run-program path arguments
                                        :directory directory
                                        :environment env
                                        :input input
                                        :output :stream
                                        :error stderr
                                        :wait nil))
           (out (sb-ext:process-output process)))
      (loop for byte = (read-byte out nil nil)
            while byte do (vector-push-extend byte stdout))
      (sb-ext:process-wait process)
      (values (sb-ext:process-exit-code process)
              (coerce stdout '(simple-array (unsigned-byte 8) (*)))
              (get-output-stream-string stderr)))))

(defun call-with-scratch-directory (function)
  "Call FUNCTION with the real path (symlinks resolved, no trailing `/`) of a
fresh private directory, removing it afterwards."
  (let* ((template (concatenate 'string (string-right-trim "/" (or (sb-posix:getenv "TMPDIR") "/tmp"))
                                "/aitools-it-XXXXXX"))
         (created (sb-posix:mkdtemp template))
         (real (string-right-trim "/" (sb-ext:native-namestring
                                       (truename (sb-ext:parse-native-namestring created nil
                                                                                 *default-pathname-defaults*
                                                                                 :as-directory t))))))
    (unwind-protect (funcall function real)
      (when (search "/aitools-it-" real)
        (run-command "/" "rm" (list "-rf" real))))))

(defmacro with-scratch-directory ((var) &body body)
  `(call-with-scratch-directory (lambda (,var) ,@body)))

(defun make-directories (path)
  "mkdir -p for the native PATH."
  (let ((start 1))
    (loop for slash = (position #\/ path :start start)
          do (let ((prefix (subseq path 0 (or slash (length path)))))
               (handler-case (sb-posix:mkdir prefix #o755)
                 (sb-posix:syscall-error () nil)))
             (if slash (setf start (1+ slash)) (return)))))

(defun write-file (path content)
  "Write CONTENT (string, UTF-8, or octets) to the native PATH, creating
parent directories."
  (make-directories (subseq path 0 (position #\/ path :from-end t)))
  (with-open-file (stream (sb-ext:parse-native-namestring path) :direction :output :if-exists :supersede
                                                                :element-type '(unsigned-byte 8))
    (write-sequence (if (stringp content) (sb-ext:string-to-octets content :external-format :utf-8) content)
                    stream))
  path)

(defun read-file (path)
  (with-open-file (stream (sb-ext:parse-native-namestring path) :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

(defun make-symlink (target path)
  (make-directories (subseq path 0 (position #\/ path :from-end t)))
  (sb-posix:symlink target path))

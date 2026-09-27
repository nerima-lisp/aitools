;;;; t/integration/process-ports-launch-test.lisp
;;;;
;;;; The production run-program and launcher ports exercised on the real
;;;; filesystem: --stdout-to, launcher lookup, and the spawn trampoline.
(in-package #:aitools.process.integration-test)

;;; ------------------------------------------------------------- run port

(describe "aitools process production ports: run-program with --stdout-to (integration)"
  (it "reports a target in a missing directory as a port error without running anything"
    (with-temporary-directory (directory)
      (let ((target (merge-pathnames "missing/out.txt" directory))
            (called '()))
        (expect (starts-with-p (format nil "creating ~A failed: " target)
                               (port-error-message
                                (lambda ()
                                  (call-port (production-ports) 'aitools.process.application::process-ports-run-program
                                             (list "true") 5000 :stdout-path target
                                             :on-exited (lambda (outcome) (push outcome called))
                                             :on-unavailable (lambda (&rest arguments) (push arguments called))))))
                :to-be t)
        (expect called :to-equal '()))))

  (it "removes the target it created when the program cannot start"
    (with-temporary-directory (directory)
      (let ((target (merge-pathnames "out.txt" directory))
            (unavailable nil))
        (call-port (production-ports) 'aitools.process.application::process-ports-run-program
                   (list "aitools-no-such-program") 5000 :stdout-path target
                   :on-exited (lambda (outcome) (declare (ignore outcome)) (setf unavailable :exited))
                   :on-unavailable (lambda (message program)
                                     (declare (ignore message))
                                     (setf unavailable program)))
        (expect unavailable :to-equal "aitools-no-such-program")
        (expect (probe-file target) :to-be nil)))))

;;; ------------------------------------------------------------- launcher

(defun call-with-launcher-environment (directory function &key override (path ""))
  "Call FUNCTION with the trampoline search confined to DIRECTORY: the
running executable pretends to live in DIRECTORY/bin/, CL_PROCESS_KIT_SPAWN is
OVERRIDE, and PATH is PATH."
  ;; *RUNTIME-PATHNAME* is a global lexical, so it is set and restored, not bound.
  (let ((runtime sb-ext:*runtime-pathname*))
    (ensure-directories-exist (merge-pathnames "bin/" directory))
    (setf sb-ext:*runtime-pathname* (namestring (merge-pathnames "bin/sbcl" directory)))
    (unwind-protect
         (call-with-environment (list (cons "CL_PROCESS_KIT_SPAWN" override) (cons "PATH" path)) function)
      (setf sb-ext:*runtime-pathname* runtime))))

(defun launch (ports argv log exit)
  "Call the launch-detached port; return (:STARTED PID) or (:UNAVAILABLE
MESSAGE PROGRAM). A started process is killed by group at once, so a test
that expected otherwise never leaks it."
  (call-port ports 'aitools.process.application::process-ports-launch-detached argv log exit
             :on-started (lambda (pid) (kill-group pid) (list :started pid))
             :on-unavailable (lambda (message program) (list :unavailable message program))))

(describe "aitools process production ports: launcher lookup (integration)"
  (it "prefers an executable $CL_PROCESS_KIT_SPAWN"
    (with-temporary-directory (directory)
      (let ((helper (make-executable (merge-pathnames "helper" directory) "")))
        (call-with-launcher-environment
         directory
         (lambda ()
           (expect (aitools.process.infrastructure:find-spawn-trampoline)
                   :to-equal (uiop:native-namestring helper)))
         :override (uiop:native-namestring helper)))))

  (it "falls back to the helper beside the running executable when the override is not executable"
    (with-temporary-directory (directory)
      (let ((plain (write-file (merge-pathnames "plain" directory) ""))
            (sibling (merge-pathnames "bin/cl-process-kit-spawn" directory)))
        (call-with-launcher-environment
         directory
         (lambda ()
           (make-executable sibling "")
           (expect (aitools.process.infrastructure:find-spawn-trampoline)
                   :to-equal (uiop:native-namestring sibling)))
         :override (uiop:native-namestring plain)))))

  (it "searches only absolute PATH entries, skipping a directory named like the helper"
    (with-temporary-directory (directory)
      (let ((shadow (merge-pathnames "shadow/" directory))
            (tools (merge-pathnames "tools/" directory)))
        (ensure-directories-exist (merge-pathnames "cl-process-kit-spawn/" shadow))
        (ensure-directories-exist tools)
        (let ((helper (make-executable (merge-pathnames "cl-process-kit-spawn" tools) ""))
              (path (format nil "::relative:~A:~A" (uiop:native-namestring shadow) (uiop:native-namestring tools))))
          (call-with-launcher-environment
           directory
           (lambda ()
             (expect (aitools.process.infrastructure:find-spawn-trampoline)
                     :to-equal (uiop:native-namestring helper)))
           :override "" :path path)
          (call-with-launcher-environment
           directory
           (lambda () (expect (aitools.process.infrastructure:find-spawn-trampoline) :to-be nil))
           :path (format nil "relative:~A" (uiop:native-namestring shadow)))))))

  (it "reports a missing helper as unavailable, naming the helper"
    (with-temporary-directory (directory)
      (let ((sleep-path (program-on-path "sleep")))
        (call-with-launcher-environment
         directory
         (lambda ()
           (destructuring-bind (kind message program)
               (launch (production-ports) (list sleep-path "30")
                       (merge-pathnames "bg-1.log" directory) (merge-pathnames "bg-1.exit" directory))
             (expect kind :to-be :unavailable)
             (expect (starts-with-p "bg start needs the cl-process-kit-spawn helper" message) :to-be t)
             (expect program :to-equal "cl-process-kit-spawn")))))))

  (it "reports a program path that is not executable as unavailable"
    (with-temporary-directory (directory)
      (let ((helper (make-executable (merge-pathnames "helper" directory) ""))
            (script (namestring (write-file (merge-pathnames "script" directory) "echo hi"))))
        (call-with-launcher-environment
         directory
         (lambda ()
           (expect (launch (production-ports) (list script)
                           (merge-pathnames "bg-1.log" directory) (merge-pathnames "bg-1.exit" directory))
                   :to-equal (list :unavailable
                                   (format nil "cannot start ~A: not an executable file or not found on PATH" script)
                                   script)))
         :override (uiop:native-namestring helper)))))

  (it "reports a helper that cannot be executed as unavailable for the program"
    (with-temporary-directory (directory)
      (let ((garbage (merge-pathnames "garbage" directory))
            (sleep-path (program-on-path "sleep")))
        (make-executable garbage (make-string 16 :initial-element (code-char 0)))
        (call-with-launcher-environment
         directory
         (lambda ()
           (destructuring-bind (kind message program)
               (launch (production-ports) (list sleep-path "30")
                       (merge-pathnames "bg-1.log" directory) (merge-pathnames "bg-1.exit" directory))
             (expect kind :to-be :unavailable)
             (expect (starts-with-p (format nil "cannot start ~A: " sleep-path) message) :to-be t)
             (expect program :to-equal sleep-path)))
         :override (uiop:native-namestring garbage)))))

  (it "refuses a bg log that was swapped for a symlink, starting nothing"
    (with-temporary-directory (directory)
      (let ((garbage (merge-pathnames "garbage" directory))
            (log (merge-pathnames "bg-1.log" directory))
            (target (merge-pathnames "elsewhere" directory))
            (sleep-path (program-on-path "sleep")))
        (make-executable garbage (make-string 16 :initial-element (code-char 0)))
        (sb-posix:symlink (uiop:native-namestring target) (uiop:native-namestring log))
        (call-with-launcher-environment
         directory
         (lambda ()
           (expect (starts-with-p (format nil "starting ~A failed: " sleep-path)
                                  (port-error-message
                                   (lambda ()
                                     (launch (production-ports) (list sleep-path "30")
                                             log (merge-pathnames "bg-1.exit" directory)))))
                   :to-be t))
         :override (uiop:native-namestring garbage))
        (expect (probe-file target) :to-be nil)))))

(describe-skip-if (null (aitools.process.infrastructure:find-spawn-trampoline))
    "aitools process production ports: process groups (integration; skipped without the cl-process-kit-spawn trampoline)"
  (it "sees a started group alive, signals it, and then reports it gone"
    (with-temporary-directory (directory)
      (let* ((ports (production-ports))
             (log (merge-pathnames "bg-1.log" directory))
             (pid nil))
        (call-port ports 'aitools.process.application::process-ports-create-file-exclusive log "")
        (unwind-protect
             (progn
               (call-port ports 'aitools.process.application::process-ports-launch-detached
                          (list (program-on-path "sleep") "30") log (merge-pathnames "bg-1.exit" directory)
                          :on-started (lambda (started) (setf pid started))
                          :on-unavailable (lambda (message program) (error "~A: ~A" program message)))
               (expect (call-port ports 'aitools.process.application::process-ports-group-alive-p pid) :to-be t)
               ;; A signal number the kernel rejects is an I/O failure, not "gone".
               (expect (starts-with-p (format nil "signalling process group ~D failed: " pid)
                                      (port-error-message
                                       (lambda ()
                                         (call-port ports 'aitools.process.application::process-ports-signal-group
                                                    pid 999))))
                       :to-be t)
               (expect (call-port ports 'aitools.process.application::process-ports-signal-group pid 15) :to-be t)
               (expect (loop repeat 500
                             unless (call-port ports 'aitools.process.application::process-ports-group-alive-p pid)
                               return t
                             do (sleep 0.02))
                       :to-be t)
               (expect (call-port ports 'aitools.process.application::process-ports-signal-group pid 15) :to-be nil))
          (when pid (kill-group pid)))))))

;;;; packages/feature/process/src/infrastructure/bg-launcher.lisp
;;;;
;;;; The LAUNCH-DETACHED port. A bg process must outlive aitools, so it
;;;; is started through process-kit:SPAWN-NATIVE with :SESSION T: the native
;;;; trampoline calls setsid(2) before exec, leaving the process in its own
;;;; session and process group, detached from aitools and any terminal.
;;;;
;;;; Nobody waits on the process once aitools exits, so its exit status would
;;;; be lost. A fixed POSIX sh supervisor records it: the target runs as the
;;;; supervisor's child and its `$?` is written to the exit file. The target
;;;; argv reaches sh only as positional parameters expanded by "$@", never as
;;;; script text, so no argument is ever interpreted by a shell.
;;;;
;;;; The supervisor deliberately does not trap signals. A trap would let it
;;;; outlive `bg stop`'s group SIGTERM and record the target's real status,
;;;; but a SIGTERM that arrives before sh has forked the target would then be
;;;; swallowed and the target started anyway (observed in the integration
;;;; test). Untrapped, sh dies with its group and the status of a stopped
;;;; process comes from the signal `bg stop` recorded.
(in-package #:aitools.process.infrastructure)

(defparameter +spawn-trampoline-name+ "cl-process-kit-spawn")

(defparameter +supervisor-script+
  "\"$@\"
status=$?
printf '%s\\n' \"$status\" > \"$0\"
"
  "Run as `sh -c SCRIPT EXIT-PATH PROGRAM ARG...`.")

(defun %executable-file-p (pathname)
  (let ((native (uiop:native-namestring pathname)))
    ;; FILE-EXISTS-P is false for a directory (and a symlink to one), so a
    ;; searchable directory never passes the X_OK check below.
    (and (uiop:file-exists-p pathname)
         (handler-case (progn (sb-posix:access native sb-posix:x-ok) t)
           (sb-posix:syscall-error () nil)))))

(defun %search-path (name)
  "The first executable NAME in an absolute $PATH directory, or NIL. Relative
entries (including an empty entry, which means the working directory) are
skipped: bg exec resolves the spawn trampoline and the target through PATH, so
a relative entry could point at a workspace-controlled binary."
  (let ((path (or (sb-ext:posix-getenv "PATH") "")))
    (loop for start = 0 then (1+ end)
          for end = (position #\: path :start start)
          for directory = (subseq path start end)
          for candidate = (and (plusp (length directory))
                               (char= (char directory 0) #\/)
                               (merge-pathnames name (uiop:ensure-directory-pathname directory)))
          when (and candidate (%executable-file-p candidate))
            return (uiop:native-namestring candidate)
          while end)))

(defun find-spawn-trampoline ()
  "The native spawn trampoline's path, or NIL. Searched in order:
$CL_PROCESS_KIT_SPAWN, the directory of the running executable (where the
aitools package installs it), then $PATH."
  (let ((override (sb-ext:posix-getenv "CL_PROCESS_KIT_SPAWN"))
        (sibling (and sb-ext:*runtime-pathname*
                      (merge-pathnames +spawn-trampoline-name+
                                       (uiop:pathname-directory-pathname sb-ext:*runtime-pathname*)))))
    (cond ((and override (plusp (length override)) (%executable-file-p override)) override)
          ((and sibling (%executable-file-p sibling)) (uiop:native-namestring sibling))
          (t (%search-path +spawn-trampoline-name+)))))

(defun %resolve-program (name)
  "NAME as an absolute executable path: relative to the working directory
when it contains a slash, else searched in $PATH. NIL when not executable."
  (if (find #\/ name)
      (let ((pathname (merge-pathnames name (uiop:getcwd))))
        (and (%executable-file-p pathname) (uiop:native-namestring pathname)))
      (%search-path name)))

(defun %spawn-supervisor (trampoline argv program log exit-path)
  ;; cl-process-kit v3.3.1 (flake.nix; PR #7) made :SESSION spawns reliable on
  ;; Darwin: the trampoline retries setsid on EPERM until the just-vacated
  ;; process group disappears, and SPAWN-NATIVE checks the child's group only
  ;; after exec is confirmed. The launch-retry loop v3.2.0 needed for those
  ;; two races is gone; a genuine failure surfaces to %LAUNCH-DETACHED.
  (let ((process-kit:*native-spawn-program* trampoline))
    (process-kit:spawn-native "/bin/sh"
                              (list* "-c" +supervisor-script+
                                     (uiop:native-namestring exit-path)
                                     program (rest argv))
                              :session t :detached t
                              :environment (sb-ext:posix-environ)
                              :output log :error :output)))

(defun %launch-detached (argv log-path exit-path &key on-started on-unavailable)
  (let ((trampoline (find-spawn-trampoline))
        (program (%resolve-program (first argv))))
    (cond
      ((null trampoline)
       (funcall on-unavailable
                (format nil "bg start needs the ~A helper, which was not found beside aitools, in $CL_PROCESS_KIT_SPAWN, or on PATH"
                        +spawn-trampoline-name+)
                +spawn-trampoline-name+))
      ((null program)
       (funcall on-unavailable (format nil "cannot start ~A: not an executable file or not found on PATH" (first argv))
                (first argv)))
      (t
       (let ((handle
               (handler-case
                   ;; The bg log is acquired as a resource: reopened for append
                   ;; under O_NOFOLLOW and closed on any exit (WITH-BG-LOG).
                   (with-bg-log (log log-path)
                     (%spawn-supervisor trampoline argv program log exit-path))
                 (process-kit:process-launch-error (condition)
                   (return-from %launch-detached
                     (funcall on-unavailable (format nil "cannot start ~A: ~A" (first argv) condition)
                              (first argv))))
                 ((or file-error stream-error sb-posix:syscall-error process-kit:process-error) (condition)
                   (error 'aitools.process.application:process-port-error
                          :message (format nil "starting ~A failed: ~A" (first argv) condition))))))
         (%remember-supervisor handle)
         (funcall on-started (process-kit:process-id handle)))))))

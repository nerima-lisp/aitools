;;;; t/e2e/harness.lisp
;;;;
;;;; Process-level harness for the correspondence-table e2e cases. Every
;;;; aitools invocation runs the real executable as a child process: the one
;;;; named by AITOOLS_E2E_BINARY (the `nix build` output), or
;;;; one this harness builds once per image with ASDF's program-op. Every
;;;; case gets its own workspace directory and its own XDG_STATE_HOME, both
;;;; under one scratch root removed when the image exits.
;;;;
;;;; This file holds the runner side: scratch space, child processes, and
;;;; resolving the executable. Workspaces, oracles, and case registration
;;;; are in harness-cases.lisp.
(in-package #:aitools.e2e.test)

(defvar *run-token*
  (format nil "e2e~(~8,'0X~)" (random #xFFFFFFFF (make-random-state t)))
  "Names this image's scratch root and every workspace in it, so the meta
spec can prove no workspace of this run reached the real state directory.")

(defvar *scratch-root* nil)
(defvar *invocation-counter* 0)
(defvar *binary* nil
  "NIL before resolution, then the executable's pathname, or (:UNAVAILABLE
REASON) when it could not be built.")

(defvar *current-case* nil
  "The (ROW NAME) of the case whose body is running.")

(defvar *oracle-log* '()
  "One (ROW NAME KIND DETAIL) entry per expected value a case obtained:
KIND :SHELL with the script, or :FIXED with the missing tools.")

(defparameter +process-timeout-seconds+ 120)
(defparameter +build-timeout-seconds+ 1500)

;;; Scratch space

(defun scratch-root ()
  (or *scratch-root*
      (let ((root (uiop:ensure-directory-pathname
                   (merge-pathnames (format nil "aitools-~A" *run-token*)
                                    (uiop:temporary-directory)))))
        (ensure-directories-exist root)
        (setf *scratch-root* (truename root))
        (push (lambda () (remove-scratch-root)) sb-ext:*exit-hooks*)
        *scratch-root*)))

(defun remove-scratch-root ()
  (when (and *scratch-root* (probe-file *scratch-root*))
    (uiop:delete-directory-tree
     *scratch-root*
     :validate (lambda (path) (search *run-token* (namestring path))))))

(defun scratch-path (name)
  (merge-pathnames name (scratch-root)))

(defun native (pathname)
  (uiop:native-namestring pathname))

;;; Child processes

(defstruct (process-result (:conc-name result-))
  exit-code status stdout stderr)

(defun read-octets (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defun write-octets (pathname octets)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction :output :element-type '(unsigned-byte 8)
                                   :if-exists :supersede)
    (write-sequence octets stream))
  pathname)

(defun utf8 (octets)
  (sb-ext:octets-to-string octets :external-format :utf-8))

(defun octets (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun environment-with (overrides)
  "The inherited environment with each (NAME . VALUE) of OVERRIDES replacing
any inherited binding of NAME; a NIL VALUE removes the binding."
  (append (loop for (name . value) in overrides
                when value collect (format nil "~A=~A" name value))
          (remove-if (lambda (entry)
                       (let ((name (subseq entry 0 (or (position #\= entry) (length entry)))))
                         (assoc name overrides :test #'string=)))
                     (sb-ext:posix-environ))))

(defun run-process (program arguments &key directory environment input
                                           (timeout-seconds +process-timeout-seconds+))
  "Run PROGRAM with ARGUMENTS to completion, stdin from the octet vector INPUT
or /dev/null, and return a PROCESS-RESULT with both output streams as
octets. A process still running after TIMEOUT-SECONDS is killed (TERM, then
KILL) and the case fails naming the command."
  (let* ((n (incf *invocation-counter*))
         (out (scratch-path (format nil "io/~D.out" n)))
         (err (scratch-path (format nil "io/~D.err" n)))
         (in (and input (write-octets (scratch-path (format nil "io/~D.in" n)) input))))
    (ensure-directories-exist out)
    (let ((process (sb-ext:run-program program arguments
                                       :search t :wait nil
                                       :directory (and directory (native directory))
                                       :environment (or environment (sb-ext:posix-environ))
                                       :input in
                                       :output out :if-output-exists :supersede
                                       :error err :if-error-exists :supersede))
          (deadline (+ (get-internal-real-time)
                       (* timeout-seconds internal-time-units-per-second))))
      (loop while (sb-ext:process-alive-p process)
            do (when (> (get-internal-real-time) deadline)
                 (sb-ext:process-kill process 15)
                 (sleep 2)
                 (when (sb-ext:process-alive-p process)
                   (sb-ext:process-kill process 9))
                 (sb-ext:process-wait process)
                 (fail "~A" (format nil "~A ~{~A~^ ~} did not finish within ~Ds"
                                    program arguments timeout-seconds)))
               (sleep 0.01))
      (sb-ext:process-wait process)
      (prog1 (make-process-result :exit-code (sb-ext:process-exit-code process)
                                  :status (sb-ext:process-status process)
                                  :stdout (read-octets out)
                                  :stderr (read-octets err))
        (sb-ext:process-close process)))))

(defun tool-path (name)
  "NAME's absolute path on PATH, or NIL."
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
        for candidate = (and (plusp (length directory))
                             (probe-file (merge-pathnames name (uiop:ensure-directory-pathname directory))))
        when (and candidate (not (uiop:directory-pathname-p candidate)))
          return candidate))

;;; The executable under test

(defun build-script (root fasl-directory target)
  ;; program-op writes to aitools/cli's :build-pathname, which is the
  ;; checkout root; the OUTPUT-FILES method sends it to TARGET instead, so a
  ;; test run never leaves a binary in the working tree.
  (format nil "(require :asdf)
(asdf:initialize-output-translations
 '(:output-translations (t (~S :**/ :*.*.*)) :ignore-inherited-configuration))
(asdf:initialize-source-registry
 '(:source-registry (:directory ~S) :inherit-configuration))
(asdf:load-system \"aitools/cli\")
(defmethod asdf:output-files :around ((o asdf:program-op) (c (eql (asdf:find-system \"aitools/cli\"))))
  (values (list (pathname ~S)) t))
(asdf:operate 'asdf:program-op \"aitools/cli\")
"
          (native fasl-directory) (native root) (native target)))

(defun build-spawn-helper (directory)
  "Compile cl-process-kit's detach trampoline next to the built executable,
where the process context looks for it (flake.nix's delivered package does
the same). Returns NIL, or a string naming why it could not be built."
  (let ((cc (tool-path "cc"))
        (source (asdf:system-relative-pathname "cl-process-kit" "native/spawn.c")))
    (cond ((null cc) "cc is not on PATH")
          ((null (probe-file source)) (format nil "~A is missing" (native source)))
          (t (let ((result (run-process (native cc)
                                        (list "-std=c11" "-O2" "-Wall" "-Wextra" "-Werror"
                                              (native source) "-o"
                                              (native (merge-pathnames "cl-process-kit-spawn" directory))))))
               (unless (eql (result-exit-code result) 0)
                 (format nil "cc exited ~A: ~A" (result-exit-code result) (utf8 (result-stderr result)))))))))

(defun build-binary ()
  "Build aitools/cli into the scratch root in a child SBCL. Returns the
executable's pathname, or (:UNAVAILABLE REASON)."
  (let* ((directory (scratch-path "build/"))
         (target (merge-pathnames "aitools" directory))
         (script (merge-pathnames "build.lisp" directory))
         (log (merge-pathnames "build.log" directory)))
    (ensure-directories-exist script)
    (with-open-file (stream script :direction :output :if-exists :supersede)
      (write-string (build-script (asdf:system-source-directory "aitools")
                                  (merge-pathnames "fasl/" directory) target)
                    stream))
    (let ((result (run-process (native sb-ext:*runtime-pathname*)
                               (list "--non-interactive" "--no-sysinit" "--no-userinit"
                                     "--disable-debugger" "--load" (native script))
                               :timeout-seconds +build-timeout-seconds+)))
      (write-octets log (concatenate '(vector (unsigned-byte 8))
                                     (result-stdout result) (result-stderr result)))
      (cond ((not (and (eql (result-exit-code result) 0) (probe-file target)))
             (list :unavailable
                   (format nil "building aitools/cli with program-op failed (exit ~A); see ~A"
                           (result-exit-code result) (native log))))
             (t (let ((helper-problem (build-spawn-helper directory)))
                  (when helper-problem
                    (format *error-output* "~&aitools e2e: no bg spawn helper beside the built binary: ~A~%"
                            helper-problem)))
                target)))))

(defun executable-file-p (pathname)
  "True when PATHNAME is a regular file with an execute bit set."
  (and (not (uiop:directory-pathname-p pathname))
       (let ((mode (sb-posix:stat-mode (sb-posix:stat (native pathname)))))
         (and (= (logand mode #o170000) #o100000)
              (plusp (logand mode #o111))))))

(defun resolve-binary ()
  "Resolve the executable under test. A NAMED binary (AITOOLS_E2E_BINARY,
which the delivered Nix package sets) that is absent or not runnable is a
HARD unavailability: CI asked for the delivered package and must fail rather
than quietly skip. A build this harness attempts itself is a SOFT
unavailability, so a checkout that cannot build (no compiler, no source)
skips instead of failing."
  (let ((named (uiop:getenv "AITOOLS_E2E_BINARY")))
    (cond ((and named (plusp (length named)))
           (let ((path (probe-file named)))
             (cond ((null path)
                    (list :unavailable
                          (format nil "AITOOLS_E2E_BINARY names ~A, which does not exist" named)
                          :hard))
                   ((not (executable-file-p path))
                    (list :unavailable
                          (format nil "AITOOLS_E2E_BINARY names ~A, which is not an executable file" named)
                          :hard))
                   (t path))))
          (t (build-binary)))))

(defun aitools-binary ()
  "The executable under test. A soft unavailability skips the calling case
with the reason; a hard one (a broken AITOOLS_E2E_BINARY) fails it, so a CI
run that was handed an unusable delivered binary cannot report green."
  (unless *binary*
    (setf *binary* (resolve-binary)))
  (if (and (consp *binary*) (eq (first *binary*) :unavailable))
      (if (eq (third *binary*) :hard)
          (fail "~A" (second *binary*))
          (skip (second *binary*)))
      *binary*))

;;;; t/unit/process/fakes.lisp
;;;;
;;;; In-memory PROCESS-PORTS for the application flows: a file table keyed by
;;;; namestring, a clock that only moves when a flow sleeps, and a process
;;;; table whose members react to signals the way a scenario says.
(in-package #:aitools.process.test)

(defstruct (fake-world (:constructor %make-fake-world))
  (files (make-hash-table :test 'equal))
  (clock 0)
  ;; pid -> (:alive BOOLEAN :on-term KEYWORD :on-kill KEYWORD :exit-path STRING
  ;;         [:exit-on-probe STATUS])
  (processes (make-hash-table))
  (signals '())
  (next-pid 1000)
  (launches '())
  (run-calls '())
  (reads '())
  run-behavior
  (launch-behavior :start)
  ;; True: every exclusive create finds the path taken, as if concurrent
  ;; starts kept claiming each ID first.
  (create-fails nil)
  ;; True: REPLACE-FILE signals PROCESS-PORT-ERROR, as a full disk would.
  (replace-fails nil)
  (bg-directory #p"/state/ws/bg/")
  (open-ports '())
  ;; (TIME . PATH/TEXT): files that appear once the clock reaches TIME.
  (scheduled-files '()))

(defun make-fake-world (&rest initargs)
  (apply #'%make-fake-world initargs))

(defun fake-file (world path)
  (gethash (namestring path) (fake-world-files world)))

(defun (setf fake-file) (text world path)
  (setf (gethash (namestring path) (fake-world-files world)) text))

(defun %materialize-scheduled (world)
  (setf (fake-world-scheduled-files world)
        (remove-if (lambda (entry)
                     (destructuring-bind (time path . text) entry
                       (when (<= time (fake-world-clock world))
                         (setf (fake-file world path) text)
                         t)))
                   (fake-world-scheduled-files world))))

(defun %process-exit (world pid status)
  (let ((process (gethash pid (fake-world-processes world))))
    (setf (getf process :alive) nil)
    (when status
      (setf (fake-file world (getf process :exit-path)) (format nil "~D~%" status)))
    (setf (gethash pid (fake-world-processes world)) process)))

(defun %fake-signal (world pid signal)
  (let ((process (gethash pid (fake-world-processes world))))
    (when (and process (getf process :alive))
      (push (cons pid signal) (fake-world-signals world))
      (cond ((and (= signal 9) (not (eq (getf process :on-kill) :ignore))) (%process-exit world pid nil))
            ((eq (getf process :on-term) :exit) (%process-exit world pid nil)))
      t)))

(defun fake-ports (world)
  (aitools.process.application:make-process-ports
   :run-program (lambda (argv timeout-ms &key stdout-path on-exited on-timed-out on-unavailable on-exists)
                  (declare (ignore stdout-path on-exists))
                  (push (list argv timeout-ms) (fake-world-run-calls world))
                  (destructuring-bind (kind &rest arguments) (fake-world-run-behavior world)
                    (ecase kind
                      (:exited (funcall on-exited (apply #'aitools.process.domain:make-process-outcome arguments)))
                      (:timed-out (funcall on-timed-out (apply #'aitools.process.domain:make-process-outcome
                                                               :timed-out t arguments)))
                      (:unavailable (funcall on-unavailable "not found" (first argv))))))
   :bg-directory (lambda () (fake-world-bg-directory world))
   :launch-detached (lambda (argv log-path exit-path &key on-started on-unavailable)
                      (push (list argv log-path exit-path) (fake-world-launches world))
                      (ecase (fake-world-launch-behavior world)
                        (:unavailable (funcall on-unavailable "no trampoline" "cl-process-kit-spawn"))
                        ((:start :ignore-term :ignore-kill)
                         (let ((pid (incf (fake-world-next-pid world))))
                           (setf (gethash pid (fake-world-processes world))
                                 (list :alive t :exit-path (namestring exit-path)
                                       :on-term (if (eq (fake-world-launch-behavior world) :start) :exit :ignore)
                                       :on-kill (if (eq (fake-world-launch-behavior world) :ignore-kill) :ignore :exit)))
                           (funcall on-started pid)))))
   :list-directory (lambda (directory)
                     (loop for path being the hash-keys of (fake-world-files world)
                           when (string= (namestring directory) (directory-namestring path))
                             collect (file-namestring path)))
   :read-file-text (lambda (path)
                     (push (namestring path) (fake-world-reads world))
                     (fake-file world path))
   :read-file-octets (lambda (path start end)
                       (push (namestring path) (fake-world-reads world))
                       (subseq (string-bytes (or (fake-file world path) "")) start end))
   :file-size (lambda (path)
                (let ((text (fake-file world path)))
                  (and text (length (string-bytes text)))))
   :create-file-exclusive (lambda (path text)
                            (unless (or (fake-world-create-fails world) (fake-file world path))
                              (setf (fake-file world path) text)
                              t))
   :replace-file (lambda (path text)
                   (when (fake-world-replace-fails world)
                     (error 'aitools.process.application:process-port-error
                            :message (format nil "writing ~A failed: No space left on device" path)))
                   (setf (fake-file world path) text))
   :remove-file (lambda (path) (remhash (namestring path) (fake-world-files world)))
   :group-alive-p (lambda (pid)
                    (let ((status (getf (gethash pid (fake-world-processes world)) :exit-on-probe)))
                      ;; The supervisor writes its exit file just as the probe
                      ;; finds the group gone.
                      (when status
                        (%process-exit world pid status)))
                    (getf (gethash pid (fake-world-processes world)) :alive))
   :signal-group (lambda (pid signal) (%fake-signal world pid signal))
   :tcp-connectable-p (lambda (port) (member port (fake-world-open-ports world)))
   :universal-time (lambda () (encode-universal-time 0 0 0 26 9 2026 0))
   :monotonic-ms (lambda () (fake-world-clock world))
   :sleep-ms (lambda (milliseconds)
               (incf (fake-world-clock world) milliseconds)
               (%materialize-scheduled world))))

(defun run-flow (flow &rest arguments)
  "Run FLOW through the command-result contract; return (VALUES KIND FIELDS)."
  (let ((result (aitools.protocol.application:call-with-command-result/k
                 (lambda (&rest continuations) (apply flow (append arguments continuations))))))
    (values (aitools.protocol.application:command-result-kind result)
            (aitools.protocol.application:command-result-fields result))))

(defun field (fields key)
  "KEY's value in an :OK/:PARTIAL alist, or in an :ERROR plist when KEY is a
keyword."
  (if (keywordp key)
      (getf fields key)
      (cdr (assoc key fields :test #'string=))))

(defun repair-commands (fields)
  (mapcar (lambda (repair) (getf repair :command)) (getf fields :repairs)))

(in-package #:aitools.env.test)

(defun snapshot-section (fields name)
  (object-field (field fields "sections") name))

(defun expect-snapshot-deadline-reason (section)
  (expect (object-field section "status") :to-equal "unavailable")
  (expect (object-field (object-field section "reason") "code")
          :to-equal "environment.timeout")
  (expect (json-null-p (object-field section "data")) :to-be t))

(describe "aitools.env.application snapshot deadline"
  (it "collects every section before the configured deadline"
    (let* ((clock (cl-boundary-kit:make-fake-clock
                   :start *fake-epoch-milliseconds* :monotonic-start 0))
           (boundaries
             (cl-boundary-kit:boundary-context-with
              (metrics-boundaries) :env-clock clock)))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-snapshot/k boundaries :timeout "1s")
        (expect kind :to-be :ok)
        (expect (field fields "deadline_reached") :to-be json-kit:+json-false+)
        (expect (field fields "overall_timeout_ms") :to-be 1000)
        (dolist (name '("info" "metrics" "network" "disk" "ports"))
          (expect (object-field (snapshot-section fields name) "status")
                  :to-equal "ok")))))

  (it "marks the active and later sections unavailable after the deadline"
    (let* ((clock (cl-boundary-kit:make-fake-clock
                   :start *fake-epoch-milliseconds* :monotonic-start 0))
           (boundaries
             (cl-boundary-kit:boundary-context-with
              (cl-boundary-kit:boundary-context-with
               (metrics-boundaries) :env-clock clock)
              :env-sleep-seconds
              (lambda (seconds)
                (declare (ignore seconds))
                (cl-boundary-kit:advance-fake-clock
                 clock 2000 :monotonic-delta (* 2 internal-time-units-per-second))))))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-snapshot/k boundaries :timeout "1s")
        (expect kind :to-be :partial)
        (expect (field fields "deadline_reached") :to-be t)
        (expect (object-field (snapshot-section fields "info") "status")
                :to-equal "ok")
        (dolist (name '("metrics" "network" "disk" "ports"))
          (expect-snapshot-deadline-reason (snapshot-section fields name))))))
  (it "caps the CPU sample to the remaining budget"
    (let* ((clock (cl-boundary-kit:make-fake-clock
                   :start *fake-epoch-milliseconds* :monotonic-start 0))
           (sleep-calls 0)
           (sleep-seconds nil)
           (boundaries
             (cl-boundary-kit:boundary-context-with
              (cl-boundary-kit:boundary-context-with
               (metrics-boundaries) :env-clock clock)
              :env-sleep-seconds
              (lambda (seconds)
                (incf sleep-calls)
                (setf sleep-seconds seconds)
                (cl-boundary-kit:advance-fake-clock
                 clock (* 1000 seconds)
                 :monotonic-delta (* seconds internal-time-units-per-second))))))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-snapshot/k boundaries :timeout "500ms")
        (expect kind :to-be :partial)
        (expect sleep-calls :to-be 1)
        (expect (= sleep-seconds 1/2) :to-be t)
        (expect (field fields "overall_timeout_ms") :to-be 500)
        (expect (not (search "600-second" (field fields "collection_bounds")))
                :to-be t)
        (expect (field fields "deadline_reached") :to-be t)
        (expect (object-field (snapshot-section fields "info") "status")
                :to-equal "ok")
        (dolist (name '("metrics" "network" "disk" "ports"))
          (expect-snapshot-deadline-reason (snapshot-section fields name))))))

  #+linux
  (it "stops the proc socket-owner walk before later fd entries"
    (let* ((clock (cl-boundary-kit:make-fake-clock
                   :start *fake-epoch-milliseconds* :monotonic-start 0))
           (base (cl-boundary-kit:boundary-context-with
                  (linux-metrics-boundaries) :env-clock clock))
           (list-directory
             (cl-boundary-kit:boundary-context-require base :env-list-directory))
           (read-link (cl-boundary-kit:boundary-context-require base :env-read-link))
           (visited nil)
           (fd-entries nil)
           (links nil)
           (with-directories
             (cl-boundary-kit:boundary-context-with
              base :env-list-directory
              (lambda (path &key check-deadline)
                (push path visited)
                (loop for entry in (funcall list-directory path)
                      do (when check-deadline (funcall check-deadline))
                         (when (string= path "/proc/77/fd")
                           (push entry fd-entries)
                           (when (string= entry "3")
                             (cl-boundary-kit:advance-fake-clock
                              clock 2000 :monotonic-delta (* 2 internal-time-units-per-second))))
                      collect entry))))
           (boundaries
             (cl-boundary-kit:boundary-context-with
              with-directories :env-read-link
              (lambda (path)
                (push path links)
                (funcall read-link path)))))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-snapshot/k boundaries :timeout "1s")
        (expect kind :to-be :partial)
        (expect (field fields "deadline_reached") :to-be t)
        (expect-snapshot-deadline-reason (snapshot-section fields "ports"))
        (expect (nreverse visited)
                :to-equal '("/proc" "/proc/1/fd" "/proc/77/fd"))
        (expect (nreverse fd-entries) :to-equal '("0" "3"))
        (expect (nreverse links)
                :to-equal '("/proc/1/fd/9" "/proc/1/fd/10")))))
  #+linux
  (it "checks the deadline between workspace ancestor stats"
    (let ((checks 0))
      (handler-case
          (aitools.env.infrastructure::%workspace-root
           :check-deadline
           (lambda ()
             (incf checks)
             (when (= checks 2)
               (error 'aitools.env.application::snapshot-deadline-exceeded))))
        (aitools.env.application::snapshot-deadline-exceeded () nil))
      (expect checks :to-be 2)))

  #+darwin
  (it "gives lsof only the remaining overall time"
    (let* ((clock (cl-boundary-kit:make-fake-clock
                   :start *fake-epoch-milliseconds* :monotonic-start 0))
           (base (cl-boundary-kit:boundary-context-with
                  (make-fake-env-boundaries :host (metrics-host)) :env-clock clock))
           (run-program (cl-boundary-kit:boundary-context-require base :env-run-program))
           (lsof-timeout nil)
           (boundaries
             (cl-boundary-kit:boundary-context-with
              base :env-run-program
              (lambda (program arguments &key timeout)
                (if (string= program "/usr/sbin/lsof")
                    (progn
                      (setf lsof-timeout timeout)
                      (cl-boundary-kit:advance-fake-clock
                       clock 1000 :monotonic-delta internal-time-units-per-second)
                      (values :timeout nil "" ""))
                    (funcall run-program program arguments :timeout timeout))))))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-snapshot/k boundaries :timeout "1s")
        (expect kind :to-be :partial)
        (expect lsof-timeout :to-be 1)
        (expect (field fields "deadline_reached") :to-be t)
        (expect-snapshot-deadline-reason (snapshot-section fields "ports"))))))
(describe "aitools.env.application snapshot deadline guards"
  (it "omits the username when id does not return a usable name"
    (let ((clock (cl-boundary-kit:make-fake-clock :start *fake-epoch-milliseconds* :monotonic-start 0)))
      (dolist (outcome '((:not-started nil nil "") (:exited 1 "tester" "") (:exited 0 "  " "")))
        (let* ((host (make-fake-host :programs (list (cons "/usr/bin/id"
                          (lambda (arguments timeout) (declare (ignore arguments timeout)) (values-list outcome))))))
               (boundaries (cl-boundary-kit:boundary-context-with
                            (make-fake-env-boundaries :host host) :env-clock clock))
               (aitools.env.application::*snapshot-deadline* (* 2 internal-time-units-per-second)))
          (expect (aitools.env.application::%snapshot-user-name boundaries) :to-be nil)))))

  (it "requires an explicit positive timeout for process operations"
    (let* ((clock (cl-boundary-kit:make-fake-clock :start *fake-epoch-milliseconds* :monotonic-start 0))
           (boundaries (cl-boundary-kit:boundary-context-with (make-fake-env-boundaries) :env-clock clock)))
      (let ((aitools.env.application::*snapshot-deadline* (* 2 internal-time-units-per-second)))
        (dolist (arguments '(("/bin/true" nil) ("/bin/true" nil :timeout 0)))
          (let ((failed nil) (called nil))
            (handler-case
                (aitools.env.application::%call-with-env-operation-deadline
                 boundaries :env-run-program
                 (lambda (program args &key timeout) (declare (ignore program args timeout)) (setf called t))
                 arguments)
              (error () (setf failed t)))
            (expect failed :to-be t)
            (expect called :to-be nil))))))

  (it "does not invoke process or sleep operations after the remaining budget expires"
    (dolist (case '((:env-run-program ("/bin/true" nil :timeout 1)) (:env-sleep-seconds (1))))
      (destructuring-bind (key arguments) case
        (let* ((reads 0)
               (clock (cl-boundary-kit:make-clock :monotonic-fn
                        (lambda () (if (= (incf reads) 1) 0 internal-time-units-per-second))))
               (boundaries (cl-boundary-kit:boundary-context-with (make-fake-env-boundaries) :env-clock clock))
               (expired nil) (called nil))
          (let ((aitools.env.application::*snapshot-deadline* internal-time-units-per-second))
            (handler-case
                (aitools.env.application::%call-with-env-operation-deadline
                 boundaries key (lambda (&rest ignored) (declare (ignore ignored)) (setf called t)) arguments)
              (aitools.env.application::snapshot-deadline-exceeded () (setf expired t))))
          (expect expired :to-be t)
          (expect called :to-be nil)))))

  (it "rejects nonpositive and unrepresentable snapshot timeouts"
    (let ((boundaries (make-fake-env-boundaries)))
      (dolist (timeout (list nil "0ms" (format nil "~As" (expt 10 309))))
        (multiple-value-bind (kind fields) (run-flow #'sys-snapshot/k boundaries :timeout timeout)
          (expect kind :to-be :error)
          (expect (first fields) :to-equal "argument.invalid")))))

  (it "preserves the source file error when deadline cleanup runs"
    (let* ((clock (cl-boundary-kit:make-fake-clock :start *fake-epoch-milliseconds* :monotonic-start 0))
           (boundaries
             (cl-boundary-kit:boundary-context-with
              (cl-boundary-kit:boundary-context-with (metrics-boundaries) :env-clock clock)
              :env-workspace-root
              (lambda (&key check-deadline)
                (declare (ignore check-deadline))
                (cl-boundary-kit:advance-fake-clock clock 1000
                  :monotonic-delta internal-time-units-per-second)
                (error 'file-error :pathname #p"/work")))))
      (multiple-value-bind (kind fields) (run-flow #'sys-snapshot/k boundaries :timeout "1s")
        (expect kind :to-be :error)
        (let* ((diagnostics (getf (cddr fields) :diagnostics))
               (snapshot (first diagnostics))
               (info (object-field (object-field snapshot "sections") "info")))
          (expect (length diagnostics) :to-be 1)
          (expect (object-field snapshot "deadline_reached") :to-be t)
          (expect (object-field info "status") :to-equal "unavailable")
          (expect (object-field info "reason") :to-equal "source_file_unreadable"))))))

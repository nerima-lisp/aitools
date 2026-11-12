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

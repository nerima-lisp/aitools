(in-package #:aitools.env.test)

(describe "aitools.env.application sys metrics"
  #+linux
  (it-each (("/fixture/bin/ps" :ok 30 30d0 1000)
            ("/fixture/bin/ps" :ok 9 37.5d0 1000)
            (nil :error 30 nil 1000)
            ("/fixture/bin/ps" :error 30 nil 0))
      "resolves Linux process metrics via PATH: ~S"
      (executable expected-kind iowait expected-cpu total-memory)
    (let ((reads 0) (sleeps nil) (runs 0) (unused (constantly nil)))
      (let ((boundaries
              (aitools.env.infrastructure:make-env-boundaries
               :clock (cl-boundary-kit:make-fake-clock :start 0)
               :environment
               (cl-boundary-kit:make-test-environment
                :initial-values '(("PATH" . "/fixture/bin")))
               :filesystem
               (cl-boundary-kit:make-filesystem
                :read-file-fn
                (lambda (path &key external-format)
                  (declare (ignore external-format))
                  (cond ((equal path "/proc/stat")
                         (if (= (incf reads) 1)
                             "cpu 10 0 10 70 10 0 0 0"
                             (format nil "cpu 30 0 20 120 ~D 0 0 0" iowait)))
                        ((equal path "/proc/cpuinfo")
                         (format nil "processor : 0~%processor : 1~%"))
                        ((equal path "/proc/meminfo")
                         (format nil
                                 "MemTotal: ~D kB~%MemAvailable: 250 kB~%"
                                 total-memory))
                        ((equal path "/proc/loadavg") "0.75 1.0 2.5 3/100 42")))
                :path-exists-p-fn
                (lambda (path)
                  (member path '("/proc/stat" "/proc/cpuinfo" "/proc/meminfo" "/proc/loadavg")
                          :test #'equal)))
               :host-info (cl-boundary-kit:make-test-host-info
                           :hostname "fake-host" :username "tester")
               :list-directory unused
               :read-link unused
               :executable-p (lambda (path) (equal path executable))
               :file-system-space unused
               :user-id unused
               :workspace-root unused
               :system-identity (lambda () (values "Linux" "test" "generic"))
               :sleep-seconds (lambda (seconds) (push seconds sleeps))
               :run-program (lambda (program arguments timeout)
                              (declare (ignore timeout))
                              (incf runs)
                              (expect program :to-equal executable)
                              (expect arguments :to-equal
                                      '("-ax" "-o" "pid=,pcpu=,rss=,comm="))
                              (values :exited 0 "10 120.0 100 worker" "")))))
        (multiple-value-bind (kind fields) (run-flow #'sys-metrics/k boundaries)
          (expect kind :to-be expected-kind)
          (expect runs :to-be (if executable 1 0))
          (expect reads :to-be 2)
          (expect sleeps :to-equal '(1))
          (if expected-cpu
              (progn
                (expect (object-field (field fields "cpu") "busy_percent") :to-equal expected-cpu)
                (expect (object-field (field fields "memory") "available_bytes") :to-be 256000)
                (expect (mapcar (lambda (name)
                                  (object-field (field fields "load_average") name))
                                (list "one_minute" "five_minutes" "fifteen_minutes"))
                        :to-equal (list 0.75d0 1d0 2.5d0))
                (expect (field fields "process_cpu_basis") :to-equal "lifetime_average"))
              (expect (first fields) :to-equal "environment.unavailable"))))))
  #+linux
(it "uses the default process limit for metrics"
  (let ((process-output
          (with-output-to-string (stream)
            (loop for pid from 1 to 21
                  do (format stream "~D 1.0 100 worker~D~%" pid pid)))))
    (multiple-value-bind (kind fields)
        (run-flow #'sys-metrics/k
                  (linux-metrics-boundaries :process-output process-output))
      (expect kind :to-be :partial)
      (expect (field fields "total") :to-be 21)
      (expect (length (field fields "items")) :to-be 20)
      (expect (field fields "truncated") :to-be t))))
  #+darwin
  (it "bounds sampling and process output with deterministic CPU ordering"
    (let ((host (metrics-host)))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-metrics/k
                    (make-fake-env-boundaries :host host :environment '(("PATH" . "/untrusted/bin"))
                                               :executables '("/untrusted/bin/ps"))
                    :limit 1)
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 3)
        (expect (field fields "truncated") :to-be t)
        (expect (length (field fields "items")) :to-be 1)
        (expect (object-field (first (field fields "items")) "pid") :to-be 10)
        (expect (object-field (field fields "cpu") "busy_percent") :to-equal 7.5d0)
        (expect (field fields "sample_interval_ms") :to-be 1000)
        (expect (field fields "process_cpu_basis") :to-equal "decaying_average")
        (expect (assoc "/usr/bin/top" (fake-host-runs host) :test #'equal) :to-be nil)
        (expect (second (assoc "/bin/ps" (fake-host-runs host) :test #'equal))
                :to-equal '("-ax" "-o" "pid=,pcpu=,rss=,comm=")))))
  #+darwin
  (it "returns success when all process rows fit"
    (multiple-value-bind (kind fields)
        (run-flow (function sys-metrics/k)
                  (make-fake-env-boundaries :host (metrics-host)))
      (expect kind :to-be :ok)
      (expect (length (field fields "items")) :to-be 3)
      (expect (field fields "truncated") :to-be nil)
      (expect (mapcar (lambda (name)
                        (object-field (field fields "load_average") name))
                      (list "one_minute" "five_minutes" "fifteen_minutes"))
              :to-equal (list 0.5d0 0.25d0 0.125d0))))
  #+darwin
  (it-each (("/bin/ps") ("/usr/bin/vm_stat") ("/usr/sbin/sysctl"))
      "fails when metrics source ~S fails" (program)
    (let ((host (metrics-host)))
      (push (cons program (exits-with "" :exit-code 1)) (fake-host-programs host))
      (multiple-value-bind (kind error) (run-flow #'sys-metrics/k (make-fake-env-boundaries :host host))
        (expect kind :to-be :error)
        (expect (first error) :to-equal "environment.unavailable"))))
  #+darwin
  (it "samples Darwin counters twice with exactly one requested second"
    (let ((reads 0) (sleeps nil))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-metrics/k
                    (make-fake-env-boundaries :host (metrics-host)
                                     :sleep-seconds (lambda (seconds) (push seconds sleeps))
                                     :cpu-counters (lambda () (incf reads) (list reads 0 0 reads 0 0 0 0))))
        (expect kind :to-be :ok)
        (expect reads :to-be 2)
        (expect sleeps :to-equal '(1))
        (expect (object-field (field fields "cpu") "busy_percent") :to-equal 50d0))))
  #+darwin
  (it-each ((nil) (((0 0 0 0 0 0 0 0)))
            (((0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)))
            (((4294967295 0 0 0 0 0 0 0) (0 0 0 10 0 0 0 0))))
      "fails when Darwin counter samples are unavailable, stationary, or wrapped: ~S" (samples)
    (multiple-value-bind (kind error)
        (run-flow #'sys-metrics/k (make-fake-env-boundaries :host (metrics-host)
                                                  :cpu-counters (lambda () (pop samples))))
      (expect kind :to-be :error)
      (expect (first error) :to-equal "environment.unavailable")))
  (it "rejects an unsupported operating system"
    (multiple-value-bind (kind error)
        (run-flow (function sys-metrics/k)
                  (make-fake-env-boundaries
                   :identity (list "Other" "1" "generic")))
      (expect kind :to-be :error)
      (expect (first error) :to-equal "environment.unavailable")))
  #+darwin
  (it "fails when Darwin load averages are unavailable"
    (multiple-value-bind (kind error)
        (run-flow (function sys-metrics/k)
                  (make-fake-env-boundaries :sysctl-mibs nil
                                  :host (metrics-host)))
      (expect kind :to-be :error)
      (expect (first error) :to-equal "environment.unavailable"))))

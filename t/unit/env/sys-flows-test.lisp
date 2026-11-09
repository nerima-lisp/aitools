(in-package #:aitools.env.test)

#+linux
(describe "aitools.env.application sys metrics Linux unavailable load average"
  (it "fails when load average is unavailable"
    (multiple-value-bind (kind error)
        (run-flow (function sys-metrics/k)
                  (linux-metrics-boundaries :load-average-text nil))
      (expect kind :to-be :error)
      (expect (first error) :to-equal "environment.unavailable"))))

(defun metrics-host ()
  (let ((host (darwin-host)))
    (push (cons "/bin/ps"
                (exits-with
                 (format nil
                         "20 25.0 200 /usr/bin/worker~%10 25.0 100 /usr/bin/worker~%30 0.0 0 idle~%")))
          (fake-host-programs host))
    (push (cons "/usr/sbin/netstat" (exits-with *network-darwin-text*))
          (fake-host-programs host))
    host))
(defun linux-metrics-boundaries (&key (space '(1000 400)) (load-average-text "0.75 1.0 2.5 3/100 42"))
  (let ((stat-reads 0)
        (files
          (list (cons "/proc/uptime" "1234.5 600.0")
                (cons "/proc/loadavg" load-average-text)
                (cons "/proc/cpuinfo" (format nil "processor : 0~%processor : 1~%"))
                (cons "/proc/meminfo" (format nil "MemTotal: 16384000 kB~%MemAvailable: 12000000 kB~%"))
                (cons "/etc/passwd" (format nil "root:x:0:0::/root:/bin/sh~%take:x:1000:100::/home/take:/bin/sh~%"))
                (cons "/proc/1/stat" "1 (systemd) S 0 1 1 0 -1 4194560 1 0 0 0 1 1 0 0 20 0 1 0 5 1 1")
                (cons "/proc/1/status" (format nil "Uid:~C0~C0~C0~C0~%" #\Tab #\Tab #\Tab #\Tab))
                (cons "/proc/1/cmdline" (format nil "/sbin/init~C" (code-char 0)))
                (cons "/proc/1/comm" (format nil "systemd~%"))
                (cons "/proc/77/stat" "77 (python3) S 1 77 77 0 -1 4194560 1 0 0 0 1 1 0 0 20 0 1 0 250 1 1")
                (cons "/proc/77/status" (format nil "Uid:~C1000~C1000~C1000~C1000~%" #\Tab #\Tab #\Tab #\Tab))
                (cons "/proc/77/cmdline"
                      (format nil "python3~C-m~Chttp.server~C"
                              (code-char 0) (code-char 0) (code-char 0)))
                (cons "/proc/77/comm" (format nil "python3~%"))
                (cons "/proc/net/dev" *network-linux-text*)
                (cons "/proc/net/tcp" *proc-net-tcp-text*)
                (cons "/proc/net/tcp6" *proc-net-tcp6-text*)))
        (directories '(("/proc" "1" "77" "self" "net")
                       ("/proc/1/fd" "9" "10")
                       ("/proc/77/fd" "0" "3" "4" "5")))
        (links '(("/proc/77/fd/0" . "/dev/null")
                 ("/proc/77/fd/3" . "socket:[55501]")
                 ("/proc/77/fd/4" . "socket:[55502]")
                 ("/proc/77/fd/5" . "socket:[21343]")
                 ("/proc/1/fd/9" . "socket:[55501]")
                 ("/proc/1/fd/10" . "socket:[21342]"))))
    (aitools.env.infrastructure:make-env-boundaries
     :clock (cl-boundary-kit:make-fake-clock :start *fake-epoch-milliseconds*)
     :environment
     (cl-boundary-kit:make-test-environment
      :initial-values '(("PATH" . "/fixture/bin") ("SHELL" . "/bin/bash")))
     :filesystem
     (cl-boundary-kit:make-filesystem
      :read-file-fn
      (lambda (path &key external-format)
        (declare (ignore external-format))
        (if (equal path "/proc/stat")
            (let ((sample (incf stat-reads)))
              (format nil "cpu ~D 0 0 ~D 0 0 0 0~%btime 1700000000~%"
                      (* sample 10) (* sample 90)))
            (cdr (assoc path files :test #'string=))))
      :path-exists-p-fn
      (lambda (path)
        (or (equal path "/proc/stat") (assoc path files :test #'string=))))
     :host-info (cl-boundary-kit:make-test-host-info :hostname "fake-host" :username "tester")
     :list-directory (lambda (path) (cdr (assoc path directories :test #'string=)))
     :read-link (lambda (path) (cdr (assoc path links :test #'string=)))
     :executable-p (lambda (path) (equal path "/fixture/bin/ps"))
     :run-program
     (lambda (program arguments timeout)
       (declare (ignore arguments timeout))
       (if (equal program "/fixture/bin/ps")
           (values :exited 0
                   (format nil
                           "20 25.0 200 /usr/bin/worker~%10 25.0 100 /usr/bin/worker~%30 0.0 0 idle~%")
                   "")
           (values :not-started nil nil nil)))
     :file-system-space (lambda (path) (declare (ignore path)) (values-list space))
     :system-identity (lambda () (values "Linux" "6.8.0-45-generic" "x86_64"))
     :user-id (lambda () 1000)
     :workspace-root (lambda () "/work")
     :sleep-seconds (lambda (seconds) (declare (ignore seconds))))))

(defun metrics-boundaries (&key (space '(1000 400)))
  #+linux (linux-metrics-boundaries :space space)
  #+darwin (make-fake-env-boundaries :host (metrics-host) :space space)
  #-(or linux darwin) (make-fake-env-boundaries :space space))

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
(describe "aitools.env.application Linux CPU and memory"
  (it "accepts populated sources and preserves missing values"
    (expect (multiple-value-list
             (aitools.env.application::%cpus-and-memory
              (linux-metrics-boundaries) :linux))
            :to-equal (list 2 (* 16384000 1024) (* 12000000 1024)))
    (expect (multiple-value-list
             (aitools.env.application::%cpus-and-memory
              (make-fake-env-boundaries :files (list (cons "/proc/cpuinfo" "")))
              :linux))
            :to-equal (list nil nil nil))))


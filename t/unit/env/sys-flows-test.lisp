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
(defun linux-metrics-boundaries (&key
     (space '(1000 400))
     (load-average-text "0.75 1.0 2.5 3/100 42")
     (process-output
       (format nil
               "20 25.0 200 /usr/bin/worker~%10 25.0 100 /usr/bin/worker~%30 0.0 0 idle~%")))
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
           (values :exited 0 process-output "")
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


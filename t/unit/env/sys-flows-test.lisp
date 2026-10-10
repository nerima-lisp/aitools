;;;; t/unit/env/sys-flows-test.lisp
(in-package #:aitools.env.test)

(describe "aitools.env.application network and disk"
  (it "limits sorted Linux interfaces and reports cumulative units"
    (multiple-value-bind (kind fields)
        (run-flow #'sys-network/k (make-fake-ports :identity '("Linux" "test" "generic")
                                                  :files (list (cons "/proc/net/dev" *network-linux-text*))) :limit 1)
      (expect kind :to-be :partial)
      (expect (field fields "total") :to-be 2)
      (expect (field fields "truncated") :to-be t)
      (expect (field fields "basis") :to-equal "cumulative_interface_counters")
      (expect (object-field (field fields "units") "bytes") :to-equal "bytes")
      (expect (object-field (first (field fields "items")) "name") :to-equal "a0")))
  (it "uses trusted Darwin netstat with fixed arguments despite PATH"
    (let ((host (make-fake-host :programs (list (cons "/usr/sbin/netstat" (exits-with *network-darwin-text*))))))
      (expect (run-flow #'sys-network/k (make-fake-ports :host host :environment '(("PATH" . "/untrusted")))) :to-be :ok)
      (expect (fake-host-runs host) :to-equal '(("/usr/sbin/netstat" ("-ibn"))))))
  (it-each ((0) (1001) (-1)) "rejects out of range network limit ~S" (limit)
    (multiple-value-bind (kind error) (run-flow #'sys-network/k (make-fake-ports) :limit limit)
      (expect kind :to-be :error)
      (expect (first error) :to-equal "argument.invalid")))
  (it "does not turn unavailable or unsupported counters into an empty success"
    (dolist (identity '(("Other" "1" "generic") ("Linux" "1" "generic") ("Darwin" "1" "generic")))
      (multiple-value-bind (kind error) (run-flow #'sys-network/k (make-fake-ports :identity identity))
        (expect kind :to-be :error)
        (expect (first error) :to-equal "environment.unavailable"))))
  (it "reports one filesystem with bytes and an explicit availability basis"
    (multiple-value-bind (kind fields) (run-flow #'sys-disk/k (make-fake-ports))
      (expect kind :to-be :ok)
      (expect (field fields "path") :to-equal "/work")
      (expect (field fields "scope") :to-equal "filesystem_containing_path")
      (expect (field fields "total_bytes") :to-be 1000)
      (expect (field fields "available_bytes") :to-be 400)
      (expect (field fields "available_basis") :to-equal "unprivileged_available_blocks")))
  (it-each ((nil) ((100 101)) ((100 -1))) "rejects unavailable or inconsistent disk capacity ~S" (space)
    (multiple-value-bind (kind error) (run-flow #'sys-disk/k (make-fake-ports :space space))
      (expect kind :to-be :error)
      (expect (first error) :to-equal "environment.unavailable"))))

(describe "aitools.env.application snapshot"
  (it "succeeds when all five sources are available within output limits"
    (let ((host (metrics-host)))
      (push (cons "/usr/sbin/netstat" (exits-with *network-darwin-text*))
            (fake-host-programs host))
      (multiple-value-bind (kind fields) (run-flow #'sys-snapshot/k (make-fake-ports :host host))
        (expect kind :to-be :ok)
        (dolist (name '("info" "metrics" "network" "disk" "ports"))
          (expect (object-field (object-field (field fields "sections") name) "status")
                  :to-equal "ok")))))
  (it "preserves missing observations and independent source failures"
    (multiple-value-bind (kind fields) (run-flow #'sys-snapshot/k (make-fake-ports))
      (expect kind :to-be :partial)
      (expect (field fields "scope") :to-equal "host")
      (expect (field fields "atomic") :to-be json-kit:+json-false+)
      (expect (integerp (field fields "started_at_epoch_ms")) :to-be t)
      (let ((sections (field fields "sections")))
        (expect (object-field (object-field sections "info") "status") :to-equal "partial")
        (expect (object-field (object-field sections "disk") "status") :to-equal "ok")
        (expect (object-field (object-field sections "network") "status") :to-equal "unavailable")
        (expect (object-field (object-field sections "metrics") "status") :to-equal "unavailable")
        (expect (object-field (object-field sections "ports") "status") :to-equal "unavailable"))))
  (it "bounds port results without changing the standalone ports flow"
    (let ((ports (make-fake-ports :host (darwin-host))))
      (multiple-value-bind (kind fields) (run-flow #'sys-snapshot/k ports :port-limit 1)
        (expect kind :to-be :partial)
        (let* ((section (object-field (field fields "sections") "ports"))
               (data (object-field section "data")))
          (expect (object-field section "status") :to-equal "partial")
          (expect (length (object-field data "items")) :to-be 1)
          (expect (object-field data "truncated") :to-be t)))
      (multiple-value-bind (kind fields) (run-flow #'sys-ports/k ports)
        (expect kind :to-be :ok)
        (expect (> (length (field fields "items")) 1) :to-be t)
        (expect (assoc "limit" fields :test #'string=) :to-be nil))))
  (it-each ((0) (1001) (1.5)) "rejects invalid snapshot limits ~S" (limit)
    (multiple-value-bind (kind error) (run-flow #'sys-snapshot/k (make-fake-ports) :port-limit limit)
      (expect kind :to-be :error)
      (expect (first error) :to-equal "argument.invalid")))
  (it "fails instead of succeeding when every source file is unreadable"
    (let ((ports (make-env-ports
                  :now-milliseconds (constantly 0)
                  :environment-variable (constantly nil) :environment-variables (constantly nil)
                  :read-file (constantly nil) :list-directory (constantly nil)
                  :read-link (constantly nil) :executable-p (constantly nil)
                  :run-program (constantly nil) :file-system-space (constantly nil)
                  :user-id (constantly nil) :host-name (constantly nil)
                  :user-name (constantly nil) :workspace-root (constantly nil)
                  :system-identity (lambda () (error 'file-error :pathname "fixture")))))
      (multiple-value-bind (kind error) (run-flow #'sys-snapshot/k ports :path "/fixture")
        (expect kind :to-be :error)
        (expect (first error) :to-equal "environment.unavailable")
        (expect (length (getf (cddr error) :diagnostics)) :to-be 1)))))

(defun metrics-host ()
  (let ((host (darwin-host)))
    (push (cons "/bin/ps" (exits-with (format nil "20 25.0 200 /usr/bin/worker~%10 25.0 100 /usr/bin/worker~%30 0.0 0 idle~%")))
          (fake-host-programs host))
    host))

(describe "aitools.env.application sys metrics"
  (it "samples Linux aggregate counters once across a one-second interval"
    (let ((reads 0) (sleeps nil) (unused (constantly nil)))
      (let ((ports (make-env-ports
                    :now-milliseconds unused :environment-variable unused :environment-variables unused
                    :list-directory unused :read-link unused :executable-p unused
                    :file-system-space unused :user-id unused :host-name unused :user-name unused :workspace-root unused
                    :system-identity (lambda () (values "Linux" "test" "generic"))
                    :sleep-seconds (lambda (seconds) (push seconds sleeps))
                    :read-file (lambda (path external-format)
                                 (declare (ignore external-format))
                                 (cond ((equal path "/proc/stat")
                                        (if (= (incf reads) 1) "cpu 10 0 10 70 10 0 0 0" "cpu 30 0 20 120 30 0 0 0"))
                                       ((equal path "/proc/cpuinfo") (format nil "processor : 0~%processor : 1~%"))
                                       ((equal path "/proc/meminfo") (format nil "MemTotal: 1000 kB~%MemAvailable: 250 kB~%"))))
                    :run-program (lambda (program arguments timeout)
                                   (declare (ignore program arguments timeout))
                                   (values :exited 0 "10 120.0 100 worker" "")))))
        (multiple-value-bind (kind fields) (run-flow #'sys-metrics/k ports)
          (expect kind :to-be :ok)
          (expect reads :to-be 2)
          (expect sleeps :to-equal '(1))
          (expect (object-field (field fields "cpu") "busy_percent") :to-equal 30d0)
          (expect (object-field (field fields "memory") "available_bytes") :to-be 256000)
          (expect (field fields "process_cpu_basis") :to-equal "lifetime_average")))))
  (it "bounds sampling and process output with deterministic CPU ordering"
    (let ((host (metrics-host)))
      (multiple-value-bind (kind fields) (run-flow #'sys-metrics/k (make-fake-ports :host host) :limit 1)
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
  (it "returns success when all process rows fit"
    (multiple-value-bind (kind fields) (run-flow #'sys-metrics/k (make-fake-ports :host (metrics-host)))
      (expect kind :to-be :ok)
      (expect (length (field fields "items")) :to-be 3)
      (expect (field fields "truncated") :to-be nil)))
  (it-each (("/bin/ps") ("/usr/bin/vm_stat") ("/usr/sbin/sysctl"))
      "fails when metrics source ~S fails" (program)
    (let ((host (metrics-host)))
      (push (cons program (exits-with "" :exit-code 1)) (fake-host-programs host))
      (multiple-value-bind (kind error) (run-flow #'sys-metrics/k (make-fake-ports :host host))
        (expect kind :to-be :error)
        (expect (first error) :to-equal "environment.unavailable"))))
  (it "samples Darwin counters twice with exactly one requested second"
    (let ((reads 0) (sleeps nil))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-metrics/k
                    (make-fake-ports :host (metrics-host)
                                     :sleep-seconds (lambda (seconds) (push seconds sleeps))
                                     :cpu-counters (lambda () (incf reads) (list reads 0 0 reads 0 0 0 0))))
        (expect kind :to-be :ok)
        (expect reads :to-be 2)
        (expect sleeps :to-equal '(1))
        (expect (object-field (field fields "cpu") "busy_percent") :to-equal 50d0))))
  (it-each ((nil) (((0 0 0 0 0 0 0 0)))
            (((0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)))
            (((4294967295 0 0 0 0 0 0 0) (0 0 0 10 0 0 0 0))))
      "fails when Darwin counter samples are unavailable, stationary, or wrapped: ~S" (samples)
    (multiple-value-bind (kind error)
        (run-flow #'sys-metrics/k (make-fake-ports :host (metrics-host)
                                                  :cpu-counters (lambda () (pop samples))))
      (expect kind :to-be :error)
      (expect (first error) :to-equal "environment.unavailable")))
  (it "rejects an unsupported operating system"
    (expect (run-flow #'sys-metrics/k (make-fake-ports :identity '("Other" "1" "generic"))) :to-be :error)))

(defparameter *linux-identity* '("Linux" "6.8.0-45-generic" "x86_64"))

(defparameter *info-fields*
  '("os" "os_version" "arch" "cpus" "user" "uid" "hostname" "shell" "memory" "disk"))

(defun darwin-host ()
  (make-fake-host
   :programs (list (cons "/usr/sbin/sysctl" (exits-with (format nil "16~%137438953472~%")))
                   (cons "/usr/bin/vm_stat" (exits-with *vm-stat-text*))
                   (cons "/bin/ps" (exits-with *ps-text*))
                   (cons "/usr/sbin/lsof" (exits-with *lsof-text*)))))

(defun linux-proc-ports (&key (host (make-fake-host)))
  (make-fake-ports
   :identity *linux-identity*
   :environment '(("SHELL" . "/bin/bash"))
   :files (list (cons "/proc/cpuinfo" (format nil "processor	: 0~%~%processor	: 1~%"))
                (cons "/proc/meminfo" *meminfo-text*)
                (cons "/proc/stat" (format nil "cpu 1~%btime 1700000000~%"))
                (cons "/etc/passwd" (format nil "root:x:0:0::/root:/bin/sh~%take:x:1000:100::/home/take:/bin/sh~%"))
                (cons "/proc/1/stat" "1 (systemd) S 0 1 1 0 -1 4194560 1 0 0 0 1 1 0 0 20 0 1 0 5 1 1")
                (cons "/proc/1/status" (format nil "Uid:	0	0	0	0~%"))
                (cons "/proc/1/cmdline" (format nil "/sbin/init~C" (code-char 0)))
                (cons "/proc/1/comm" (format nil "systemd~%"))
                (cons "/proc/77/stat" "77 (python3) S 1 77 77 0 -1 4194560 1 0 0 0 1 1 0 0 20 0 1 0 250 1 1")
                (cons "/proc/77/status" (format nil "Uid:	1000	1000	1000	1000~%"))
                (cons "/proc/77/cmdline" (format nil "python3~C-m~Chttp.server~C" (code-char 0) (code-char 0) (code-char 0)))
                (cons "/proc/77/comm" (format nil "python3~%"))
                (cons "/proc/net/tcp" *proc-net-tcp-text*)
                (cons "/proc/net/tcp6" *proc-net-tcp6-text*))
   :directories '(("/proc" "1" "77" "self" "net") ("/proc/77/fd" "0" "3" "4") ("/proc/1/fd"))
   :links '(("/proc/77/fd/0" . "/dev/null") ("/proc/77/fd/3" . "socket:[55501]") ("/proc/77/fd/4" . "socket:[55502]"))
   :host host))

(describe "aitools.env.application sys info"
  (it "fills every field from Darwin sysctl and vm_stat"
    (let ((host (darwin-host)))
      (multiple-value-bind (kind fields) (run-flow #'sys-info/k (make-fake-ports :host host
                                                                                :environment '(("SHELL" . "/bin/zsh"))))
        (expect kind :to-be :ok)
        (expect (mapcar #'car fields) :to-equal *info-fields*)
        (expect (field fields "os") :to-equal "darwin")
        (expect (field fields "arch") :to-equal "arm64")
        (expect (field fields "cpus") :to-be 16)
        (expect (field fields "uid") :to-be 1000)
        (expect (field fields "shell") :to-equal "/bin/zsh")
        (expect (object-field (field fields "memory") "total") :to-be 137438953472)
        (expect (object-field (field fields "memory") "available") :to-be (* 16384 (+ 2508908 2336582 97304)))
        (expect (object-field (field fields "disk") "available") :to-be 400))))

  (it "fills the same fields from Linux /proc"
    (multiple-value-bind (kind fields) (run-flow #'sys-info/k (linux-proc-ports))
      (expect kind :to-be :ok)
      (expect (mapcar #'car fields) :to-equal *info-fields*)
      (expect (field fields "os") :to-equal "linux")
      (expect (field fields "cpus") :to-be 2)
      (expect (object-field (field fields "memory") "available") :to-be (* 11240304 1024))))

  (it "reports unknown values as null instead of failing"
    (multiple-value-bind (kind fields) (run-flow #'sys-info/k (make-fake-ports :environment nil))
      (expect kind :to-be :ok)
      (expect (json-null-p (field fields "cpus")) :to-be-truthy)
      (expect (json-null-p (field fields "shell")) :to-be-truthy))))

(describe "aitools.env.application sys env"
  (it "masks values of secret-named variables and counts them"
    (multiple-value-bind (kind fields)
        (run-flow #'sys-env/k (make-fake-ports :environment '(("GITHUB_TOKEN" . "plain-looking-value")
                                                              ("HOME" . "/home/take")
                                                              ("DB_PASSWORD" . "hunter2"))))
      (expect kind :to-be :ok)
      (expect (mapcar (lambda (item) (list (object-field item "name") (object-field item "value")))
                      (field fields "items"))
              :to-equal '(("DB_PASSWORD" "[REDACTED_SECRET]") ("GITHUB_TOKEN" "[REDACTED_SECRET]")
                          ("HOME" "/home/take")))
      (expect (field fields "redactions") :to-be 2)))

  (it "filters by name prefix"
    (multiple-value-bind (kind fields)
        (run-flow #'sys-env/k (make-fake-ports :environment '(("LANG" . "C") ("LC_ALL" . "C") ("HOME" . "/h")))
                  :prefix "L")
      (expect kind :to-be :ok)
      (expect (field fields "total") :to-be 2))))

(describe "aitools.env.application sys tools"
  (it "finds a tool on PATH and reports null for a missing one"
    (let ((host (make-fake-host :programs (list (cons "/opt/bin/git" (exits-with (format nil "git version 2.55.0~%")))))))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-tools/k (make-fake-ports :environment '(("PATH" . "/usr/bin:/opt/bin"))
                                                   :executables '("/opt/bin/git") :host host)
                    :names '("git" "no-such-tool"))
        (expect kind :to-be :ok)
        (let ((items (field fields "items")))
          (expect (object-field (first items) "path") :to-equal "/opt/bin/git")
          (expect (object-field (first items) "version") :to-equal "git version 2.55.0")
          (expect (json-null-p (object-field (second items) "path")) :to-be-truthy)
          (expect (json-null-p (object-field (second items) "version")) :to-be-truthy))
        (expect (fake-host-runs host) :to-equal '(("/opt/bin/git" ("--version")))))))

  (it "passes the per-command timeout and reports null on timeout"
    (let* ((seen nil)
           (host (make-fake-host :programs (list (cons "/b/slow" (lambda (arguments timeout)
                                                                   (declare (ignore arguments))
                                                                   (setf seen timeout)
                                                                   (values :timeout nil "" "")))))))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-tools/k (make-fake-ports :environment '(("PATH" . "/b")) :executables '("/b/slow") :host host)
                    :names '("slow") :timeout "1500ms")
        (expect kind :to-be :ok)
        (expect seen :to-equal 3/2)
        (expect (json-null-p (object-field (first (field fields "items")) "version")) :to-be-truthy))))

  (it "probes the default list when no names are given"
    (multiple-value-bind (kind fields) (run-flow #'sys-tools/k (make-fake-ports :environment nil))
      (expect kind :to-be :ok)
      (expect (field fields "total") :to-be (length aitools.data:*env-default-tools*))))

  (it "rejects a name with a directory part and a bad timeout"
    (expect (first (nth-value 1 (run-flow #'sys-tools/k (make-fake-ports) :names '("../git"))))
            :to-equal "argument.invalid")
    (expect (first (nth-value 1 (run-flow #'sys-tools/k (make-fake-ports) :names '("git") :timeout "soon")))
            :to-equal "argument.invalid")))

(describe "aitools.env.application sys procs"
  (it "lists Darwin processes from ps only, with started from elapsed time"
    (let ((host (darwin-host)))
      (multiple-value-bind (kind fields) (run-flow #'sys-procs/k (make-fake-ports :host host))
        (expect kind :to-be :ok)
        (expect (fake-host-runs host)
                :to-equal '(("/bin/ps" ("-axww" "-o" "pid=,ppid=,user=,etime=,command="))))
        (let ((last (third (field fields "items"))))
          (expect (object-field last "pid") :to-be 900)
          (expect (object-field last "started") :to-equal "2026-03-08T06:24:53Z")))))

  (it "lists Linux processes from /proc with the same fields"
    (multiple-value-bind (kind fields) (run-flow #'sys-procs/k (linux-proc-ports) :pattern "HTTP")
      (expect kind :to-be :ok)
      (expect (field fields "total") :to-be 1)
      (let ((item (first (field fields "items"))))
        (expect (mapcar #'car (aitools.protocol.domain:json-object-members item))
                :to-equal '("pid" "ppid" "user" "command" "started"))
        (expect (object-field item "user") :to-equal "take")
        (expect (object-field item "command") :to-equal "python3 -m http.server")
        (expect (object-field item "started") :to-equal "2023-11-14T22:13:22.500Z"))))

  (it "runs no command at all on Linux"
    (let ((host (make-fake-host)))
      (run-flow #'sys-procs/k (linux-proc-ports :host host))
      (expect (fake-host-runs host) :to-equal nil)))

  (it "returns partial with next_commands over --limit"
    (multiple-value-bind (kind fields) (run-flow #'sys-procs/k (make-fake-ports :host (darwin-host)) :limit 2)
      (expect kind :to-be :partial)
      (expect (length (field fields "items")) :to-be 2)
      (expect (field fields "total") :to-be 3)
      (expect (field fields "truncated") :to-be t)
      (expect (field fields "next_commands") :to-equal '("aitools sys procs --limit 3"))))

  (it "is environment.unavailable without a source"
    (expect (first (nth-value 1 (run-flow #'sys-procs/k (make-fake-ports))))
            :to-equal "environment.unavailable")))

(describe "aitools.env.application sys ports"
  (it-each (("/proc/net/tcp" "ipv4" "ipv6")
            ("/proc/net/tcp6" "ipv6" "ipv4"))
      "preserves listeners and reports missing family beside ~A" (path available missing)
    (let* ((text (if (string= available "ipv4") *proc-net-tcp-text* *proc-net-tcp6-text*))
           (ports (make-fake-ports :identity *linux-identity* :files (list (cons path text)))))
      (multiple-value-bind (kind fields) (run-flow #'sys-ports/k ports)
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 2)
        (expect (length (field fields "items")) :to-be 2)
        (expect (object-field (field fields "coverage") available) :to-equal "ok")
        (expect (object-field (field fields "coverage") missing) :to-equal "source_unavailable")
        (expect (field fields "source_unavailable")
                :to-equal (list (if (string= missing "ipv4") "/proc/net/tcp" "/proc/net/tcp6"))))
      (multiple-value-bind (kind fields) (run-flow #'sys-snapshot/k ports :port-limit 1)
        (let* ((section (object-field (field fields "sections") "ports"))
               (data (object-field section "data")))
          (expect kind :to-be :partial)
          (expect (object-field section "status") :to-equal "partial")
          (expect (object-field (object-field section "reason") "source_unavailable")
                  :to-equal (object-field data "source_unavailable"))
          (expect (length (object-field data "items")) :to-be 1)
          (expect (object-field data "truncated") :to-be t)))))
  (it "does not turn a single empty available table into complete coverage"
    (multiple-value-bind (kind fields)
        (run-flow #'sys-ports/k (make-fake-ports :identity *linux-identity*
                                              :files '(("/proc/net/tcp" . ""))))
      (expect kind :to-be :partial)
      (expect (field fields "total") :to-be 0)))
  (it "maps Linux listeners to their owning process"
    (multiple-value-bind (kind fields) (run-flow #'sys-ports/k (linux-proc-ports))
      (expect kind :to-be :ok)
      (expect (mapcar (lambda (item)
                        (list (object-field item "port") (object-field item "address")
                              (let ((pid (object-field item "pid"))) (if (json-null-p pid) nil pid))))
                      (field fields "items"))
              :to-equal '((631 "127.0.0.1" nil) (631 "::1" nil) (8080 "0.0.0.0" 77) (8080 "::" 77)))))

  (it "reads Darwin listeners from lsof with the same fields"
    (multiple-value-bind (kind fields) (run-flow #'sys-ports/k (make-fake-ports :host (darwin-host)))
      (expect kind :to-be :ok)
      (let ((item (first (field fields "items"))))
        (expect (mapcar #'car (aitools.protocol.domain:json-object-members item))
                :to-equal '("port" "address" "protocol" "pid" "command"))
        (expect (object-field item "port") :to-be 8799))))

  (it "treats lsof's exit 1 with no output as no listeners"
    (let ((host (make-fake-host :programs (list (cons "/usr/sbin/lsof" (exits-with "" :exit-code 1))))))
      (multiple-value-bind (kind fields) (run-flow #'sys-ports/k (make-fake-ports :host host))
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 0))))

  (it "is environment.unavailable with a sys tools repair when lsof is missing"
    (multiple-value-bind (kind error) (run-flow #'sys-ports/k (make-fake-ports))
      (expect kind :to-be :error)
      (expect (first error) :to-equal "environment.unavailable")
      (expect (getf (first (getf (cddr error) :repairs)) :command) :to-equal "aitools sys tools lsof"))))

(defun null-or (value)
  (if (json-null-p value) nil value))

(describe "aitools.env.application sys commands on an unsupported or sparse host"
  (it "answers sys info on an unknown OS with null CPU and memory and no host command"
    (let ((host (darwin-host)))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-info/k (make-fake-ports :identity '("FreeBSD" "14.1" "amd64") :host host))
        (expect kind :to-be :ok)
        (expect (field fields "os") :to-equal "freebsd")
        (expect (null-or (field fields "cpus")) :to-be nil)
        (expect (null-or (object-field (field fields "memory") "total")) :to-be nil)
        (expect (object-field (field fields "disk") "total") :to-be 1000)
        (expect (fake-host-runs host) :to-equal nil))))

  (it-each ((sys-procs/k "aitools sys tools ps") (sys-ports/k "aitools sys tools lsof"))
      "answers ~S on an unknown OS with environment.unavailable and `~A`"
      (flow repair)
    (multiple-value-bind (kind error)
        (run-flow (symbol-function flow) (make-fake-ports :identity '("FreeBSD" "14.1" "amd64") :host (darwin-host)))
      (expect kind :to-be :error)
      (expect (first error) :to-equal "environment.unavailable")
      (expect (getf (first (getf (cddr error) :repairs)) :command) :to-equal repair)))

  (it "reports null CPUs and memory on Linux without processors or /proc/meminfo"
    (multiple-value-bind (kind fields)
        (run-flow #'sys-info/k (make-fake-ports :identity *linux-identity* :files (list (cons "/proc/cpuinfo" ""))))
      (expect kind :to-be :ok)
      (expect (null-or (field fields "cpus")) :to-be nil)
      (expect (null-or (object-field (field fields "memory") "total")) :to-be nil)
      (expect (null-or (object-field (field fields "memory") "available")) :to-be nil)))

  (it "matches no variable whose name is shorter than the prefix"
    (multiple-value-bind (kind fields)
        (run-flow #'sys-env/k (make-fake-ports :environment '(("LANG" . "C") ("LANGUAGE" . "en"))) :prefix "LANGU")
      (expect kind :to-be :ok)
      (expect (mapcar (lambda (item) (object-field item "name")) (field fields "items")) :to-equal '("LANGUAGE"))))

  (it "takes the version from stderr when stdout is blank"
    (let ((host (make-fake-host :programs (list (cons "/b/java" (exits-with (format nil "~%  ~%")
                                                                           :stderr (format nil "openjdk 21.0.2~%")))))))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-tools/k (make-fake-ports :environment '(("PATH" . "/b")) :executables '("/b/java") :host host)
                    :names '("java"))
        (expect kind :to-be :ok)
        (expect (object-field (first (field fields "items")) "version") :to-equal "openjdk 21.0.2"))))

  (it "keeps Linux processes whose uid has no passwd name, or no status, with null start without btime"
    (multiple-value-bind (kind fields)
        (run-flow #'sys-procs/k
                  (make-fake-ports
                   :identity *linux-identity*
                   :files (list (cons "/proc/5/stat" "5 (a) S 1 5 5 0 -1 0 1 0 0 0 1 1 0 0 20 0 1 0 5 1 1")
                                (cons "/proc/5/status" (format nil "Uid:	4242	4242	4242	4242~%"))
                                (cons "/proc/6/stat" "6 (b) S 1 6 6 0 -1 0 1 0 0 0 1 1 0 0 20 0 1 0 5 1 1")
                                (cons "/proc/8/stat" "garbage"))
                   :directories '(("/proc" "5" "6" "7" "8" "self"))))
      (expect kind :to-be :ok)
      (expect (mapcar (lambda (item)
                        (list (object-field item "pid") (null-or (object-field item "user"))
                              (object-field item "command") (null-or (object-field item "started"))))
                      (field fields "items"))
              :to-equal '((5 "4242" "[a]" nil) (6 nil "[b]" nil)))))

  (it "repeats the pattern in the next command of a partial process list"
    (multiple-value-bind (kind fields)
        (run-flow #'sys-procs/k (make-fake-ports :host (darwin-host)) :pattern "/" :limit 1)
      (expect kind :to-be :partial)
      (expect (field fields "total") :to-be 2)
      (expect (field fields "next_commands") :to-equal '("aitools sys procs / --limit 2"))))

  (it "attributes a socket shared by two processes to the first one listed"
    (let ((ports (make-fake-ports
                  :identity *linux-identity*
                  :files (list (cons "/proc/net/tcp" *proc-net-tcp-text*)
                               (cons "/proc/net/tcp6" "")
                               (cons "/proc/1/comm" (format nil "systemd~%"))
                               (cons "/proc/77/comm" (format nil "python3~%")))
                  :directories '(("/proc" "1" "77") ("/proc/1/fd" "9") ("/proc/77/fd" "3" "4"))
                  :links '(("/proc/1/fd/9" . "socket:[55501]") ("/proc/77/fd/3" . "socket:[55501]")
                           ("/proc/77/fd/4" . "socket:[55501]")))))
      (multiple-value-bind (kind fields) (run-flow #'sys-ports/k ports)
        (expect kind :to-be :ok)
        (expect (mapcar (lambda (item)
                          (list (object-field item "port") (null-or (object-field item "pid"))
                                (null-or (object-field item "command"))))
                        (field fields "items"))
                :to-equal '((631 nil nil) (8080 1 "systemd"))))))

  (it "is environment.unavailable on Linux without /proc/net/tcp or tcp6"
    (multiple-value-bind (kind error) (run-flow #'sys-ports/k (make-fake-ports :identity *linux-identity*))
      (expect kind :to-be :error)
      (expect (first error) :to-equal "environment.unavailable")))

  (it-each ((1 "p1") (2 "") (0 nil))
      "treats lsof exit ~D with output ~S as no usable listener source"
      (exit-code stdout)
    (let ((host (make-fake-host :programs (list (cons "/usr/sbin/lsof"
                                                      (if stdout
                                                          (exits-with stdout :exit-code exit-code)
                                                          (lambda (arguments timeout)
                                                            (declare (ignore arguments timeout))
                                                            (values :timeout nil nil nil))))))))
      (expect (first (nth-value 1 (run-flow #'sys-ports/k (make-fake-ports :host host))))
              :to-equal "environment.unavailable"))))

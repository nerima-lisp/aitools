(in-package #:aitools.env.test)

(defun null-or (value)
  (if (json-null-p value) nil value))

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

(in-package #:aitools.env.test)

(describe "aitools.env.application sys commands on an unsupported or sparse host"
  (it "answers sys info on an unknown OS with null CPU and memory and no host command"
    (let ((host (darwin-host)))
      (multiple-value-bind (kind fields)
          (run-flow #'sys-info/k (make-fake-ports :identity '("FreeBSD" "14.1" "amd64")
                                                   :host host))
        (expect kind :to-be :ok)
        (expect (field fields "os") :to-equal "freebsd")
        (expect (null-or (field fields "cpus")) :to-be nil)
        (expect (null-or (object-field (field fields "memory") "total")) :to-be nil)
        (expect (object-field (field fields "disk") "total") :to-be 1000)
        (expect (fake-host-runs host) :to-equal nil))
      (multiple-value-bind (kind error)
          (run-flow #'sys-disk/k (make-fake-ports :identity '("FreeBSD" "14.1" "amd64")))
        (expect kind :to-be :error)
        (expect (first error) :to-equal "environment.unavailable"))))

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

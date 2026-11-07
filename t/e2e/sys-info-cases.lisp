(in-package #:aitools.e2e.test)

;;;; Correspondence table row 41: system and host information, including env query tracing.

(define-row-case (41 "lsof -i's listener is sys ports" :foreign ("lsof")) (ws)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen socket 5)
           (let* ((port (nth-value 1 (sb-bsd-sockets:socket-name socket)))
                  (item (find port (jlist (jget (run-ok ws '("sys" "ports")) "items"))
                              :key (lambda (item) (jget item "port")))))
             (expect (and item (jget item "pid"))
                     :to-be (parse-integer (oracle ws '("lsof") (format nil "lsof -nP -iTCP:~D -sTCP:LISTEN -t" port) nil)))))
      (sb-bsd-sockets:socket-close socket))))

(define-row-case (41 "netstat's interface names are sys network" :foreign ("netstat")) (ws)
  (let* ((names (remove-duplicates
                 (text-lines
                  (oracle ws '("netstat" "awk")
                          "netstat -in > netstat.txt && awk 'NF >= 5 && $1 != \"Name\" && $1 != \"Iface\" {sub(/[*]$/, \"\", $1); print $1}' netstat.txt"
                          nil))
                 :test #'string=))
         (envelope (run-ok ws '("sys" "network" "--limit" "1000")))
         (items (jlist (jget envelope "items"))))
    (expect (sorted (mapcar (lambda (item) (jget item "name")) items))
            :to-equal (sorted names))
    (expect (jget envelope "basis") :to-equal "cumulative_interface_counters")
    (dolist (item items)
      (dolist (key '("rx_bytes" "tx_bytes" "rx_packets" "tx_packets" "rx_errors" "tx_errors"))
        (expect (>= (jget item key) 0) :to-be t)))))

(define-row-case (41 "df's filesystem capacity is sys disk" :foreign ("df")) (ws)
  (let* ((blocks (parse-integer (oracle ws '("df" "awk")
                                      "df -Pk . > df.txt && awk 'NR == 2 {print $2}' df.txt"
                                      nil)))
         (envelope (run-ok ws '("sys" "disk" "."))))
    (expect (floor (jget envelope "total_bytes") 1024) :to-be blocks)
    (expect (jget envelope "units") :to-equal "bytes")
    (expect (jget envelope "scope") :to-equal "filesystem_containing_path")
    (expect (<= 0 (jget envelope "available_bytes") (jget envelope "total_bytes"))
            :to-be t)))

(define-row-case (41 "top's process snapshot is sys metrics" :foreign ("top")) (ws)
  (let ((process (sb-ext:run-program "sleep" '("47") :search t :wait nil)))
    (unwind-protect
         (let* ((pid (sb-ext:process-pid process))
                (expected (trimmed
                           (oracle ws '("top" "awk")
                                   (format nil
                                           "case $(uname -s) in Darwin) /usr/bin/top -l 1 -pid ~D -stats pid,rsize -n 1 ;; *) top -b -n 1 -p ~D ;; esac > top.txt && awk '$1 == ~D {print $1}' top.txt"
                                           pid pid pid)
                                   nil)))
                (result (aitools ws '("sys" "metrics" "--limit" "10000")))
                (envelope (aitools-envelope result))
                (item (find pid (jlist (jget envelope "items"))
                            :key (lambda (item) (jget item "pid")))))
           (expect expected :to-equal (princ-to-string pid))
           (expect (aitools-exit-code result) :to-be 0)
           (expect (aitools-stream result) :to-be :stdout)
           (expect (and item (jget item "pid")) :to-be pid)
           (expect (jget item "command") :to-equal "sleep")
           (expect (>= (jget item "rss_bytes") 0) :to-be t)
           (expect (<= 0 (jget envelope "cpu" "busy_percent") 100) :to-be t)
           (expect (jget envelope "sample_interval_ms") :to-be 1000))
      (sb-ext:process-kill process 9)
      (sb-ext:process-wait process)
      (sb-ext:process-close process))))

(define-row-case (41 "ps -p is sys procs" :foreign ("ps")) (ws)
  (let ((process (sb-ext:run-program "sleep" '("47") :search t :wait nil)))
    (unwind-protect
         (let* ((pid (sb-ext:process-pid process))
                (items (jlist (jget (run-ok ws '("sys" "procs" "sleep" "--limit" "1000")) "items")))
                (item (find pid items :key (lambda (item) (jget item "pid")))))
           (expect (trimmed (oracle ws '("ps") (format nil "ps -p ~D -o pid=" pid) nil))
                   :to-equal (princ-to-string pid))
           (expect (and item (jget item "pid")) :to-be pid))
      (sb-ext:process-kill process 9)
      (sb-ext:process-wait process)
      (sb-ext:process-close process))))

(define-row-case (41 "which is sys tools" :foreign ("which")) (ws)
  (let ((items (jlist (jget (run-ok ws '("sys" "tools" "sh" "no-such-tool-e2e")) "items"))))
    (expect (jget (first items) "path") :to-equal (trimmed (oracle ws '("which") "which sh" nil)))
    (expect (list (jget (second items) "path")
                  (nth-value 1 (shell ws "which no-such-tool-e2e" :expect-status :any)))
            :to-equal (list nil 1))))

(define-row-case (41 "env is sys env" :foreign ("env")) (ws)
  (expect (mapcar (lambda (item) (format nil "~A=~A" (jget item "name") (jget item "value")))
                  (jlist (jget (run-ok ws '("sys" "env" "E2E_MARK") :env '(("E2E_MARK" . "hello world"))) "items")))
          :to-equal (text-lines (oracle ws '("env" "grep") "env | grep '^E2E_MARK'" nil
                                        :env '(("E2E_MARK" . "hello world"))))))

(define-row-case (41 "nproc is sys info's cpus" :foreign ("nproc")) (ws)
  (expect (jget (run-ok ws '("sys" "info")) "cpus")
          :to-be (parse-integer (oracle ws '("getconf") "nproc 2>/dev/null || getconf _NPROCESSORS_ONLN" nil))))

(define-row-case (41 "hostname is sys info's hostname" :foreign ("hostname")) (ws)
  (expect (jget (run-ok ws '("sys" "info")) "hostname") :to-equal (trimmed (oracle ws '("hostname") "hostname" nil))))

(define-row-case (41 "whoami is sys info's user" :foreign ("whoami")) (ws)
  (expect (jget (run-ok ws '("sys" "info")) "user") :to-equal (trimmed (oracle ws '("whoami") "whoami" nil))))

(define-row-case (41 "uname -s and -m are sys info's os and arch" :foreign ("uname")) (ws)
  (let ((envelope (run-ok ws '("sys" "info"))))
    (expect (jget envelope "os") :to-equal (string-downcase (trimmed (oracle ws '("uname") "uname -s" nil))))
    (expect (jget envelope "arch") :to-equal (trimmed (oracle ws '("uname") "uname -m" nil)))
    (expect (jget envelope "os_version") :to-equal (trimmed (oracle ws '("uname") "uname -r" nil)))))

(describe "aitools environment child-process trace span (e2e)"
  (it "records env.system-query with the command parent"
    (:tags (quote (:trace-positive)))
    (when (let ((directory (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")))
            (and directory (plusp (length directory))))
      (cl-weave:skip "trace specs require a binary with --trace"))
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (let ((result (aitools ws '("--trace" "sys" "tools" "sh") :expect-trace t)))
         (expect (aitools-exit-code result) :to-be 0)
         (expect-child-process-span (aitools-envelope result) "env.system-query" "ok"))))))

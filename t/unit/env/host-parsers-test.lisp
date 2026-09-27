;;;; t/unit/env/host-parsers-test.lisp
;;;;
;;;; Fixture text follows the formats documented in proc(5) (Linux) and the
;;;; output captured from Darwin 25 `vm_stat`, `ps`, and `lsof`.
(in-package #:aitools.env.test)

(defparameter *meminfo-text*
  "MemTotal:       16318212 kB
MemFree:         1022468 kB
MemAvailable:   11240304 kB
Buffers:          532124 kB
Cached:          9312724 kB
")

(defparameter *vm-stat-text*
  "Mach Virtual Memory Statistics: (page size of 16384 bytes)
Pages free:                                  2508908.
Pages active:                                2436202.
Pages inactive:                              2336582.
Pages speculative:                             97304.
Pages throttled:                                   0.
")

(defparameter *ps-text*
  "    1     0 root             03-16:08:45 /sbin/launchd
  262     1 take             01-03:29:29 /usr/bin/some  tool --flag
  900   262 take                   05:07 zsh
")

(defparameter *proc-net-tcp-text*
  "  sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
   0: 0100007F:0277 00000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 21342 1 0000000000000000 100 0 0 10 0
   1: 00000000:1F90 00000000:0000 0A 00000000:00000000 00:00000000 00000000  1000        0 55501 1 0000000000000000 100 0 0 10 0
   2: 0100007F:0277 0100007F:D2A4 01 00000000:00000000 00:00000000 00000000     0        0 0 1 0000000000000000 20 4 30 10 -1
")

(defparameter *proc-net-tcp6-text*
  "  sl  local_address                         remote_address                        st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
   0: 00000000000000000000000001000000:0277 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 21343 1 0000000000000000 100 0 0 10 0
   1: 00000000000000000000000000000000:1F90 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000  1000        0 55502 1 0000000000000000 100 0 0 10 0
")

(defparameter *lsof-text*
  "p23148
cqemu-system-aarch64
f13
tIPv4
n*:31022
p31635
cworkerd
f20
tIPv4
n127.0.0.1:8799
f21
tIPv6
n[::1]:8799
f22
tIPv6
n*:8800
")

(describe "aitools.env.domain host parsers"
  (it "reads total and available memory from /proc/meminfo"
    (expect (multiple-value-list (parse-meminfo *meminfo-text*))
            :to-equal (list (* 16318212 1024) (* 11240304 1024))))

  (it "falls back to MemFree+Buffers+Cached without MemAvailable"
    (expect (nth-value 1 (parse-meminfo "MemTotal: 100 kB
MemFree: 10 kB
Buffers: 2 kB
Cached: 3 kB
"))
            :to-be (* 15 1024)))

  (it "reads available memory from vm_stat pages"
    (expect (parse-vm-stat *vm-stat-text*) :to-be (* 16384 (+ 2508908 2336582 97304))))

  (it "counts /proc/cpuinfo processors"
    (expect (count-cpuinfo-processors (format nil "processor	: 0~%model name	: x~%~%processor	: 1~%"))
            :to-be 2))

  (it "reads ps etime values"
    (expect (parse-etime "05:07") :to-be 307)
    (expect (parse-etime "01:02:03") :to-be 3723)
    (expect (parse-etime "03-16:08:45") :to-be (+ (* 3 86400) (* 16 3600) (* 8 60) 45))
    (expect (parse-etime "bad") :to-be nil))

  (it "reads ps rows keeping the command's own spacing"
    (let ((rows (parse-ps-output *ps-text*)))
      (expect (length rows) :to-be 3)
      (expect (getf (second rows) :command) :to-equal "/usr/bin/some  tool --flag")
      (expect (getf (third rows) :ppid) :to-be 262)
      (expect (getf (third rows) :elapsed-seconds) :to-be 307)
      (expect (getf (first rows) :user) :to-equal "root")))

  (it "reads /proc/<pid>/stat even when the command name holds `) `"
    (expect (multiple-value-list
             (parse-proc-stat-process
              "4242 (my) prog) S 1 4242 4242 0 -1 4194560 100 0 0 0 1 2 0 0 20 0 1 0 123456 1000 10"))
            :to-equal '(4242 "my) prog" 1 123456)))

  (it "reads btime, the status uid, passwd names, and cmdline"
    (expect (parse-proc-boot-time (format nil "cpu 1 2 3~%btime 1700000000~%processes 5~%")) :to-be 1700000000)
    (expect (parse-proc-status-uid (format nil "Name:	x~%Uid:	1000	1000	1000	1000~%")) :to-be 1000)
    (expect (parse-passwd (format nil "root:x:0:0:root:/root:/bin/sh~%take:x:1000:100::/home/take:/bin/sh~%"))
            :to-equal '((0 . "root") (1000 . "take")))
    (expect (proc-cmdline-command (format nil "python3~Ca.py~C" (code-char 0) (code-char 0)) "python3")
            :to-equal "python3 a.py")
    (expect (proc-cmdline-command "" "kthreadd") :to-equal "[kthreadd]"))

  (it "reads LISTEN rows of /proc/net/tcp and tcp6 only"
    (expect (parse-proc-net-tcp *proc-net-tcp-text* :ipv4)
            :to-equal '((:address "127.0.0.1" :port 631 :inode 21342)
                        (:address "0.0.0.0" :port 8080 :inode 55501)))
    (expect (parse-proc-net-tcp *proc-net-tcp6-text* :ipv6)
            :to-equal '((:address "::1" :port 631 :inode 21343)
                        (:address "::" :port 8080 :inode 55502))))

  (it "compresses IPv6 per RFC 5952"
    (expect (format-ipv6 '(#x2001 #xdb8 0 0 1 0 0 1)) :to-equal "2001:db8::1:0:0:1")
    (expect (format-ipv6 '(#x2001 #xdb8 0 1 1 1 1 1)) :to-equal "2001:db8:0:1:1:1:1:1")
    (expect (format-ipv6 '(0 0 0 0 0 0 0 0)) :to-equal "::"))

  (it "reads socket inodes from fd links"
    (expect (parse-socket-inode "socket:[55501]") :to-be 55501)
    (expect (parse-socket-inode "pipe:[1]") :to-be nil)
    (expect (parse-socket-inode "/dev/null") :to-be nil))

  (it "reads lsof -F output with wildcard and bracketed addresses"
    (expect (parse-lsof-listen *lsof-text*)
            :to-equal '((:address "0.0.0.0" :port 31022 :pid 23148 :command "qemu-system-aarch64")
                        (:address "127.0.0.1" :port 8799 :pid 31635 :command "workerd")
                        (:address "::1" :port 8799 :pid 31635 :command "workerd")
                        (:address "::" :port 8800 :pid 31635 :command "workerd"))))

  (it "orders ports and drops the same socket seen twice"
    (expect (sort-and-deduplicate-ports
             '((:address "::" :port 80 :pid 1 :command "a")
               (:address "0.0.0.0" :port 22 :pid 2 :command "b")
               (:address "::" :port 80 :pid 1 :command "a")))
            :to-equal '((:address "0.0.0.0" :port 22 :pid 2 :command "b")
                        (:address "::" :port 80 :pid 1 :command "a")))))

(describe "aitools.env.domain host values"
  (it-each (("GITHUB_TOKEN" t) ("AWS_SECRET_ACCESS_KEY" t) ("DB_PASSWORD" t) ("api_key" t)
            ("OPENAI_API_KEY" t) ("HOME" nil) ("TOKENIZER_PATH" nil) ("PATH" nil))
      "classifies ~A as secret: ~A"
      (name secret)
    (expect (secret-environment-name-p name) :to-be secret))

  (it "splits PATH in order without empty or repeated entries"
    (expect (split-search-path "/a::/b/:/a") :to-equal '("/a" "/b/")))

  (it "joins directories without doubling the slash"
    (expect (join-directory "/b/" "git") :to-equal "/b/git")
    (expect (join-directory "/a" "git") :to-equal "/a/git"))

  (it "takes the first nonblank output line"
    (expect (first-output-line (format nil "~%  git version 2.55.0  ~%more~%")) :to-equal "git version 2.55.0")
    (expect (first-output-line "") :to-be nil))

  (it "accepts IANA names and refuses anything that could leave the zoneinfo directory"
    (expect (valid-zone-name-p "America/New_York") :to-be-truthy)
    (expect (valid-zone-name-p "Etc/GMT+5") :to-be-truthy)
    (expect (valid-zone-name-p "../etc/passwd") :to-be-falsy)
    (expect (valid-zone-name-p "/usr/share/zoneinfo/UTC") :to-be-falsy)
    (expect (valid-zone-name-p "America//X") :to-be-falsy))

  (it "finds the local zone name from TZ and from the /etc/localtime link"
    (expect (zone-name-from-tz-variable ":Asia/Tokyo") :to-equal "Asia/Tokyo")
    (expect (zone-name-from-tz-variable "EST5EDT") :to-be nil)
    (expect (zone-name-from-localtime-link "/var/db/timezone/zoneinfo/Asia/Tokyo") :to-equal "Asia/Tokyo")
    (expect (zone-name-from-localtime-link "/etc/zoneinfo/../x") :to-be nil)))

(defun tcp-row (local &key (state "0A") (inode "7"))
  "A /proc/net/tcp row whose local address is LOCAL."
  (format nil "   0: ~A 00000000:0000 ~A 00000000:00000000 00:00000000 00000000     0        0 ~A 1 0 100 0 0 10 0"
          local state inode))

(defun tcp-table (&rest rows)
  (format nil "  sl  local_address rem_address   st~%~{~A~%~}" rows))

(describe "aitools.env.domain host parsers on malformed input"
  (it "skips ps rows without four fields or with a non-numeric pid, ppid or etime"
    (expect (mapcar (lambda (row) (getf row :pid))
                    (parse-ps-output (format nil "  12~%x 1 u 00:01 cmd~%  7 y u 00:01 cmd~%  8 1 u later cmd~%  5 1 u 00:02 ok~%")))
            :to-equal '(5)))

  (it-each (("1 systemd S 0 1") ("1 )systemd( S 0 1") ("x (init) S 0 1 1 0 -1 4194560 1 0 0 0 1 1 0 0 20 0 1 0 5 1 1")
            ("1 (init) S 0 1"))
      "ignores the /proc/<pid>/stat text ~S"
      (text)
    (expect (parse-proc-stat-process text) :to-be nil))

  (it-each (("00000000:1F9G" "a non-hex port")
            ("0000000:1F90" "a short IPv4 address")
            ("0000000Z:1F90" "a non-hex IPv4 word")
            ("0000000１:1F90" "a fullwidth digit, which DIGIT-CHAR-P would accept")
            ("000000001F90" "no port separator"))
      "skips a LISTEN row with ~S (~A)"
      (local reason)
    (declare (ignore reason))
    (expect (parse-proc-net-tcp (tcp-table (tcp-row local) (tcp-row "0100007F:0050")) :ipv4)
            :to-equal '((:address "127.0.0.1" :port 80 :inode 7))))

  (it "skips short rows and rows whose inode is not a number"
    (expect (parse-proc-net-tcp (tcp-table "   0: 0100007F:0050 00000000:0000 0A"
                                           (tcp-row "0100007F:0050" :inode "x")
                                           (tcp-row "0100007F:0051"))
                                :ipv4)
            :to-equal '((:address "127.0.0.1" :port 81 :inode 7))))

  (it-each (("0000000000000000000000000100000:0050" "a short IPv6 address")
            ("000000000000000000000000010000ZZ:0050" "a non-hex IPv6 word"))
      "skips an IPv6 LISTEN row with ~S (~A)"
      (local reason)
    (declare (ignore reason))
    (expect (parse-proc-net-tcp (tcp-table (tcp-row local)) :ipv6) :to-equal '()))

  (it "keeps a one-character host, skips blank lines, and drops an address without a port"
    (expect (parse-lsof-listen (format nil "p1~%cd~%~%f3~%tIPv4~%nx:80~%nlocalhost~%nhost:port~%"))
            :to-equal '((:address "x" :port 80 :pid 1 :command "d")))))

(describe "aitools.env.domain host values at their edges"
  (it "joins onto an empty directory as an absolute path"
    (expect (join-directory "" "git") :to-equal "/git"))

  (it-each (("" nil) ("git" t) ("bin/git" nil) ("gi t" t))
      "judges the tool name ~S valid: ~A"
      (name valid)
    (expect (valid-tool-name-p name) :to-be valid))

  (it "refuses a tool name holding NUL and a zone name over 128 characters"
    (expect (valid-tool-name-p (format nil "git~C" (code-char 0))) :to-be nil)
    (expect (valid-zone-name-p (make-string 129 :initial-element #\A)) :to-be nil)
    (expect (valid-zone-name-p (make-string 128 :initial-element #\A)) :to-be-truthy))

  (it "searches $TZDIR first only when it is set and nonempty"
    (expect (first (zoneinfo-directories "/opt/zoneinfo")) :to-equal "/opt/zoneinfo")
    (expect (zoneinfo-directories "") :to-equal (zoneinfo-directories nil))
    (expect (zoneinfo-directories nil)
            :to-equal '("/usr/share/zoneinfo" "/usr/lib/zoneinfo" "/usr/share/lib/zoneinfo" "/etc/zoneinfo")))

  (it-each (("UTC" "UTC") (":Etc/UTC" "Etc/UTC") ("" nil) ("EST5EDT,M3.2.0,M11.1.0" nil) ("Europe/../x" nil))
      "reads the zone name from TZ=~S as ~S"
      (value name)
    (expect (zone-name-from-tz-variable value) :to-equal name)))

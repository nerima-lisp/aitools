;;;; t/e2e/process-env-cases.lisp
;;;;
;;;; Correspondence table rows 38-45: running commands, background processes and waiting,
;;;; git, the host environment, time, and the util group.
(in-package #:aitools.e2e.test)

(defun trimmed (text)
  (string-trim '(#\Newline #\Space) text))

;;; Row 38

(define-row-case (38 "cmd | grep is run --grep" :foreign ("grep")) (ws)
  (expect (format nil "~{~A~%~}"
                  (mapcar (lambda (match) (jget match "text"))
                          (jlist (jget (run-ok ws '("run" "--grep" "1" "--" "seq" "1" "12")) "stdout" "matches"))))
          :to-equal (oracle ws '("seq" "grep") "seq 1 12 | grep 1" (format nil "1~%10~%11~%12~%"))))

(define-row-case (38 "cmd | head -n 2 is run --head 2 --tail 0" :foreign ("head")) (ws)
  (expect (lines-text (jget (run-ok ws '("run" "--head" "2" "--tail" "0" "--" "seq" "1" "12")) "stdout" "head"))
          :to-equal (oracle ws '("seq" "head") "seq 1 12 | head -n 2" (format nil "1~%2~%"))))

(define-row-case (38 "cmd | tail -n 2 is run --head 0 --tail 2" :foreign ("tail")) (ws)
  (expect (lines-text (jget (run-ok ws '("run" "--head" "0" "--tail" "2" "--" "seq" "1" "12")) "stdout" "tail"))
          :to-equal (oracle ws '("seq" "tail") "seq 1 12 | tail -n 2" (format nil "11~%12~%"))))

(define-row-case (38 "cmd > file is run --stdout-to" :foreign ()) (ws)
  (let ((expected (oracle ws '("seq") "seq 1 3 > o.txt && cat o.txt" (format nil "1~%2~%3~%"))))
    (run-ok ws '("run" "--stdout-to" "a.txt" "--" "seq" "1" "3"))
    (expect (file-text ws "a.txt") :to-equal expected)))

(define-row-case (38 "stripping ANSI colors with perl is run's default" :foreign ("perl")) (ws)
  (let ((script "printf '\\033[1;31mred\\033[0m plain\\n'"))
    (expect (lines-text (jget (run-ok ws (list "run" "--" "sh" "-c" script)) "stdout" "head"))
            :to-equal (oracle ws '("perl") (format nil "~A | perl -pe 's/\\e\\[[0-9;]*m//g'" script)
                              (format nil "red plain~%")))))

;;; Row 39

(defmacro with-bg ((id-var workspace argv) &body body)
  "Start ARGV with bg start, bind its id, and stop it on the way out."
  (let ((ws (gensym "WS")))
    `(let* ((,ws ,workspace)
            (,id-var (jget (run-ok ,ws (list* "bg" "start" "--" ,argv)) "id")))
       (unwind-protect (progn ,@body)
         (aitools ,ws (list "bg" "stop" ,id-var "--grace" "1s"))))))

(define-row-case (39 "cmd & with tail -f's output is bg start, wait --bg, and bg logs" :foreign ("tail" "nohup")) (ws)
  (let ((script "echo one; echo two; sleep 30"))
    (with-bg (id ws (list "sh" "-c" script))
      (expect (jget (run-ok ws (list "wait" "--bg" id "--pattern" "two" "--timeout" "10s")) "matched") :to-be t)
      (expect (lines-text (jget (run-ok ws (list "bg" "logs" id)) "lines"))
              :to-equal (oracle ws '() "echo one; echo two" (format nil "one~%two~%"))))))

(define-row-case (39 "tail -c +N of a log is bg logs --from" :foreign ("tail")) (ws)
  (with-bg (id ws (list "sh" "-c" "echo one; echo two; sleep 30"))
    (run-ok ws (list "wait" "--bg" id "--pattern" "two" "--timeout" "10s"))
    (expect (lines-text (jget (run-ok ws (list "bg" "logs" id "--from" "4")) "lines"))
            :to-equal (oracle ws '("tail") "printf 'one\\ntwo\\n' | tail -c +5" (format nil "two~%")))))

(define-row-case (39 "sleep is wait --duration" :foreign ("sleep")) (ws)
  (let ((envelope (run-ok ws '("wait" "--duration" "300ms"))))
    (expect (jget envelope "matched") :to-be t)
    (expect (>= (jget envelope "elapsed_ms") 300) :to-be t)))

(define-row-case (39 "waiting for a port as nc -z sees it is wait --port" :foreign ("nc")) (ws)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen socket 5)
           (let ((port (nth-value 1 (sb-bsd-sockets:socket-name socket))))
             (expect (trimmed (oracle ws '("nc") (format nil "nc -z 127.0.0.1 ~D; echo $?" port) (format nil "0~%")))
                     :to-equal "0")
             (expect (jget (run-ok ws (list "wait" "--port" (princ-to-string port) "--timeout" "5s")) "matched")
                     :to-be t)))
      (sb-bsd-sockets:socket-close socket))))

(define-row-case (39 "timeout's 124 is run --timeout's timed_out" :foreign ("timeout")) (ws)
  (expect (if (jget (run-ok ws '("run" "--timeout" "1s" "--" "sleep" "5")) "timed_out") "124" "0")
          :to-equal (trimmed (oracle ws '("timeout") "timeout 1 sleep 5; echo $?" (format nil "124~%")))))

;;; Row 40

(defun make-history (workspace)
  "Two commits of f, a staged and an unstaged change to f, and an untracked u."
  (shell workspace "printf 'l1\\nl2\\n' > f && git add f && git commit -qm first && printf 'l1\\nL2\\nl3\\n' > f && git commit -qam second && printf 'x\\n' > f && git add f && printf 'y\\n' > f && printf 'u\\n' > u"))

(define-row-case (40 "git status --porcelain is git status" :foreign ("git") :git t) (ws)
  (make-history ws)
  (let* ((envelope (run-ok ws '("git" "status")))
         (codes (make-hash-table :test #'equal)))
    (dolist (entry (jlist (jget envelope "staged")))
      (setf (gethash (jget entry "path") codes) (list (jget entry "status") " ")))
    (dolist (entry (jlist (jget envelope "unstaged")))
      (setf (gethash (jget entry "path") codes)
            (list (or (first (gethash (jget entry "path") codes)) " ") (jget entry "status"))))
    (expect (sorted (append (loop for path being the hash-keys of codes using (hash-value (x y))
                                  collect (format nil "~A~A ~A" x y path))
                            (mapcar (lambda (path) (format nil "?? ~A" path)) (jlist (jget envelope "untracked")))))
            :to-equal (sorted (text-lines (oracle ws '("git") "git status --porcelain=v1" nil))))))

(define-row-case (40 "git log is git log" :foreign ("git") :git t) (ws)
  (make-history ws)
  (expect (mapcar (lambda (item) (format nil "~A ~A" (jget item "sha") (jget item "subject")))
                  (jlist (jget (run-ok ws '("git" "log")) "items")))
          :to-equal (text-lines (oracle ws '("git") "git log --format='%H %s'" nil))))

(define-row-case (40 "git diff --numstat is git diff --output stat" :foreign ("git") :git t) (ws)
  (make-history ws)
  (dolist (staged '(nil t))
    (expect (mapcar (lambda (file) (format nil "~D~C~D~C~A" (jget file "added") #\Tab (jget file "deleted") #\Tab (jget file "path")))
                    (jlist (jget (run-ok ws (append '("git" "diff" "--output" "stat") (and staged '("--staged")))) "files")))
            :to-equal (text-lines (oracle ws '("git") (format nil "git diff --numstat~:[~; --staged~]" staged) nil)))))

(define-row-case (40 "git diff's changed lines are git diff's hunks" :foreign ("git") :git t) (ws)
  (make-history ws)
  (expect (loop for file in (jlist (jget (run-ok ws '("git" "diff")) "files"))
                append (loop for hunk in (jlist (jget file "hunks")) append (jlist (jget hunk "lines"))))
          :to-equal (remove-if (lambda (line) (or (search "---" line :end2 (min 3 (length line)))
                                                  (search "+++" line :end2 (min 3 (length line)))))
                               (text-lines (oracle ws '("git" "grep") "git diff | grep -E '^[-+ ]'" nil)))))

(define-row-case (40 "git blame is git blame" :foreign ("git") :git t) (ws)
  (make-history ws)
  (shell ws "git checkout -q HEAD -- f")
  (expect (mapcar (lambda (line) (format nil "~A ~A" (jget line "sha") (jget line "text")))
                  (jlist (jget (run-ok ws '("git" "blame" "f")) "lines")))
          :to-equal (text-lines (oracle ws '("git" "perl")
                                        "git blame --line-porcelain f | perl -ne 'if (/^([0-9a-f]{40}) /) { $s = $1 } elsif (/^\\t(.*)/) { print \"$s $1\\n\" }'"
                                        nil))))

(define-row-case (40 "git show rev:path is git show" :foreign ("git") :git t) (ws)
  (make-history ws)
  (expect (lines-text (jget (run-ok ws '("git" "show" "HEAD~1:f")) "lines"))
          :to-equal (oracle ws '("git") "git show HEAD~1:f" (format nil "l1~%l2~%"))))

;;; Row 41

(define-row-case (41 "uname -s and -m are sys info's os and arch" :foreign ("uname")) (ws)
  (let ((envelope (run-ok ws '("sys" "info"))))
    (expect (jget envelope "os") :to-equal (string-downcase (trimmed (oracle ws '("uname") "uname -s" nil))))
    (expect (jget envelope "arch") :to-equal (trimmed (oracle ws '("uname") "uname -m" nil)))
    (expect (jget envelope "os_version") :to-equal (trimmed (oracle ws '("uname") "uname -r" nil)))))

(define-row-case (41 "whoami is sys info's user" :foreign ("whoami")) (ws)
  (expect (jget (run-ok ws '("sys" "info")) "user") :to-equal (trimmed (oracle ws '("whoami") "whoami" nil))))

(define-row-case (41 "hostname is sys info's hostname" :foreign ("hostname")) (ws)
  (expect (jget (run-ok ws '("sys" "info")) "hostname") :to-equal (trimmed (oracle ws '("hostname") "hostname" nil))))

(define-row-case (41 "nproc is sys info's cpus" :foreign ("nproc")) (ws)
  (expect (jget (run-ok ws '("sys" "info")) "cpus")
          :to-be (parse-integer (oracle ws '("getconf") "nproc 2>/dev/null || getconf _NPROCESSORS_ONLN" nil))))

(define-row-case (41 "env is sys env" :foreign ("env")) (ws)
  (expect (mapcar (lambda (item) (format nil "~A=~A" (jget item "name") (jget item "value")))
                  (jlist (jget (run-ok ws '("sys" "env" "E2E_MARK") :env '(("E2E_MARK" . "hello world"))) "items")))
          :to-equal (text-lines (oracle ws '("env" "grep") "env | grep '^E2E_MARK'" nil
                                        :env '(("E2E_MARK" . "hello world"))))))

(define-row-case (41 "which is sys tools" :foreign ("which")) (ws)
  (let ((items (jlist (jget (run-ok ws '("sys" "tools" "sh" "no-such-tool-e2e")) "items"))))
    (expect (jget (first items) "path") :to-equal (trimmed (oracle ws '("which") "which sh" nil)))
    (expect (list (jget (second items) "path")
                  (nth-value 1 (shell ws "which no-such-tool-e2e" :expect-status :any)))
            :to-equal (list nil 1))))

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

;;; Row 42

(define-row-case (42 "date +%s brackets time now's epoch" :foreign ("date")) (ws)
  (let* ((before (parse-integer (oracle ws '("date") "date +%s" nil)))
         (epoch (floor (jget (run-ok ws '("time" "now")) "epoch_ms") 1000))
         (after (parse-integer (oracle ws '("date") "date +%s" nil))))
    (expect (<= before epoch after) :to-be t)))

(define-row-case (42 "date -d '+1 day' is time convert --add 1d" :foreign ("date")) (ws)
  (expect (format nil "~A~%" (jget (run-ok ws '("time" "convert" "2026-01-31T00:00:00Z" "--add" "1d" "--tz" "UTC")) "result"))
          :to-equal (oracle ws '("date")
                            "date -u -d '2026-01-31 00:00:00 UTC + 1 day' +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || date -u -j -v+1d -f %Y-%m-%dT%H:%M:%SZ 2026-01-31T00:00:00Z +%Y-%m-%dT%H:%M:%SZ"
                            (format nil "2026-02-01T00:00:00Z~%"))))

(define-row-case (42 "TZ=Asia/Tokyo date is time now --tz Asia/Tokyo" :foreign ("date")) (ws)
  (let ((envelope (run-ok ws '("time" "now" "--tz" "Asia/Tokyo"))))
    (expect (format nil "~A ~A~%" (remove #\: (jget envelope "utc_offset")) (jget envelope "abbreviation"))
            :to-equal (oracle ws '("date") "TZ=Asia/Tokyo date '+%z %Z'" (format nil "+0900 JST~%")))))

(define-row-case (42 "the difference of two date +%s values is time diff" :foreign ("date")) (ws)
  (expect (format nil "~D~%" (jget (run-ok ws '("time" "diff" "2026-01-01T00:00:00Z" "2026-01-02T01:30:00Z")) "diff_ms"))
          :to-equal (oracle ws '("date")
                            "e() { date -u -d \"$1\" +%s 2>/dev/null || date -u -j -f %Y-%m-%dT%H:%M:%SZ \"$1\" +%s; }; echo $(( ($(e 2026-01-02T01:30:00Z) - $(e 2026-01-01T00:00:00Z)) * 1000 ))"
                            (format nil "91800000~%"))))

;;; Row 43

(define-row-case (43 "base64 is util encode base64" :foreign ("base64")) (ws)
  (put ws "b.bin" (coerce #(0 1 2 250 255 104 105) '(vector (unsigned-byte 8))))
  (expect (format nil "~A~%" (jget (run-ok ws '("util" "encode" "base64" "--content-file" "b.bin")) "output"))
          :to-equal (oracle ws '("base64") "base64 < b.bin" (format nil "AAEC+v9oaQ==~%"))))

(define-row-case (43 "base64 -d > file is util decode --to" :foreign ("base64")) (ws)
  (let ((expected (oracle ws '("base64") "printf 'AAEC+v9oaQ==' | base64 -d > o.bin && cat o.bin"
                          (coerce #(0 1 2 250 255 104 105) '(vector (unsigned-byte 8))) :octets t)))
    (run-ok ws '("util" "decode" "base64" "--content" "AAEC+v9oaQ==" "--to" "a.bin"))
    (expect (file-octets ws "a.bin") :to-equalp expected)))

(define-row-case (43 "xxd -r -p is util decode hex" :foreign ("xxd")) (ws)
  (expect (jget (run-ok ws '("util" "decode" "hex" "--content" "68692074686572650a")) "output")
          :to-equal (oracle ws '("xxd") "echo 68692074686572650a | xxd -r -p" (format nil "hi there~%"))))

(define-row-case (43 "URL encoding as jq @uri does is util encode url" :foreign ("jq")) (ws)
  (let ((value (format nil "a b&c/~C?x=1" (code-char #xE9))))
    (expect (format nil "~A~%" (jget (run-ok ws (list "util" "encode" "url" "--content" value)) "output"))
            :to-equal (oracle ws '("jq") "jq -rn --arg v \"$E2E_VALUE\" '$v | @uri'" "a%20b%26c%2F%C3%A9%3Fx%3D1
"
                              :env `(("E2E_VALUE" . ,value))))))

;;; Row 44

(define-row-case (44 "bc is util calc" :foreign ("bc")) (ws)
  (expect (format nil "~A~%" (jget (run-ok ws '("util" "calc" "2**100 - 3*(4+5)")) "result"))
          :to-equal (oracle ws '("bc") "echo '2^100 - 3*(4+5)' | BC_LINE_LENGTH=0 bc"
                            (format nil "1267650600228229401496703205349~%"))))

(define-row-case (44 "expr is util calc" :foreign ("expr")) (ws)
  (expect (format nil "~A~%" (jget (run-ok ws '("util" "calc" "7*6-5")) "result"))
          :to-equal (oracle ws '("expr") "expr 7 \\* 6 - 5" (format nil "37~%"))))

(define-row-case (44 "$((...)) is util calc" :foreign ()) (ws)
  (expect (format nil "~A~%" (jget (run-ok ws '("util" "calc" "(17 + 3) % 7")) "result"))
          :to-equal (oracle ws '() "echo $(( (17 + 3) % 7 ))" (format nil "6~%"))))

(define-row-case (44 "python3 -c 'print(10/4)' is util calc" :foreign ("python3")) (ws)
  (expect (format nil "~A~%" (jget (run-ok ws '("util" "calc" "10/4")) "result"))
          :to-equal (oracle ws '("python3") "python3 -c 'print(10/4)'" (format nil "2.5~%"))))

;;; Row 45

(defun uuid-shape (text)
  "TEXT with each hex digit shown as x, except the version nibble and the
RFC 4122 variant (v when it is 8, 9, a, or b)."
  (let ((lower (string-downcase (trimmed text))))
    (map 'string (lambda (char index)
                   (cond ((char= char #\-) #\-)
                         ((= index 14) char)
                         ((and (= index 19) (find char "89ab")) #\v)
                         ((digit-char-p char 16) #\x)
                         (t #\?)))
         lower (loop for i below (length lower) collect i))))

(define-row-case (45 "uuidgen's format is util uuid's" :foreign ("uuidgen")) (ws)
  (expect (uuid-shape (jget (run-ok ws '("util" "uuid")) "values" 0))
          :to-equal (uuid-shape (oracle ws '("uuidgen") "uuidgen" nil))))

(define-row-case (45 "openssl rand -hex 16's format is util random --length 32 --alphabet hex" :foreign ("openssl")) (ws)
  (flet ((shape (text) (map 'string (lambda (char) (if (find char "0123456789abcdef") #\x #\?)) (trimmed text))))
    (expect (shape (jget (run-ok ws '("util" "random" "--length" "32" "--alphabet" "hex")) "values" 0))
            :to-equal (shape (oracle ws '("openssl") "openssl rand -hex 16" nil)))))

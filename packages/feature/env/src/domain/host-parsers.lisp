;;;; packages/feature/env/src/domain/host-parsers.lisp
;;;;
;;;; Pure parsers for the OS sources behind `sys info`, `sys procs`, and `sys
;;;; ports` (Linux reads /proc, Darwin runs sysctl, vm_stat, ps, and lsof). Each takes the text the application layer already read
;;;; or captured, so the Linux parsers are tested on any host against
;;;; captured fixture text. Records are plists; the application layer turns
;;;; them into JSON.
(in-package #:aitools.env.domain)

(defun %split-lines (text)
  (with-input-from-string (stream text)
    (loop for line = (read-line stream nil nil) while line collect line)))

(defun %whitespace-p (char)
  (member char '(#\Space #\Tab #\Return #\Newline)))

(defun split-fields (line)
  "LINE split on runs of spaces and tabs."
  (let (fields (start nil))
    (loop for index from 0 to (length line)
          do (let ((blank (or (= index (length line)) (%whitespace-p (char line index)))))
               (cond ((and blank start) (push (subseq line start index) fields) (setf start nil))
                     ((and (not blank) (null start)) (setf start index)))))
    (nreverse fields)))

(defun %parse-field-integer (text)
  (and text (ascii-digits-p text) (parse-ascii-integer text 0 (length text))))

(defun %labelled-number (text label)
  "The first integer after `LABEL` at the start of a line of TEXT."
  (dolist (line (%split-lines text))
    (when (and (> (length line) (length label)) (string= label line :end2 (length label)))
      ;; vm_stat ends each count with a period ("2508908.").
      (let ((fields (mapcar (lambda (field) (string-right-trim "." field))
                            (split-fields (subseq line (length label))))))
        (return (%parse-field-integer (find-if #'ascii-digits-p fields)))))))

;;; ------------------------------------------------------------------ memory

(defun parse-meminfo (text)
  "(VALUES TOTAL-BYTES AVAILABLE-BYTES) from /proc/meminfo. Kernels before
3.14 have no MemAvailable; MemFree + Buffers + Cached stands in there."
  (let ((total (%labelled-number text "MemTotal:"))
        (available (or (%labelled-number text "MemAvailable:")
                       (let ((free (%labelled-number text "MemFree:")))
                         (and free (+ free (or (%labelled-number text "Buffers:") 0)
                                      (or (%labelled-number text "Cached:") 0)))))))
    (values (and total (* total 1024)) (and available (* available 1024)))))

(defun parse-vm-stat (text)
  "Available bytes from Darwin `vm_stat`: free, inactive, and speculative
pages (the pages the kernel hands out without paging anything out) times
the page size from the header line."
  (let* ((header (first (%split-lines text)))
         (marker (and header (search "page size of " header)))
         (page-size (and marker
                         (%parse-field-integer (first (split-fields (subseq header (+ marker 13))))))))
    (when page-size
      (* page-size (+ (or (%labelled-number text "Pages free:") 0)
                      (or (%labelled-number text "Pages inactive:") 0)
                      (or (%labelled-number text "Pages speculative:") 0))))))

(defun parse-sysctl-values (text)
  "The lines of `sysctl -n a b ...` output, one value per requested name."
  (mapcar (lambda (line) (string-trim " " line)) (%split-lines text)))

(defun count-cpuinfo-processors (text)
  (count-if (lambda (line)
              (let ((fields (split-fields line)))
                (and fields (string= (first fields) "processor"))))
            (%split-lines text)))

;;; --------------------------------------------------------------- processes

(defun parse-etime (text)
  "Seconds from a `ps -o etime` value `[[dd-]hh:]mm:ss`, or NIL."
  (let* ((dash (position #\- text))
         (days (if dash (%parse-field-integer (subseq text 0 dash)) 0))
         (clock (if dash (subseq text (1+ dash)) text))
         (parts (let (result (start 0))
                  (loop for colon = (position #\: clock :start start)
                        do (push (subseq clock start colon) result)
                           (if colon (setf start (1+ colon)) (return)))
                  (nreverse result)))
         (numbers (mapcar #'%parse-field-integer parts)))
    (when (and days (<= 2 (length numbers) 3) (every #'identity numbers))
      (destructuring-bind (seconds minutes &optional (hours 0)) (reverse numbers)
        (+ (* days 86400) (* hours 3600) (* minutes 60) seconds)))))

(defun parse-ps-output (text)
  "Records (:PID :PPID :USER :ELAPSED-SECONDS :COMMAND) from Darwin
`ps -axww -o pid=,ppid=,user=,etime=,command=`. The command keeps its
internal spacing."
  (let (records)
    (dolist (line (%split-lines text) (nreverse records))
      (let ((index 0) (fields nil))
        (dotimes (i 4)
          (let* ((start (or (position-if-not #'%whitespace-p line :start index) (length line)))
                 (end (or (position-if #'%whitespace-p line :start start) (length line))))
            (push (subseq line start end) fields)
            (setf index end)))
        (destructuring-bind (etime user ppid pid) fields
          (let ((pid (%parse-field-integer pid))
                (ppid (%parse-field-integer ppid))
                (elapsed (parse-etime etime)))
            (when (and pid ppid elapsed)
              (push (list :pid pid :ppid ppid :user user :elapsed-seconds elapsed
                          :command (string-trim " " (subseq line index)))
                    records))))))))

(defun parse-proc-stat-process (text)
  "(VALUES PID COMM PPID START-TICKS) from /proc/<pid>/stat. The command
name sits in parentheses and may itself contain spaces and `)`, so fields
are counted from the LAST `)`."
  (let ((open (position #\( text))
        (close (position #\) text :from-end t)))
    (when (and open close (< open close))
      (let ((pid (%parse-field-integer (string-trim " " (subseq text 0 open))))
            (fields (split-fields (subseq text (1+ close)))))
        ;; FIELDS starts at field 3 (state): ppid is field 4, starttime 22.
        (when (and pid (>= (length fields) 20))
          (values pid (subseq text (1+ open) close)
                  (%parse-field-integer (nth 1 fields))
                  (%parse-field-integer (nth 19 fields))))))))

(defun parse-proc-boot-time (text)
  "Boot time in epoch seconds from /proc/stat's `btime` line."
  (%labelled-number text "btime"))

(defun parse-proc-status-uid (text)
  "The real uid from /proc/<pid>/status's `Uid:` line."
  (%labelled-number text "Uid:"))

(defun parse-passwd (text)
  "An alist (UID . NAME) from /etc/passwd."
  (loop for line in (%split-lines text)
        for fields = (let (result (start 0))
                       (loop for colon = (position #\: line :start start)
                             do (push (subseq line start colon) result)
                                (if colon (setf start (1+ colon)) (return)))
                       (nreverse result))
        for uid = (%parse-field-integer (third fields))
        when uid collect (cons uid (first fields))))

(defun proc-cmdline-command (cmdline comm)
  "The command line from /proc/<pid>/cmdline (NUL-separated). Kernel threads
have an empty cmdline and are shown as `[COMM]`, as ps does."
  (let ((joined (string-trim " " (substitute #\Space (code-char 0) cmdline))))
    (if (zerop (length joined)) (format nil "[~A]" comm) joined)))

(defun process-matches-pattern-p (command pattern)
  "Case-insensitive substring match; a NIL PATTERN matches everything."
  (or (null pattern) (and (search pattern command :test #'char-equal) t)))

;;; ------------------------------------------------------------------- ports

(defun %hex-value (text start end)
  (let ((value 0))
    (loop for index from start below end
          for digit = (digit-char-p (char text index) 16)
          do (unless (and digit (char< (char text index) (code-char 128))) (return-from %hex-value nil))
             (setf value (+ (* value 16) digit)))
    value))

(defun format-ipv6 (groups)
  "RFC 5952 text for eight 16-bit GROUPS: lowercase, the longest run of two
or more zero groups (the first on a tie) compressed to `::`."
  (let ((best-start nil) (best-length 1))
    (loop with start = nil
          for index from 0 to 8
          do (if (and (< index 8) (zerop (nth index groups)))
                 (unless start (setf start index))
                 (when start
                   (when (> (- index start) best-length)
                     (setf best-start start best-length (- index start)))
                   (setf start nil))))
    (flet ((join (list) (format nil "~(~{~X~^:~}~)" list)))
      (if best-start
          (format nil "~A::~A"
                  (join (subseq groups 0 best-start))
                  (join (subseq groups (+ best-start best-length))))
          (join groups)))))

(defun %proc-net-address (text family)
  "Decode the hex address of /proc/net/tcp{,6}. Each 32-bit word is in the
kernel's byte order, little-endian on every architecture aitools builds
for (x86_64, aarch64)."
  (flet ((word-octets (start)
           (let ((word (%hex-value text start (+ start 8))))
             (and word (loop for shift from 0 below 32 by 8 collect (ldb (byte 8 shift) word))))))
    (ecase family
      (:ipv4 (let ((octets (and (= (length text) 8) (word-octets 0))))
               (and octets (format nil "~{~D~^.~}" octets))))
      (:ipv6 (when (= (length text) 32)
               (let ((words (loop for word from 0 below 4 collect (word-octets (* word 8)))))
                 (when (every #'identity words)
                   (format-ipv6 (loop for (high low) on (reduce #'append words) by #'cddr
                                      collect (+ (* high 256) low))))))))))

(defun parse-proc-net-tcp (text family)
  "Records (:ADDRESS :PORT :INODE) for every LISTEN (state 0A) row of
/proc/net/tcp (FAMILY :IPV4) or /proc/net/tcp6 (:IPV6)."
  (let (records)
    (dolist (line (rest (%split-lines text)) (nreverse records))
      (let ((fields (split-fields line)))
        (when (and (>= (length fields) 10) (string-equal (nth 3 fields) "0A"))
          (let* ((local (nth 1 fields))
                 (colon (position #\: local))
                 (address (and colon (%proc-net-address (subseq local 0 colon) family)))
                 (port (and colon (%hex-value local (1+ colon) (length local))))
                 (inode (%parse-field-integer (nth 9 fields))))
            (when (and address port inode)
              (push (list :address address :port port :inode inode) records))))))))

(defun parse-socket-inode (link)
  "The inode of a `socket:[12345]` fd link target, or NIL."
  (let ((prefix "socket:["))
    (when (and (> (length link) (1+ (length prefix)))
               (string= prefix link :end2 (length prefix))
               (char= (char link (1- (length link))) #\]))
      (%parse-field-integer (subseq link (length prefix) (1- (length link)))))))

(defun %split-host-port (name family)
  "(VALUES ADDRESS PORT) for an lsof `n` field: `*:80`, `127.0.0.1:631`,
`[::1]:631`. The wildcard becomes the family's unspecified address, matching
what /proc/net reports on Linux."
  (let ((colon (position #\: name :from-end t)))
    (when colon
      (let ((host (subseq name 0 colon))
            (port (%parse-field-integer (subseq name (1+ colon)))))
        (when port
          (values (cond ((string= host "*") (if (eq family :ipv6) "::" "0.0.0.0"))
                        ((and (> (length host) 1) (char= (char host 0) #\[))
                         (string-trim "[]" host))
                        (t host))
                  port))))))

(defun parse-lsof-listen (text)
  "Records (:ADDRESS :PORT :PID :COMMAND) from
`lsof -nP -iTCP -sTCP:LISTEN -Fpcnt`, whose output is one field per line:
`p` starts a process, `c` names it, `f` starts a file, `t` gives IPv4/IPv6,
`n` the local address."
  (let (records pid command family)
    (dolist (line (%split-lines text) (nreverse records))
      (when (plusp (length line))
        (let ((value (subseq line 1)))
          (case (char line 0)
            (#\p (setf pid (%parse-field-integer value) command nil family nil))
            (#\c (setf command value))
            (#\f (setf family nil))
            (#\t (setf family (if (string= value "IPv6") :ipv6 :ipv4)))
            (#\n (multiple-value-bind (address port) (%split-host-port value family)
                   (when port
                     (push (list :address address :port port :pid pid :command command) records))))))))))

(defun sort-and-deduplicate-ports (records)
  "RECORDS ordered by port, address, then pid, with duplicates (one socket
seen through several fds) removed."
  (let ((sorted (sort (copy-list records)
                      (lambda (a b)
                        (let ((port-a (getf a :port)) (port-b (getf b :port)))
                          (cond ((/= port-a port-b) (< port-a port-b))
                                ((string/= (getf a :address) (getf b :address))
                                 (string< (getf a :address) (getf b :address)))
                                (t (< (or (getf a :pid) -1) (or (getf b :pid) -1)))))))))
    (remove-duplicates sorted
                       :test (lambda (a b)
                               (and (= (getf a :port) (getf b :port))
                                    (string= (getf a :address) (getf b :address))
                                    (eql (getf a :pid) (getf b :pid))))
                       :from-end t)))

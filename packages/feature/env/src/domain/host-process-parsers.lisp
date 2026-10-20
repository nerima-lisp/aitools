(in-package #:aitools.env.domain)

;;; processes

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

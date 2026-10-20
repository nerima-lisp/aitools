(in-package #:aitools.env.domain)

;;; ports

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

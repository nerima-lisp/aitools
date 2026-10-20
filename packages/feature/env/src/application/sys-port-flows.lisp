(in-package #:aitools.env.application)

(defun %linux-socket-owners (ports)
  "Hash table inode -> (PID . COMMAND) from /proc/<pid>/fd links. Other
users' fd directories are unreadable without privileges; their sockets keep
pid and command null."
  (let ((owners (make-hash-table)))
    (dolist (entry (funcall (env-ports-list-directory ports) "/proc") owners)
      (when (aitools.env.domain:ascii-digits-p entry)
        (%linux-pid-socket-owners ports owners entry)))))

(defun %linux-socket-inode (ports fd-directory fd)
  (aitools.env.domain:parse-socket-inode
   (or (funcall (env-ports-read-link ports)
                (format nil "~A/~A" fd-directory fd))
       "")))

(defun %record-linux-socket-owner (ports owners pid inode command)
  (if (or (null inode) (gethash inode owners))
      command
      (let ((command
              (or command
                  (string-trim
                   '(#\Newline #\Space)
                   (or (%read-text ports
                                   (format nil "/proc/~A/comm" pid))
                       "")))))
        (setf (gethash inode owners)
              (cons (parse-integer pid) command))
        command)))

(defun %linux-pid-socket-owners (ports owners entry)
  (let ((fd-directory (format nil "/proc/~A/fd" entry))
        (command nil))
    (dolist (fd (funcall (env-ports-list-directory ports) fd-directory))
      (setf command
            (%record-linux-socket-owner
             ports
             owners
             entry
             (%linux-socket-inode ports fd-directory fd)
             command)))))

(defun %linux-listeners (ports)
  (let ((tcp (%read-text ports "/proc/net/tcp"))
        (tcp6 (%read-text ports "/proc/net/tcp6")))
    (if (and (null tcp) (null tcp6))
        :unavailable
        (let ((owners (%linux-socket-owners ports)))
          (values (mapcar (lambda (record)
                    (let ((owner (gethash (getf record :inode) owners)))
                      (list :port (getf record :port) :address (getf record :address)
                            :pid (car owner) :command (cdr owner))))
                  (append (and tcp (aitools.env.domain:parse-proc-net-tcp tcp :ipv4))
                          (and tcp6 (aitools.env.domain:parse-proc-net-tcp tcp6 :ipv6))))
                  (%object (cons "ipv4" (if tcp "ok" "source_unavailable"))
                           (cons "ipv6" (if tcp6 "ok" "source_unavailable")))
                  (append (unless tcp (list "/proc/net/tcp"))
                          (unless tcp6 (list "/proc/net/tcp6"))))))))

(defun %darwin-listeners (ports)
  "lsof exits 1 when nothing matches, so 1 with empty output is an empty
list rather than a failure."
  (multiple-value-bind (status exit-code stdout)
      (%run ports (%darwin-program :lsof) '("-nP" "-iTCP" "-sTCP:LISTEN" "-Fpcnt") *source-timeout-seconds*)
    (cond ((not (eq status :exited)) :unavailable)
          ((eql exit-code 0) (aitools.env.domain:parse-lsof-listen stdout))
          ((and (eql exit-code 1) (zerop (length (string-trim '(#\Newline #\Space) (or stdout ""))))) nil)
          (t :unavailable))))

(aitools.kernel.domain:define-flow
    (sys-ports/k ports &key on-ok on-partial on-error)
  "TCP sockets in LISTEN state."
  (let ((os
          (%os-kind
           (nth-value
            0
            (funcall (env-ports-system-identity ports))))))
    (multiple-value-bind (records coverage unavailable)
        (case os
          (:linux (%linux-listeners ports))
          (:darwin (%darwin-listeners ports))
          (t :unavailable))
      (if (eq records :unavailable)
          (aitools.kernel.domain:flow-call
              (%unavailable
               (concatenate
                'string
                "no listening-socket source (/proc/net/tcp on Linux, "
                "lsof on Darwin) is available")
               "lsof")
            (:on-error (code message &key repairs)
              (aitools.kernel.domain:flow-continue
               :on-error
               code
               message
               :repairs repairs)))
          (let* ((records
                   (aitools.env.domain:sort-and-deduplicate-ports records))
                 (items
                   (mapcar
                    (lambda (record)
                      (%object
                       (cons "port" (getf record :port))
                       (cons "address" (getf record :address))
                       (cons "protocol" "tcp")
                       (cons "pid" (%null-if-nil (getf record :pid)))
                       (cons "command" (%null-if-nil (getf record :command)))))
                    records))
                 (fields
                   (append
                    (list (cons "items" items)
                          (cons "total" (length items)))
                    (when coverage
                      (list (cons "coverage" coverage)))
                    (when unavailable
                      (list (cons "source_unavailable" unavailable))))))
            (if unavailable
                (aitools.kernel.domain:flow-continue
                 :on-partial
                 fields)
                (aitools.kernel.domain:flow-continue
                 :on-ok
                 fields)))))))

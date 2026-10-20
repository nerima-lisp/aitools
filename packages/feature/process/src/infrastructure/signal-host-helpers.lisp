(in-package #:aitools.process.infrastructure)

#+linux
(defun %linux-process-identity (pid base uids fields)
  (when (and (>= (length uids) 2) fields)
    (let ((ruid (first uids))
          (uid (second uids)))
      (aitools.process.domain:make-process-identity
       :pid pid
       :ppid (parse-integer (nth 1 fields))
       :pgid (parse-integer (nth 2 fields))
       :ruid ruid
       :uid uid
       :start (parse-integer (nth 19 fields))
       ;; Do not inspect another user's argv. An empty command
       ;; line is a private sentinel for an ineligible identity;
       ;; NIL means this user's argv could not be read.
       :command-line (%linux-command-line base uid ruid)))))

#+linux
(defun %linux-command-line (base uid ruid)
  (if (and (= uid (sb-posix:getuid))
           (= ruid (sb-posix:getuid)))
      (%linux-argv (%linux-file-bytes (concatenate 'string base "cmdline")))
      ""))

#+linux
(defun %linux-stat-fields (stat)
  (let ((close (position #\) stat :from-end t)))
    (when close
      (let ((fields
              (remove "" (uiop:split-string (subseq stat (+ close 2))
                                             :separator '(#\Space))
                      :test #'string=)))
        (when (>= (length fields) 20)
          fields)))))

#+linux
(defun %linux-status-uids (status)
  (let ((uid-line
          (find-if (lambda (line)
                     (and (>= (length line) 4)
                          (string= line "Uid:" :end1 4)))
                   (uiop:split-string status :separator '(#\Newline)))))
    (when uid-line
      (mapcar #'parse-integer
              (remove "" (uiop:split-string (subseq uid-line 4)
                                             :separator '(#\Space #\Tab))
                      :test #'string=)))))

#+darwin
(defun %darwin-process-identity (pid bytes)
  (let ((uid (%le bytes 20 4))
        (ruid (%le bytes 28 4)))
    (aitools.process.domain:make-process-identity
     :pid pid
     :ppid (%le bytes 16 4)
     :pgid (%le bytes 100 4)
     :uid uid
     :ruid ruid
     :start (format nil "~D.~6,'0D" (%le bytes 120 8) (%le bytes 128 8))
     ;; Do not inspect another user's argv. An empty command line is a
     ;; private sentinel for an identity that is not eligible to signal; NIL
     ;; means this user's argv could not be read.
     :command-line (%darwin-command-line uid ruid pid))))

#+darwin
(defun %darwin-command-line (uid ruid pid)
  (if (and (= uid (sb-posix:getuid))
           (= ruid (sb-posix:getuid)))
      (%darwin-argv pid)
      ""))

#+darwin
(defun %darwin-parse-argv (bytes)
  (multiple-value-bind (argc offset) (%darwin-argv-layout bytes)
    (when argc
      (multiple-value-bind (arguments count) (%darwin-argv-fields bytes argc offset)
        (when (= count argc)
          (%join-argv arguments))))) )

#+darwin
(defun %darwin-argv-fields (bytes argc offset)
  (loop with arguments = nil
        repeat argc
        for end = (position 0 bytes :start offset)
          then (position 0 bytes :start offset)
        while end
        do (push (sb-ext:octets-to-string bytes :start offset :end end
                                          :external-format :utf-8)
                 arguments)
           (setf offset (1+ end))
        finally
          (let ((count (length arguments)))
            (return (values (nreverse arguments) count)))))

#+darwin
(defun %darwin-argv-layout (bytes)
  (when (>= (length bytes) 6)
    (let* ((argc (%le bytes 0 4))
           (offset (nth-value 1 (%nul-field bytes 4))))
      (when (and offset (<= offset (length bytes))
                 (plusp argc) (< argc 4096))
        (values argc (%darwin-skip-argv-padding bytes offset))))))

#+darwin
(defun %darwin-skip-argv-padding (bytes offset)
  (loop while (and (< offset (length bytes))
                   (zerop (aref bytes offset)))
        do (incf offset))
  offset)

#+darwin
(defun %darwin-query-argv-bytes (pid size)
  (sb-alien:with-alien ((mib (array sb-alien:int 3))
                        (actual-size sb-alien:unsigned-long))
    (setf (sb-alien:deref mib 0) 1
          (sb-alien:deref mib 1) 49
          (sb-alien:deref mib 2) pid
          actual-size size)
    (let ((bytes (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
      (let ((sysctl (sb-alien:extern-alien "sysctl"
                      (function sb-alien:int (* (array sb-alien:int 3)) sb-alien:unsigned-int
                                (* sb-alien:unsigned-char) (* sb-alien:unsigned-long)
                                (* sb-alien:unsigned-char) sb-alien:unsigned-long))))
        (when (zerop (sb-alien:alien-funcall sysctl (sb-alien:addr mib) 3
                                             (sb-alien:sap-alien (sb-sys:vector-sap bytes)
                                                                (* sb-alien:unsigned-char))
                                             (sb-alien:addr actual-size)
                                             (sb-alien:sap-alien (sb-sys:int-sap 0) (* sb-alien:unsigned-char)) 0))
          (subseq bytes 0 (min size actual-size)))))))

#+darwin
(defun %darwin-query-argv-size (pid)
  (sb-alien:with-alien ((mib (array sb-alien:int 3))
                        (size sb-alien:unsigned-long))
    (setf (sb-alien:deref mib 0) 1
          (sb-alien:deref mib 1) 49
          (sb-alien:deref mib 2) pid
          size 0)
    (let ((sysctl (sb-alien:extern-alien "sysctl"
                    (function sb-alien:int (* (array sb-alien:int 3)) sb-alien:unsigned-int
                              (* sb-alien:unsigned-char) (* sb-alien:unsigned-long)
                              (* sb-alien:unsigned-char) sb-alien:unsigned-long))))
      (when (zerop (sb-alien:alien-funcall sysctl (sb-alien:addr mib) 3
                                           (sb-alien:sap-alien (sb-sys:int-sap 0) (* sb-alien:unsigned-char))
                                           (sb-alien:addr size)
                                           (sb-alien:sap-alien (sb-sys:int-sap 0) (* sb-alien:unsigned-char)) 0))
        size))))

(defun %join-argv (argv)
  (format nil "~{~A~^ ~}" argv))

(defun %nul-field (bytes start)
  (let ((end (position 0 bytes :start start)))
    (when end
      (values (sb-ext:octets-to-string bytes :start start :end end :external-format :utf-8)
              (1+ end)))))

(defun %le (bytes offset width)
  (loop for index below width sum (ash (aref bytes (+ offset index)) (* 8 index))))

(in-package #:aitools.env.application)

(defparameter +snapshot-section-names+ '("info" "metrics" "network" "disk" "ports"))

(defun %snapshot-info-missing (data)
  (let ((missing nil))
    (dolist (key '("os" "os_version" "arch" "cpus" "user" "uid" "hostname"))
      (let ((value (cdr (assoc key data :test #'string=))))
        (when (or (null value) (eq value (aitools.protocol.domain:json-null)))
          (push key missing))))
    (dolist (key '("memory" "disk"))
      (dolist (entry
               (aitools.protocol.domain:json-object-members
                (cdr (assoc key data :test #'string=))))
        (when (eq (cdr entry) (aitools.protocol.domain:json-null))
          (push (format nil "~A.~A" key (car entry)) missing))))
    (when missing
      (%object (cons "code" "missing_observations")
               (cons "fields" (nreverse missing))))))

(defun %snapshot-limit-ports (data limit)
  (let* ((items (cdr (assoc "items" data :test #'string=)))
         (truncated (> (length items) limit))
         (unavailable (cdr (assoc "source_unavailable" data :test #'string=)))
         (unknown
           (some (lambda (item)
                   (eq (cdr (assoc "pid"
                                   (aitools.protocol.domain:json-object-members item)
                                   :test #'string=))
                       (aitools.protocol.domain:json-null)))
                 items)))
    (setf (cdr (assoc "items" data :test #'string=))
          (subseq items 0 (min limit (length items))))
    (nconc data
           (list (cons "limit" limit)
                 (cons "truncated" (aitools.protocol.domain:json-boolean truncated))
                 (cons "scope" "tcp_listen")
                 (cons "owner_scan_bounded" (aitools.protocol.domain:json-boolean t))))
    (when (or truncated unknown unavailable)
      (%object (cons "output_truncated" (aitools.protocol.domain:json-boolean truncated))
               (cons "source_unavailable" unavailable)
               (cons "owner_unknown" (aitools.protocol.domain:json-boolean unknown)))))
)
(in-package #:aitools.env.application)

(aitools.kernel.domain:define-flow
    (sys-snapshot/k ports &key path (process-limit 20) (network-limit 100)
                    (port-limit 100) on-ok on-partial on-error)
  (if (not
       (every
        (lambda (limit)
          (and (integerp limit) (<= 1 limit 1000)))
        (list process-limit network-limit port-limit)))
      (aitools.kernel.domain:flow-continue
       :on-error
       "argument.invalid"
       "Snapshot limits must be between 1 and 1000.")
      (let ((started (%now ports))
            (sections nil)
            (usable 0)
            (incomplete nil))
        (labels
            ((finish (name begin status data reason)
               (unless (string= status "unavailable")
                 (incf usable))
               (unless (string= status "ok")
                 (setf incomplete t))
               (push
                (cons
                 name
                 (%object
                  (cons "status" status)
                  (cons "data"
                        (if data
                            (apply #'%object data)
                            (aitools.protocol.domain:json-null)))
                  (cons "reason" (%null-if-nil reason))
                  (cons "started_at_epoch_ms" begin)
                  (cons "finished_at_epoch_ms" (%now ports))))
                sections))
             (fail (name begin code message)
               (finish
                name
                begin
                "unavailable"
                nil
                (%object
                 (cons "code" code)
                 (cons "message" message))))
             (finish-snapshot ()
               (let ((fields
                       (list
                        (cons "scope" "host")
                        (cons "atomic"
                              (aitools.protocol.domain:json-boolean nil))
                        (cons "started_at_epoch_ms" started)
                        (cons "finished_at_epoch_ms" (%now ports))
                        (cons "source_timeout_seconds"
                              *source-timeout-seconds*)
                        (cons "limits"
                              (%object
                               (cons "processes" process-limit)
                               (cons "interfaces" network-limit)
                               (cons "ports" port-limit)))
                        (cons
                         "collection_bounds"
                         (concatenate
                          'string
                          "Output limits only; no overall deadline or memory bound. "
                          "OS calls and socket-owner scans are not time bounded; "
                          "external commands capture complete output."))
                        (cons "sections" (apply #'%object (nreverse sections))))))
                 (cond
                   ((zerop usable)
                    (aitools.kernel.domain:flow-continue
                     :on-error
                     "environment.unavailable"
                     "All snapshot sections are unavailable."
                     :diagnostics
                     (list (apply #'%object fields))))
                   (incomplete
                    (aitools.kernel.domain:flow-continue :on-partial fields))
                   (t
                    (aitools.kernel.domain:flow-continue :on-ok fields)))))
             (observe-info ()
               (let ((begin (%now ports)))
                 (handler-case
                     (aitools.kernel.domain:flow-call
                         (sys-info/k ports)
                       (:on-ok (data)
                         (let ((reason (%snapshot-info-missing data)))
                           (finish
                            "info"
                            begin
                            (if reason "partial" "ok")
                            data
                            reason))
                         (observe-metrics))
                       (:on-error (code message &key repairs)
                         (declare (ignore repairs))
                         (fail "info" begin code message)
                         (observe-metrics)))
                   (file-error ()
                     (finish
                      "info" begin "unavailable" nil
                      "source_file_unreadable")
                     (observe-metrics)))))
             (observe-metrics ()
               (let ((begin (%now ports)))
                 (handler-case
                     (aitools.kernel.domain:flow-call
                         (sys-metrics/k ports :limit process-limit)
                       (:on-ok (data)
                         (finish "metrics" begin "ok" data nil)
                         (observe-network))
                       (:on-partial (data)
                         (finish
                          "metrics" begin "partial" data "output_limit")
                         (observe-network))
                       (:on-error (code message)
                         (fail "metrics" begin code message)
                         (observe-network)))
                   (file-error ()
                     (finish
                      "metrics" begin "unavailable" nil
                      "source_file_unreadable")
                     (observe-network)))))
             (observe-network ()
               (let ((begin (%now ports)))
                 (handler-case
                     (aitools.kernel.domain:flow-call
                         (sys-network/k ports :limit network-limit)
                       (:on-ok (data)
                         (finish "network" begin "ok" data nil)
                         (observe-disk))
                       (:on-partial (data)
                         (finish
                          "network" begin "partial" data "output_limit")
                         (observe-disk))
                       (:on-error (code message)
                         (fail "network" begin code message)
                         (observe-disk)))
                   (file-error ()
                     (finish
                      "network" begin "unavailable" nil
                      "source_file_unreadable")
                     (observe-disk)))))
             (observe-disk ()
               (let ((begin (%now ports)))
                 (handler-case
                     (aitools.kernel.domain:flow-call
                         (sys-disk/k ports :path path)
                       (:on-ok (data)
                         (finish "disk" begin "ok" data nil)
                         (observe-ports))
                       (:on-error (code message)
                         (fail "disk" begin code message)
                         (observe-ports)))
                   (file-error ()
                     (finish
                      "disk" begin "unavailable" nil
                      "source_file_unreadable")
                     (observe-ports)))))
             (observe-ports ()
               (let ((begin (%now ports)))
                 (handler-case
                     (aitools.kernel.domain:flow-call
                         (sys-ports/k ports)
                       (:on-ok (data)
                         (let ((reason
                                 (%snapshot-limit-ports data port-limit)))
                           (finish
                            "ports"
                            begin
                            (if reason "partial" "ok")
                            data
                            reason))
                         (finish-snapshot))
                       (:on-partial (data)
                         (finish
                          "ports"
                          begin
                          "partial"
                          data
                          (%snapshot-limit-ports data port-limit))
                         (finish-snapshot))
                       (:on-error (code message &key repairs)
                         (declare (ignore repairs))
                         (fail "ports" begin code message)
                         (finish-snapshot)))
                   (file-error ()
                     (finish
                      "ports" begin "unavailable" nil
                      "source_file_unreadable")
                     (finish-snapshot))))))
          (observe-info)))))

(defun %snapshot-info-missing (data)
  (let ((missing nil))
    (dolist (key '("os" "os_version" "arch" "cpus" "user" "uid" "hostname"))
      (let ((value (cdr (assoc key data :test #'string=))))
        (when (or (null value) (eq value (aitools.protocol.domain:json-null))) (push key missing))))
    (dolist (key '("memory" "disk"))
      (dolist (entry (aitools.protocol.domain:json-object-members (cdr (assoc key data :test #'string=))))
        (when (eq (cdr entry) (aitools.protocol.domain:json-null))
          (push (format nil "~A.~A" key (car entry)) missing))))
    (when missing (%object (cons "code" "missing_observations") (cons "fields" (nreverse missing))))))

(defun %snapshot-limit-ports (data limit)
  (let* ((items (cdr (assoc "items" data :test #'string=)))
         (truncated (> (length items) limit))
         (unavailable (cdr (assoc "source_unavailable" data :test #'string=)))
         (unknown (some (lambda (item)
                          (eq (cdr (assoc "pid" (aitools.protocol.domain:json-object-members item) :test #'string=))
                              (aitools.protocol.domain:json-null))) items)))
    (setf (cdr (assoc "items" data :test #'string=)) (subseq items 0 (min limit (length items))))
    (nconc data
          (list
           (cons "limit" limit)
           (cons "truncated" (aitools.protocol.domain:json-boolean truncated))
           (cons "scope" "tcp_listen")
           (cons "owner_scan_bounded"
                 (aitools.protocol.domain:json-boolean nil))))
    (when (or truncated unknown unavailable)
      (%object (cons "output_truncated" (aitools.protocol.domain:json-boolean truncated))
               (cons "source_unavailable" unavailable)
               (cons "owner_unknown" (aitools.protocol.domain:json-boolean unknown))))))

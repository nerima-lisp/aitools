(in-package #:aitools.env.application)

(aitools.kernel.domain:define-flow
    (sys-snapshot/k boundaries &key path (timeout "600s") (process-limit 20)
                    (network-limit 100) (port-limit 100) on-ok on-partial on-error)
  (let ((timeout-seconds (%snapshot-timeout-seconds timeout)))
    (cond
      ((not (every (lambda (limit) (and (integerp limit) (<= 1 limit 1000)))
                   (list process-limit network-limit port-limit)))
       (aitools.kernel.domain:flow-continue
        :on-error "argument.invalid" "Snapshot limits must be between 1 and 1000."))
      ((null timeout-seconds)
       (aitools.kernel.domain:flow-continue
        :on-error "argument.invalid" "Snapshot timeout must be a positive finite duration."))
      (t
       (let ((started (%now boundaries))
             (sections nil)
             (usable 0)
             (incomplete nil)
             (deadline-reached nil)
             (active-section nil)
             (active-started-at nil))
         (labels
             ((finish (name begin status data reason)
                (unless (string= status "unavailable") (incf usable))
                (unless (string= status "ok") (setf incomplete t))
                (push
                 (cons name
                       (%object
                        (cons "status" status)
                        (cons "data"
                              (if data (apply #'%object data)
                                  (aitools.protocol.domain:json-null)))
                        (cons "reason" (%null-if-nil reason))
                        (cons "started_at_epoch_ms" begin)
                        (cons "finished_at_epoch_ms" (%now boundaries))))
                 sections)
                (setf active-section nil active-started-at nil))
              (fail (name begin code message)
                (finish name begin "unavailable" nil
                        (%object (cons "code" code) (cons "message" message))))
              (complete-deadline-sections ()
                (dolist (name +snapshot-section-names+)
                  (unless (assoc name sections :test #'string=)
                    (fail name
                          (if (string= name active-section)
                              (or active-started-at (%now boundaries))
                              (%now boundaries))
                          "environment.timeout" "Snapshot deadline was reached."))))
              (finish-snapshot ()
                (unless deadline-reached (%check-snapshot-deadline boundaries))
                (when deadline-reached (complete-deadline-sections))
                (let ((fields
                        (list
                         (cons "scope" "host")
                         (cons "atomic" (aitools.protocol.domain:json-boolean nil))
                         (cons "started_at_epoch_ms" started)
                         (cons "finished_at_epoch_ms" (%now boundaries))
                         (cons "source_timeout_seconds" *source-timeout-seconds*)
                         (cons "overall_timeout_ms" (* timeout-seconds 1000))
                         (cons "deadline_reached"
                               (aitools.protocol.domain:json-boolean deadline-reached))
                         (cons "limits"
                               (%object (cons "processes" process-limit)
                                        (cons "interfaces" network-limit)
                                        (cons "ports" port-limit)))
                         (cons "collection_bounds"
                               (concatenate
                                'string
                                "The configured monotonic deadline covers every section and socket-owner scan. "
                                "External commands are killed and reaped at min(60 seconds, remaining budget). "
                                "Bounded local reads check before and after each operation; directory and ancestor walks check between entries. "
                                "Captured subprocess output has no memory bound."))
                         (cons "sections" (apply #'%object (nreverse sections))))))
                  (cond
                    ((zerop usable)
                     (aitools.kernel.domain:flow-continue
                      :on-error "environment.unavailable"
                      "All snapshot sections are unavailable."
                      :diagnostics (list (apply #'%object fields))))
                    (incomplete (aitools.kernel.domain:flow-continue :on-partial fields))
                    (t (aitools.kernel.domain:flow-continue :on-ok fields)))))
              (observe-info ()
                (let ((begin (%now boundaries)))
                  (setf active-section "info" active-started-at begin)
                  (handler-case
                      (aitools.kernel.domain:flow-call
                          (sys-info/k boundaries)
                        (:on-ok (data)
                          (let ((reason (%snapshot-info-missing data)))
                            (finish "info" begin (if reason "partial" "ok") data reason))
                          (observe-metrics))
                        (:on-error (code message &key repairs)
                          (declare (ignore repairs))
                          (fail "info" begin code message)
                          (observe-metrics)))
                    (file-error ()
                      (finish "info" begin "unavailable" nil "source_file_unreadable")
                      (observe-metrics)))))
              (observe-metrics ()
                (let ((begin (%now boundaries)))
                  (setf active-section "metrics" active-started-at begin)
                  (handler-case
                      (aitools.kernel.domain:flow-call
                          (sys-metrics/k boundaries :limit process-limit)
                        (:on-ok (data)
                          (finish "metrics" begin "ok" data nil)
                          (observe-network))
                        (:on-partial (data)
                          (finish "metrics" begin "partial" data "output_limit")
                          (observe-network))
                        (:on-error (code message &key repairs)
                          (declare (ignore repairs))
                          (fail "metrics" begin code message)
                          (observe-network)))
                    (file-error ()
                      (finish "metrics" begin "unavailable" nil "source_file_unreadable")
                      (observe-network)))))
              (observe-network ()
                (let ((begin (%now boundaries)))
                  (setf active-section "network" active-started-at begin)
                  (handler-case
                      (aitools.kernel.domain:flow-call
                          (sys-network/k boundaries :limit network-limit)
                        (:on-ok (data)
                          (finish "network" begin "ok" data nil)
                          (observe-disk))
                        (:on-partial (data)
                          (finish "network" begin "partial" data "output_limit")
                          (observe-disk))
                        (:on-error (code message &key repairs)
                          (declare (ignore repairs))
                          (fail "network" begin code message)
                          (observe-disk)))
                    (file-error ()
                      (finish "network" begin "unavailable" nil "source_file_unreadable")
                      (observe-disk)))))
              (observe-disk ()
                (let ((begin (%now boundaries)))
                  (setf active-section "disk" active-started-at begin)
                  (handler-case
                      (aitools.kernel.domain:flow-call
                          (sys-disk/k boundaries :path path)
                        (:on-ok (data)
                          (finish "disk" begin "ok" data nil)
                          (observe-ports))
                        (:on-error (code message &key repairs)
                          (declare (ignore repairs))
                          (fail "disk" begin code message)
                          (observe-ports)))
                    (file-error ()
                      (finish "disk" begin "unavailable" nil "source_file_unreadable")
                      (observe-ports)))))
              (observe-ports ()
                (let ((begin (%now boundaries)))
                  (setf active-section "ports" active-started-at begin)
                  (handler-case
                      (aitools.kernel.domain:flow-call
                          (sys-ports/k boundaries)
                        (:on-ok (data)
                          (let ((reason (%snapshot-limit-ports data port-limit)))
                            (finish "ports" begin (if reason "partial" "ok") data reason))
                          (finish-snapshot))
                        (:on-partial (data)
                          (finish "ports" begin "partial" data
                                  (%snapshot-limit-ports data port-limit))
                          (finish-snapshot))
                        (:on-error (code message &key repairs)
                          (declare (ignore repairs))
                          (fail "ports" begin code message)
                          (finish-snapshot)))
                    (file-error ()
                      (finish "ports" begin "unavailable" nil "source_file_unreadable")
                      (finish-snapshot))))))
           (handler-case
               (let ((*snapshot-deadline*
                       (+ (%snapshot-monotonic-time boundaries)
                          (* timeout-seconds internal-time-units-per-second))))
                 (observe-info))
             (snapshot-deadline-exceeded ()
               (setf deadline-reached t)
               (finish-snapshot))))))))
)

;;;; packages/feature/env/src/application/time-flows.lisp
;;;;
;;;; `time now`, `time convert`, `time diff`. Each flow calls
;;;; exactly one of ON-OK / ON-ERROR with the protocol's shapes.
;;;;
;;;; Durations from --add/--sub move the instant, not the wall clock: `--add
;;;; 1d` across a DST change lands 24 hours later, which may read 23:00 or
;;;; 01:00 on the next day's wall clock.
(in-package #:aitools.env.application)

(defparameter *time-convert-targets* '("iso8601" "epoch_ms" "epoch_s"))

(defun %object (&rest alist)
  (aitools.protocol.domain:json-object-from-alist alist))

(defun local-zone-name (ports)
  "The zone `time` uses without --tz: $TZ when it names an IANA zone, else
the /etc/localtime link target, else UTC."
  (or (aitools.env.domain:zone-name-from-tz-variable (%getenv ports "TZ"))
      (aitools.env.domain:zone-name-from-localtime-link
       (funcall (env-ports-read-link ports) "/etc/localtime"))
      "UTC"))

(aitools.kernel.domain:define-flow
    (time-now/k ports &key tz on-ok on-error)
  (let ((now (%now ports)))
    (aitools.kernel.domain:flow-call
        (%with-zone/k ports (or tz (local-zone-name ports)))
      (:on-zone (zone)
        (multiple-value-bind (offset daylight-p abbreviation)
            (aitools.env.domain:zone-offset-at zone now)
          (declare (ignore daylight-p))
          (aitools.kernel.domain:flow-continue
           :on-ok
           (list (cons "iso8601" (%zone-iso8601 zone now))
                 (cons "utc" (aitools.env.domain:format-iso8601 now 0 :utc-designator t))
                 (cons "epoch_ms" now)
                 (cons "timezone" (aitools.env.domain:zone-name zone))
                 (cons "utc_offset" (aitools.env.domain:format-utc-offset offset))
                 (cons "abbreviation" abbreviation)))))
      (:on-error (code message &key repairs)
        (aitools.kernel.domain:flow-continue
         :on-error code message :repairs repairs)))))

(aitools.kernel.domain:define-flow
    (time-convert/k ports value &key (to "iso8601") add sub tz on-ok on-error)
  "ADD and SUB are duration strings. TZ controls ISO output and offset-less input; otherwise the local zone is used."
  (let ((zone-name (or tz (local-zone-name ports)))
        (example (format nil "aitools time convert 2026-03-08T01:30:00Z --to ~A" to)))
    (if (not (member to *time-convert-targets* :test #'string=))
        (aitools.kernel.domain:flow-continue
         :on-error
         "argument.invalid"
         (format nil "--to must be one of ~{~A~^, ~}"
                 *time-convert-targets*)
         :repairs (list (repair "fix-target" "Pick an output format." example)))
        (aitools.kernel.domain:flow-call
            (%parse-durations/k add)
          (:on-ok (added)
            (aitools.kernel.domain:flow-call
                (%parse-durations/k sub)
              (:on-ok (subtracted)
                (aitools.kernel.domain:flow-call
                    (%parse-instant/k ports value zone-name example)
                  (:on-instant (instant input)
                    (let ((result-instant (+ instant added (- subtracted))))
                      (if (not
                           (aitools.env.domain:epoch-milliseconds-in-range-p
                            result-instant))
                          (aitools.kernel.domain:flow-continue
                           :on-error
                           "input.syntax-error"
                           (format nil
                                   "~A is outside years 0000-9999 after --add/--sub"
                                   value)
                           :repairs
                           (list (repair "fix-value"
                                         "Keep the result within years 0000-9999."
                                         example)))
                          (aitools.kernel.domain:flow-call
                              (%with-zone/k ports zone-name)
                            (:on-zone (zone)
                              (aitools.kernel.domain:flow-continue
                               :on-ok
                               (list
                                (cons "input" value)
                                (cons "input_format"
                                      (if input
                                          (aitools.env.domain:time-input-format input)
                                          "now"))
                                (cons "to" to)
                                (cons "result"
                                      (cond
                                        ((string= to "epoch_ms") result-instant)
                                        ((string= to "epoch_s")
                                         (floor result-instant 1000))
                                        (t (%zone-iso8601 zone result-instant))))
                                (cons "timezone"
                                      (aitools.env.domain:zone-name zone)))))
                            (:on-error (code message &key repairs)
                              (aitools.kernel.domain:flow-continue
                               :on-error code message :repairs repairs))))))
                  (:on-error (code message &key repairs)
                    (aitools.kernel.domain:flow-continue
                     :on-error code message :repairs repairs))))
              (:on-error (code message &key repairs)
                (aitools.kernel.domain:flow-continue
                 :on-error code message :repairs repairs))))
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message :repairs repairs))))))

(aitools.kernel.domain:define-flow
    (time-diff/k ports a b &key on-ok on-error)
  "diff_ms is B minus A; offset-less inputs are read in the local zone."
  (let ((zone-name (local-zone-name ports))
        (example "aitools time diff 2026-03-08T00:00:00Z 2026-03-08T01:23:00Z"))
    (aitools.kernel.domain:flow-call
        (%parse-instant/k ports a zone-name example)
      (:on-instant (first-instant first-input)
        (declare (ignore first-input))
        (aitools.kernel.domain:flow-call
            (%parse-instant/k ports b zone-name example)
          (:on-instant (second-instant second-input)
            (declare (ignore second-input))
            (let ((difference (- second-instant first-instant)))
              (aitools.kernel.domain:flow-continue
               :on-ok
               (list (cons "a" a)
                     (cons "b" b)
                     (cons "diff_ms" difference)
                     (cons "human"
                           (aitools.env.domain:format-human-duration difference))))))
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message :repairs repairs))))
      (:on-error (code message &key repairs)
        (aitools.kernel.domain:flow-continue
         :on-error code message :repairs repairs)))))

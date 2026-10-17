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

(defun resolve-zone/k (ports name &key on-zone on-unknown on-unavailable)
  "Call ON-ZONE with the ZONE for IANA NAME, ON-UNKNOWN when no zoneinfo
directory has a valid TZif file by that name, or ON-UNAVAILABLE when no
zoneinfo directory exists at all."
  (cond
    ((aitools.env.domain:utc-zone-name-p name)
     (funcall on-zone (aitools.env.domain:make-fixed-zone name 0)))
    ((not (aitools.env.domain:valid-zone-name-p name))
     (funcall on-unknown))
    (t
     (let ((any-directory nil))
       (dolist (directory (aitools.env.domain:zoneinfo-directories (%getenv ports "TZDIR")))
         (when (funcall (env-ports-list-directory ports) directory)
           (setf any-directory t)
           (let ((octets (%read-octets ports (aitools.env.domain:join-directory directory name))))
             (when octets
               (handler-case
                   (return-from resolve-zone/k
                     (funcall on-zone (aitools.env.domain:parse-tzif name octets)))
                 (aitools.env.domain:tzif-format-error () nil))))))
       (if any-directory (funcall on-unknown) (funcall on-unavailable))))))

(defun %with-zone/k (ports name on-zone on-error)
  (resolve-zone/k
   ports name
   :on-zone on-zone
   :on-unknown (lambda ()
                 (funcall on-error "argument.invalid"
                          (format nil "unknown time zone ~S; use an IANA name such as Asia/Tokyo" name)
                          :repairs (list (repair "use-utc" "Use UTC, or an IANA zone name."
                                                  "aitools time now --tz UTC"))))
   :on-unavailable (lambda ()
                     (funcall on-error "environment.unavailable"
                              "no zoneinfo database found; set TZDIR to its directory"
                              :repairs (list (repair "use-utc" "UTC needs no zoneinfo database."
                                                      "aitools time now --tz UTC"))))))

(defun %zone-iso8601 (zone instant)
  (aitools.env.domain:format-iso8601
   instant (aitools.env.domain:zone-offset-at zone instant)
   :utc-designator (aitools.env.domain:utc-zone-name-p (aitools.env.domain:zone-name zone))))

(defun time-now/k (ports &key tz on-ok on-error)
  (let ((now (%now ports)))
    (%with-zone/k
     ports (or tz (local-zone-name ports))
     (lambda (zone)
       (multiple-value-bind (offset daylight-p abbreviation) (aitools.env.domain:zone-offset-at zone now)
         (declare (ignore daylight-p))
         (funcall on-ok
                  (list (cons "iso8601" (%zone-iso8601 zone now))
                        (cons "utc" (aitools.env.domain:format-iso8601 now 0 :utc-designator t))
                        (cons "epoch_ms" now)
                        (cons "timezone" (aitools.env.domain:zone-name zone))
                        (cons "utc_offset" (aitools.env.domain:format-utc-offset offset))
                        (cons "abbreviation" abbreviation)))))
     on-error)))

(defun %syntax-error (on-error condition example)
  (funcall on-error "input.syntax-error" (princ-to-string condition)
           :repairs (list (repair "fix-value"
                                   "Pass ISO 8601 (2026-03-08T01:30:00, optionally with Z or +09:00) or Unix epoch seconds/milliseconds."
                                   example))))

(defun %parse-durations/k (texts on-ok on-error)
  "Call ON-OK with the summed milliseconds of TEXTS, or ON-ERROR with
argument.invalid for the first one that is not a duration."
  (let ((total 0))
    (dolist (text texts (funcall on-ok total))
      (handler-case
          (incf total (aitools.kernel.domain:duration-milliseconds (aitools.kernel.domain:parse-duration text)))
        (aitools.kernel.domain:invalid-duration-error ()
          (return (funcall on-error "argument.invalid"
                           (format nil "not a duration: ~S (use <number>ms|s|m|h|d; negative values go in --sub)" text)
                           :repairs (list (repair "fix-duration" "Durations are <number>ms|s|m|h|d."
                                                   "aitools time convert now --add 1h")))))))))

(defun %parse-instant/k (ports text zone-name on-instant on-error example)
  "Resolve TEXT to epoch milliseconds: `now` reads the clock, an offset-less
ISO time is read in ZONE-NAME. Calls ON-INSTANT with (INSTANT TIME-INPUT-OR-NIL)."
  (if (string-equal text "now")
      (funcall on-instant (%now ports) nil)
      (let ((input (handler-case (aitools.env.domain:parse-time-input text)
                     (aitools.env.domain:time-syntax-error (condition)
                       (return-from %parse-instant/k (%syntax-error on-error condition example))))))
        (if (eq (aitools.env.domain:time-input-kind input) :instant)
            (funcall on-instant (aitools.env.domain:time-input-milliseconds input) input)
            (%with-zone/k ports zone-name
                          (lambda (zone)
                            (funcall on-instant
                                     (aitools.env.domain:zone-local-to-epoch-milliseconds
                                      zone (aitools.env.domain:time-input-milliseconds input))
                                     input))
                          on-error)))))

(defun time-convert/k (ports value &key (to "iso8601") add sub tz on-ok on-error)
  "ADD and SUB are lists of duration strings. The result zone (for iso8601
output and for reading an offset-less input) is TZ, else the local zone."
  (let ((zone-name (or tz (local-zone-name ports)))
        (example (format nil "aitools time convert 2026-03-08T01:30:00Z --to ~A" to)))
    (unless (member to *time-convert-targets* :test #'string=)
      (return-from time-convert/k
        (funcall on-error "argument.invalid" (format nil "--to must be one of ~{~A~^, ~}" *time-convert-targets*)
                 :repairs (list (repair "fix-target" "Pick an output format." example)))))
    (%parse-durations/k
     add
     (lambda (added)
       (%parse-durations/k
        sub
        (lambda (subtracted)
          (%parse-instant/k
           ports value zone-name
           (lambda (instant input)
             (let ((result-instant (+ instant added (- subtracted))))
               (if (not (aitools.env.domain:epoch-milliseconds-in-range-p result-instant))
                   (funcall on-error "input.syntax-error"
                            (format nil "~A is outside years 0000-9999 after --add/--sub" value)
                            :repairs (list (repair "fix-value" "Keep the result within years 0000-9999." example)))
                   (flet ((finish (zone)
                            (funcall on-ok
                                     (list (cons "input" value)
                                           (cons "input_format" (if input (aitools.env.domain:time-input-format input) "now"))
                                           (cons "to" to)
                                           (cons "result"
                                                 (cond ((string= to "epoch_ms") result-instant)
                                                       ((string= to "epoch_s") (floor result-instant 1000))
                                                       (t (%zone-iso8601 zone result-instant))))
                                           (cons "timezone" (aitools.env.domain:zone-name zone))))))
                     (%with-zone/k ports zone-name #'finish on-error)))))
           on-error example))
        on-error))
     on-error)))

(defun time-diff/k (ports a b &key on-ok on-error)
  "diff_ms is B minus A; offset-less inputs are read in the local zone."
  (let ((zone-name (local-zone-name ports))
        (example "aitools time diff 2026-03-08T00:00:00Z 2026-03-08T01:23:00Z"))
    (%parse-instant/k
     ports a zone-name
     (lambda (first-instant first-input)
       (declare (ignore first-input))
       (%parse-instant/k
        ports b zone-name
        (lambda (second-instant second-input)
          (declare (ignore second-input))
          (let ((difference (- second-instant first-instant)))
            (funcall on-ok (list (cons "a" a) (cons "b" b)
                                 (cons "diff_ms" difference)
                                 (cons "human" (aitools.env.domain:format-human-duration difference))))))
        on-error example))
     on-error example)))

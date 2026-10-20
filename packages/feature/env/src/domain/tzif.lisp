;;;; Adapts TZif octets (RFC 9636), the format used by the compiled IANA time
;;;; zone database, into domain zone values. Callers own file access.
(in-package #:aitools.env.domain)

(define-condition tzif-format-error (error)
  ((reason :initarg :reason :reader tzif-format-error-reason))
  (:report (lambda (condition stream)
             (format stream "invalid TZif data: ~A"
                     (tzif-format-error-reason condition)))))

;; Keep NAME for output when the TZif data has no abbreviation.
(defstruct (zone (:constructor %make-zone (name kit-zone))
                      (:copier nil))
  (name "" :type string :read-only t)
  (kit-zone nil
            :type (or cl-date-kit:time-zone cl-date-kit:zone-offset)
            :read-only t))

(defun make-fixed-zone (name offset-seconds)
  "Creates a fixed zone using an integer offset in the inclusive range -64800 to 64800 seconds."
  (%make-zone name
              (cl-date-kit:zone-offset-of-total-seconds offset-seconds)))

(defun parse-tzif (name octets)
  "Converts TZif OCTETS for NAME into a zone.
   Translates MALFORMED-TZIF to TZIF-FORMAT-ERROR; other conditions propagate."
  (handler-case
      (%make-zone name (cl-date-kit:time-zone-from-tzif-octets name octets))
    (cl-date-kit:malformed-tzif (condition)
      (error 'tzif-format-error
             :reason (cl-date-kit:malformed-tzif-reason condition)))))

(defun zone-offset-at (zone epoch-milliseconds)
  "Returns offset seconds, daylight-saving status, and abbreviation as three values.
   A missing abbreviation falls back to the zone name."
  (let* ((state
           (cl-date-kit:zone-state-for-instant
            (zone-kit-zone zone)
            (cl-date-kit:instant-of-epoch-millis epoch-milliseconds)))
         (offset
           (cl-date-kit:zone-offset-total-seconds
            (cl-date-kit:zone-state-offset state))))
    (values offset
            (cl-date-kit:zone-state-daylight-saving-p state)
            (or (cl-date-kit:zone-state-abbreviation state)
                (zone-name zone)))))

(defun zone-local-to-epoch-milliseconds (zone local-milliseconds)
  "LOCAL-MILLISECONDS encodes wall-clock fields as UTC epoch milliseconds.
   Nonexistent times move forward by the gap width; repeated times use their
   earlier instant."
  (let* ((local-date-time
           (cl-date-kit:local-date-time-of-instant
            (cl-date-kit:instant-of-epoch-millis local-milliseconds)
            (cl-date-kit:zone-offset-utc)))
         (resolved
           (cl-date-kit:zoned-date-time-of-local
            local-date-time (zone-kit-zone zone))))
    (cl-date-kit:instant-to-epoch-millis
     (cl-date-kit:zoned-date-time-to-instant resolved))))

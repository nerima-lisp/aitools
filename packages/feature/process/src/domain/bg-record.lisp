;;;; packages/feature/process/src/domain/bg-record.lisp
;;;;
;;;; A `bg start` process is remembered as three files in the workspace's
;;;; state `bg/` directory: `<id>.json` (this record), `<id>.log` (its
;;;; stdout and stderr), and `<id>.exit` (its exit status, written by the
;;;; supervisor when it ends). The record is persisted, user-editable input
;;;; on every later read, so parsing rejects anything it did not write.
(in-package #:aitools.process.domain)

(defstruct (bg-record (:copier nil))
  (id nil :type string :read-only t)
  (name nil :type (or null string) :read-only t)
  (argv nil :type list :read-only t)
  (pid nil :type (integer 1) :read-only t)
  (started nil :type string :read-only t)
  ;; The signal `bg stop` last had to send, recorded because a supervisor
  ;; killed by SIGKILL cannot write its own exit file.
  (stop-signal nil :type (or null (integer 1)) :read-only t))

(defun copy-bg-record-with-stop-signal (record signal)
  (make-bg-record :id (bg-record-id record) :name (bg-record-name record)
                  :argv (bg-record-argv record) :pid (bg-record-pid record)
                  :started (bg-record-started record) :stop-signal signal))

(define-condition invalid-bg-record (error)
  ((message :initarg :message :reader invalid-bg-record-message))
  (:report (lambda (condition stream)
             (write-string (invalid-bg-record-message condition) stream))))

;;; ------------------------------------------------------------------ ids

(defparameter +bg-id-prefix+ "bg-")

(declaim (inline %ascii-digit-p))
(defun %ascii-digit-p (char)
  (char<= #\0 char #\9))

(defun bg-id-p (text)
  "True for `bg-<n>` with N a decimal integer without leading zeros. Every
path aitools builds from a user-supplied ID passes this first, so an ID can
never name a file outside `bg/`."
  (let ((prefix-length (length +bg-id-prefix+)))
    (and (stringp text)
         (< prefix-length (length text) (+ prefix-length 10))
         (string= +bg-id-prefix+ text :end2 prefix-length)
         (every #'%ascii-digit-p (subseq text prefix-length))
         (char/= (char text prefix-length) #\0))))

(defun bg-name-valid-p (name)
  "A `--name` label: 1 to 64 characters, none of them a control character."
  (and (stringp name)
       (<= 1 (length name) 64)
       (notany (lambda (char) (or (< (char-code char) 32) (= (char-code char) 127))) name)))

(defun bg-id-number (id)
  (parse-integer id :start (length +bg-id-prefix+)))

(defun next-bg-id (existing-ids)
  "The ID after the highest of EXISTING-IDS (non-IDs are ignored)."
  (format nil "~A~D" +bg-id-prefix+
          (1+ (reduce #'max (mapcar #'bg-id-number (remove-if-not #'bg-id-p existing-ids))
                      :initial-value 0))))

(defun bg-record-file-name (id) (concatenate 'string id ".json"))
(defun bg-log-file-name (id) (concatenate 'string id ".log"))
(defun bg-exit-file-name (id) (concatenate 'string id ".exit"))

(defun %id-before-suffix (file-name suffix)
  (when (and (> (length file-name) (length suffix))
             (string= suffix file-name :start2 (- (length file-name) (length suffix))))
    (let ((id (subseq file-name 0 (- (length file-name) (length suffix)))))
      (and (bg-id-p id) id))))

(defun bg-record-id-from-file-name (file-name)
  "The ID a `<id>.json` FILE-NAME records, or NIL for any other file."
  (%id-before-suffix file-name ".json"))

(defun bg-file-id (file-name)
  "The ID any of a bg process's three files belongs to, or NIL. An ID is
taken once any of them exists, even before its record is written."
  (some (lambda (suffix) (%id-before-suffix file-name suffix)) '(".json" ".log" ".exit")))

;;; ------------------------------------------------------ serialization

(defparameter +bg-record-keys+ '("id" "name" "argv" "pid" "started" "stop_signal"))

(defun serialize-bg-record (record)
  (json-kit:stringify
   (json-object "id" (bg-record-id record)
                "name" (json-or-null (bg-record-name record))
                "argv" (bg-record-argv record)
                "pid" (bg-record-pid record)
                "started" (bg-record-started record)
                "stop_signal" (json-or-null (bg-record-stop-signal record)))))

(defun %invalid-record (format-control &rest arguments)
  (error 'invalid-bg-record :message (apply #'format nil format-control arguments)))

(defun parse-bg-record (text expected-id)
  "Parse TEXT as the record for EXPECTED-ID. Signals INVALID-BG-RECORD on
malformed JSON, a missing or unknown key, a wrongly typed value, or an `id`
that differs from EXPECTED-ID (the file it was read from)."
  (let ((table (handler-case (json-kit:parse text :max-input-length 1048576)
                 (json-kit:json-kit-error (condition)
                   (%invalid-record "bg record ~A is not valid JSON: ~A" expected-id condition)))))
    (unless (hash-table-p table)
      (%invalid-record "bg record ~A is not a JSON object" expected-id))
    (let ((keys (loop for key being the hash-keys of table collect key)))
      (unless (and (= (length keys) (length +bg-record-keys+))
                   (every (lambda (key) (member key +bg-record-keys+ :test #'string=)) keys))
        (%invalid-record "bg record ~A must have exactly the keys ~{~A~^, ~}" expected-id +bg-record-keys+)))
    (flet ((field (key) (gethash key table)))
      (let ((id (field "id")) (name (field "name")) (argv (field "argv"))
            (pid (field "pid")) (started (field "started")) (stop-signal (field "stop_signal")))
        (unless (and (stringp id) (string= id expected-id))
          (%invalid-record "bg record ~A has a mismatched id" expected-id))
        (unless (or (json-kit:json-null-p name) (stringp name))
          (%invalid-record "bg record ~A has a non-string name" expected-id))
        (unless (and (vectorp argv) (plusp (length argv)) (every #'stringp argv))
          (%invalid-record "bg record ~A has an invalid argv" expected-id))
        (unless (typep pid '(integer 2 2147483647))
          (%invalid-record "bg record ~A has an invalid pid" expected-id))
        (unless (stringp started)
          (%invalid-record "bg record ~A has an invalid start time" expected-id))
        (unless (or (json-kit:json-null-p stop-signal) (typep stop-signal '(integer 1 64)))
          (%invalid-record "bg record ~A has an invalid stop_signal" expected-id))
        (make-bg-record :id id
                        :name (if (json-kit:json-null-p name) nil name)
                        :argv (coerce argv 'list)
                        :pid pid
                        :started started
                        :stop-signal (if (json-kit:json-null-p stop-signal) nil stop-signal))))))

(defun parse-exit-status (text)
  "(VALUES EXIT-CODE SIGNAL) from the supervisor's exit file TEXT, a shell
`$?`. POSIX shells report death by signal N as 128+N, and that encoding is
all the supervisor can observe, so a status above 128 is read as a signal. NIL
for both when TEXT is empty or malformed (the file may still be being
written)."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (status (and (<= 1 (length trimmed) 3) (every #'%ascii-digit-p trimmed)
                      (parse-integer trimmed))))
    (cond ((or (null status) (> status 255)) (values nil nil))
          ((> status 128) (values nil (- status 128)))
          (t (values status nil)))))

(defun format-utc-timestamp (universal-time)
  "UNIVERSAL-TIME as RFC 3339 UTC, second precision (`2026-09-26T01:02:03Z`)."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year month day hour minute second)))

(defun bg-status-item (record running exit-code signal)
  "One `bg status` `items[]` entry."
  (json-object "id" (bg-record-id record)
               "name" (json-or-null (bg-record-name record))
               "pid" (bg-record-pid record)
               "argv" (bg-record-argv record)
               "running" (json-boolean running)
               "exit_code" (json-or-null exit-code)
               "signal" (json-or-null signal)
               "started" (bg-record-started record)))

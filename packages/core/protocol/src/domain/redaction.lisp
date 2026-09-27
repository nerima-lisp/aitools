;;;; packages/core/protocol/src/domain/redaction.lisp
;;;;
;;;; Redaction: mask only known secret formats, never by entropy (the reasoning
;;;; for that choice: an entropy detector would keep refusing to
;;;; mask -- or worse, keep masking -- a git SHA or a UUID). This is why the
;;;; matchers below are all anchored on a literal prefix or a named key,
;;;; never on "looks random enough".
;;;;
;;;; Independent from nshell's redaction.lisp on purpose:
;;;; that implementation also masks "any 20+ character alphanumeric run",
;;;; which is exactly the false-positive risk this file avoids.
(in-package #:aitools.protocol.domain)

;; DEFPARAMETER, not DEFCONSTANT: a string literal is not EQL-comparable, and
;; DEFCONSTANT of a non-EQL value signals on every recompile that finds the
;; symbol already bound to an EQUAL-but-not-EQL string.
(defparameter +redacted-secret+ "[REDACTED_SECRET]")

(declaim (inline %token-char-p %boundary-before-p))

(defun %token-char-p (char)
  (or (alphanumericp char) (find char "_-./+=")))

(defun %boundary-before-p (text index)
  "True when INDEX is the string start or not preceded by an alphanumeric
character -- the guard that keeps a short prefix like `sk-` from matching
inside an ordinary word such as \"desk-lamp\" or \"task-1\"."
  (or (zerop index) (not (alphanumericp (char text (1- index))))))

(defun %scan-token-run-end (text start)
  (let ((end start))
    (loop while (and (< end (length text)) (%token-char-p (char text end)))
          do (incf end))
    end))

(defun %aws-key-body-char-p (char)
  (or (char<= #\A char #\Z) (char<= #\0 char #\9)))

(defun %word-boundary-match-p (text start length)
  (and (or (zerop start) (not (alphanumericp (char text (1- start)))))
       (let ((end (+ start length)))
         (or (= end (length text)) (not (alphanumericp (char text end)))))))

(defun %string-at-p (needle text start test)
  "True when TEXT[START, START+|NEEDLE|) lies inside TEXT and matches NEEDLE
character by character under TEST."
  (declare (type string needle) (type simple-string text) (type fixnum start)
           (type function test))
  (let ((len (length needle)) (n (length text)))
    (declare (type fixnum len n))
    (and (<= (+ start len) n)
         (loop for k fixnum below len
               always (funcall test (char needle k) (schar text (+ start k)))))))

(defconstant +literal-candidate+ 1)
(defconstant +aws-candidate+ 2)
(defconstant +bearer-candidate+ 4)
(defconstant +assignment-candidate+ 8)

(defun %redaction-candidate-table ()
  (let ((table (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (flet ((mark (character kind)
             (let ((code (char-code character)))
               (when (< code 128)
                 (setf (aref table code) (logior (aref table code) kind))))))
      (dolist (prefix (append aitools.data:*redaction-literal-prefixes*
                              aitools.data:*redaction-slack-prefixes*))
        (when (plusp (length prefix))
          (mark (char prefix 0) +literal-candidate+)))
      (dolist (prefix aitools.data:*redaction-aws-key-prefixes*)
        (when (plusp (length prefix))
          (mark (char prefix 0) +aws-candidate+)))
      (dolist (name aitools.data:*redaction-secret-key-names*)
        (when (plusp (length name))
          (mark (char-upcase (char name 0)) +assignment-candidate+)
          (mark (char-downcase (char name 0)) +assignment-candidate+)))
      (mark #\b +bearer-candidate+)
      (mark #\B +bearer-candidate+))
    table))

(defun %collect-token-spans (text spans)
  "Left-to-right, single pass over the simple-string TEXT. Push onto SPANS one
(START . END) per literal/Slack prefix, AWS access-key id, `Bearer` token, and
secret-key assignment found, producing exactly the spans the four separate
scanners did (their union under %MERGE-SPANS is order-independent). Return the
extended list."
  (declare (type simple-string text))
  (let ((n (length text))
        (table (%redaction-candidate-table))
        (literals (append aitools.data:*redaction-literal-prefixes*
                          aitools.data:*redaction-slack-prefixes*))
        (aws-prefixes aitools.data:*redaction-aws-key-prefixes*)
        (aws-body aitools.data:*redaction-aws-key-body-length*)
        (key-names aitools.data:*redaction-secret-key-names*))
    (declare (type fixnum n aws-body) (type (simple-array (unsigned-byte 8) (128)) table))
    (labels ((try-literal (j)
               (dolist (prefix literals)
                 (when (and (plusp (length prefix))
                            (%string-at-p prefix text j #'char=)
                            (%boundary-before-p text j))
                   (let* ((body (+ j (length prefix)))
                          (end (%scan-token-run-end text body)))
                     (when (> end body) (push (cons j end) spans))))))
             (try-aws (j)
               (dolist (prefix aws-prefixes)
                 (when (and (plusp (length prefix))
                            (%string-at-p prefix text j #'char=)
                            (%boundary-before-p text j))
                   (let* ((body-start (+ j (length prefix)))
                          (end (+ body-start aws-body)))
                     (declare (type fixnum body-start end))
                     (when (and (<= end n)
                                (loop for k fixnum from body-start below end
                                      always (%aws-key-body-char-p (schar text k)))
                                (or (= end n) (not (alphanumericp (schar text end)))))
                       (push (cons j end) spans))))))
             (try-bearer (j)
               (when (and (%string-at-p "Bearer " text j #'char-equal)
                          (%boundary-before-p text j))
                 (let* ((token-start (+ j 7))
                        (end (%scan-token-run-end text token-start)))
                   (when (> end token-start) (push (cons token-start end) spans)))))
             (try-assignment (j c)
               (dolist (key-name key-names)
                 (when (and (plusp (length key-name))
                            (char-equal (char key-name 0) c)
                            (%string-at-p key-name text j #'char-equal)
                            (%word-boundary-match-p text j (length key-name)))
                   (let ((index (+ j (length key-name))))
                     ;; Skip whitespace/quotes, require `=` or `:`, skip spaces/tabs.
                     (loop while (and (< index n) (member (schar text index) '(#\Space #\Tab #\" #\')))
                           do (incf index))
                     (when (and (< index n) (find (schar text index) "=:"))
                       (incf index)
                       (loop while (and (< index n) (member (schar text index) '(#\Space #\Tab)))
                             do (incf index))
                       (multiple-value-bind (value-start value-end) (%assignment-value-bounds text index)
                         (when (and (> value-end value-start)
                                    (not (%marker-at-p text value-start value-end)))
                           (push (cons value-start value-end) spans)))))))))
      (loop with i fixnum = 0
            while (< i n)
            do (let* ((c (schar text i))
                      (code (char-code c)))
                 (declare (type fixnum code))
                 (when (< code 128)
                   (let ((kind (aref table code)))
                     (when (logtest +literal-candidate+ kind) (try-literal i))
                     (when (logtest +aws-candidate+ kind) (try-aws i))
                     (when (logtest +bearer-candidate+ kind) (try-bearer i))
                     (when (logtest +assignment-candidate+ kind) (try-assignment i c))))
                 (incf i))))
    spans))

(defun %assignment-value-bounds (text index)
  "The value starting at INDEX: inside the quotes when it is quoted (a
passphrase may contain spaces), else up to whitespace or the end of line."
  (let ((quote-char (and (< index (length text)) (find (char text index) "\"'"))))
    (if quote-char
        (let ((start (1+ index)))
          (values start (or (position-if (lambda (char) (or (char= char quote-char) (char= char #\Newline)))
                                         text :start start)
                            (length text))))
        (values index (or (position-if (lambda (char) (member char '(#\Space #\Tab #\Return #\Newline)))
                                       text :start index)
                          (length text))))))

(defun %marker-at-p (text start end)
  "True when TEXT[START,END) is exactly the marker: a value masked by an
earlier pass must not be counted a second time."
  (and (= (- end start) (length +redacted-secret+))
       (string= +redacted-secret+ text :start2 start :end2 end)))

(defun %find-pem-spans (text)
  "Spans of whole PEM blocks in TEXT whose BEGIN label names a private key.
Redaction masks known secret formats only: a certificate or public key passes
through, since masking it would make a read-then-write round trip refuse the
marker."
  (let ((length (length text)) (search-start 0) spans)
    (loop
      (let ((begin (%find-pem-private-key-marker "BEGIN" text search-start length)))
        (unless begin (return spans))
        (let ((end-marker (search "-----END" text :start2 begin)))
          (if (null end-marker)
              (setf search-start (+ begin 10))
              (let* ((dash-after (search "-----" text :start2 (+ end-marker 8)))
                     (line-end (or (position #\Newline text :start (or dash-after end-marker))
                                   length)))
                (push (cons begin line-end) spans)
                (setf search-start line-end))))))))

(defun %merge-spans (spans)
  "Sort SPANS by start and merge any that touch or overlap, so a value that
happens to satisfy two matchers (e.g. a `sk-...` token assigned to
`api_key`) counts and masks as one redaction."
  (let ((sorted (sort (copy-list spans) #'< :key #'car)) merged)
    (dolist (span sorted (nreverse merged))
      (let ((top (first merged)))
        (if (and top (<= (car span) (cdr top)))
            (setf (cdr top) (max (cdr top) (cdr span)))
            (push (cons (car span) (cdr span)) merged))))))

(defun %replace-spans (text spans)
  (with-output-to-string (out)
    (let ((cursor 0))
      (dolist (span spans)
        (write-string text out :start cursor :end (car span))
        (write-string +redacted-secret+ out)
        (setf cursor (cdr span)))
      (write-string text out :start cursor))))

(defun %redact-patterns (text)
  ;; One SIMPLE-STRING so the single scan and every matcher use SCHAR; the
  ;; generic PEM spans seed the list the token pass extends, and %MERGE-SPANS
  ;; makes the whole set order-independent.
  (let* ((simple (coerce text 'simple-string))
         (spans (%merge-spans (%collect-token-spans simple (%find-pem-spans simple)))))
    (if (null spans)
        (values text 0)
        (values (%replace-spans simple spans) (length spans)))))

;;; PEM private keys. A command that returns a file one line per string
;;; (`read`, search blocks, `run` output, diff hunks) splits a key block
;;; across strings, so these are found by one pass over every line of every
;;; string of one sequence rather than string by string. A caller passes one
;;; sequence per text (one JSON array of lines, or one string): block state
;;; must not run on into an unrelated field or another file's lines.

(defun %find-pem-private-key-marker (kind text start end)
  "(VALUES MARKER-START MARKER-END) of the first `-----KIND <label>PRIVATE
KEY-----` (or OpenPGP's `-----KIND PGP PRIVATE KEY BLOCK-----`) in
TEXT[START,END), KIND being \"BEGIN\" or \"END\", or NIL."
  (let ((opener (concatenate 'string "-----" kind " "))
        (from start))
    (loop
      (let ((found (search opener text :start2 from :end2 end)))
        (unless found (return nil))
        (let* ((label-start (+ found (length opener)))
               (close (search "-----" text :start2 label-start :end2 end)))
          (flet ((label-ends-with-p (suffix)
                   (and (>= (- close label-start) (length suffix))
                        (string= suffix text :start2 (- close (length suffix)) :end2 close))))
            (when (and close (or (label-ends-with-p "PRIVATE KEY") (label-ends-with-p "PRIVATE KEY BLOCK")))
              (return (values found (+ close 5)))))
          (setf from (1+ found)))))))

(defun %pem-body-line-p (text start end)
  "True for a nonblank line of base64 characters."
  (flet ((blank-p (char) (member char '(#\Space #\Tab #\Return))))
    (let ((first (position-if-not #'blank-p text :start start :end end))
          (last (position-if-not #'blank-p text :start start :end end :from-end t)))
      (and first
           (loop for index from first to last
                 always (let ((char (char text index)))
                          (or (char<= #\A char #\Z) (char<= #\a char #\z) (char<= #\0 char #\9)
                              (find char "+/="))))))))

(defun %pem-header-line-p (text start end)
  "True for an RFC 1421 header such as `Proc-Type: 4,ENCRYPTED`."
  (let ((colon (position #\: text :start start :end end)))
    (and colon (> colon start)
         (loop for index from start below colon
               always (let ((char (char text index))) (or (alphanumericp char) (char= char #\-)))))))

(defun %pem-private-key-spans (texts)
  "(VALUES SPANS BLOCKS). SPANS holds, for each string of the vector TEXTS,
the (START . END) line spans that belong to a PEM private key. A BEGIN line
opens a block that runs to its END line, or to the end of TEXTS when the END
line is not in the output; inside it, base64 and header lines are masked and
other lines (a path between two search blocks) are kept. An END line with no
BEGIN before it takes the base64 lines immediately preceding it. BLOCKS
counts each block once however many strings it spans."
  (let ((spans (make-array (length texts) :initial-element '()))
        (blocks 0)
        (open nil)
        (pending '()))
    (labels ((mask (index start end)
             (push (cons start end) (aref spans index)))
           (visit-line (index text start end)
             (let ((dashes (search "-----" text :start2 start :end2 end)))
               (multiple-value-bind (begin-start begin-end)
                   (and dashes (%find-pem-private-key-marker "BEGIN" text dashes end))
                 (cond
                   (begin-start
                    (incf blocks)
                    (setf pending '()
                          open (not (%find-pem-private-key-marker "END" text begin-end end)))
                    (mask index begin-start end))
                   (open
                    (cond ((and dashes (%find-pem-private-key-marker "END" text dashes end))
                           (mask index start end)
                           (setf open nil))
                          ((or (%pem-body-line-p text start end) (%pem-header-line-p text start end))
                           (mask index start end))))
                   ((and dashes (%find-pem-private-key-marker "END" text dashes end))
                    (incf blocks)
                    (dolist (line pending) (apply #'mask line))
                    (setf pending '())
                    (mask index start end))
                   ((%pem-body-line-p text start end) (push (list index start end) pending))
                   (t (setf pending '())))))))
      (loop for text across texts
            for index from 0
            do (loop with start = 0
                     for newline = (position #\Newline text :start start)
                     do (visit-line index text start (or newline (length text)))
                     while newline
                     do (setf start (1+ newline)))))
    (values spans blocks)))

(defun %merge-pem-spans (text spans)
  "SPANS (line spans of TEXT, any order) sorted, with lines separated only by
line breaks joined, so a block inside one string becomes one marker."
  (let ((merged '()))
    (dolist (span (sort (copy-list spans) #'< :key #'car) (nreverse merged))
      (let ((top (first merged)))
        (if (and top (loop for index from (cdr top) below (car span)
                           always (member (char text index) '(#\Newline #\Return))))
            (setf (cdr top) (cdr span))
            (push (cons (car span) (cdr span)) merged))))))

(defun redact-secret-sequence (texts)
  "Return (VALUES REDACTED-TEXTS COUNT) for TEXTS, a list of the strings of
one text in order (e.g. the lines of one file). Each string is masked as by REDACT-SECRETS,
and a PEM private key split across several strings is masked in all of them
and counted once."
  (let ((vector (coerce texts 'simple-vector)))
    (multiple-value-bind (pem-spans total) (%pem-private-key-spans vector)
      (values (loop for text across vector
                    for spans across pem-spans
                    collect (multiple-value-bind (redacted count)
                                (%redact-patterns (if spans
                                                      (%replace-spans text (%merge-pem-spans text spans))
                                                      text))
                              (incf total count)
                              redacted))
              total))))

(defun redact-secrets (text)
  "Return (VALUES REDACTED-TEXT COUNT). COUNT is the number of masked regions,
each replaced by the literal string \"[REDACTED_SECRET]\"."
  (multiple-value-bind (texts count) (redact-secret-sequence (list text))
    (values (first texts) count)))

(defun secret-key-name-p (name)
  "True when NAME contains one of the redacted secret key names as a whole word,
case-insensitively; `_` and other non-alphanumerics separate words, so
GITHUB_TOKEN and DB_PASSWORD match and TOKENIZER_PATH does not."
  (loop for key in aitools.data:*redaction-secret-key-names*
        thereis (and (plusp (length key))
                     (loop with start = 0
                           for found = (search key name :start2 start :test #'char-equal)
                           while found
                           thereis (%word-boundary-match-p name found (length key))
                           do (setf start (1+ found))))))

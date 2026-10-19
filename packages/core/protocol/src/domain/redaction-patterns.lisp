;;;; Redaction matches known secret formats, not strings selected by entropy.
;;;; Entropy matching can mask a git SHA or UUID. These matchers use literal
;;;; prefixes or named keys.
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
  (let ((table (make-hash-table :test #'eql)))
    (flet ((mark (character kind)
             (setf (gethash character table)
                   (logior (gethash character table 0) kind))))
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
  "Collect spans for literal/Slack prefixes, AWS access-key IDs, Bearer tokens,
and values assigned to configured secret keys. TEXT must be a simple string;
return the extended SPANS list."
  (declare (type simple-string text))
  (let ((n (length text))
        (table (%redaction-candidate-table))
        (literals (append aitools.data:*redaction-literal-prefixes*
                          aitools.data:*redaction-slack-prefixes*))
        (aws-prefixes aitools.data:*redaction-aws-key-prefixes*)
        (aws-body aitools.data:*redaction-aws-key-body-length*)
        (key-names aitools.data:*redaction-secret-key-names*))
    (declare (type fixnum n aws-body) (type hash-table table))
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
                      (kind (gethash c table 0)))
                 (when (logtest +literal-candidate+ kind) (try-literal i))
                 (when (logtest +aws-candidate+ kind) (try-aws i))
                 (when (logtest +bearer-candidate+ kind) (try-bearer i))
                 (when (logtest +assignment-candidate+ kind) (try-assignment i c))
                 (incf i))))
    spans))

(defun %assignment-value-bounds (text index)
  "Return the value bounds at INDEX. Quoted values end at the first matching
quote or newline; backslash escapes are not interpreted. Unquoted values end
at whitespace or the end of line."
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
  "True when TEXT[START,END) is the redaction marker from an earlier call."
  (and (= (- end start) (length +redacted-secret+))
       (string= +redacted-secret+ text :start2 start :end2 end)))

(defun %find-pem-spans (text)
  "Return spans from private-key BEGIN markers through the line containing the
next `-----END` marker. Skip a BEGIN marker with no later END marker."
  (let ((length (length text)) (search-start 0) spans)
    (loop
      (let ((begin (%find-pem-private-key-marker "BEGIN" text search-start length)))
        (unless begin (return spans))
        (let ((end-marker (search "-----END" text :start2 begin)))
          (if (null end-marker)
              (setf search-start (+ begin (length "-----BEGIN")))
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
  ;; Coerce once so the token scan can use SCHAR.
  (let* ((simple (coerce text 'simple-string))
         (spans (%merge-spans (%collect-token-spans simple (%find-pem-spans simple)))))
    (if (null spans)
        (values text 0)
        (values (%replace-spans simple spans) (length spans)))))

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

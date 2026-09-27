;;;; packages/feature/edit/src/domain/regex.lisp
;;;;
;;;; cl-regex-kit as the edit commands use it: compiling a user pattern
;;;; (--fixed/--word/--ignore-case), turning the kit's errors into
;;;; input.syntax-error refusals, and `replace`'s matching and expansion
;;;; over a TEXT-DOCUMENT: per line, or across lines with
;;;; --multiline, limited to selected lines, all matches or only the Nth.
(in-package #:aitools.edit.domain)

(defun compile-search-pattern/k (pattern &key fixed word ignore-case on-regex on-invalid)
  "Compile PATTERN and call ON-REGEX (regex) or ON-INVALID (message)."
  (declare (type function on-regex on-invalid))
  (let ((source (if fixed (cl-regex-kit:escape pattern) pattern)))
    (when word (setf source (format nil "\\b(?:~A)\\b" source)))
    (let ((regex (handler-case (cl-regex-kit:compile-regex source :case-insensitive ignore-case)
                   (cl-regex-kit:cl-regex-kit-error (condition)
                     (return-from compile-search-pattern/k
                       (funcall on-invalid (format nil "bad regular expression ~S: ~A" pattern condition)))))))
      (funcall on-regex regex))))

(defun call-with-regex-refusals (thunk)
  "Call THUNK, turning a cl-regex-kit error raised while matching (the
advanced executor's step and nesting limits, timeouts) into an
input.syntax-error EDIT-REFUSAL."
  (handler-case (funcall thunk)
    (cl-regex-kit:cl-regex-kit-error (condition)
      (refuse "input.syntax-error" "regular expression failed: ~A" condition))))

(defun regex-group-count (regex)
  (cl-regex-kit:regex-capture-count regex))

(defstruct (replacer (:constructor make-replacer (regex expand &key nth)) (:copier nil))
  "REGEX's matches are replaced by (EXPAND match text), or only the NTH
match counted from the last RESET-REPLACER."
  (regex nil :read-only t)
  (expand nil :type function :read-only t)
  (nth nil :read-only t)
  (seen 0 :type (integer 0)))

(defun reset-replacer (replacer)
  (setf (replacer-seen replacer) 0)
  replacer)

(defun octets-search (needle haystack)
  "The index of the first occurrence of the byte vector NEEDLE in HAYSTACK,
or NIL: a first-byte scan with POSITION, then a full compare, so a file
lacking NEEDLE's lead byte is swept in one pass. Kept here rather than
reaching into the search context, which edit/application may not depend on."
  (declare (type (simple-array (unsigned-byte 8) (*)) needle haystack)
           (optimize (speed 3) (safety 1)))
  (let* ((n (length needle))
         (limit (- (length haystack) n)))
    (declare (type fixnum n limit))
    (cond
      ((zerop n) 0)
      ((minusp limit) nil)
      (t (let ((first (aref needle 0)))
           (loop for i = (position first haystack :end (1+ limit))
                   then (position first haystack :start (1+ (the fixnum i)) :end (1+ limit))
                 while i
                 do (when (loop for j of-type fixnum from 1 below n
                                always (= (aref haystack (+ (the fixnum i) j)) (aref needle j)))
                      (return i))))))))

(defun replacer-required-literal (replacer)
  "The UTF-8 bytes of a literal that every match of REPLACER's pattern must
contain, for a byte-level prefilter over a file's raw bytes (so non-matching files skip decoding), or
NIL when the pattern yields no usable required literal (so the file must be
decoded and matched). A literal holding a line end is rejected: a CR LF on
disk decodes to one LF in the document, so its bytes need not appear
verbatim in the file, and a byte search for them could wrongly skip a
matching file."
  (let ((literals (handler-case
                      (cl-regex-kit:regex-required-literals (replacer-regex replacer))
                    (error () nil))))
    (when literals
      (let ((octets (coerce (aitools.text.domain:encode-utf8 (first literals))
                            '(simple-array (unsigned-byte 8) (*)))))
        (unless (or (find 10 octets) (find 13 octets))
          octets)))))

(defun literal-replacement (text)
  "A replacement inserting TEXT verbatim."
  (lambda (match subject) (declare (ignore match subject)) text))

(defun %replace-span (replacer text start end)
  "(values new-text count first last): REPLACER's matches in TEXT[START,END)
replaced. FIRST/LAST bound the replaced region in TEXT's coordinates (the
matches are found before any replacement), NIL when nothing changed."
  (let ((pieces '()) (position start) (count 0) (first nil) (last nil))
    (dolist (match (cl-regex-kit:all-matches (replacer-regex replacer) text :start start :end end))
      (incf (replacer-seen replacer))
      (when (or (null (replacer-nth replacer)) (= (replacer-seen replacer) (replacer-nth replacer)))
        (push (subseq text position (cl-regex-kit:match-start match)) pieces)
        (push (funcall (replacer-expand replacer) match text) pieces)
        (setf position (cl-regex-kit:match-end match))
        (incf count)
        (setf first (or first (cl-regex-kit:match-start match)) last (cl-regex-kit:match-end match))))
    (if (zerop count)
        (values text 0 nil nil)
        (values (concatenate 'string (subseq text 0 start) (apply #'concatenate 'string (nreverse pieces))
                             (subseq text position))
                count first last))))

(defun replace-document (document replacer ranges multiline)
  "(values document count): REPLACER applied to DOCUMENT. RANGES (1-based
inclusive line pairs, ascending) limits the lines, NIL meaning all. Without
MULTILINE each line is matched alone and a replacement may not introduce a
line break."
  (let ((ranges (or ranges (and (plusp (document-line-count document))
                                (list (cons 1 (document-line-count document))))))
        (total 0))
    (if multiline
        (let* ((text (document-logical-text document))
               (offsets (document-line-offsets document))
               (shift 0) (first-line nil) (last-line nil))
          (dolist (range ranges)
            (let ((start (svref offsets (1- (car range))))
                  (end (svref offsets (cdr range))))
              (multiple-value-bind (new count first last) (%replace-span replacer text (+ start shift) (+ end shift))
                (when (plusp count)
                  (incf total count)
                  (let ((first (- first shift)) (last (- last shift)))
                    (setf first-line (min (or first-line most-positive-fixnum) (offset-line-index offsets first))
                          last-line (max (or last-line -1) (offset-line-index offsets (max first (1- last))))))
                  (incf shift (- (length new) (length text)))
                  (setf text new)))))
          (values (if (plusp total)
                      (document-with-logical-text document text first-line
                                                  (- (document-line-count document) (1+ last-line)))
                      document)
                  total))
        (let ((lines (copy-seq (text-document-lines document))))
          (dolist (range ranges)
            (loop for index from (1- (car range)) below (cdr range)
                  do (let ((line (svref lines index)))
                       (multiple-value-bind (new count) (%replace-span replacer line 0 (length line))
                         (when (plusp count)
                           (when (find #\Newline new)
                             (refuse "argument.invalid" "a replacement containing a line break needs --multiline"))
                           (setf (svref lines index) new)
                           (incf total count))))))
          (values (if (plusp total) (document-with-lines document lines) document)
                  total)))))

;;; `touch --mtime` times

(defparameter +iso-time-pattern+
  "^([0-9]{4})-([0-9]{2})-([0-9]{2})(?:[T ]([0-9]{2}):([0-9]{2})(?::([0-9]{2})(?:\\.[0-9]+)?)?)?(Z|[+-][0-9]{2}:?[0-9]{2})?$")

(defun %unix-epoch () (encode-universal-time 0 0 0 1 1 1970 0))

(defun parse-mtime (text)
  "Unix seconds for TEXT: digits, @digits, or ISO 8601
YYYY-MM-DD[THH:MM[:SS[.fraction]]][Z|+HH:MM|-HH:MM] (UTC without a zone);
NIL when malformed."
  (flet ((digits-p (string) (and (plusp (length string)) (every #'%ascii-digit-p string))))
    (cond
      ((digits-p text) (parse-integer text))
      ((and (> (length text) 1) (char= (char text 0) #\@) (digits-p (subseq text 1))) (parse-integer text :start 1))
      (t
       (let ((match (cl-regex-kit:scan (cl-regex-kit:compile-regex +iso-time-pattern+) text)))
         (when match
           (flet ((group (n) (let ((value (cl-regex-kit:match-group-string match n text)))
                               (if value (parse-integer value) 0))))
             (let ((zone (cl-regex-kit:match-group-string match 7 text))
                   (fields (list (group 6) (group 5) (group 4) (group 3) (group 2) (group 1))))
               (handler-case
                   (let ((universal (apply #'encode-universal-time (append fields '(0)))))
                     ;; ENCODE-UNIVERSAL-TIME rolls an impossible day over
                     ;; (February 30 becomes March 1); only a date that decodes
                     ;; back to the fields given is a date.
                     (when (equal (subseq (multiple-value-list (decode-universal-time universal 0)) 0 6) fields)
                       (- universal
                          (%unix-epoch)
                          (if (or (null zone) (string= zone "Z"))
                              0
                              (let ((digits (remove #\: (subseq zone 1))))
                                (* (if (char= (char zone 0) #\-) -1 1)
                                   (+ (* 3600 (parse-integer digits :end 2)) (* 60 (parse-integer digits :start 2)))))))))
                 (error () nil))))))))))

(defun iso-utc (unix-seconds)
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (+ unix-seconds (%unix-epoch)) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year month day hour minute second)))

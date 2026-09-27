;;;; packages/feature/edit/src/domain/template.lisp
;;;;
;;;; Replacement templates: `$0`, `$1`, `$name`, `${name}`, `$$`, and
;;;; aitools' `${group:filter:filter}` extension. The template is parsed once
;;;; into parts, then expanded per match through cl-regex-kit's function
;;;; replacement, so the expansion rules live in this one place. Reference
;;;; syntax follows cl-regex-kit's dollar templates: a bare name is the
;;;; longest run of [A-Za-z0-9_], all digits naming a group number, and a `$`
;;;; that starts no reference is literal.
(in-package #:aitools.edit.domain)

(defun %template-fail (control &rest arguments)
  (apply #'refuse "argument.invalid" control arguments))

(defun %ascii-digit-p (char)
  (char<= #\0 char #\9))

(defun %name-char-p (char)
  (or (%ascii-digit-p char) (char<= #\a char #\z) (char<= #\A char #\Z) (char= char #\_)))

(defun %designator (token)
  (if (every #'%ascii-digit-p token) (parse-integer token) token))

(defparameter +template-filters+
  '("upper" "lower" "capitalize" "snake" "camel" "kebab" "trim" "inc" "dec" "pad<N>")
  "The template filter names, as `schema` lists them.")

(defun %pad-width (name)
  (and (> (length name) 3) (string= name "pad" :end1 3)
       (every #'%ascii-digit-p (subseq name 3))
       (parse-integer name :start 3)))

(defun %check-filter (name)
  (unless (or (member name '("upper" "lower" "capitalize" "snake" "camel" "kebab" "trim" "inc" "dec")
                      :test #'string=)
              (%pad-width name))
    (%template-fail "unknown replacement filter ~S (known: ~{~A~^, ~})" name +template-filters+))
  name)

(defun parse-replacement-template (template)
  "TEMPLATE as a list of parts: strings (literal text) and (:GROUP designator
filters). Signals EDIT-REFUSAL for an unknown filter."
  (let ((parts '()) (literal (make-string-output-stream)) (length (length template)) (position 0))
    (flet ((flush ()
             (let ((text (get-output-stream-string literal)))
               (when (plusp (length text)) (push text parts))))
           (emit (char) (write-char char literal)))
      (loop while (< position length)
            do (let ((char (char template position)))
                 (cond
                   ((char/= char #\$) (emit char) (incf position))
                   ((= (1+ position) length) (emit #\$) (incf position))
                   ((char= (char template (1+ position)) #\$) (emit #\$) (incf position 2))
                   ((char= (char template (1+ position)) #\{)
                    (let ((close (position #\} template :start (+ position 2))))
                      (if (null close)
                          (progn (emit #\$) (incf position))
                          (let* ((body (subseq template (+ position 2) close))
                                 (fields (%split-on #\: body)))
                            (when (plusp (length body))
                              (flush)
                              (push (list :group (%designator (first fields))
                                          (mapcar #'%check-filter (rest fields)))
                                    parts))
                            (setf position (1+ close))))))
                   (t
                    (let ((end (or (position-if-not #'%name-char-p template :start (1+ position)) length)))
                      (if (= end (1+ position))
                          (progn (emit #\$) (incf position))
                          (progn (flush)
                                 (push (list :group (%designator (subseq template (1+ position) end)) '()) parts)
                                 (setf position end))))))))
      (flush))
    (nreverse parts)))

(defun %split-on (char string)
  (loop with start = 0
        for end = (position char string :start start)
        collect (subseq string start end)
        while end
        do (setf start (1+ end))))

(defun perl-backreferences (template group-count)
  "The group numbers N (1-9, at most GROUP-COUNT) written `\\N` in TEMPLATE,
which cl-regex-kit would copy literally."
  (loop for position from 0 below (1- (length template))
        for next = (char template (1+ position))
        when (and (char= (char template position) #\\) (char<= #\1 next #\9)
                  (<= (- (char-code next) (char-code #\0)) group-count))
          collect (- (char-code next) (char-code #\0))))

(defun rewrite-perl-backreferences (template)
  "TEMPLATE with each `\\N` (N 1-9) written `${N}`, for the repair command."
  (with-output-to-string (out)
    (loop with position = 0
          while (< position (length template))
          do (let ((char (char template position)))
               (if (and (char= char #\\) (< (1+ position) (length template))
                        (char<= #\1 (char template (1+ position)) #\9))
                   (progn (format out "${~C}" (char template (1+ position)))
                          (incf position 2))
                   (progn (write-char char out) (incf position)))))))

;;; ------------------------------------------------------------------ filters

(defun %words (string)
  "STRING split into words at non-alphanumeric characters, at a lower-case
letter or digit followed by an upper-case letter, and before the last
capital of an upper-case run followed by a lower-case letter (HTTPServer ->
HTTP Server)."
  (let ((words '()) (current (make-string-output-stream)) (length (length string)))
    (flet ((cut ()
             (let ((word (get-output-stream-string current)))
               (when (plusp (length word)) (push word words)))))
      (dotimes (index length)
        (let ((char (char string index)))
          (if (not (alphanumericp char))
              (cut)
              (let ((previous (and (plusp index) (char string (1- index))))
                    (next (and (< (1+ index) length) (char string (1+ index)))))
                (when (and previous (alphanumericp previous) (upper-case-p char)
                           (or (lower-case-p previous) (digit-char-p previous)
                               (and (upper-case-p previous) next (lower-case-p next))))
                  (cut))
                (write-char char current)))))
      (cut))
    (nreverse words)))

(defun %capitalize (word)
  (if (zerop (length word))
      word
      (concatenate 'string (string-upcase (subseq word 0 1)) (string-downcase (subseq word 1)))))

(defun %step-integer (value delta)
  "VALUE (a decimal integer, optional leading `-`) plus DELTA, keeping its
digit count as zero padding."
  (let* ((negative (and (plusp (length value)) (char= (char value 0) #\-)))
         (digits (if negative (subseq value 1) value)))
    (unless (and (plusp (length digits)) (every #'%ascii-digit-p digits))
      (%template-fail "filter ~A needs a decimal integer, got ~S" (if (plusp delta) "inc" "dec") value))
    (let ((result (+ (* (if negative -1 1) (parse-integer digits)) delta)))
      (format nil "~:[~;-~]~v,'0D" (minusp result) (length digits) (abs result)))))

(defun apply-filter (name value)
  (let ((width (%pad-width name)))
    (cond
      (width (if (< (length value) width)
                 (concatenate 'string (make-string (- width (length value)) :initial-element #\0) value)
                 value))
      ((string= name "upper") (string-upcase value))
      ((string= name "lower") (string-downcase value))
      ((string= name "capitalize") (%capitalize value))
      ((string= name "snake") (format nil "~{~A~^_~}" (mapcar #'string-downcase (%words value))))
      ((string= name "kebab") (format nil "~{~A~^-~}" (mapcar #'string-downcase (%words value))))
      ((string= name "camel")
       (let ((words (%words value)))
         (format nil "~A~{~A~}" (string-downcase (or (first words) "")) (mapcar #'%capitalize (rest words)))))
      ((string= name "trim") (trim-whitespace value))
      ((string= name "inc") (%step-integer value 1))
      ((string= name "dec") (%step-integer value -1))
      (t (%template-fail "unknown replacement filter ~S" name)))))

(defun expand-template (parts group-value)
  "Expand parsed PARTS. GROUP-VALUE (designator) returns the group's text
or NIL when it did not participate (expanded as empty). Signals
EDIT-REFUSAL when a filter rejects its value."
  (with-output-to-string (out)
    (dolist (part parts)
      (if (stringp part)
          (write-string part out)
          (destructuring-bind (designator filters) (rest part)
            (let ((value (or (funcall group-value designator) "")))
              (dolist (filter filters)
                (setf value (apply-filter filter value)))
              (write-string value out)))))))

(defun template-regex-replacement (parts)
  "A cl-regex-kit replacement function expanding PARTS for each match."
  (lambda (match text)
    (expand-template parts
                     (lambda (designator)
                       (handler-case (cl-regex-kit:match-group-string match designator text)
                         (error () nil))))))

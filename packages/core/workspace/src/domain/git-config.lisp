;;;; packages/core/workspace/src/domain/git-config.lisp
;;;;
;;;; A reader for git's config file syntax, enough to answer the ignore decision's
;;;; questions without starting git: core.excludesFile, core.ignoreCase,
;;;; include.path, and extensions.objectFormat. Section and key names fold to
;;;; lowercase; a quoted subsection keeps its case, a legacy `[a.b]`
;;;; subsection folds, as in git. includeIf is not evaluated.
(in-package #:aitools.workspace.domain)

(defun %config-space-p (char)
  (member char '(#\Space #\Tab #\Return)))

(defun %config-key-char-p (char)
  (or (alphanumericp char) (char= char #\-)))

(defun %parse-section-header (text start)
  "Parse `[...]` whose `[` is at START. Returns (VALUES SECTION NEXT), SECTION
being NIL for a malformed header; NEXT is the index after `]` or at the end
of the line."
  (let* ((line-end (or (position #\Newline text :start start) (length text)))
         (close (position #\] text :start start :end line-end)))
    (if (null close)
        (values nil line-end)
        (let* ((body (subseq text (1+ start) close))
               (quote-start (position #\" body)))
          (values
           (cond
             (quote-start
              (let ((name (string-right-trim '(#\Space #\Tab) (subseq body 0 quote-start)))
                    (sub (with-output-to-string (out)
                           (loop with i = (1+ quote-start)
                                 while (< i (length body))
                                 do (let ((char (char body i)))
                                      (cond ((char= char #\") (return))
                                            ((and (char= char #\\) (< (1+ i) (length body)))
                                             (incf i)
                                             (write-char (char body i) out))
                                            (t (write-char char out))))
                                    (incf i)))))
                (format nil "~(~A~).~A" name sub)))
             (t (string-downcase (string-trim '(#\Space #\Tab) body))))
           (1+ close))))))

(defun %parse-config-value (text start)
  "Parse a value starting at START (just after `=`). Returns (VALUES VALUE
NEXT): leading and trailing unquoted whitespace dropped, quotes removed,
escapes and backslash-newline continuations applied, and an unquoted `#` or
`;` ending the value."
  (let ((out (make-string-output-stream))
        (pending (make-string-output-stream))
        (quoted nil) (started nil) (i start) (n (length text)))
    (flet ((emit (char)
             (when started (write-string (get-output-stream-string pending) out))
             (get-output-stream-string pending)
             (setf started t)
             (write-char char out)))
      (loop
        (when (>= i n) (return))
        (let ((char (char text i)))
          (cond
            ((char= char #\Newline) (return))
            ((and (not quoted) (member char '(#\# #\;)))
             (setf i (or (position #\Newline text :start i) n))
             (return))
            ((and (not quoted) (%config-space-p char))
             (when started (write-char char pending))
             (incf i))
            ((char= char #\\)
             (let ((next (and (< (1+ i) n) (char text (1+ i)))))
               (cond ((eql next #\Newline) (incf i 2))
                     ((and (eql next #\Return) (< (+ i 2) n) (char= (char text (+ i 2)) #\Newline))
                      (incf i 3))
                     ((null next) (incf i))
                     (t (emit (case next (#\n #\Newline) (#\t #\Tab) (#\b #\Backspace) (t next)))
                        (incf i 2)))))
            ((char= char #\")
             (when started (write-string (get-output-stream-string pending) out))
             (setf started t quoted (not quoted))
             (incf i))
            (t (emit char) (incf i))))))
    (values (get-output-stream-string out) i)))

(defun parse-git-config (text)
  "A list of (KEY . VALUE) in file order. KEY is \"section.key\" or
\"section.subsection.key\"; VALUE is a string, or T for a bare key (git's
implicit true)."
  (let ((i 0) (n (length text)) (section nil) (entries '()))
    (loop
      (loop while (and (< i n) (or (%config-space-p (char text i)) (char= (char text i) #\Newline)))
            do (incf i))
      (when (>= i n) (return))
      (let ((char (char text i)))
        (cond
          ((member char '(#\# #\;))
           (setf i (or (position #\Newline text :start i) n)))
          ((char= char #\[)
           (multiple-value-setq (section i) (%parse-section-header text i)))
          ((and section (alpha-char-p char))
           (let ((key-start i))
             (loop while (and (< i n) (%config-key-char-p (char text i))) do (incf i))
             (let ((key (string-downcase (subseq text key-start i))))
               (loop while (and (< i n) (%config-space-p (char text i))) do (incf i))
               (cond
                 ((and (< i n) (char= (char text i) #\=))
                  (multiple-value-bind (value next) (%parse-config-value text (1+ i))
                    (push (cons (format nil "~A.~A" section key) value) entries)
                    (setf i next)))
                 ((or (>= i n) (member (char text i) '(#\Newline #\# #\;)))
                  (push (cons (format nil "~A.~A" section key) t) entries))
                 (t (setf i (or (position #\Newline text :start i) n)))))))
          (t (setf i (or (position #\Newline text :start i) n))))))
    (nreverse entries)))

(defun git-config-values (entries key)
  "Every value of KEY (case-insensitive section and key) in ENTRIES order."
  (loop for (entry-key . value) in entries
        when (string-equal entry-key key) collect value))

(defun git-config-value (entries key)
  "The last value of KEY in ENTRIES (git's last-one-wins), or NIL."
  (car (last (git-config-values entries key))))

(defun git-config-boolean (value)
  "git's boolean reading of VALUE (T for a bare key). NIL for anything that
is not a recognized true value."
  (cond ((eq value t) t)
        ((null value) nil)
        (t (and (member (string-downcase value) '("true" "yes" "on" "1") :test #'string=) t))))

(defun expand-config-path (value home)
  "git's pathname expansion of `~/` and a bare `~` to HOME. Other values,
including `~user/`, are returned unchanged."
  (cond ((string= value "~") home)
        ((and (>= (length value) 2) (string= "~/" value :end2 2))
         (join-path home (subseq value 2)))
        (t value)))

(defun decode-git-text (octets)
  "OCTETS of a git metadata file (config, `.git` gitdir file, commondir) as
a string. git treats these as bytes; invalid UTF-8 becomes U+FFFD, which no
real key or path contains."
  (cl-codec-kit:octets-to-string octets :encoding :utf-8 :errorp nil))

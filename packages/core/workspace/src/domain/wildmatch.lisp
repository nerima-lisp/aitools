;;;;
;;;; A transcription of git's wildmatch.c (dowild), the matcher behind every
;;;; .gitignore pattern. Ignore decisions must give the same verdicts as
;;;; `git ls-files --exclude-standard`, so this follows git's control flow
;;;; step by step -- including its abort codes, which prune the backtracking
;;;; of `*` and `**` -- rather than translating patterns to regexes.
;;;; Character classes and case folding are ASCII-only, as in git.
(in-package #:aitools.workspace.domain)

(declaim (inline %ascii-upper-p %ascii-lower-p %ascii-downcase %ascii-upcase))
(defun %ascii-upper-p (char) (char<= #\A char #\Z))
(defun %ascii-lower-p (char) (char<= #\a char #\z))
(defun %ascii-downcase (char)
  (if (%ascii-upper-p char) (code-char (+ (char-code char) 32)) char))
(defun %ascii-upcase (char)
  (if (%ascii-lower-p char) (code-char (- (char-code char) 32)) char))

(defun %ascii-class-member-p (class char)
  "True when CHAR is in the POSIX CLASS (a string such as \"alpha\"), using
git's ASCII-only ctype table. Returns :MALFORMED for an unknown class name."
  (let ((code (char-code char)))
    (flet ((digit-p () (<= 48 code 57))
           (alpha-p () (or (<= 65 code 90) (<= 97 code 122))))
      (cond ((string= class "alnum") (or (alpha-p) (digit-p)))
            ((string= class "alpha") (alpha-p))
            ((string= class "blank") (or (= code 32) (= code 9)))
            ((string= class "cntrl") (or (< code 32) (= code 127)))
            ((string= class "digit") (digit-p))
            ((string= class "graph") (<= 33 code 126))
            ((string= class "lower") (<= 97 code 122))
            ((string= class "print") (<= 32 code 126))
            ((string= class "punct") (and (<= 33 code 126) (not (alpha-p)) (not (digit-p))))
            ((string= class "space") (member code '(32 9 10 13)))
            ((string= class "upper") (<= 65 code 90))
            ((string= class "xdigit") (or (digit-p) (<= 65 code 70) (<= 97 code 102)))
            (t :malformed)))))

(defun %glob-special-p (char)
  (and char (find char "*?[\\")))

(defun %pattern-char (pattern index)
  (and (< index (length pattern)) (char pattern index)))

(defun %text-char (text index)
  (and (< index (length text)) (char text index)))

(defun %seek-wildmatch-literal (pattern text literal start match-slash casefold)
  (let ((literal (if casefold (%ascii-downcase literal) literal)))
    (loop for candidate from start
          for char = (%text-char text candidate)
          do (cond
              ((null char) (return (values nil :abort-all)))
              ((and (not match-slash) (eql char #\/))
               (return (values nil :abort-to-starstar)))
              ((char= (if casefold (%ascii-downcase char) char) literal)
               (return (values candidate nil)))))))

(defun %wildmatch-star-search (pattern text p ti match-slash pathname casefold)
  (loop
   (let ((t-ch (%text-char text ti)))
     (when (null t-ch) (return :abort-all))
     (unless (%glob-special-p (%pattern-char pattern p))
       (multiple-value-bind (candidate status)
           (%seek-wildmatch-literal pattern text (%pattern-char pattern p)
                                    ti match-slash casefold)
         (when status (return status))
         (setf ti candidate t-ch (%text-char text ti))))
     (let ((matched (%dowild pattern text p ti pathname casefold)))
       (cond
        ((not (eq matched :nomatch))
         (when (or (not match-slash) (not (eq matched :abort-to-starstar)))
           (return matched)))
        ((and (not match-slash) (eql t-ch #\/))
         (return :abort-to-starstar))))
     (incf ti))))

(defun %wildmatch-star (pattern text p-start star-position ti pathname casefold)
  (let ((p (1+ star-position)) (match-slash nil) (shortcut-match nil))
    (if (eql (%pattern-char pattern p) #\*)
        (let ((prev (1- star-position)))
          (loop do (incf p) while (eql (%pattern-char pattern p) #\*))
          (cond
           ((not pathname) (setf match-slash t))
           ((and (or (< prev p-start) (eql (%pattern-char pattern prev) #\/))
                 (or (null (%pattern-char pattern p))
                     (eql (%pattern-char pattern p) #\/)
                     (and (eql (%pattern-char pattern p) #\\)
                          (eql (%pattern-char pattern (1+ p)) #\/))))
            (when (and (eql (%pattern-char pattern p) #\/)
                       (eq (%dowild pattern text (1+ p) ti pathname casefold) :match))
              (setf shortcut-match t))
            (setf match-slash t))
           (t (setf match-slash nil))))
      (setf match-slash (not pathname)))
    (cond
     (shortcut-match (values :match nil nil))
     ((null (%pattern-char pattern p))
      (values (if (and (not match-slash) (position #\/ text :start ti))
                  :abort-to-starstar :match)
              nil nil))
     ((and (not match-slash) (eql (%pattern-char pattern p) #\/))
      (let ((slash (position #\/ text :start ti)))
        (if slash (values nil p slash) (values :abort-all nil nil))))
     (t (values (%wildmatch-star-search pattern text p ti match-slash pathname casefold)
                nil nil)))))

(defun %wildmatch-class-step (pattern p p-ch prev-ch t-ch casefold)
  (cond
   ((null p-ch) (values :abort nil p nil))
   ((char= p-ch #\\)
    (let ((escaped (%pattern-char pattern (1+ p))))
      (if (null escaped)
          (values :abort nil p nil)
        (values nil (eql t-ch escaped) (+ p 2) escaped))))
   ((and (char= p-ch #\-) prev-ch (%pattern-char pattern (1+ p))
         (not (eql (%pattern-char pattern (1+ p)) #\])))
    (multiple-value-bind (endpoint next-p)
        (if (eql (%pattern-char pattern (1+ p)) #\\)
            (values (%pattern-char pattern (+ p 2)) (+ p 3))
          (values (%pattern-char pattern (1+ p)) (+ p 2)))
      (if (null endpoint)
          (values :abort nil p nil)
        (values nil
                (or (char<= prev-ch t-ch endpoint)
                    (and casefold (%ascii-lower-p t-ch)
                         (char<= prev-ch (%ascii-upcase t-ch) endpoint)))
                next-p nil))))
   ((and (char= p-ch #\[) (eql (%pattern-char pattern (1+ p)) #\:))
    (let* ((start (+ p 2))
           (close (loop for i from start
                        while (and (%pattern-char pattern i)
                                   (char/= (%pattern-char pattern i) #\]))
                        finally (return i))))
      (cond
       ((null (%pattern-char pattern close)) (values :abort nil p nil))
       ((or (< (- close start 1) 0)
            (char/= (%pattern-char pattern (1- close)) #\:))
        ;; Without ":]", the opening bracket is an ordinary member.
        (values nil (eql t-ch #\[) (+ p 2) #\[))
       (t
        (let* ((class (subseq pattern start (1- close)))
               (member (%ascii-class-member-p class t-ch)))
          (if (eq member :malformed)
              (values :abort nil p nil)
            (values nil
                    (or member
                        (and casefold (string= class "upper")
                             (%ascii-lower-p t-ch)))
                    (1+ close) nil)))))))
   (t (values nil (eql t-ch p-ch) (1+ p) p-ch))))

(defun %wildmatch-class-match (pattern open-position t-ch pathname casefold)
  (block class-result
    (let ((p (1+ open-position)) (p-ch nil) (matched nil) (negated nil) (prev-ch nil))
      (setf p-ch (%pattern-char pattern p))
      (when (eql p-ch #\^) (setf p-ch #\!))
      (when (eql p-ch #\!)
        (setf negated t)
        (incf p)
        (setf p-ch (%pattern-char pattern p)))
      (loop
       (multiple-value-bind (status member next-p next-prev)
           (%wildmatch-class-step pattern p p-ch prev-ch t-ch casefold)
         (when status (return-from class-result (values :abort-all nil)))
         (when member (setf matched t))
         (setf p next-p prev-ch next-prev p-ch (%pattern-char pattern next-p)))
       (when (eql p-ch #\]) (return)))
      (values (if (or (eq matched negated) (and pathname (eql t-ch #\/)))
                  :nomatch nil)
              p))))

(defun %wildmatch-step (pattern text p-start p ti p-ch t-ch pathname casefold)
  (case p-ch
    (#\\ (values (unless (eql t-ch (%pattern-char pattern (1+ p))) :nomatch)
                 (+ p 2) (1+ ti)))
    (#\? (values (when (and pathname (eql t-ch #\/)) :nomatch)
                 (1+ p) (1+ ti)))
    (#\*
     (multiple-value-bind (result next-p next-ti)
         (%wildmatch-star pattern text p-start p ti pathname casefold)
       (if result (values result nil nil)
         (values nil (1+ next-p) (1+ next-ti)))))
    (#\[
     (multiple-value-bind (result next-p)
         (%wildmatch-class-match pattern p t-ch pathname casefold)
       (if result (values result nil nil)
         (values nil (1+ next-p) (1+ ti)))))
    (t (values (unless (eql t-ch p-ch) :nomatch) (1+ p) (1+ ti)))))

(defun %dowild (pattern text p-start t-start pathname casefold)
  "Match PATTERN from P-START against TEXT from T-START. Returns :MATCH,
:NOMATCH, :ABORT-ALL, or :ABORT-TO-STARSTAR exactly as git's dowild does."
  (declare (type simple-string pattern text)
           (type fixnum p-start t-start))
  (let ((p p-start) (ti t-start) (tlen (length text)))
    (declare (type fixnum p ti tlen))
    (loop
     (let ((p-ch (%pattern-char pattern p))
           (t-ch (%text-char text ti)))
       (when (null p-ch)
         (return (if (< ti tlen) :nomatch :match)))
       (when (and (null t-ch) (char/= p-ch #\*))
         (return :abort-all))
       (when (and casefold t-ch) (setf t-ch (%ascii-downcase t-ch)))
       (when casefold (setf p-ch (%ascii-downcase p-ch)))
       (multiple-value-bind (result next-p next-ti)
           (%wildmatch-step pattern text p-start p ti p-ch t-ch pathname casefold)
         (when result (return result))
         (setf p next-p ti next-ti))))))

(defun wildmatch (pattern text &key pathname casefold)
  "True when TEXT matches the git wildmatch PATTERN. PATHNAME is git's
WM_PATHNAME (`*`, `?`, and classes never match `/`; `**` spans directories
only as a whole component); CASEFOLD is WM_CASEFOLD (ASCII only)."
  (eq :match (%dowild (coerce pattern 'simple-string) (coerce text 'simple-string)
                      0 0 pathname casefold)))

;;;; packages/core/workspace/src/domain/wildmatch.lisp
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

(defun %dowild (pattern text p-start t-start pathname casefold)
  "Match PATTERN from P-START against TEXT from T-START. Returns :MATCH,
:NOMATCH, :ABORT-ALL, or :ABORT-TO-STARSTAR exactly as git's dowild does."
  (declare (type simple-string pattern text)
           (type fixnum p-start t-start))
  (let ((p p-start) (ti t-start)
        (plen (length pattern)) (tlen (length text)))
    (declare (type fixnum p ti plen tlen))
    (flet ((pat (i) (and (< i plen) (char pattern i)))
           (txt (i) (and (< i tlen) (char text i))))
      (loop
        (let ((p-ch (pat p)))
          (when (null p-ch)
            (return (if (< ti tlen) :nomatch :match)))
          (let ((t-ch (txt ti)))
            (when (and (null t-ch) (char/= p-ch #\*))
              (return :abort-all))
            (when (and casefold t-ch) (setf t-ch (%ascii-downcase t-ch)))
            (when casefold (setf p-ch (%ascii-downcase p-ch)))
            (case p-ch
              (#\\
               (incf p)
               (unless (eql t-ch (pat p)) (return :nomatch)))
              (#\?
               (when (and pathname (eql t-ch #\/)) (return :nomatch)))
              (#\*
               (let ((match-slash nil))
                 (incf p)
                 (cond
                   ((eql (pat p) #\*)
                    (let ((prev (- p 2)))
                      (loop do (incf p) while (eql (pat p) #\*))
                      (cond
                        ((not pathname) (setf match-slash t))
                        ((and (or (< prev p-start) (eql (pat prev) #\/))
                              (or (null (pat p)) (eql (pat p) #\/)
                                  (and (eql (pat p) #\\) (eql (pat (1+ p)) #\/))))
                         (when (and (eql (pat p) #\/)
                                    (eq (%dowild pattern text (1+ p) ti pathname casefold) :match))
                           (return :match))
                         (setf match-slash t))
                        (t (setf match-slash nil)))))
                   (t (setf match-slash (not pathname))))
                 (cond
                   ((null (pat p))
                    (return (if (and (not match-slash) (position #\/ text :start ti))
                                :abort-to-starstar
                                :match)))
                   ((and (not match-slash) (eql (pat p) #\/))
                    (let ((slash (position #\/ text :start ti)))
                      (unless slash (return :abort-all))
                      (setf ti slash)))
                   (t
                    (return
                      (loop
                        (when (null t-ch) (return :abort-all))
                        (unless (%glob-special-p (pat p))
                          (let ((literal (pat p)))
                            (when casefold (setf literal (%ascii-downcase literal)))
                            (loop
                              (setf t-ch (txt ti))
                              (when (or (null t-ch) (and (not match-slash) (char= t-ch #\/)))
                                (return))
                              (when casefold (setf t-ch (%ascii-downcase t-ch)))
                              (when (char= t-ch literal) (return))
                              (incf ti))
                            (unless (eql t-ch literal)
                              (return (if match-slash :abort-all :abort-to-starstar)))))
                        (let ((matched (%dowild pattern text p ti pathname casefold)))
                          (cond ((not (eq matched :nomatch))
                                 (when (or (not match-slash) (not (eq matched :abort-to-starstar)))
                                   (return matched)))
                                ((and (not match-slash) (eql t-ch #\/))
                                 (return :abort-to-starstar))))
                        (incf ti)
                        (setf t-ch (txt ti))))))))
              (#\[
               (let ((matched nil) (negated nil) (prev-ch nil))
                 (incf p)
                 (let ((p-ch (pat p)))
                   (when (eql p-ch #\^) (setf p-ch #\!))
                   (when (eql p-ch #\!)
                     (setf negated t)
                     (incf p)
                     (setf p-ch (pat p)))
                   (loop
                     (block member
                       (when (null p-ch) (return-from %dowild :abort-all))
                       (cond
                         ((char= p-ch #\\)
                          (incf p)
                          (setf p-ch (pat p))
                          (when (null p-ch) (return-from %dowild :abort-all))
                          (when (eql t-ch p-ch) (setf matched t)))
                         ((and (char= p-ch #\-) prev-ch (pat (1+ p)) (not (eql (pat (1+ p)) #\])))
                          (incf p)
                          (setf p-ch (pat p))
                          (when (eql p-ch #\\)
                            (incf p)
                            (setf p-ch (pat p))
                            (when (null p-ch) (return-from %dowild :abort-all)))
                          (cond ((char<= prev-ch t-ch p-ch) (setf matched t))
                                ((and casefold (%ascii-lower-p t-ch)
                                      (char<= prev-ch (%ascii-upcase t-ch) p-ch))
                                 (setf matched t)))
                          (setf p-ch nil))
                         ((and (char= p-ch #\[) (eql (pat (1+ p)) #\:))
                          (let* ((s (+ p 2))
                                 (close (loop for i from s
                                              while (and (pat i) (char/= (pat i) #\]))
                                              finally (return i))))
                            (when (null (pat close)) (return-from %dowild :abort-all))
                            (cond
                              ((or (< (- close s 1) 0) (char/= (pat (1- close)) #\:))
                               ;; No ":]": the `[` is an ordinary member.
                               (setf p (- s 1))
                               (setf p-ch #\[)
                               (when (eql t-ch #\[) (setf matched t)))
                              (t
                               (setf p close)
                               (let* ((class (subseq pattern s (1- close)))
                                      (member (%ascii-class-member-p class t-ch)))
                                 (when (eq member :malformed) (return-from %dowild :abort-all))
                                 (when (or member
                                           (and casefold (string= class "upper") (%ascii-lower-p t-ch)))
                                   (setf matched t)))
                               (setf p-ch nil)))))
                         ((eql t-ch p-ch) (setf matched t))))
                     (setf prev-ch p-ch)
                     (incf p)
                     (setf p-ch (pat p))
                     (when (eql p-ch #\]) (return))))
                 (when (or (eq matched negated) (and pathname (eql t-ch #\/)))
                   (return :nomatch))))
              (t
               (unless (eql t-ch p-ch) (return :nomatch))))
            (incf p)
            (incf ti)))))))

(defun wildmatch (pattern text &key pathname casefold)
  "True when TEXT matches the git wildmatch PATTERN. PATHNAME is git's
WM_PATHNAME (`*`, `?`, and classes never match `/`; `**` spans directories
only as a whole component); CASEFOLD is WM_CASEFOLD (ASCII only)."
  (eq :match (%dowild (coerce pattern 'simple-string) (coerce text 'simple-string)
                      0 0 pathname casefold)))

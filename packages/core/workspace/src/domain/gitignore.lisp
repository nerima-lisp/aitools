;;;; packages/core/workspace/src/domain/gitignore.lisp
;;;;
;;;; .gitignore parsing and matching with git's semantics (dir.c:
;;;; add_patterns_from_buffer, parse_path_pattern, match_basename,
;;;; match_pathname, last_matching_pattern). An IGNORE-LIST is one source
;;;; file; an ignore stack is a list of IGNORE-LISTs in precedence order
;;;; (deepest .gitignore first, then shallower ones, then
;;;; $GIT_DIR/info/exclude, then the global excludes file). Within a list the
;;;; last matching pattern wins; across the stack the first list with any
;;;; matching pattern decides.
(in-package #:aitools.workspace.domain)

(defstruct (ignore-pattern (:copier nil))
  "TEXT is the wildmatch pattern with any leading `!`, trailing `/`, and (for
path patterns) one leading `/` already removed."
  (text "" :type simple-string :read-only t)
  (negative-p nil :type boolean :read-only t)
  (directory-only-p nil :type boolean :read-only t)
  (basename-only-p nil :type boolean :read-only t))

(defstruct (ignore-list (:copier nil))
  "BASE is the directory holding the source file, relative to the repository
top, without a trailing `/` (\"\" for the top itself). SOURCE labels the
file for diagnostics."
  (base "" :type simple-string :read-only t)
  (patterns #() :type simple-vector :read-only t)
  (source nil :read-only t))

(defun %trim-trailing-spaces (line)
  "git's trim_trailing_spaces: drop unescaped trailing spaces (not tabs)."
  (let ((last-space nil) (i 0) (n (length line)))
    (loop while (< i n)
          do (let ((char (char line i)))
               (cond ((char= char #\Space)
                      (unless last-space (setf last-space i)))
                     ((char= char #\\)
                      (incf i)
                      (when (>= i n) (return-from %trim-trailing-spaces line))
                      (setf last-space nil))
                     (t (setf last-space nil))))
             (incf i))
    (if last-space (subseq line 0 last-space) line)))

(defun %parse-pattern (line)
  "An IGNORE-PATTERN for one already-trimmed, non-comment LINE, or NIL when
nothing matchable remains."
  (let ((negative nil) (text line) (directory-only nil))
    (when (and (plusp (length text)) (char= (char text 0) #\!))
      (setf negative t text (subseq text 1)))
    (when (and (plusp (length text)) (char= (char text (1- (length text))) #\/))
      (setf directory-only t text (subseq text 0 (1- (length text)))))
    (when (plusp (length text))
      (let ((basename-only (null (position #\/ text))))
        (when (and (not basename-only) (char= (char text 0) #\/))
          (setf text (subseq text 1)))
        (make-ignore-pattern :text (coerce text 'simple-string)
                             :negative-p negative
                             :directory-only-p directory-only
                             :basename-only-p basename-only)))))

(defun parse-ignore-lines (lines &key (base "") source)
  "An IGNORE-LIST from LINES (strings without line terminators)."
  (make-ignore-list
   :base (coerce base 'simple-string)
   :source source
   :patterns (coerce (loop for line in lines
                           for trimmed = (%trim-trailing-spaces line)
                           for pattern = (and (plusp (length trimmed))
                                              (char/= (char trimmed 0) #\#)
                                              (%parse-pattern trimmed))
                           when pattern collect pattern)
                     'simple-vector)))

(defun parse-ignore-octets (octets &key (base "") source)
  "An IGNORE-LIST from the raw bytes of an ignore file, split the way git
splits it: on LF, dropping one CR before each LF and a leading UTF-8 BOM.
Invalid UTF-8 decodes to U+FFFD, which then matches no real file name."
  (let* ((start (if (and (>= (length octets) 3)
                         (= (aref octets 0) #xEF) (= (aref octets 1) #xBB) (= (aref octets 2) #xBF))
                    3 0))
         (text (cl-codec-kit:octets-to-string octets :start start :encoding :utf-8 :errorp nil))
         (lines '())
         (line-start 0))
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline)
            do (let ((end (if (and (> i line-start) (char= (char text (1- i)) #\Return)) (1- i) i)))
                 (push (subseq text line-start end) lines)
                 (setf line-start (1+ i))))
    (when (< line-start (length text))
      (push (subseq text line-start) lines))
    (parse-ignore-lines (nreverse lines) :base base :source source)))

(defun %path-under-base (path base casefold)
  "PATH's remainder below BASE (both relative to the repository top), or
NIL when PATH is not strictly below BASE."
  (let ((base-length (length base)))
    (cond ((zerop base-length) path)
          ((and (> (length path) base-length)
                (char= (char path base-length) #\/)
                (if casefold
                    (string-equal path base :end1 base-length)
                    (string= path base :end1 base-length)))
           (subseq path (1+ base-length)))
          (t nil))))

(defun ignore-list-verdict (list path directory-p &key casefold)
  ":IGNORED or :INCLUDED from the last pattern of LIST matching PATH (relative
to the repository top, no trailing `/`), or NIL when no pattern matches.
DIRECTORY-P says whether PATH is a directory (lstat semantics: a symlink to a
directory is not one), which directory-only patterns require."
  (let* ((patterns (ignore-list-patterns list))
         (basename (path-basename path))
         (below-base :unset))
    (loop for i from (1- (length patterns)) downto 0
          for pattern = (svref patterns i)
          do (when (or directory-p (not (ignore-pattern-directory-only-p pattern)))
               (when (if (ignore-pattern-basename-only-p pattern)
                         (wildmatch (ignore-pattern-text pattern) basename :casefold casefold)
                         (progn
                           (when (eq below-base :unset)
                             (setf below-base (%path-under-base path (ignore-list-base list) casefold)))
                           (and below-base
                                (wildmatch (ignore-pattern-text pattern) below-base
                                           :pathname t :casefold casefold))))
                 (return (if (ignore-pattern-negative-p pattern) :included :ignored)))))))

(defun ignore-stack-verdict (stack path directory-p &key casefold)
  "The verdict of the first list in STACK (highest precedence first) that
has any pattern matching PATH; NIL when none does."
  (dolist (list stack nil)
    (let ((verdict (ignore-list-verdict list path directory-p :casefold casefold)))
      (when verdict (return verdict)))))

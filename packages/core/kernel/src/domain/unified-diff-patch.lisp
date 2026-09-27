;;;; packages/core/kernel/src/domain/unified-diff-patch.lisp
;;;;
;;;; The consuming side of unified diffs, for `apply`: parse a
;;;; `diff -u` or `git diff` document into FILE-PATCHes and apply their hunks
;;;; to a list of lines, searching within a fuzz window. The DIFF-LINE and
;;;; DIFF-HUNK structures are defined in unified-diff.lisp.
(in-package #:aitools.kernel.domain)

;;; ---------------------------------------------------------------- parsing

(defstruct (file-patch
            (:constructor make-file-patch (old-path new-path hunks))
            (:copier nil))
  (old-path nil :type (or null string) :read-only t)
  (new-path nil :type (or null string) :read-only t)
  (hunks nil :type list :read-only t))

(defun strip-path-components (path count)
  "Drop the first COUNT `/`-separated components of PATH, the behavior of
`patch -pN` / `apply --strip N`. A COUNT of 0 returns PATH unchanged."
  (let ((remaining path))
    (dotimes (i count remaining)
      (let ((slash (position #\/ remaining)))
        (unless slash (return-from strip-path-components remaining))
        (setf remaining (subseq remaining (1+ slash)))))))

(defun %parse-hunk-header (line)
  "Parse `@@ -old_start[,old_count] +new_start[,new_count] @@`. A missing
count defaults to 1, matching diff(1)'s own shorthand. Every number is ASCII
digits with no sign; anything else signals a SIMPLE-ERROR."
  (let* ((minus (position #\- line)) (plus (position #\+ line))
         (at-end (and plus (search " @@" line :start2 plus))))
    (unless (and minus plus at-end) (error "malformed hunk header: ~S" line))
    (flet ((parse-number (text)
             (if (and (plusp (length text)) (every #'%ascii-digit-char-p text))
                 (parse-integer text)
                 (error "malformed hunk header: ~S" line))))
      (flet ((parse-range (text)
               (let ((comma (position #\, text)))
                 (if comma
                     (values (parse-number (subseq text 0 comma)) (parse-number (subseq text (1+ comma))))
                     (values (parse-number text) 1)))))
        (multiple-value-bind (old-start old-count)
            (parse-range (string-trim " " (subseq line (1+ minus) plus)))
          (multiple-value-bind (new-start new-count)
              (parse-range (string-trim " " (subseq line (1+ plus) at-end)))
            (values old-start old-count new-start new-count)))))))

(defun split-diff-lines (text)
  "Split TEXT into lines without trailing newlines, without inventing a
trailing empty line when TEXT itself ends in a newline."
  (let ((lines nil) (start 0) (length (length text)))
    (loop for newline = (position #\Newline text :start start)
          while newline
          do (push (subseq text start newline) lines) (setf start (1+ newline)))
    (when (< start length) (push (subseq text start) lines))
    (nreverse lines)))

(defun parse-unified-diff (text &key (strip 1))
  "Parse unified-diff TEXT (a `diff -u` or `git diff` document, possibly
covering several files) into a list of FILE-PATCHes. STRIP is the number of
leading path components dropped from each file's `---`/`+++` header path
(default 1, stripping `a/`/`b/`); pass 0 to
keep paths verbatim."
  (let ((raw-lines (split-diff-lines text)) (patches nil)
        (old-path nil) (new-path nil) (hunks nil) (pending-lines nil)
        (old-start 0) (old-count 0) (new-start 0) (new-count 0)
        (in-hunk nil) (remaining-old 0) (remaining-new 0))
    (labels ((flush-hunk ()
               (when pending-lines
                 (push (make-diff-hunk old-start old-count new-start new-count (nreverse pending-lines))
                       hunks)
                 (setf pending-lines nil)))
             (flush-file ()
               (flush-hunk)
               (when (or old-path new-path)
                 (push (make-file-patch old-path new-path (nreverse hunks)) patches))
               (setf hunks nil old-path nil new-path nil in-hunk nil))
             (consume (consumes-old consumes-new)
               ;; Spend this body line's old/new budget; a hunk closes only
               ;; once both counts are used up, so a following `--- ` is a
               ;; file header again while an in-hunk `--- ...` stays content.
               (when consumes-old (decf remaining-old))
               (when consumes-new (decf remaining-new))
               (when (and (<= remaining-old 0) (<= remaining-new 0))
                 (setf in-hunk nil))))
      (dolist (line raw-lines)
        (cond
          ((and (not in-hunk) (>= (length line) 4) (string= line "--- " :end1 4))
           (flush-file)
           (setf old-path (strip-path-components (string-trim '(#\Space #\Tab) (subseq line 4)) strip)))
          ((and (not in-hunk) (>= (length line) 4) (string= line "+++ " :end1 4))
           (setf new-path (strip-path-components (string-trim '(#\Space #\Tab) (subseq line 4)) strip)))
          ((and (>= (length line) 2) (string= line "@@" :end1 2))
           (flush-hunk)
           (multiple-value-setq (old-start old-count new-start new-count) (%parse-hunk-header line))
           (setf remaining-old old-count remaining-new new-count
                 in-hunk (or (plusp old-count) (plusp new-count))))
          ((string= line "\\ No newline at end of file")
           (when pending-lines
             (setf (car pending-lines)
                   (make-diff-line (diff-line-op (car pending-lines)) (diff-line-text (car pending-lines))
                                   :no-newline t))))
          ((and in-hunk (plusp (length line)) (char= (char line 0) #\-))
           (push (make-diff-line :delete (subseq line 1)) pending-lines)
           (consume t nil))
          ((and in-hunk (plusp (length line)) (char= (char line 0) #\+))
           (push (make-diff-line :insert (subseq line 1)) pending-lines)
           (consume nil t))
          ((and in-hunk (plusp (length line)) (char= (char line 0) #\Space))
           (push (make-diff-line :context (subseq line 1)) pending-lines)
           (consume t t))
          ((and in-hunk (zerop (length line)))
           (push (make-diff-line :context "") pending-lines)
           (consume t t))
          (t nil)))
      (flush-file))
    (nreverse patches)))

;;; -------------------------------------------------------------- applying

(defun reverse-hunk (hunk)
  "Swap the roles of the old and new file, the effect of `apply --reverse`."
  (make-diff-hunk (diff-hunk-new-start hunk) (diff-hunk-new-count hunk)
                  (diff-hunk-old-start hunk) (diff-hunk-old-count hunk)
                  (mapcar (lambda (line)
                            (make-diff-line (ecase (diff-line-op line)
                                             (:context :context) (:delete :insert) (:insert :delete))
                                            (diff-line-text line) :no-newline (diff-line-no-newline line)))
                          (diff-hunk-lines hunk))))

(defun %hunk-old-lines (hunk)
  (mapcar #'diff-line-text (remove :insert (diff-hunk-lines hunk) :key #'diff-line-op)))

(defun %hunk-new-lines (hunk)
  (mapcar #'diff-line-text (remove :delete (diff-hunk-lines hunk) :key #'diff-line-op)))

(defun %lines-match-at (lines-vector index expected)
  (and (<= (+ index (length expected)) (length lines-vector))
       (loop for i from 0 below (length expected)
             always (string= (nth i expected) (aref lines-vector (+ index i))))))

(defun %find-hunk-position (lines-vector hunk fuzz)
  "Search for HUNK's old-side lines in LINES-VECTOR, trying its recorded
position first and then +/-1..FUZZ lines away (nearest offsets first),
returning the 0-based index or NIL."
  (let* ((expected (%hunk-old-lines hunk))
         (anchor (max 0 (1- (diff-hunk-old-start hunk)))))
    (if (null expected)
        anchor
        (loop for offset from 0 to fuzz
              do (dolist (candidate (if (zerop offset)
                                         (list anchor)
                                         (list (- anchor offset) (+ anchor offset))))
                   (when (and (>= candidate 0) (%lines-match-at lines-vector candidate expected))
                     (return-from %find-hunk-position candidate)))
              finally (return nil)))))

(defun apply-hunks/k (lines hunks &key (fuzz 0) reverse on-applied on-conflict)
  "Apply HUNKS (from one FILE-PATCH) to LINES (a list of line strings without
trailing newlines), searching within FUZZ lines of each hunk's recorded
position. REVERSE applies the inverse patch (see REVERSE-HUNK).

Calls (FUNCALL ON-APPLIED new-lines final-newline-p applied-count) if every
hunk placed. Otherwise calls (FUNCALL ON-CONFLICT hunk-index hunk
nearby-lines) for the first hunk that could not be placed and returns that
call's value; nearby-lines is the current content around the hunk's expected
position, for the caller to offer as SELECTION.NO-MATCH candidates."
  (let ((hunks (if reverse (mapcar #'reverse-hunk hunks) hunks))
        (result (coerce lines 'list))
        (final-newline t))
    (loop for hunk in hunks
          for hunk-index from 0
          do (let* ((vector-lines (coerce result 'simple-vector))
                     (position (%find-hunk-position vector-lines hunk fuzz)))
                (unless position
                  (let* ((anchor (max 0 (1- (diff-hunk-old-start hunk))))
                         (window-start (max 0 (- anchor fuzz 2)))
                         (window-end (min (length vector-lines)
                                          (+ anchor (length (%hunk-old-lines hunk)) fuzz 2))))
                    (return-from apply-hunks/k
                      (funcall on-conflict hunk-index hunk
                              (coerce (subseq vector-lines window-start window-end) 'list)))))
                (let* ((old-length (length (%hunk-old-lines hunk)))
                       (new-lines (%hunk-new-lines hunk))
                       (before (subseq result 0 position))
                       (after (subseq result (+ position old-length))))
                  (setf result (append before new-lines after))
                  (let ((last-line (car (last (diff-hunk-lines hunk)))))
                    (when (and last-line (diff-line-no-newline last-line)
                              (member (diff-line-op last-line) '(:context :insert))
                              (null after))
                      (setf final-newline nil))))))
    (funcall on-applied result final-newline (length hunks))))

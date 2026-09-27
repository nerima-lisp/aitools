;;;; packages/feature/vcs/src/domain/diff.lisp
;;;;
;;;; `git diff`. Per-file counts and paths come from `git diff --numstat -z`
;;;; (parsed by cl-vcs-kit, NUL-separated, so no path quoting applies); the
;;;; hunks come from the patch of the same diff, split at each `diff --git`
;;;; line and paired with the numstat records by position, since both list
;;;; files in the same diffcore order.
;;;;
;;;; Hunk bodies are parsed by AITOOLS.KERNEL.DOMAIN:PARSE-UNIFIED-DIFF. With
;;;; known issue 8 fixed, that parser spends each hunk's `@@` line budget before
;;;; a `--- `/`+++ ` line can close the file, so such a line inside a hunk stays
;;;; its removed `-- ...`/added `++ ...` content (an SQL or Lua comment, for
;;;; instance) instead of being taken for a file header. SPLIT-PATCH-BY-FILE
;;;; still cuts the multi-file patch at each `diff --git` line so every file --
;;;; including a binary or pure-rename file with no hunks -- stays paired with
;;;; its numstat record by position.
(in-package #:aitools.vcs.domain)

(defun %starts-with-p (prefix text)
  (and (>= (length text) (length prefix)) (string= prefix text :end2 (length prefix))))

(defun split-patch-by-file (text)
  "Split TEXT, a multi-file `git diff` patch, into one list of lines per
file, each list starting with its `diff --git` line."
  (let ((files nil) (current nil))
    (dolist (line (aitools.kernel.domain:split-diff-lines text))
      (when (%starts-with-p "diff --git " line)
        (when current (push (nreverse current) files))
        (setf current nil))
      (when (or current (%starts-with-p "diff --git " line))
        (push line current)))
    (when current (push (nreverse current) files))
    (nreverse files)))

(defun %file-hunks (lines)
  "The kernel DIFF-HUNKs of one file's patch LINES (from SPLIT-PATCH-BY-FILE).
Reconstructs the file's patch text and defers to
AITOOLS.KERNEL.DOMAIN:PARSE-UNIFIED-DIFF. A file with no hunks -- binary, a
pure rename, a mode change -- carries no `--- `/`+++ ` header, so the parser
yields no FILE-PATCH and this returns NIL."
  (let ((patches (aitools.kernel.domain:parse-unified-diff
                  (format nil "~{~A~%~}" lines) :strip 0)))
    (when patches
      (aitools.kernel.domain:file-patch-hunks (first patches)))))

(defun %rendered-hunk-lines (hunk)
  (loop for line in (aitools.kernel.domain:diff-hunk-lines hunk)
        collect (concatenate 'string
                             (ecase (aitools.kernel.domain:diff-line-op line)
                               (:context " ") (:delete "-") (:insert "+"))
                             (aitools.kernel.domain:diff-line-text line))
        when (aitools.kernel.domain:diff-line-no-newline line)
          collect "\\ No newline at end of file"))

(defun %hunk-object (hunk lines)
  (json-object-from-alist (list (cons "old_start" (aitools.kernel.domain:diff-hunk-old-start hunk))
                                (cons "old_count" (aitools.kernel.domain:diff-hunk-old-count hunk))
                                (cons "new_start" (aitools.kernel.domain:diff-hunk-new-start hunk))
                                (cons "new_count" (aitools.kernel.domain:diff-hunk-new-count hunk))
                                (cons "lines" lines))))

(defun %file-object (record mode &optional (hunks nil hunks-p))
  (destructuring-bind (&key path original-path added deleted binary) record
    (json-object-from-alist (append (list (cons "path" path))
                                    (when original-path (list (cons "from" original-path)))
                                    (list (cons "mode" mode)
                                          (cons "added" (if binary (json-null) added))
                                          (cons "deleted" (if binary (json-null) deleted)))
                                    (when binary (list (cons "binary" t)))
                                    (when hunks-p (list (cons "hunks" hunks)))))))

(defun diff-files/k (records file-patches &key (output :hunks) (max-lines 400)
                                              on-complete on-truncated)
  "Shape `git diff`'s `files` array. RECORDS is one plist (:PATH :ORIGINAL-PATH
:ADDED :DELETED :BINARY) per changed file; FILE-PATCHES (from
SPLIT-PATCH-BY-FILE, ignored for OUTPUT :STAT) holds the same files' patch
lines in the same order.

With OUTPUT :HUNKS, files are taken in order while their rendered hunk lines
(one per hunk header plus one per body line) fit in MAX-LINES; that file and
every later one become mode \"summary\" (counts only) once one does not.

Calls (ON-COMPLETE files character-count) when nothing was summarized, or
(ON-TRUNCATED files character-count first-summary-path first-summary-lines)
otherwise. CHARACTER-COUNT covers the returned hunk lines, for
`approx_tokens`; FIRST-SUMMARY-LINES is the budget that file alone needs.
The caller guarantees RECORDS and FILE-PATCHES have the same length."
  (if (eq output :stat)
      (funcall on-complete (mapcar (lambda (record) (%file-object record "stat")) records) 0)
      (let ((used 0) (characters 0) (summary-from nil) (files nil))
        (loop for record in records
              for patch in file-patches
              do (if summary-from
                     (push (%file-object record "summary") files)
                     (let* ((hunks (%file-hunks patch))
                            (rendered (mapcar #'%rendered-hunk-lines hunks))
                            (cost (loop for lines in rendered sum (1+ (length lines)))))
                       (if (> (+ used cost) max-lines)
                           (progn (setf summary-from (list (getf record :path) cost))
                                  (push (%file-object record "summary") files))
                           (progn
                             (incf used cost)
                             (loop for lines in rendered
                                   do (loop for line in lines do (incf characters (1+ (length line)))))
                             (push (%file-object record "hunks" (mapcar #'%hunk-object hunks rendered))
                                   files))))))
        (setf files (nreverse files))
        (if summary-from
            (funcall on-truncated files characters (first summary-from) (second summary-from))
            (funcall on-complete files characters)))))

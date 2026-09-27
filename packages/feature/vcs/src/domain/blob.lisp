;;;; packages/feature/vcs/src/domain/blob.lisp
;;;;
;;;; `git show` returns a blob in `read`'s shape and `git blame` pages blame output
;;;; the same way, so both share the line-window arithmetic here.
(in-package #:aitools.vcs.domain)

(defun line-window/k (total-lines &key start end max-lines on-window)
  "Resolve the lines to return from a TOTAL-LINES document. START and END
are the 1-based inclusive `--range` bounds (NIL START means line 1, NIL END
means the last line); MAX-LINES caps the count. START must not lie past the
last line; selector resolution rejects such a range first.

Calls (ON-WINDOW first last truncated next-range): FIRST..LAST is the
inclusive window (LAST is FIRST - 1 when empty), TRUNCATED is true when the
cap stopped the window before the requested end, and NEXT-RANGE is then the
`S:E` range string that continues it."
  (let* ((first (or start 1))
         (wanted-last (min total-lines (or end total-lines)))
         (last (if max-lines (min wanted-last (+ first max-lines -1)) wanted-last)))
    (if (< last wanted-last)
        (funcall on-window first last t
                 (format nil "~D:~D" (1+ last)
                         (if max-lines (min wanted-last (+ last max-lines)) wanted-last)))
        (funcall on-window first last nil nil))))

(defun split-object-spec (spec)
  "Split a `<rev>:<path>` object name at its first colon, returning
(VALUES REV PATH), or NIL when SPEC has no colon."
  (let ((colon (position #\: spec)))
    (when colon
      (values (subseq spec 0 colon) (subseq spec (1+ colon))))))

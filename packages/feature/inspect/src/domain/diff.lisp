;;;; packages/feature/inspect/src/domain/diff.lisp
;;;;
;;;; The comparisons of `diff` over already-split lines: unified hunks (with the
;;;; `-w` and `--strip-trailing-cr` style normalisations), `stat` counts,
;;;; the `comm`-style line sets, and directory listings compared by path.
;;;; Hunk generation itself is the kernel's; normalised comparison runs the
;;;; kernel over comparison keys and then puts the original text back.
(in-package #:aitools.inspect.domain)

(defun diff-key (line &key ignore-whitespace ignore-eol)
  "The string LINE is compared by."
  (let ((line (if (and ignore-eol (plusp (length line)) (char= (char line (1- (length line))) #\Return))
                  (subseq line 0 (1- (length line)))
                  line)))
    (if ignore-whitespace
        (remove-if (lambda (char) (member char '(#\Space #\Tab #\Return #\Page))) line)
        line)))

(defun %retext-hunk (hunk lines-a lines-b)
  "HUNK computed over comparison keys, with each line's text replaced by
the original line it stands for (context lines show file A's text)."
  (let ((a (if (zerop (diff-hunk-old-count hunk)) (diff-hunk-old-start hunk) (1- (diff-hunk-old-start hunk))))
        (b (if (zerop (diff-hunk-new-count hunk)) (diff-hunk-new-start hunk) (1- (diff-hunk-new-start hunk)))))
    (make-diff-hunk (diff-hunk-old-start hunk) (diff-hunk-old-count hunk)
                    (diff-hunk-new-start hunk) (diff-hunk-new-count hunk)
                    (loop for line in (diff-hunk-lines hunk)
                          collect (make-diff-line
                                   (diff-line-op line)
                                   (ecase (diff-line-op line)
                                     (:context (prog1 (svref lines-a a) (incf a) (incf b)))
                                     (:delete (prog1 (svref lines-a a) (incf a)))
                                     (:insert (prog1 (svref lines-b b) (incf b))))
                                   :no-newline (diff-line-no-newline line))))))

(defun compare-lines (lines-a lines-b &key (context 3) ignore-whitespace ignore-eol
                                            (final-newline-a t) (final-newline-b t))
  "DIFF-HUNKs from LINES-A to LINES-B (simple-vectors). With IGNORE-EOL a
missing final newline is not a difference either."
  (flet ((keys (lines)
           (map 'simple-vector (lambda (line) (diff-key line :ignore-whitespace ignore-whitespace
                                                             :ignore-eol ignore-eol))
                lines)))
    (let ((hunks (generate-diff-hunks (keys lines-a) (keys lines-b)
                                      :context context
                                      :final-newline-a (or ignore-eol final-newline-a)
                                      :final-newline-b (or ignore-eol final-newline-b))))
      (if (or ignore-whitespace ignore-eol)
          (mapcar (lambda (hunk) (%retext-hunk hunk lines-a lines-b)) hunks)
          hunks))))

(defun hunk-line-counts (hunks)
  "(VALUES added deleted) over HUNKS."
  (let ((added 0) (deleted 0))
    (dolist (hunk hunks)
      (dolist (line (diff-hunk-lines hunk))
        (case (diff-line-op line)
          (:insert (incf added))
          (:delete (incf deleted)))))
    (values added deleted)))

(defun compare-line-sets (lines-a lines-b &key ignore-whitespace ignore-eol)
  "`comm` over distinct lines: (VALUES only-a only-b both-count), the two
lists in order of first appearance, each holding the first original text of
its key."
  (flet ((index (lines)
           (let ((table (make-hash-table :test 'equal)) (order '()))
             (loop for line across lines
                   for key = (diff-key line :ignore-whitespace ignore-whitespace :ignore-eol ignore-eol)
                   unless (nth-value 1 (gethash key table))
                     do (setf (gethash key table) line)
                        (push key order))
             (values table (nreverse order)))))
    (multiple-value-bind (table-a order-a) (index lines-a)
      (multiple-value-bind (table-b order-b) (index lines-b)
        (values (loop for key in order-a
                      unless (nth-value 1 (gethash key table-b)) collect (gethash key table-a))
                (loop for key in order-b
                      unless (nth-value 1 (gethash key table-a)) collect (gethash key table-b))
                (loop for key in order-a count (nth-value 1 (gethash key table-b))))))))

(defun compare-path-lists (paths-a paths-b)
  "(VALUES only-a only-b both), each sorted, for two lists of relative
path strings."
  (let ((table-b (make-hash-table :test 'equal))
        (table-a (make-hash-table :test 'equal)))
    (dolist (path paths-b) (setf (gethash path table-b) t))
    (dolist (path paths-a) (setf (gethash path table-a) t))
    (values (sort (remove-if (lambda (path) (gethash path table-b)) (copy-list paths-a)) #'string<)
            (sort (remove-if (lambda (path) (gethash path table-a)) (copy-list paths-b)) #'string<)
            (sort (remove-if-not (lambda (path) (gethash path table-b)) (copy-list paths-a)) #'string<))))

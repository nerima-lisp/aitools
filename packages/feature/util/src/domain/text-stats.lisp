;;;; packages/feature/util/src/domain/text-stats.lisp
;;;;
;;;; `util tokens`. `approx_tokens` comes from the kernel's shared
;;;; estimator so this command and every content-returning command agree.
;;;; Lines follow `wc -l` plus one for an unterminated last line; a line's
;;;; length excludes its LF and a CR immediately before it; words are runs of
;;;; non-whitespace separated by ASCII whitespace.
(in-package #:aitools.util.domain)

(defun %ascii-whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return #\Page #.(code-char 11))))

(defun text-statistics (text byte-count)
  "Return an alist of (FIELD . INTEGER) for TEXT, in output order.
BYTE-COUNT is the size of the input as received, so a `--content-file`
reports its on-disk size even when TEXT carries U+FFFD replacements."
  (declare (type string text))
  (let ((lines 0) (words 0) (max-line 0) (line-length 0) (in-word nil) (previous nil))
    (flet ((end-line (had-cr)
             (setf max-line (max max-line (if had-cr (1- line-length) line-length))
                   line-length 0)
             (incf lines)))
      (loop for char across text
            do (if (char= char #\Newline)
                   (end-line (and previous (char= previous #\Return)))
                   (incf line-length))
               (if (%ascii-whitespace-char-p char)
                   (setf in-word nil)
                   (unless in-word (setf in-word t) (incf words)))
               (setf previous char))
      (when (plusp line-length) (end-line nil)))
    (list (cons "approx_tokens" (aitools.kernel.domain:approx-token-count (length text)))
          (cons "chars" (length text))
          (cons "bytes" byte-count)
          (cons "lines" lines)
          (cons "words" words)
          (cons "max_line_chars" max-line))))

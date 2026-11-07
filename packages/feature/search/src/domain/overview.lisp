;;;;
;;;; The pure parts of `overview`: build-file recognition and the per-language
;;;; tally.
(in-package #:aitools.search.domain)

(defun build-file-name-p (name)
  (some (lambda (pattern) (aitools.workspace.domain:wildmatch pattern name))
        aitools.data:*search-build-file-patterns*))

(defstruct (language-tally (:constructor make-language-tally ()) (:copier nil) (:predicate nil))
  (table (make-hash-table :test 'equal) :type hash-table :read-only t))

(defun tally-file (tally language lines bytes)
  (let ((row (or (gethash language (language-tally-table tally))
                 (setf (gethash language (language-tally-table tally)) (list 0 0 0)))))
    (incf (first row))
    (incf (second row) lines)
    (incf (third row) bytes)))

(defun language-tally-rows (tally)
  "Rows (LANGUAGE FILES LINES BYTES), most lines first; ties by bytes, then
by name, so the order is deterministic."
  (sort (loop for language being the hash-keys of (language-tally-table tally)
                using (hash-value (files lines bytes))
              collect (list language files lines bytes))
        (lambda (a b)
          (cond ((/= (third a) (third b)) (> (third a) (third b)))
                ((/= (fourth a) (fourth b)) (> (fourth a) (fourth b)))
                (t (string< (first a) (first b)))))))

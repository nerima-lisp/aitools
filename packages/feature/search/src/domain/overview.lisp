;;;; packages/feature/search/src/domain/overview.lisp
;;;;
;;;; The pure parts of `overview`: git facts from the bytes of `.git` files
;;;; (no scanning command starts a git process), build-file
;;;; recognition, and the per-language tally.
(in-package #:aitools.search.domain)

(defun %trim (text)
  (string-trim '(#\Space #\Tab #\Return #\Newline) text))

(defun parse-head-file (text)
  "(VALUES branch sha ref) from the text of a HEAD file. A symbolic ref
`ref: refs/heads/<branch>` gives the branch, a NIL sha, and the full ref
name to resolve; a detached HEAD gives only the sha."
  (let ((text (%trim text)))
    (if (and (> (length text) 5) (string= "ref: " text :end2 5))
        (let* ((ref (%trim (subseq text 5)))
               (prefix "refs/heads/"))
          (values (if (and (> (length ref) (length prefix)) (string= prefix ref :end2 (length prefix)))
                      (subseq ref (length prefix))
                      ref)
                  nil
                  ref))
        (values nil (and (plusp (length text)) text) nil))))

(defun packed-ref-sha (text ref)
  "The object id REF has in the text of a packed-refs file, or NIL."
  (with-input-from-string (in text)
    (loop for line = (read-line in nil)
          while line
          do (let ((space (position #\Space line)))
               (when (and space (not (find (char line 0) "#^"))
                          (string= (%trim (subseq line (1+ space))) ref))
                 (return (subseq line 0 space)))))))

(defun build-file-name-p (name)
  (some (lambda (pattern) (aitools.workspace.domain:wildmatch pattern name))
        aitools.data:*search-build-file-patterns*))

(defstruct (language-tally (:constructor make-language-tally ()) (:copier nil))
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

;;;; packages/feature/edit/src/domain/split.lisp
;;;;
;;;; `split`: cut a file's bytes into pieces by line count, before
;;;; each line matching a pattern, or by byte count. Pieces are byte ranges
;;;; of the original, so the concatenation of the pieces is the file.
(in-package #:aitools.edit.domain)

(defstruct (split-piece (:constructor make-split-piece (start end start-line lines)) (:copier nil))
  ;; byte range [START, END) of the source, its first line (1-based), and
  ;; the number of lines it starts (a piece cut mid-line counts that line)
  (start 0 :type (integer 0) :read-only t)
  (end 0 :type (integer 0) :read-only t)
  (start-line 1 :type (integer 1) :read-only t)
  (lines 0 :type (integer 0) :read-only t))

(defun %line-starts (octets)
  "Byte offsets where each line of OCTETS starts."
  (cons 0 (loop for index = (position 10 octets) then (position 10 octets :start (1+ index))
                while (and index (< (1+ index) (length octets)))
                collect (1+ index))))

(defun %pieces-from-cuts (octets cuts)
  "Pieces between successive byte offsets in CUTS (sorted, starting at 0)."
  (let* ((starts (coerce (%line-starts octets) 'simple-vector))
         (bounds (append cuts (list (length octets)))))
    (flet ((line-at (offset)
             (1+ (or (position-if (lambda (start) (<= start offset)) starts :from-end t) 0))))
      (loop for (start end) on bounds
            while end
            when (< start end)
              collect (let ((first (line-at start)))
                        (make-split-piece start end first (1+ (- (line-at (1- end)) first))))))))

(defun split-by-lines (octets count)
  (let ((starts (%line-starts octets)))
    (%pieces-from-cuts octets (loop for start in starts for index from 0
                                    when (zerop (mod index count)) collect start))))

(defun split-by-bytes (octets count)
  (%pieces-from-cuts octets (loop for start from 0 below (max 1 (length octets)) by count collect start)))

(defun split-at-matches (octets lines regex)
  "Cut before every line of LINES (the decoded lines of OCTETS, one per
line start) that REGEX matches; the first piece holds any lines before the
first match."
  (let ((starts (%line-starts octets)))
    (%pieces-from-cuts octets
                       (cons 0 (loop for start in starts
                                     for line across lines
                                     when (and (plusp start) (cl-regex-kit:scan regex line))
                                       collect start)))))

(defun split-piece-name (prefix index digits)
  (format nil "~A~v,'0D" prefix digits index))

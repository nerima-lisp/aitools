;;;; packages/feature/edit/src/domain/old-match.lisp
;;;;
;;;; `edit --old`: an exact search of the logical
;;;; text first, then a line-wise search that ignores leading and trailing
;;;; whitespace, and on failure the three most similar places in the file,
;;;; ranked by Levenshtein distance.
(in-package #:aitools.edit.domain)

(defparameter +similarity-text-limit+ 400
  "Characters of each side compared when ranking candidates; bounds the
distance table so a long --old or a long line cannot make ranking quadratic
in the file size.")

(defparameter +similarity-window-limit+ 50000
  "Windows ranked at most; past it the file is sampled from the start.")

(defun levenshtein-distance (a b)
  (let* ((n (length a)) (m (length b))
         (previous (make-array (1+ m) :element-type 'fixnum))
         (current (make-array (1+ m) :element-type 'fixnum)))
    (dotimes (j (1+ m)) (setf (aref previous j) j))
    (dotimes (i n (aref previous m))
      (setf (aref current 0) (1+ i))
      (dotimes (j m)
        (setf (aref current (1+ j))
              (min (1+ (aref previous (1+ j)))
                   (1+ (aref current j))
                   (+ (aref previous j) (if (char= (char a i) (char b j)) 0 1)))))
      (rotatef previous current))))

(defun %whitespace-p (char)
  (member char '(#\Space #\Tab #\Return #\Page)))

(defun trim-whitespace (string)
  (string-trim '(#\Space #\Tab #\Return #\Page) string))

(defun leading-whitespace (string)
  (subseq string 0 (or (position-if-not #'%whitespace-p string) (length string))))

(defun %clip (string)
  (if (> (length string) +similarity-text-limit+) (subseq string 0 +similarity-text-limit+) string))

(defun similar-windows (lines needle-lines &key (limit 3))
  "Up to LIMIT (line-number . text) windows of LINES (a vector of strings)
most similar to NEEDLE-LINES (strings), whitespace-trimmed, best first;
ties go to the earlier line."
  (let* ((width (max 1 (length needle-lines)))
         (needle (%clip (format nil "~{~A~^~%~}" (mapcar #'trim-whitespace needle-lines))))
         (count (length lines))
         (scored '()))
    (when (plusp count)
      (loop for start from 0 to (max 0 (- count width))
            repeat +similarity-window-limit+
            do (let* ((end (min count (+ start width)))
                      (window (%clip (format nil "~{~A~^~%~}"
                                             (loop for index from start below end
                                                   collect (trim-whitespace (aref lines index)))))))
                 (push (list (levenshtein-distance needle window) (1+ start)
                             (format nil "~{~A~^~%~}" (coerce (subseq lines start end) 'list)))
                       scored))))
    (loop for (nil line text) in (sort scored (lambda (a b)
                                                (or (< (first a) (first b))
                                                    (and (= (first a) (first b)) (< (second a) (second b))))))
          repeat limit
          collect (cons line text))))

(defun %all-occurrences (needle haystack)
  (loop for position = (search needle haystack) then (search needle haystack :start2 (1+ position))
        while position
        collect position))

(defun %whitespace-windows (lines needle-lines)
  "Start indices where NEEDLE-LINES match LINES line by line after trimming."
  (let ((trimmed (mapcar #'trim-whitespace needle-lines))
        (width (length needle-lines))
        (count (length lines)))
    (when (plusp width)
      (loop for start from 0 to (- count width)
            when (loop for expected in trimmed
                       for index from start
                       always (string= expected (trim-whitespace (aref lines index))))
              collect start))))

(defun find-old/k (document old &key on-exact on-whitespace on-ambiguous on-no-match)
  "Locate OLD (non-empty) in DOCUMENT and call exactly one continuation:
  ON-EXACT (start end)            logical-text offsets of the one exact match
  ON-WHITESPACE (start end)       0-based line span [START, END) of the one
                                  whitespace-insensitive line match
  ON-AMBIGUOUS (matches)          (line-number . text) of every match
  ON-NO-MATCH (candidates)        (line-number . text) of up to 3 similar places"
  (declare (type function on-exact on-whitespace on-ambiguous on-no-match))
  (let* ((text (document-logical-text document))
         (lines (text-document-lines document))
         (offsets (document-line-offsets document))
         (exact (%all-occurrences old text)))
    (flet ((line-of (offset) (1+ (offset-line-index offsets offset))))
      (cond
        ((= (length exact) 1)
         (funcall on-exact (first exact) (+ (first exact) (length old))))
        (exact
         (funcall on-ambiguous (mapcar (lambda (offset)
                                         (let ((line (line-of offset)))
                                           (cons line (aref lines (1- line)))))
                                       exact)))
        (t
         (let* ((needle-lines (content-lines old))
                (windows (%whitespace-windows lines needle-lines)))
           (cond
             ((and windows (null (rest windows)))
              (funcall on-whitespace (first windows) (+ (first windows) (length needle-lines))))
             (windows
              (funcall on-ambiguous (mapcar (lambda (start) (cons (1+ start) (aref lines start))) windows)))
             (t (funcall on-no-match (similar-windows lines needle-lines))))))))))

(defun reindent-lines (new-lines old-indent file-indent)
  "NEW-LINES moved from the indentation of --old's first line (OLD-INDENT)
to that of the file's matched line (FILE-INDENT): each line loses as much of
OLD-INDENT as it starts with and gains FILE-INDENT. Blank lines stay blank."
  (mapcar (lambda (line)
            (if (zerop (length (trim-whitespace line)))
                ""
                (let ((shared (mismatch old-indent line)))
                  (concatenate 'string file-indent (subseq line (min shared (length (leading-whitespace line))))))))
          new-lines))

(defun first-indent (lines)
  "Leading whitespace of the first non-blank line of LINES, or \"\"."
  (let ((line (find-if (lambda (line) (plusp (length (trim-whitespace line)))) lines)))
    (if line (leading-whitespace line) "")))

(defun apply-old-edit (document old new &key on-edited on-ambiguous on-no-match)
  "`edit --old OLD --new NEW` on DOCUMENT: ON-EDITED (document strategy
first-line) with STRATEGY :EXACT or :WHITESPACE and the 1-based first line
of the match; the other continuations as for FIND-OLD/K."
  (declare (type function on-edited))
  (find-old/k document old
              :on-exact
              (lambda (start end)
                (let* ((text (document-logical-text document))
                       (offsets (document-line-offsets document))
                       (first (offset-line-index offsets start))
                       (last (offset-line-index offsets (max start (1- end))))
                       (new-text (concatenate 'string (subseq text 0 start) new (subseq text end))))
                  (funcall on-edited
                           (document-with-logical-text document new-text first
                                                       (- (document-line-count document) (1+ last)))
                           :exact (1+ first))))
              :on-whitespace
              (lambda (start end)
                (let* ((old-lines (content-lines old))
                       (new-lines (reindent-lines (content-lines new) (first-indent old-lines)
                                                  (leading-whitespace (document-line document start)))))
                  (funcall on-edited (document-replace-lines document start end new-lines)
                           :whitespace (1+ start))))
              :on-ambiguous on-ambiguous
              :on-no-match on-no-match))

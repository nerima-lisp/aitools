(in-package #:aitools.protocol.domain)

;;; PEM private keys. A command that returns a file one line per string
;;; (`read`, search blocks, `run` output, diff hunks) splits a key block
;;; across strings, so these are found by one pass over every line of every
;;; string of one sequence rather than string by string. A caller passes one
;;; sequence per text (one JSON array of lines, or one string): block state
;;; must not run on into an unrelated field or another file's lines.

(defun %pem-body-line-p (text start end)
  (flet ((blank-p (char) (member char '(#\Space #\Tab #\Return))))
    (let ((first (position-if-not #'blank-p text :start start :end end))
          (last (position-if-not #'blank-p text :start start :end end :from-end t)))
      (and first
           (loop for index from first to last
                 always (let ((char (char text index)))
                          (or (char<= #\A char #\Z) (char<= #\a char #\z) (char<= #\0 char #\9)
                              (find char "+/="))))))))

(defun %pem-header-line-p (text start end)
  "True for an RFC 1421 header such as `Proc-Type: 4,ENCRYPTED`."
  (let ((colon (position #\: text :start start :end end)))
    (and colon (> colon start)
         (loop for index from start below colon
               always (let ((char (char text index))) (or (alphanumericp char) (char= char #\-)))))))

(defun %pem-private-key-spans (texts)
  "(VALUES SPANS BLOCKS). SPANS holds, for each string of the vector TEXTS,
the (START . END) line spans that belong to a PEM private key. A BEGIN line
opens a block that runs to its END line, or to the end of TEXTS when the END
line is not in the output; inside it, base64 and header lines are masked and
other lines (a path between two search blocks) are kept. An END line with no
BEGIN before it takes the base64 lines immediately preceding it. BLOCKS
counts each block once however many strings it spans."
  (let ((spans (make-array (length texts) :initial-element '()))
        (blocks 0)
        (open nil)
        (pending '()))
    (labels ((mask (index start end)
             (push (cons start end) (aref spans index)))
           (visit-line (index text start end)
             (let ((dashes (search "-----" text :start2 start :end2 end)))
               (multiple-value-bind (begin-start begin-end)
                   (and dashes (%find-pem-private-key-marker "BEGIN" text dashes end))
                 (cond
                   (begin-start
                    (incf blocks)
                    (setf pending '()
                          open (not (%find-pem-private-key-marker "END" text begin-end end)))
                    (mask index begin-start end))
                   (open
                    (cond ((and dashes (%find-pem-private-key-marker "END" text dashes end))
                           (mask index start end)
                           (setf open nil))
                          ((or (%pem-body-line-p text start end) (%pem-header-line-p text start end))
                           (mask index start end))))
                   ((and dashes (%find-pem-private-key-marker "END" text dashes end))
                    (incf blocks)
                    (dolist (line pending) (apply #'mask line))
                    (setf pending '())
                    (mask index start end))
                   ((%pem-body-line-p text start end) (push (list index start end) pending))
                   (t (setf pending '())))))))
      (loop for text across texts
            for index from 0
            do (loop with start = 0
                     for newline = (position #\Newline text :start start)
                     do (visit-line index text start (or newline (length text)))
                     while newline
                     do (setf start (1+ newline)))))
    (values spans blocks)))

(defun %merge-pem-spans (text spans)
  "SPANS (line spans of TEXT, any order) sorted, with lines separated only by
line breaks joined, so a block inside one string becomes one marker."
  (let ((merged '()))
    (dolist (span (sort (copy-list spans) #'< :key #'car) (nreverse merged))
      (let ((top (first merged)))
        (if (and top (loop for index from (cdr top) below (car span)
                           always (member (char text index) '(#\Newline #\Return))))
            (setf (cdr top) (cdr span))
            (push (cons (car span) (cdr span)) merged))))))

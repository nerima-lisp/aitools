(in-package #:aitools.search.domain)

(defun %line-of (octets position)
  (1+ (count-newlines octets 0 position)))

(defun %starts-with-p (octets position marker)
  (and marker
       (<= (+ position (length marker)) (length octets))
       (loop for i from 0 below (length marker)
             always (= (aref octets (+ position i)) (char-code (char marker i))))))

(defun %sexp-end (octets start block-comment)
  "Offset of the parenthesis closing the first one at or after START,
skipping strings, `;` comments, BLOCK-COMMENT, and backslash escapes; the
last byte when it never closes."
  (let ((open (position 40 octets :start start))
        (length (length octets))
        (depth 0))
    (when (null open) (return-from %sexp-end start))
    (let ((i open))
      (loop while (< i length)
            do (let ((byte (aref octets i)))
                 (cond
                   ((= byte 92) (incf i))
                   ((= byte 34)
                    (loop do (incf i)
                          while (< i length)
                          do (case (aref octets i)
                               (92 (incf i))
                               (34 (return)))))
                   ((= byte 59) (setf i (1- (next-line-start octets i))))
                   ((%starts-with-p octets i (first block-comment))
                    (let ((close (search (map 'octets #'char-code (second block-comment)) octets
                                         :start2 (+ i (length (first block-comment))))))
                      (setf i (if close (+ close (length (second block-comment)) -1) length))))
                   ((= byte 40) (incf depth))
                   ((= byte 41)
                    (decf depth)
                    (when (zerop depth) (return-from %sexp-end i)))))
               (incf i)))
    (max start (1- length))))

(defun %blank-line-p (octets start end)
  (loop for i from start below end always (member (aref octets i) '(32 9 13))))

(defun %first-content-byte (octets start end)
  (loop for i from start below end
        unless (member (aref octets i) '(32 9 13)) return (aref octets i)))

(defun %brace-end (octets start line-comment block-comment)
  "Offset ending a brace-delimited definition that starts at START: the
brace closing its first `{`, or, before any `{`, a `;` at bracket depth
zero, or a line end whose next non-blank line does not open with `{`."
  (let ((length (length octets)) (depth 0) (brackets 0) (opened nil) (i start))
    (loop while (< i length)
          do (let ((byte (aref octets i)))
               (cond
                 ((= byte 34)
                  (loop do (incf i)
                        while (< i length)
                        do (case (aref octets i)
                             (92 (incf i))
                             (34 (return))
                             (10 (return)))))
                 ((%starts-with-p octets i line-comment)
                  (setf i (1- (next-line-start octets i))))
                 ((%starts-with-p octets i (first block-comment))
                  (let ((close (search (map 'octets #'char-code (second block-comment)) octets
                                       :start2 (+ i (length (first block-comment))))))
                    (setf i (if close (+ close (length (second block-comment)) -1) length))))
                 ((= byte 123) (setf opened t) (incf depth))
                 ((= byte 125)
                  (decf depth)
                  (when (and opened (<= depth 0)) (return-from %brace-end i)))
                 ((member byte '(40 91)) (incf brackets))
                 ((member byte '(41 93)) (decf brackets))
                 ((and (not opened) (= byte 59) (<= brackets 0)) (return-from %brace-end i))
                 ((and (not opened) (= byte 10) (<= brackets 0))
                  (let ((next (loop for pos = (1+ i) then (next-line-start octets pos)
                                    while (< pos length)
                                    unless (%blank-line-p octets pos (line-content-end octets pos))
                                      return (%first-content-byte octets pos (line-content-end octets pos)))))
                    (unless (eql next 123) (return-from %brace-end (max start (1- i))))))))
             (incf i))
    (max start (1- length))))

(defun %indentation (octets start end)
  (loop for i from start below end
        while (member (aref octets i) '(32 9))
        count t))

(defun %indent-end-line (octets line line-start)
  "The last non-blank line of the block that LINE (starting at LINE-START)
opens: lines after it until one indented no deeper than LINE."
  (let ((indent (%indentation octets line-start (line-content-end octets line-start)))
        (last line) (number line) (pos (next-line-start octets line-start)) (length (length octets)))
    (loop while (< pos length)
          do (incf number)
             (let ((end (line-content-end octets pos)))
               (unless (%blank-line-p octets pos end)
                 (when (<= (%indentation octets pos end) indent) (return))
                 (setf last number))
               (setf pos (next-line-start octets pos))))
    last))

(defun %fence-lines (octets)
  "Line numbers inside Markdown fenced code blocks (``` or ~~~), fences
included: headings there are not headings."
  (let* ((lines (make-hash-table))
         (opening nil)
         (number 0)
         (pos 0)
         (length (length octets)))
    (loop while (< pos length)
          do (incf number)
             (let* ((end (line-content-end octets pos))
                    (indent (loop for i from pos below end while (= (aref octets i) 32) count t))
                    (content (+ pos indent))
                    (marker (and (< content end) (aref octets content)))
                    (marker-p (member marker '(96 126)))
                    (run (and marker-p
                              (loop for i from content below end
                                    while (= (aref octets i) marker)
                                    count t)))
                    (fence-p (and run (>= run 3) (<= indent 3)
                                  (or (/= marker 96)
                                      (loop for i from (+ content run) below end
                                            never (= (aref octets i) 96)))))
                    (closing-p
                      (and opening fence-p
                           (= marker (car opening))
                           (>= run (cdr opening))
                           (loop for i from (+ content run) below end
                                 always (member (aref octets i) '(32 9))))))
               (when (or opening fence-p)
                 (setf (gethash number lines) t))
               (cond
                 (opening (when closing-p (setf opening nil)))
                 (fence-p (setf opening (cons marker run))))
               (setf pos (next-line-start octets pos))))
    lines))

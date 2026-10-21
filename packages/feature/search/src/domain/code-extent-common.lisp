(in-package #:aitools.search.domain)

(defun %line-of (octets position)
  (1+ (count-newlines octets 0 position)))

(defun %starts-with-p (octets position marker)
  (and marker
       (<= (+ position (length marker)) (length octets))
       (loop for i from 0 below (length marker)
             always (= (aref octets (+ position i)) (char-code (char marker i))))))

(defun %block-comment-end (octets start block-comment nested-p)
  (let* ((open (first block-comment))
         (close (second block-comment))
         (open-length (length open))
         (close-length (length close))
         (length (length octets))
         (depth 1)
         (i (+ start open-length)))
    (loop while (< i length)
          do (cond
               ((and nested-p (%starts-with-p octets i open))
                (incf depth)
                (incf i open-length))
               ((%starts-with-p octets i close)
                (decf depth)
                (incf i close-length)
                (when (zerop depth)
                  (return-from %block-comment-end (1- i))))
               (t (incf i))))
    (max start (1- length))))

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
                    (setf i (%block-comment-end
                             octets i block-comment
                             (string= (first block-comment) "#|"))))
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

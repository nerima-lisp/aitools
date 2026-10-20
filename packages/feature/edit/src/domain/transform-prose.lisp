(in-package #:aitools.edit.domain)

(defun %blank-p (line)
  (zerop (length (trim-whitespace line))))

(defun %wrap-line (line columns)
  "LINE broken at spaces into lines of at most COLUMNS characters where
possible; continuation lines repeat LINE's indentation."
  (if (<= (length line) columns)
      (list line)
      (let* ((indent (leading-whitespace line))
             (words (remove "" (%split-on #\Space (subseq line (length indent))) :test #'string=))
             (lines '())
             (current nil))
        (dolist (word words)
          (if (and current (> (+ (length current) 1 (length word)) columns))
              (progn
                (push current lines)
                (setf current (concatenate 'string indent word)))
              (setf current (if current
                                (concatenate 'string current " " word)
                                (concatenate 'string indent word)))))
        (when current
          (push current lines))
        (nreverse lines))))

(defun %fence-line-p (line)
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (or (and (>= (length trimmed) 3) (string= trimmed "```" :end1 3))
        (and (>= (length trimmed) 3) (string= trimmed "~~~" :end1 3)))))

(defun %map-outside-fences (lines function)
  "Split LINES into runs outside and inside Markdown code fences; FUNCTION
maps each outside run (a list) to its replacement, fenced runs (fence lines
included) are kept verbatim."
  (let ((result '())
        (run '())
        (in-fence nil))
    (flet ((flush ()
             (when run
               (setf result (append (reverse (funcall function (nreverse run))) result))
               (setf run '()))))
      (dolist (line lines)
        (cond
          ((%fence-line-p line)
            (unless in-fence
              (flush))
            (push line result)
            (setf in-fence (not in-fence)))
          (in-fence (push line result))
          (t (push line run))))
      (flush))
    (nreverse result)))

(defun %reflow (lines columns)
  (let ((result '())
        (paragraph '()))
    (flet ((flush ()
             (when paragraph
               (let* ((ordered (nreverse paragraph))
                      (indent (leading-whitespace (first ordered)))
                      (joined (format nil "~A~{~A~^ ~}" indent (mapcar #'trim-whitespace ordered))))
                 (setf result (append (reverse (%wrap-line joined columns)) result)))
               (setf paragraph '()))))
      (dolist (line lines)
        (if (%blank-p line)
            (progn
              (flush)
              (push line result))
            (push line paragraph)))
      (flush))
    (nreverse result)))

(defun %comment-lines (lines language)
  (let ((marker (aitools.text.domain:language-line-comment language))
        (block (aitools.text.domain:language-block-comment language))
        (column
         (loop
           for
           line
           in
           lines
           unless
           (%blank-p line)
           minimize
           (length (leading-whitespace line)))))
    (mapcar
     (lambda (line)
       (cond
         ((%blank-p line) line)
         (marker (concatenate 'string (subseq line 0 column) marker " " (subseq line column)))
         (t (concatenate 'string (subseq line 0 column) (first block) " " (subseq line column) " " (second block)))))
     lines)))

(defun %strip-prefix (string prefix)
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))
       (subseq string (length prefix))))

(defun %uncomment-lines (lines language)
  (let ((marker (aitools.text.domain:language-line-comment language))
        (block (aitools.text.domain:language-block-comment language)))
    (mapcar
     (lambda (line)
       (let* ((indent (leading-whitespace line))
              (body (subseq line (length indent))))
         (flet ((drop-space (text)
                  (if (and (plusp (length text)) (char= (char text 0) #\Space))
                      (subseq text 1)
                      text)))
           (cond
             ((and marker (%strip-prefix body marker))
              (concatenate 'string indent (drop-space (%strip-prefix body marker))))
             ((and block
                   (%strip-prefix body (first block))
                   (let ((trimmed (string-right-trim '(#\Space #\Tab) body)))
                     (and (>= (length trimmed) (+ (length (first block)) (length (second block))))
                          (string= (second block) trimmed :start2 (- (length trimmed) (length (second block)))))))
              (let* ((trimmed (string-right-trim '(#\Space #\Tab) body))
                     (inner (subseq trimmed (length (first block)) (- (length trimmed) (length (second block))))))
                (concatenate 'string indent (string-right-trim " " (drop-space inner)))))
             (t line)))))
     lines)))

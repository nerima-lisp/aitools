;;;; packages/feature/vcs/src/domain/blame.lisp
;;;;
;;;; `git blame` output from `git blame --porcelain`. Porcelain prints a commit's
;;;; author headers only the first time that commit appears, so the parser
;;;; keeps a per-commit table and every later group reuses it.
(in-package #:aitools.vcs.domain)

(defun %prefixed-value (line prefix)
  (let ((length (length prefix)))
    (when (and (>= (length line) length) (string= prefix line :end2 length))
      (subseq line length))))

(defun %strip-carriage-return (text)
  (let ((length (length text)))
    (if (and (plusp length) (char= (char text (1- length)) #\Return))
        (subseq text 0 (1- length))
        text)))

(defun map-blame-lines (text emit)
  "Call EMIT with (N SHA AUTHOR DATE TEXT) for each line of TEXT, a
`git blame --porcelain` document, in final-file order. N is the line number
in the blamed file; TEXT excludes the line terminator. EMIT returning :STOP
ends the walk."
  (declare (type string text) (type function emit))
  (let ((commits (make-hash-table :test 'equal))
        (sha nil) (final-line nil) (start 0) (length (length text)))
    (loop while (< start length)
          do (let* ((end (or (position #\Newline text :start start) length))
                    (line (subseq text start end)))
               (setf start (1+ end))
               (cond
                 ((null sha)
                  (let* ((first-space (position #\Space line))
                         (second-space (and first-space (position #\Space line :start (1+ first-space))))
                         (third-space (and second-space (position #\Space line :start (1+ second-space)))))
                    (unless second-space
                      (error "malformed blame header: ~S" line))
                    (setf sha (subseq line 0 first-space)
                          final-line (parse-integer line :start (1+ second-space) :end third-space))
                    (unless (gethash sha commits)
                      (setf (gethash sha commits) (list :author "" :time "0" :tz "+0000")))))
                 ((and (plusp (length line)) (char= (char line 0) #\Tab))
                  (let ((info (gethash sha commits)))
                    (when (eq :stop (funcall emit final-line sha (getf info :author)
                                             (epoch-seconds-to-iso8601 (parse-integer (getf info :time))
                                                                       (getf info :tz))
                                             (%strip-carriage-return (subseq line 1))))
                      (return))
                    (setf sha nil)))
                 (t
                  (let ((info (gethash sha commits)))
                    (let ((author (%prefixed-value line "author "))
                          (time (%prefixed-value line "author-time "))
                          (tz (%prefixed-value line "author-tz ")))
                      (cond (author (setf (getf info :author) author))
                            (time (setf (getf info :time) time))
                            (tz (setf (getf info :tz) tz))))
                    (setf (gethash sha commits) info))))))))

(defun blame-line (n sha author date text)
  (json-object-from-alist (list (cons "n" n) (cons "sha" sha) (cons "author" author) (cons "date" date)
                                (cons "text" text))))

;;;; packages/feature/vcs/src/domain/log.lisp
;;;;
;;;; `git log`. The log is requested as `git log -z --date=format:%z
;;;; --format=<*LOG-FORMAT*>`: every field, including the last, ends in a NUL,
;;;; so a subject containing any character but NUL cannot shift a record.
;;;; The date is rebuilt from epoch seconds plus the author's own offset, the
;;;; same way blame dates are, so both commands render dates identically.
(in-package #:aitools.vcs.domain)

(defparameter *log-format* "%H%x00%an%x00%at%x00%ad%x00%s"
  "Five NUL-separated fields per commit: sha, author name, author epoch
seconds, author offset (`%ad` under `--date=format:%z`), subject.")

(defun map-log-records (text emit)
  "Call EMIT with (SHA AUTHOR DATE SUBJECT) for each commit in TEXT, a
`git log -z` document in *LOG-FORMAT*. EMIT returning :STOP ends the walk.
A trailing incomplete record signals an error: the output was cut short."
  (declare (type string text) (type function emit))
  (let ((start 0) (length (length text)) (fields (make-array 5)) (count 0))
    (loop while (< start length)
          do (let ((end (or (position (code-char 0) text :start start)
                            (error "git log output ends inside a record"))))
               (setf (svref fields count) (subseq text start end)
                     start (1+ end))
               (incf count)
               (when (= count 5)
                 (setf count 0)
                 (when (eq :stop (funcall emit (svref fields 0) (svref fields 1)
                                          (epoch-seconds-to-iso8601 (parse-integer (svref fields 2))
                                                                    (svref fields 3))
                                          (svref fields 4)))
                   (return)))))
    (unless (zerop count)
      (error "git log output ends inside a record"))))

(defun log-item (sha author date subject)
  (json-object-from-alist (list (cons "sha" sha) (cons "author" author) (cons "date" date) (cons "subject" subject))))

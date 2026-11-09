(in-package #:aitools.store.application)

(defun %tx-commit-symlink-refuse (path)
  (error (quote store-refusal)
         :code
         "refusal.outside-workspace"
         :message
         (format nil
                 "Symlink ~A has an unsafe target or crosses the workspace boundary"
                 path)))

(defun %tx-commit-symlink-components (path)
  (loop with start = 0
        for
        end = (position #\/ path :start start)
        for component = (subseq path start end)
        unless (zerop (length component))
          collect component
        while
        end
        do (setf start (1+ end))))

(defun %tx-commit-symlink-unsafe-target-p (target)
  (or (not (stringp target))
      (zerop (length target))
      (char= (char target 0) #\/)
      (position (code-char 92) target)
      (position (code-char 0) target)
      (and (> (length target) 1)
           (alpha-char-p (char target 0))
           (char= (char target 1) #\:))))

(defun %tx-commit-symlink-enter (checker cursor component path)
  (multiple-value-bind (next status) (funcall checker :enter cursor component)
    (if (eq status :clear)
        next
        (%tx-commit-symlink-refuse path))))

(defun %tx-commit-symlink-parent (checker cursor path)
  (multiple-value-bind (next status) (funcall checker :parent cursor)
    (if (eq status :clear)
        next
        (%tx-commit-symlink-refuse path))))

(defun %tx-commit-symlink-path-components (checker cursor components path)
  (dolist (component components cursor)
    (setf cursor (%tx-commit-symlink-enter checker cursor component path))))

(defun %tx-commit-symlink-target-components (checker cursor components path)
  (dolist (component components cursor)
    (setf cursor (cond
                   ((string= component ".") cursor)
                   ((string= component "..")
                    (%tx-commit-symlink-parent checker cursor path))
                   (t (%tx-commit-symlink-enter checker cursor component path))))))

(defun %tx-commit-symlink-entries (index)
  (sort
   (loop for entry being the hash-values of (tx-index-paths index)
         when (eq (entry-state-kind (tx-path-staged entry)) :symlink)
           collect entry)
   (function string<)
   :key
   (function tx-path-path)))

(defun %recheck-tx-symlink-path (checker path target)
  (when (%tx-commit-symlink-unsafe-target-p target)
    (%tx-commit-symlink-refuse path))
  (let ((cursor (funcall checker :start)))
    (unwind-protect
        (progn
          (setf cursor (%tx-commit-symlink-path-components checker
                                                           cursor
                                                           (%tx-commit-symlink-components
                                                            (parent-relative-path
                                                             path))
                                                           path))
          (setf cursor (%tx-commit-symlink-target-components checker
                                                             cursor
                                                             (%tx-commit-symlink-components
                                                              target)
                                                             path)))
      (funcall checker :finish cursor))))

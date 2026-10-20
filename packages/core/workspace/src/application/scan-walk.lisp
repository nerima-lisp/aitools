(in-package #:aitools.workspace.application)

(defun scan-filter-reason (entry path explicit filter lang skip-larger-than newer)
  "Return the first filter reason for ENTRY, or NIL when it is accepted."
  (cond
   ((and skip-larger-than (eq (workspace-entry-kind entry) :file)
         (> (workspace-entry-size entry) skip-larger-than))
    :too-large)
   ((and newer (<= (workspace-entry-mtime entry) newer))
    :too-old)
   ((and (not explicit) lang
         (not (and (eq (workspace-entry-kind entry) :file) (funcall lang path))))
    :language)
   ((and (not explicit) (not (glob-filter-accepts-p filter path)))
    :glob)))

(defstruct (scan-run
              (:constructor %make-scan-run
                  (host root context overlay filter lang skip-larger-than newer
                   work emit skip complete source starts))
              (:copier nil))
    host root context overlay filter lang skip-larger-than newer
    work emit skip complete source starts
    (batch nil)
    (count 0 :type integer)
    (stopped-p nil :type boolean)
    (completion-result nil))

(defun %scan-start-entries (host root context overlay paths)
  "(VALUES entries reason path) after validating the requested starts."
  (multiple-value-bind (starts bad-path) (%starts host root paths)
    (if bad-path
        (values nil :outside-root bad-path)
      (loop for start in starts
            for entry = (%lookup-entry host context overlay start)
            unless entry
            return (values nil :not-found start)
            collect (cons start entry)))))

(defun %scan-deliver (run candidate result)
  (when (eq (funcall (scan-run-emit run) candidate result) :stop)
    (setf (scan-run-stopped-p run) t
          (scan-run-completion-result run)
          (multiple-value-list (funcall (scan-run-complete run) (scan-run-source run) t)))))

(defun %scan-flush (run mapper)
  (when (scan-run-batch run)
    (let ((items (nreverse (scan-run-batch run))))
      (setf (scan-run-batch run) nil
            (scan-run-count run) 0)
      (loop for candidate in items
            for result in (funcall mapper (scan-run-work run) items)
            until (scan-run-stopped-p run)
            do (%scan-deliver run candidate result)))))

(defun %scan-offer (run mapper candidate explicit)
  (let* ((entry (scan-entry-entry candidate))
         (path (scan-entry-path candidate))
         (reason (scan-filter-reason
                  entry path explicit (scan-run-filter run)
                  (scan-run-lang run) (scan-run-skip-larger-than run)
                  (scan-run-newer run))))
    (cond
     (reason
      (when (eq reason :too-large)
        (funcall (scan-run-skip run) candidate :too-large)))
     ((scan-run-work run)
      (push candidate (scan-run-batch run))
      (when (>= (incf (scan-run-count run)) *scan-batch-size*)
        (%scan-flush run mapper)))
     (t
      (%scan-deliver run candidate nil)))))

(defun %scan-walk-entry (run mapper absolute root-relative top-relative stack
                             inside-ignored entry)
  (let ((name (workspace-entry-name entry)))
    (unless (%always-skipped-name-p name)
      (let* ((directory-p (eq (workspace-entry-kind entry) :directory))
             (child-top (join-path top-relative name))
             (child-root (join-path root-relative name))
             (child-absolute (join-path absolute name))
             (tracked (%tracked-p (scan-run-context run) child-top directory-p))
             (ignored (or inside-ignored
                          (%ignored-p (scan-run-context run)
                                      stack child-top directory-p))))
        (cond
          (directory-p
           (unless (and ignored (not tracked))
             (unless ignored
               (%scan-offer
                run mapper
                (%make-scan-entry child-root child-absolute entry nil) nil))
             (unless (scan-run-stopped-p run)
               (%scan-walk-directory
                run mapper child-absolute child-root child-top stack ignored))))
          ((or (not ignored) tracked)
           (%scan-offer
            run mapper
            (%make-scan-entry child-root child-absolute entry tracked)
            nil)))))))

(defun %scan-walk-directory (run mapper absolute root-relative top-relative
                                 stack inside-ignored)
  (multiple-value-bind (entries readable)
      (%list-entries (scan-run-host run) (scan-run-overlay run) absolute root-relative)
    (unless readable
      (funcall
       (scan-run-skip run)
       (%make-scan-entry
        root-relative absolute
        (make-workspace-entry :name (path-basename root-relative) :kind :directory)
        nil)
       :unreadable))
    (let ((own
           (and (not inside-ignored)
                (%directory-ignore-list
                 (scan-run-host run) (scan-run-context run) (scan-run-overlay run)
                 top-relative
                 (find ".gitignore" entries :key #'workspace-entry-name :test #'string=)))))
      (when own (push own stack)))
    (loop for entry in entries
          while (not (scan-run-stopped-p run))
          do (%scan-walk-entry
              run mapper absolute root-relative top-relative stack inside-ignored entry))))

(defun %scan-run-starts (run mapper)
  (loop for start-entry in (scan-run-starts run)
        while (not (scan-run-stopped-p run))
        do (let* ((start (car start-entry))
                  (entry (cdr start-entry))
                  (top-relative
                   (join-path (ignore-context-prefix (scan-run-context run)) start))
                  (absolute (join-path (workspace-root-path (scan-run-root run)) start)))
             (if (eq (workspace-entry-kind entry) :directory)
                 (%scan-walk-directory
                  run mapper absolute start top-relative
                  (%stack-above
                   (scan-run-host run) (scan-run-context run)
                   (scan-run-overlay run) top-relative)
                  nil)
               (%scan-offer
                run mapper
                (%make-scan-entry
                 start absolute entry
                 (%tracked-p (scan-run-context run) top-relative nil))
                t))))
  (unless (scan-run-stopped-p run)
    (%scan-flush run mapper))
  (if (scan-run-stopped-p run)
      (values-list (scan-run-completion-result run))
    (funcall (scan-run-complete run) (scan-run-source run) nil)))

(defun %run-scan (host root context overlay filter lang skip-larger-than newer
                       work emit skip complete source starts)
  (let ((run (%make-scan-run
              host root context overlay filter lang skip-larger-than newer
              work emit skip complete source starts)))
    (labels ((run-with-mapper (mapper)
                              (%scan-run-starts run mapper)))
      (declare (dynamic-extent #'run-with-mapper))
      (if work
          (host-call-with-ordered-mapper host #'run-with-mapper)
        (run-with-mapper nil)))))

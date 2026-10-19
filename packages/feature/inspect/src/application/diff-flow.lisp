;;;; packages/feature/inspect/src/application/diff-flow.lisp
;;;;
;;;; `diff`: two files (`diff -u`, `cmp`, `comm`), two directories
;;;; (recursive, by path then content), or one journal op's full diff
;;;; (`diff --op`, the target of a truncated diff's `next_commands`).
(in-package #:aitools.inspect.application)

(defun %limited (items limit)
  "(VALUES first-LIMIT-items truncated-p)."
  (if (> (length items) limit)
      (values (subseq items 0 limit) t)
      (values items nil)))

(defun %finish (on-ok on-partial truncated fields)
  (funcall
   (if truncated
       on-partial
       on-ok)
   (append fields (list (cons "truncated" (json-bool truncated))))))

;;; ------------------------------------------------------------ files
(defun %file-diff-fields (display-a display-b
                                    a
                                    b
                                    octets-a
                                    octets-b
                                    &key
                                    context
                                    output
                                    ignore-whitespace
                                    ignore-eol
                                    limit)
  "(VALUES fields truncated) comparing two regular files' bytes. DISPLAY-A and
DISPLAY-B are the reported paths (root-relative inside the workspace,
absolute outside) for the `a`/`b` fields; A and B are
the arguments as typed, kept as the unified-diff header labels."
  (let ((mode
         (list (cons "mode" output) (cons "a" display-a) (cons "b" display-b))))
    (if (or (binary-octets-p octets-a) (binary-octets-p octets-b))
        (values
         (append mode
                 (list (cons "binary" t)
                       (cons "identical" (json-bool (equalp octets-a octets-b)))))
         nil)
        (let ((keep-cr (not ignore-eol))
              (layout-a (detect-text-layout octets-a))
              (layout-b (detect-text-layout octets-b)))
          (multiple-value-bind (lines-a)
              (decode-text-lines octets-a :keep-cr keep-cr)
            (multiple-value-bind (lines-b)
                (decode-text-lines octets-b :keep-cr keep-cr)
              (if (string= output "set")
                  (%set-fields mode
                               lines-a
                               lines-b
                               ignore-whitespace
                               ignore-eol
                               limit)
                  (let* ((hunks
                          (compare-lines lines-a
                                         lines-b
                                         :context
                                         context
                                         :ignore-whitespace
                                         ignore-whitespace
                                         :ignore-eol
                                         ignore-eol
                                         :final-newline-a
                                         (text-layout-final-newline-p layout-a)
                                         :final-newline-b
                                         (text-layout-final-newline-p layout-b)))
                         (identical
                          (if (or ignore-whitespace ignore-eol)
                              (null hunks)
                              (equalp octets-a octets-b))))
                    (if (string= output "stat")
                        (multiple-value-bind (added deleted)
                            (hunk-line-counts hunks)
                          (values
                           (append mode
                                   (list
                                    (cons "identical" (json-bool identical))
                                    (cons "added" added)
                                    (cons "deleted" deleted)))
                           nil))
                        (multiple-value-bind (shown truncated)
                            (%limited hunks limit)
                          (values
                           (append mode
                                   (list
                                    (cons "identical" (json-bool identical))
                                    (cons "hunks" (length hunks))
                                    (cons "diff"
                                          (if shown
                                              (aitools.kernel.domain:render-hunks
                                               shown
                                               :path-a
                                               a
                                               :path-b
                                               b)
                                              ""))))
                           truncated)))))))))))

(defun %set-fields (mode lines-a lines-b ignore-whitespace ignore-eol limit)
  (multiple-value-bind (only-a only-b both)
      (compare-line-sets lines-a
                         lines-b
                         :ignore-whitespace
                         ignore-whitespace
                         :ignore-eol
                         ignore-eol)
    (multiple-value-bind (shown-a truncated-a) (%limited only-a limit)
      (multiple-value-bind (shown-b truncated-b) (%limited only-b limit)
        (values
         (append mode
                 (list
                  (cons "only_a"
                        (mapcar
                         (lambda (line)
                           (string-right-trim '(#\Return) line))
                         shown-a))
                  (cons "only_b"
                        (mapcar
                         (lambda (line)
                           (string-right-trim '(#\Return) line))
                         shown-b))
                  (cons "only_a_count" (length only-a))
                  (cons "only_b_count" (length only-b))
                  (cons "both_count" both)))
         (or truncated-a truncated-b))))))

;;; ------------------------------------------------------------ directories
(defun %tree-files (host directory)
  "Alist (relative-path . size) of the files below DIRECTORY, `.git` and
aitools temp files skipped, sorted by path."
  (let ((files '()))
    (labels ((walk (absolute prefix)
               (dolist (entry (host-list-directory host absolute))
                 (let ((name (workspace-entry-name entry)))
                   (unless
                       (or (string= name ".git")
                           (aitools-temporary-name-p name))
                     (let ((relative
                            (if prefix
                                (concatenate 'string prefix "/" name)
                                name)))
                       (case (workspace-entry-kind entry)
                         (:directory (walk (join-path absolute name) relative))
                         (t
                          (push (cons relative (workspace-entry-size entry))
                                files)))))))))
      (walk directory nil))
    (sort files #'string< :key #'car)))

(defun %directory-fields (context target-a target-b limit)
  (let* ((host (context-host context))
         (files-a (%tree-files host (file-target-real target-a)))
         (files-b (%tree-files host (file-target-real target-b))))
    (multiple-value-bind (removed added both)
        (compare-path-lists (mapcar #'car files-a) (mapcar #'car files-b))
      (let ((modified
             (remove-if
              (lambda (path)
                (and
                 (eql (cdr (assoc path files-a :test #'string=))
                      (cdr (assoc path files-b :test #'string=)))
                 (equalp
                  (source-read-octets (context-source context)
                                      (join-path (file-target-real target-a)
                                                 path))
                  (source-read-octets (context-source context)
                                      (join-path (file-target-real target-b)
                                                 path)))))
              both)))
        (multiple-value-bind (shown-added truncated-added)
            (%limited added limit)
          (multiple-value-bind (shown-removed truncated-removed)
              (%limited removed limit)
            (multiple-value-bind (shown-modified truncated-modified)
                (%limited modified limit)
              (values
               (list (cons "mode" "directory")
                     (cons "a" (target-display-path target-a))
                     (cons "b" (target-display-path target-b))
                     (cons "added" shown-added)
                     (cons "removed" shown-removed)
                     (cons "modified" shown-modified)
                     (cons "identical_count"
                           (- (length both) (length modified))))
               (or truncated-added truncated-removed truncated-modified)))))))))

(defun %compare-paths (context a b options on-ok on-partial on-error)
  (let ((target-a (probe-target context a))
        (target-b (probe-target context b)))
    (cond
      ((eq (file-target-kind target-a) :absent)
       (fail-missing context target-a on-error))
      ((eq (file-target-kind target-b) :absent)
       (fail-missing context target-b on-error))
      ((and (eq (file-target-kind target-a) :directory)
            (eq (file-target-kind target-b) :directory))
       (multiple-value-bind (fields truncated)
           (%directory-fields context target-a target-b (getf options :limit))
         (%finish on-ok on-partial truncated fields)))
      ((and (eq (file-target-kind target-a) :file)
            (eq (file-target-kind target-b) :file))
       (multiple-value-bind (octets-a problem-a)
           (read-target-octets context target-a)
         (multiple-value-bind (octets-b problem-b)
             (read-target-octets context target-b)
           (cond
             ((null octets-a)
              (fail-target-read context target-a on-error problem-a))
             ((null octets-b)
              (fail-target-read context target-b on-error problem-b))
             (t
              (multiple-value-bind (fields truncated)
                  (apply #'%file-diff-fields
                         (target-display-path target-a)
                         (target-display-path target-b)
                         a
                         b
                         octets-a
                         octets-b
                         options)
                (%finish on-ok on-partial truncated fields)))))))
      (t
       (fail on-error
             "argument.invalid"
             (format nil
                     "~A and ~A must both be files or both be directories"
                     a
                     b)
             :repairs
             (list
              (repair "describe"
                      "Check what each path is."
                      (command-line context (list "info" a)))))))))

;;; ------------------------------------------------------------ --op

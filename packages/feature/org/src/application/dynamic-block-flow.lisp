(in-package #:aitools.org.application)

(defun %org-agenda-paths (view globs)
  (when globs
    (let ((filter (aitools.workspace.domain:make-glob-filter globs)) (paths nil))
      (labels ((walk (directory)
                 (dolist (entry (aitools.store.application:view-directory-entries view directory))
                   (let ((path (if (zerop (length directory)) (car entry) (concatenate (quote string) directory "/" (car entry)))))
                     (case (cdr entry)
                       (:directory (when (and (not (member (car entry) (quote (".git" ".aitools")) :test (function string=)))
                                              (%org-glob-directory-p path globs)) (walk path)))
                       (:file (when (and (not (and (>= (length path) 4) (string-equal path ".gpg" :start1 (- (length path) 4))))
                                         (aitools.workspace.domain:glob-filter-accepts-p filter path))
                                (push path paths))))))))
        (walk ""))
      (nreverse paths))))

(defun %org-agenda-documents-preparer (boundaries globs)
  (lambda (view)
    (let ((documents nil))
      (dolist (path (%org-agenda-paths view globs) (values (list :agenda-documents (nreverse documents)) nil))
        (multiple-value-bind (document failure)
            (funcall (cl-boundary-kit:boundary-context-require boundaries :org-parse)
                     (aitools.store.application:view-read-file view path
                       :max-bytes (cl-boundary-kit:boundary-context-require boundaries :org-max-input-bytes))
                     :source-name path)
          (when failure (return (values nil failure)))
          (push (aitools.org.domain:org-document-engine-value document) documents))))))

(aitools.kernel.domain:define-flow
    (org-dynamic-block-update/k boundaries &key path block agenda-glob at tz dst-policy root lock-timeout dry-run display-argv expect-count expect-hash tx on-ok on-error on-partial)
  (declare (ignore expect-count expect-hash) (type function on-ok on-error on-partial))
  (%org-write-flow-call
   (%org-write-flow boundaries path "org.dynamic-block.update" :dynamic-block-update
                    (list :block block :agenda-glob agenda-glob :at at :tz tz :dst-policy dst-policy)
                    :prepare-options (%org-agenda-documents-preparer boundaries agenda-glob)
                    :root root :lock-timeout lock-timeout :dry-run dry-run :tx tx :display-argv display-argv)))

(defun %org-glob-directory-p (directory globs)
  (let ((includes (remove-if (lambda (glob) (and (plusp (length glob)) (char= (char glob 0) #\!))) globs))
        (parts (aitools.workspace.domain:path-components directory)))
    (or (null includes)
        (some (lambda (glob)
                (if (not (position #\/ glob))
                    t
                    (labels ((matches-prefix (patterns names)
                               (cond
                                 ((null names) (not (null patterns)))
                                 ((null patterns) nil)
                                 ((string= (car patterns) "**")
                                  (or (matches-prefix (cdr patterns) names)
                                      (matches-prefix patterns (cdr names))))
                                 ((aitools.workspace.domain:wildmatch (car patterns) (car names))
                                  (matches-prefix (cdr patterns) (cdr names)))
                                 (t nil))))
                      (matches-prefix (aitools.workspace.domain:path-components glob) parts))))
              includes))))

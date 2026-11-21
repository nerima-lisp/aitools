(in-package #:aitools.org.application)

(defun %org-agenda-paths (view globs)
  (when globs
    (let ((filter (aitools.workspace.domain:make-glob-filter globs))
          (org-only (aitools.workspace.domain:make-glob-filter '("*.org")))
          (paths nil))
      (labels ((walk (directory)
                 (dolist (entry (aitools.store.application:view-directory-entries view directory))
                   (let ((path (if (zerop (length directory)) (car entry) (concatenate (quote string) directory "/" (car entry)))))
                     (unless (aitools.workspace.domain:git-metadata-name-p (car entry))
                       (case (cdr entry)
                         (:directory (when (%org-glob-directory-p path globs) (walk path)))
                         (:file (when (and (aitools.workspace.domain:glob-filter-accepts-p org-only path)
                                           (aitools.workspace.domain:glob-filter-accepts-p filter path))
                                  (push path paths)))))))))
        (walk ""))
      (sort paths #'string<))))

(defun %org-agenda-document (boundaries view path)
  (let* ((limit (cl-boundary-kit:boundary-context-require boundaries :org-max-input-bytes))
         (octets (handler-case (aitools.store.application:view-read-file view path :max-bytes limit)
                   (aitools.store.application:store-byte-limit-error ()
                     (return-from %org-agenda-document
                       (values nil (aitools.org.domain:make-org-failure
                                    :kind :too-large
                                    :message (format nil "~A exceeds the Org input limit of ~D bytes" path limit))))))))
    (if (find 0 octets)
        (values nil (aitools.org.domain:make-org-failure
                     :kind :unsupported :message (format nil "~A: Org input contains a NUL byte" path)))
        (multiple-value-bind (document failure)
            (funcall (cl-boundary-kit:boundary-context-require boundaries :org-parse) octets :source-name path)
          (if failure
              (values nil (aitools.org.domain:make-org-failure
                           :kind (aitools.org.domain:org-failure-kind failure)
                           :message (format nil "~A: ~A" path (aitools.org.domain:org-failure-message failure))
                           :line (aitools.org.domain:org-failure-line failure)
                           :column (aitools.org.domain:org-failure-column failure)
                           :offset (aitools.org.domain:org-failure-offset failure)))
              (values (aitools.org.domain:org-document-engine-value document) nil))))))

;;; The loader runs only when the block asks for the agenda, so a block with
;;; another scope never reads the agenda files.
(defun %org-agenda-loader-preparer (boundaries globs)
  (lambda (view)
    (values
     (list :agenda-loader
           (lambda ()
             (let ((documents nil))
               (dolist (path (%org-agenda-paths view globs) (values (nreverse documents) nil))
                 (multiple-value-bind (document failure) (%org-agenda-document boundaries view path)
                   (when failure (return (values nil failure)))
                   (push document documents))))))
     nil)))

(aitools.kernel.domain:define-flow
    (org-dynamic-block-update/k boundaries &key path block agenda-glob at tz dst-policy root lock-timeout dry-run display-argv expect-count expect-hash tx on-ok on-error on-partial)
  (declare (ignore expect-count expect-hash) (type function on-ok on-error on-partial))
  (%org-write-flow-call
   (%org-write-flow boundaries path "org.dynamic-block.update" :dynamic-block-update
                    (list :block block :agenda-glob agenda-glob :at at :tz tz :dst-policy dst-policy)
                    :prepare-options (%org-agenda-loader-preparer boundaries agenda-glob)
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

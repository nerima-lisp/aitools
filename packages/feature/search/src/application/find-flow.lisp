;;;; packages/feature/search/src/application/find-flow.lisp
;;;;
;;;; `find`. One ignore-aware scan below the start path collects every
;;;; entry; directory totals for `--sizes` and emptiness for `--empty` are
;;;; computed over what that scan saw, so both follow the same ignore rules
;;;; and scan options as the listing itself. Filters apply after that, then
;;;; the ordering, then `--limit`.
(in-package #:aitools.search.application)

(defun %find-command-line (request limit)
  (destructuring-bind (&key pattern path type depth sort min-size max-size empty executable
                         output sizes glob lang no-ignore skip-larger-than newer tx &allow-other-keys)
      request
    (command-line
     (append (list "aitools" "find")
             (when pattern (list pattern))
             (when path (list path))
             (when type (list "--type" (kind-option-name type)))
             (when depth (list "--depth" (princ-to-string depth)))
             (unless (eq sort :path) (list "--sort" (string-downcase (symbol-name sort))))
             (when min-size (list "--min-size" min-size))
             (when max-size (list "--max-size" max-size))
             (when empty (list "--empty"))
             (when executable (list "--executable"))
             (when (eq output :tree) (list "--output" "tree"))
             (when sizes (list "--sizes"))
             (list "--limit" (princ-to-string limit))
             (loop for glob in glob append (list "--glob" glob))
             (when lang (list "--lang" lang))
             (when no-ignore (list "--no-ignore"))
             (when skip-larger-than (list "--skip-larger-than" skip-larger-than))
             (when newer (list "--newer" newer))
             (when tx (list "--tx" tx))))))

(defun kind-option-name (kind)
  (ecase kind (:file "file") (:directory "dir") (:symlink "symlink")))

(aitools.kernel.domain:define-flow
    (%parse-size-option/k text option &key on-value on-error)
  (if (null text)
      (aitools.kernel.domain:flow-continue :on-value nil)
      (handler-case
          (aitools.kernel.domain:flow-continue
           :on-value
           (aitools.kernel.domain:size-bytes
            (aitools.kernel.domain:parse-size text)))
        (aitools.kernel.domain:invalid-size-error ()
          (aitools.kernel.domain:flow-call
              (%argument-error (format nil "~A: not a size: ~A" option text) "find")
            (:on-error (code message &key repairs)
              (aitools.kernel.domain:flow-continue
               :on-error code message :repairs repairs)))))))

(aitools.kernel.domain:define-flow
    (%collect-entries session scan-options start &key on-collected on-error)
  "Scan below START (absolute) and call ON-COLLECTED with (entries
directory-sizes non-empty-directories ignore-source start-relative
start-kind): ENTRIES are the scan entries in path order."
  (let ((entries '()) (sizes (make-hash-table :test 'equal)) (occupied (make-hash-table :test 'equal))
        (start-relative (or (%relative session start) "")))
    (apply #'aitools.workspace.application:call-with-workspace-scan/k
           (%host session) (session-root session)
           :paths (list start)
           :on-error
           (lambda (reason path)
             (aitools.kernel.domain:flow-call
                 (scan-error "find" reason path)
               (:on-error (code message &key repairs)
                 (aitools.kernel.domain:flow-continue
                  :on-error code message :repairs repairs))))
           :emit (lambda (entry result)
                   (declare (ignore result))
                   (let ((path (aitools.workspace.application:scan-entry-path entry)))
                     (push entry entries)
                     (let ((parent (or (aitools.workspace.domain:path-parent path) "")))
                       (setf (gethash parent occupied) t))
                     (when (eq (aitools.workspace.application:scan-entry-kind entry) :file)
                       (loop for directory = (aitools.workspace.domain:path-parent path)
                               then (aitools.workspace.domain:path-parent directory)
                             while directory
                             do (incf (gethash directory sizes 0)
                                      (aitools.workspace.application:scan-entry-size entry))
                             until (string= directory start-relative))))
                   nil)
           :on-complete (lambda (source stopped)
                          (declare (ignore stopped))
                          (let* ((entries (nreverse entries))
                                 (own (find start-relative entries
                                            :key #'aitools.workspace.application:scan-entry-path :test #'string=)))
                            (aitools.kernel.domain:flow-continue
                             :on-collected entries sizes occupied source start-relative
                             (if own (aitools.workspace.application:scan-entry-kind own) :directory))))
           scan-options)))

(aitools.kernel.domain:define-flow
    (%find-results request entries sizes occupied source start-relative start-kind
                   &key min-size max-size on-ok on-partial)
  (destructuring-bind (&key pattern type depth sort empty executable output ((:sizes with-sizes)) limit
                       &allow-other-keys)
      request
    (let ((found '()))
      (dolist (entry entries)
        (let* ((path (aitools.workspace.application:scan-entry-path entry))
               (kind (aitools.workspace.application:scan-entry-kind entry))
               (size (case kind
                       (:directory (and with-sizes (gethash path sizes 0)))
                       (t (aitools.workspace.application:scan-entry-size entry))))
               (mode (aitools.workspace.application:scan-entry-mode entry)))
          (when (and (or (null depth) (<= (entry-depth start-relative path) depth))
                     (or (null type) (eq type kind))
                     (or (null pattern) (find-pattern-matches-p pattern path))
                     (or (null min-size) (and size (>= size min-size)))
                     (or (null max-size) (and size (<= size max-size)))
                     (or (not empty)
                         (case kind
                           (:file (zerop size))
                           (:directory (not (gethash path occupied)))
                           (t nil)))
                     (or (not executable) (and (eq kind :file) (logtest mode #o111))))
            (push (make-found-entry
                   path kind size mode (aitools.workspace.application:scan-entry-mtime entry))
                  found))))
      (let* ((found (nreverse found))
             (total (length found))
             (truncated (> total limit))
             (fields
               (append
                (list (cons "mode" (if (eq output :tree) "tree" "flat")))
                (if (eq output :tree)
                    (list (cons "tree" (build-find-tree start-relative start-kind found limit)))
                    (list (cons "items"
                                (mapcar #'found-entry-json
                                        (let ((sorted (sort-found-entries found sort)))
                                          (subseq sorted 0 (min limit total)))))))
                (list (cons "total" total)
                      (cons "ignore_source" (%ignore-source-name source))
                      (cons "truncated" (json-boolean truncated))))))
        (if truncated
            (aitools.kernel.domain:flow-continue
             :on-partial
             (%result-fields fields (list (%find-command-line request total))))
            (aitools.kernel.domain:flow-continue
             :on-ok (%result-fields fields)))))))

(aitools.kernel.domain:define-flow
    (find/k ports &rest request
            &key pattern path type depth (sort :path) min-size max-size empty executable
              (output :flat) sizes (limit 50) root tx glob lang no-ignore skip-larger-than newer
              on-ok on-partial on-error)
  "`find`. TYPE is :FILE, :DIRECTORY, :SYMLINK, or NIL; SORT :PATH, :MTIME,
or :SIZE; OUTPUT :FLAT or :TREE; MIN-SIZE and MAX-SIZE are size strings."
  (declare (ignore pattern type depth empty executable sizes))
  (declare (type function on-ok on-partial on-error))
  (let ((request (list* :sort sort :output output :limit limit request)))
    (aitools.kernel.domain:flow-call
        (%parse-size-option/k min-size "--min-size")
      (:on-value (min-bytes)
        (aitools.kernel.domain:flow-call
            (%parse-size-option/k max-size "--max-size")
          (:on-value (max-bytes)
            (aitools.kernel.domain:flow-call
                (call-with-session/k ports "find" :root root :tx tx)
              (:on-session (session)
                (aitools.kernel.domain:flow-call
                    (scan-options/k session "find" :glob glob :lang lang :no-ignore no-ignore
                                    :skip-larger-than skip-larger-than :newer newer)
                  (:on-options (options)
                    (aitools.kernel.domain:flow-call
                        (%collect-entries
                         session
                         ;; `find` lists large files rather than skipping them unless
                         ;; `--skip-larger-than` is given explicitly.
                         (list* :skip-larger-than
                                (and skip-larger-than (getf options :skip-larger-than)) options)
                         (first (or (%start-paths session (and path (list path)))
                                    (list (%root-path session)))))
                      (:on-collected (entries found-sizes occupied source start-relative start-kind)
                        (aitools.kernel.domain:flow-call
                            (%find-results request entries found-sizes occupied source
                                           start-relative start-kind
                                           :min-size min-bytes :max-size max-bytes)
                          (:on-ok (fields)
                            (aitools.kernel.domain:flow-continue :on-ok fields))
                          (:on-partial (fields)
                            (aitools.kernel.domain:flow-continue :on-partial fields))))
                      (:on-error (code message &key repairs)
                        (aitools.kernel.domain:flow-continue
                         :on-error code message :repairs repairs))))
                  (:on-error (code message &key repairs)
                    (aitools.kernel.domain:flow-continue
                     :on-error code message :repairs repairs))))
              (:on-error (code message &key repairs)
                (aitools.kernel.domain:flow-continue
                 :on-error code message :repairs repairs))))
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message :repairs repairs))))
      (:on-error (code message &key repairs)
        (aitools.kernel.domain:flow-continue
         :on-error code message :repairs repairs)))))


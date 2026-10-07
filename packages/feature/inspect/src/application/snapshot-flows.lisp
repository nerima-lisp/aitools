;;;; packages/feature/inspect/src/application/snapshot-flows.lisp
;;;;
;;;; `snapshot create` and `snapshot diff`: detecting changes
;;;; made outside aitools (builds, formatters, other tools). Independent of
;;;; the journal and of any tx (snapshots always see the disk). Records
;;;; live in the state directory's `<state>/<workspace-id>/snapshots/`, written through
;;;; the store's file primitives with a temp file and a rename.
(in-package #:aitools.inspect.application)

(defun %store-io (store primitive)
  (funcall primitive (store-io-port store)))

(defun %snapshot-directory (context)
  (join-path (or (let ((function (inspect-ports-state-directory-function (inspect-context-ports context))))
                   (and function (funcall function)))
                 (store-state-directory (context-store context)))
             "snapshots"))

(defun %ensure-directory (store path)
  (unless (eq (funcall (%store-io store #'store-io-lstat) path) :directory)
    (let ((parent (subseq path 0 (or (position #\/ path :from-end t) 0))))
      (when (plusp (length parent)) (%ensure-directory store parent)))
    (funcall (%store-io store #'store-io-mkdir) path)))

(defun %snapshot-path (context id)
  (join-path (%snapshot-directory context) (concatenate 'string id ".json")))

(defun %write-snapshot (context snapshot)
  (let* ((store (context-store context))
         (directory (%snapshot-directory context))
         (final (%snapshot-path context (snapshot-id snapshot)))
         (temp (join-path directory (format nil ".~A.~A.tmp" (snapshot-id snapshot)
                                            (funcall (%store-io store #'store-io-random-hex) 8)))))
    (%ensure-directory store directory)
    (funcall (%store-io store #'store-io-create-file) temp
             (sb-ext:string-to-octets (encode-snapshot snapshot) :external-format :utf-8))
    (funcall (%store-io store #'store-io-rename) temp final)))

(defun %existing-snapshot-ids (context)
  (let ((names (funcall (%store-io (context-store context) #'store-io-list-directory) (%snapshot-directory context))))
    (sort (loop for name in names
                for id = (and (> (length name) 5) (string= ".json" name :start2 (- (length name) 5))
                              (subseq name 0 (- (length name) 5)))
                when (valid-snapshot-id-p id) collect id)
          #'string<)))

(defun %now-unix (context)
  (aitools.kernel.domain:universal-time-to-unix-seconds
   (funcall (%store-io (context-store context) #'store-io-now))))

;;; ------------------------------------------------------------ scan options

(defun %scan-options/k (context &key glob lang no-ignore skip-larger-than newer on-options on-error)
  "Validate the common scan options and call ON-OPTIONS, or ON-ERROR."
  (declare (type function on-options on-error))
  (aitools.workspace.application:call-with-scan-options/k
   (context-host context)
   :path-absolute (lambda (path)
                    (resolve-real-path (context-host context) (context-absolute context path)))
   :current-time (lambda () (%now-unix context))
   :language-predicate #'language-path-predicate
   :language-names (aitools.text.domain:language-names)
   :glob glob :lang lang :no-ignore no-ignore
   :skip-larger-than skip-larger-than :newer newer
   :on-error
   (lambda (kind value names)
     (fail on-error "argument.invalid"
           (case kind
             (:unknown-language (format nil "unknown --lang ~S; known: ~{~A~^, ~}" value
                                        names))
             (:invalid-size (format nil "--skip-larger-than ~S is not a size" value))
             (:invalid-newer (format nil "--newer ~A is neither a duration nor an existing path" value)))
           :repairs (if (eq kind :invalid-newer)
                        (list (repair "use-duration" "Give a duration such as 1h."
                                      "aitools snapshot create --newer 1h"))
                        (list (repair "schema" "Show the scan options." "aitools schema snapshot create")))))
   :on-options (lambda (options)
                 ;; The snapshot record stores the language name; the workspace
                 ;; scan turns it into a predicate when it is replayed.
                 (setf (getf options :lang) lang)
                 (funcall on-options options))))

(defun %scan-files (context options &key hash)
  "(VALUES snapshot-files ignore-source) for the workspace under OPTIONS,
files only, sorted by path. HASH reads and hashes each file on the host's
ordered mapper."
  (let ((files '()) (source nil)
        (lang (getf options :lang))
        (text-source (context-source context)))
    (call-with-workspace-scan/k
     (context-host context) (inspect-context-root context)
     :glob (getf options :glob)
     :lang (and lang (language-path-predicate lang))
     :no-ignore (getf options :no-ignore)
     :skip-larger-than (getf options :skip-larger-than)
     :newer (getf options :newer)
     :work (and hash
                (lambda (entry)
                  (and (eq (scan-entry-kind entry) :file)
                       (let ((octets (source-read-octets text-source (scan-entry-absolute entry))))
                         (and octets (content-hash octets))))))
     :emit (lambda (entry result)
             (when (eq (scan-entry-kind entry) :file)
               (push (make-snapshot-file (scan-entry-path entry) (scan-entry-size entry) (scan-entry-mtime entry)
                                         (or result ""))
                     files))
             nil)
     :on-complete (lambda (ignore-source stopped)
                    (declare (ignore stopped))
                    (setf source ignore-source))
     :on-error (lambda (reason path) (declare (ignore reason path)) nil))
    (values (sort files #'string< :key #'snapshot-file-path) source)))

(defun %ignore-source-name (source)
  (if (eq source :none) "none" (string-downcase (symbol-name source))))

;;; ------------------------------------------------------------ create

(defun snapshot-create-flow (ports &key root lock-timeout glob lang no-ignore skip-larger-than newer
                                        on-ok on-partial on-error)
  "`snapshot create`. Calls ON-OK (fields) or ON-ERROR."
  (declare (ignore on-partial) (type function on-ok on-error))
  (call-with-inspect-context/k
   ports :root root :lock-timeout lock-timeout :on-error on-error
   :on-ready (lambda (context)
               (%scan-options/k context :glob glob :lang lang :no-ignore no-ignore
                                        :skip-larger-than skip-larger-than :newer newer
                                        :on-error on-error
                                        :on-options (lambda (options) (%create-snapshot context options on-ok))))))

(defun %create-snapshot (context options on-ok)
  (multiple-value-bind (files source) (%scan-files context options :hash t)
    (let* ((now (%now-unix context))
           (store (context-store context))
           (snapshot (make-snapshot (snapshot-id-from-time now (funcall (%store-io store #'store-io-random-hex) 8))
                                    (iso8601-from-unix now) files
                                    :glob (getf options :glob) :lang (getf options :lang)
                                    :no-ignore (getf options :no-ignore)
                                    :skip-larger-than (getf options :skip-larger-than)
                                    :newer (getf options :newer))))
      (%write-snapshot context snapshot)
      (funcall on-ok (list (cons "snapshot_id" (snapshot-id snapshot))
                           (cons "files" (length files))
                           (cons "ignore_source" (%ignore-source-name source)))))))

;;; ------------------------------------------------------------ diff

(defun %snapshot-missing (context id on-error)
  (let ((ids (%existing-snapshot-ids context)))
    (fail on-error "input.not-found" (format nil "snapshot ~A does not exist" id)
          :candidates (mapcar (lambda (existing) (json-object "snapshot_id" existing))
                              (rank-similar id ids :count 5))
          :repairs (append (when ids
                             (list (repair "diff-latest" "Compare with the newest snapshot."
                                           (command-line context (list "snapshot" "diff" (car (last ids))) :tx :none))))
                           (list (repair "create" "Take a new snapshot."
                                         (command-line context (list "snapshot" "create") :tx :none)))))))

(defun %load-snapshot/k (context id on-snapshot on-error)
  (let* ((store (context-store context))
         (path (and (valid-snapshot-id-p id) (%snapshot-path context id))))
    (if (or (null path) (not (eq (funcall (%store-io store #'store-io-lstat) path) :file)))
        (%snapshot-missing context id on-error)
        (let ((octets (funcall (%store-io store #'store-io-read-file) path)))
          (decode-utf8-strict/k
           octets
           :on-decoded (lambda (text)
                         (decode-snapshot/k text id
                                            :on-snapshot on-snapshot
                                            :on-invalid (lambda () (%snapshot-missing context id on-error))))
           :on-invalid (lambda (position)
                         (declare (ignore position))
                         (%snapshot-missing context id on-error)))))))

(defun %limit-list (items limit)
  (if (> (length items) limit) (subseq items 0 limit) items))

(defun %diff-snapshot (context snapshot limit on-ok on-partial)
  (let ((current (%scan-files context (list :glob (snapshot-glob snapshot) :lang (snapshot-lang snapshot)
                                            :no-ignore (snapshot-no-ignore snapshot)
                                            :skip-larger-than (snapshot-skip-larger-than snapshot)
                                            :newer (snapshot-newer snapshot)))))
    (multiple-value-bind (added removed suspects) (compare-snapshot snapshot current)
      (let* ((modified (loop for (old . new) in suspects
                             for octets = (source-read-octets (context-source context)
                                                              (join-path (workspace-root-path (inspect-context-root context))
                                                                         (snapshot-file-path new)))
                             when (or (null octets) (string/= (content-hash octets) (snapshot-file-hash old)))
                               collect (snapshot-file-path new)))
             (truncated (some (lambda (items) (> (length items) limit)) (list added removed modified))))
        (funcall (if truncated on-partial on-ok)
                 (list (cons "snapshot_id" (snapshot-id snapshot))
                       (cons "added" (%limit-list added limit))
                       (cons "removed" (%limit-list removed limit))
                       (cons "modified" (%limit-list modified limit))
                       (cons "truncated" (json-bool truncated))))))))

(defun snapshot-diff-flow (ports id &key root lock-timeout (limit 100) on-ok on-partial on-error)
  "`snapshot diff`. Calls ON-OK or ON-PARTIAL (fields), or ON-ERROR."
  (declare (type function on-ok on-partial on-error))
  (call-with-inspect-context/k
   ports :root root :lock-timeout lock-timeout :on-error on-error
   :on-ready (lambda (context)
               (%load-snapshot/k context id
                                 (lambda (snapshot) (%diff-snapshot context snapshot limit on-ok on-partial))
                                 on-error))))

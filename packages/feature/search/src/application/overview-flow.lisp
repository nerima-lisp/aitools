;;;; packages/feature/search/src/application/overview-flow.lisp
;;;;
;;;; `overview`. One scan below the path tallies languages (lines are counted
;;;; on the worker pool from the undecoded bytes), collects build files and
;;;; the top-level entries, and records every path it saw. The git summary
;;;; reads `.git` files directly -- HEAD, the ref or packed-refs, and the
;;;; index's path list -- and starts no git process, like every scanning command.
;;;;
;;;; The git summary reports untracked and deleted paths. Paths modified in
;;;; place are not counted: that needs each index entry's object id and stat
;;;; data, and the workspace context's index parser exposes only paths.
(in-package #:aitools.search.application)

(defun %read-text (session path)
  (let ((octets (aitools.workspace.application:host-read-octets (%host session) path)))
    (and octets (aitools.text.domain:decode-utf8 (coerce octets 'octets)))))

(defun %head-sha (session repository ref)
  (let* ((common (aitools.workspace.application:git-repository-common-dir repository))
         (git-dir (aitools.workspace.application:git-repository-git-dir repository))
         (loose (or (%read-text session (aitools.workspace.domain:join-path git-dir ref))
                    (%read-text session (aitools.workspace.domain:join-path common ref)))))
    (if loose
        (string-trim '(#\Space #\Tab #\Return #\Newline) loose)
        (let ((packed (%read-text session (aitools.workspace.domain:join-path common "packed-refs"))))
          (and packed (packed-ref-sha packed ref))))))

(defun %tracked-paths (session repository)
  "The index's paths relative to the workspace root, or NIL when there is
no readable index."
  (let ((octets (aitools.workspace.application:host-read-octets
                 (%host session)
                 (aitools.workspace.domain:join-path
                  (aitools.workspace.application:git-repository-git-dir repository) "index"))))
    (when octets
      (let* ((top (aitools.workspace.application:git-repository-top repository))
             (root (%root-path session))
             (prefix (if (string= top root) "" (aitools.kernel.domain:path-relative-to top root))))
        (handler-case
            (loop for path across (aitools.workspace.domain:parse-git-index-paths (coerce octets 'octets))
                  when (string= prefix "") collect path
                  else when (and (> (length path) (length prefix))
                                 (string= prefix path :end2 (length prefix))
                                 (char= (char path (length prefix)) #\/))
                         collect (subseq path (1+ (length prefix))))
          (aitools.workspace.domain:git-index-error () nil))))))

(defun %git-summary (session start-relative seen untracked)
  (let ((repository (aitools.workspace.application:workspace-root-repository (session-root session))))
    (if (null repository)
        (json-null)
        (let ((head (%read-text session (aitools.workspace.domain:join-path
                                         (aitools.workspace.application:git-repository-git-dir repository)
                                         "HEAD"))))
          (multiple-value-bind (branch sha ref) (if head (parse-head-file head) (values nil nil nil))
            (let ((tracked (%tracked-paths session repository)))
              (json-object-from-alist
               (list (cons "branch" (or branch (json-null)))
                     (cons "head" (or sha (and ref (%head-sha session repository ref)) (json-null)))
                     (cons "untracked" untracked)
                     (cons "deleted"
                           (count-if (lambda (path)
                                       (and (or (string= start-relative "")
                                                (and (> (length path) (length start-relative))
                                                     (string= start-relative path :end2 (length start-relative))
                                                     (char= (char path (length start-relative)) #\/)))
                                            (not (gethash path seen))))
                                     tracked))))))))))

(defun overview/k (ports &key path (limit 30) root tx no-ignore on-ok on-partial on-error)
  "`overview`: a summary of the workspace below PATH (default: the root)."
  (declare (type function on-ok on-partial on-error))
  (call-with-session/k
   ports "overview" :root root :tx tx :on-error on-error
   :on-session
   (lambda (session)
     (let* ((start (if path (%absolute session path) (%root-path session)))
            (start-relative (or (%relative session start) ""))
            (tally (make-language-tally))
            (seen (make-hash-table :test 'equal))
            (untracked 0) (build-files '()) (entries '()))
       (aitools.workspace.application:call-with-workspace-scan/k
        (%host session) (session-root session)
        :paths (list start)
        :no-ignore no-ignore
        :skip-larger-than nil
        :overlay (session-overlay session)
        :work (lambda (entry)
                (let ((relative (aitools.workspace.application:scan-entry-path entry)))
                  (when (eq (aitools.workspace.application:scan-entry-kind entry) :file)
                    (let ((index (language-index-for-path relative)))
                      (when index
                        (handler-case
                            (%read-file/k session (aitools.workspace.application:scan-entry-absolute entry) relative
                                          :on-text (lambda (octets)
                                                     (cons (aitools.text.domain:language-name
                                                            (language-index-language index))
                                                           (aitools.text.domain:count-lines
                                                            octets :start (aitools.text.domain:utf8-bom-length octets))))
                                          :on-binary (constantly nil)
                                          :on-missing (constantly nil))
                          ((or stream-error file-error) () nil)))))))
        :emit (lambda (entry result)
                (let ((relative (aitools.workspace.application:scan-entry-path entry))
                      (kind (aitools.workspace.application:scan-entry-kind entry)))
                  (setf (gethash relative seen) t)
                  (when (eq kind :file)
                    (unless (aitools.workspace.application:scan-entry-tracked-p entry) (incf untracked))
                    (when (build-file-name-p (aitools.workspace.application:scan-entry-name entry))
                      (push relative build-files))
                    (when result
                      (tally-file tally (car result) (cdr result)
                                  (aitools.workspace.application:scan-entry-size entry))))
                  (when (= (entry-depth start-relative relative) 1)
                    (push (json-object-from-alist (list (cons "name" (aitools.workspace.application:scan-entry-name entry))
                                             (cons "kind" (kind-name kind))))
                          entries)))
                nil)
        :on-error (lambda (reason path) (scan-error on-error "overview" reason path))
        :on-complete
        (lambda (source stopped)
          (declare (ignore stopped))
          (let* ((rows (language-tally-rows tally))
                 (total (length rows))
                 (truncated (> total limit))
                 (fields (list (cons "root" (%root-path session))
                               (cons "path" start-relative)
                               (cons "ignore_source" (%ignore-source-name source))
                               (cons "git" (%git-summary session start-relative seen untracked))
                               (cons "languages"
                                     (loop for (language files lines bytes) in rows
                                           repeat limit
                                           collect (json-object-from-alist (list (cons "lang" language) (cons "files" files)
                                                                      (cons "lines" lines) (cons "bytes" bytes)))))
                               (cons "languages_total" total)
                               (cons "build_files" (nreverse build-files))
                               (cons "entries" (nreverse entries))
                               (cons "truncated" (json-boolean truncated)))))
            (if truncated
                (%finish on-partial fields
                         (list (command-line (append (list "aitools" "overview")
                                                     (when path (list path))
                                                     (list "--limit" (princ-to-string total))
                                                     (when tx (list "--tx" tx))))))
                (%finish on-ok fields)))))))))

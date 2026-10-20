(in-package #:aitools.edit.application)

(defun %replace-plan (replacer multiline selector explicit skipped)
  "EXPLICIT: paths the user named as files, whose unreadable or non-text
state is an error; scanned files in that state are skipped."
  (lambda (context commit reject)
    (block plan
      (let ((requests '()) (per-change '()) (total 0)
            ;; A byte-level required-literal check, so a file lacking
            ;; the pattern's literal is skipped without decoding. Sound only
            ;; where a miss means zero replacements and no error, so it is
            ;; applied only to scanned (non-explicit) files and dropped when a
            ;; selector restricts the lines. A file that clears it is still
            ;; matched exactly as before.
            (literal (and (null selector) (replacer-required-literal replacer))))
        (dolist (path (write-context-paths context))
          (block one
            (flet ((skip-or-reject (&rest rejection)
                     (if (member path explicit :test #'equal)
                         (return-from plan (apply reject rejection))
                         (return-from one))))
              (read-document/k
               context path #'skip-or-reject
               (lambda (document)
                 (flet ((replace-in (ranges)
                          (reset-replacer replacer)
                          (multiple-value-bind (result count) (replace-document document replacer ranges multiline)
                            (when (plusp count)
                              (incf total count)
                              (push (write-document-request path result) requests)
                              (push (cons path (list (cons "count" count))) per-change)))))
                   (if selector
                       (resolve-lines/k document selector path #'skip-or-reject #'replace-in)
                       (replace-in nil))))
               :prefilter (and literal (not (member path explicit :test #'equal))
                               (lambda (octets) (octets-search literal octets)))
               :on-filtered (lambda () (return-from one))))))
        (check-expect-count/k context total reject
                              (lambda ()
                                (funcall commit (nreverse requests)
                                         (append (list (cons :per-change per-change))
                                                 (when skipped
                                                   (list (cons "skipped" (nreverse skipped))))))))))))

(defun %replace-with-targets (env paths options fail on-plan replacer selector pattern replacement record-options)
  "PATHS are as the user typed them (relative to the working directory);
during `tx rebase` (no ENV) they are the recorded workspace-relative paths."
  (flet ((plan (files explicit skipped)
               (funcall on-plan
                        (make-write-plan
                         :command "replace"
                         :targets (mapcar (lambda (file) (make-write-target file :base :root)) files)
                         :inputs (list pattern replacement)
                         :guard-requirements (list* (list :expect-count) (selector-guards selector (first files)))
                         :expect-hashes (getf options :expect-hash)
                         :expect-count (getf options :expect-count)
                         :replayable (and (content-selector-p selector) (null (getf options :expect-hash)))
                         :plan (%replace-plan replacer (getf options :multiline) selector explicit skipped)
                         :record-options record-options
                         :record-positionals (if (or (getf options :stdin) (getf options :stdin-data)
                                                     (getf record-options :stdin-data))
                                                 (lambda (resolved) resolved)
                                               (lambda (resolved) (list* pattern replacement resolved)))))))
    (if (null env)
        (plan paths paths nil)
      (let* ((host (command-env-host env))
             (absolute (mapcar (lambda (path) (aitools.workspace.application:user-path-absolute host path)) paths))
             ;; Workspace-relative where inside (the form the plan's context
             ;; paths take), else absolute for the boundary check to refuse.
             (relative (mapcar (lambda (path)
                                 (aitools.workspace.application:resolve-user-path/k
                                  host (command-env-root env) path
                                  :on-inside (lambda (absolute relative) (declare (ignore absolute)) relative)
                                  :on-outside #'identity))
                               paths))
             (explicit (loop for path in absolute
                             for path-relative in relative
                             for entry = (aitools.workspace.application:host-stat host path)
                             when (and entry (eq (aitools.workspace.application:workspace-entry-kind entry) :file))
                             collect path-relative)))
        (if (and paths (= (length explicit) (length paths)))
            (plan relative explicit nil)
          (let ((skipped '()))
            (scan-files/k env absolute options fail
                          (lambda (entries)
                            (let ((files (mapcar #'aitools.workspace.application:scan-entry-path entries)))
                              (if (null files)
                                  (funcall fail "selection.no-match" "no files to search" :path (first paths))
                                (plan files (intersection files explicit :test #'equal) skipped))))
                          :on-skip (lambda (entry reason)
                                     (when (eq reason :too-large)
                                       (push (aitools.protocol.domain:json-object-from-alist
                                              (list (cons "path" (aitools.workspace.application:scan-entry-path
                                                                  entry))
                                                    (cons "reason" "too-large")))
                                             skipped))))))))))

;;;;
;;;; `overview`. One scan below the path tallies languages (lines are counted
;;;; on the worker pool from the undecoded bytes), collects build files and
;;;; the top-level entries.
(in-package #:aitools.search.application)

(aitools.kernel.domain:define-flow
    (overview/k ports &key path (limit 30) root tx no-ignore on-ok on-partial on-error)
  "`overview`: a summary of the workspace below PATH (default: the root)."
  (aitools.kernel.domain:flow-call
      (call-with-session/k ports "overview" :root root :tx tx)
    (:on-error (code message &key repairs)
      (aitools.kernel.domain:flow-continue
       :on-error code message :repairs repairs))
    (:on-session (session)
      (let* ((start (if path (%absolute session path) (%root-path session)))
             (start-relative (or (%relative session start) ""))
             (tally (make-language-tally))
             (build-files '()) (entries '()))
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
                             (aitools.kernel.domain:flow-call
                                 (%read-file/k
                                  session
                                  (aitools.workspace.application:scan-entry-absolute entry)
                                  relative)
                               (:on-text (octets)
                                 (cons
                                  (aitools.text.domain:language-name
                                   (language-index-language index))
                                  (aitools.text.domain:count-lines
                                   octets
                                   :start (aitools.text.domain:utf8-bom-length octets))))
                               (:on-binary () nil)
                               (:on-missing () nil))
                           ((or stream-error file-error) () nil)))))))
         :emit (lambda (entry result)
                 (let ((relative (aitools.workspace.application:scan-entry-path entry))
                       (kind (aitools.workspace.application:scan-entry-kind entry)))
                   (when (eq kind :file)
                     (when (build-file-name-p
                            (aitools.workspace.application:scan-entry-name entry))
                       (push relative build-files))
                     (when result
                       (tally-file
                        tally (car result) (cdr result)
                        (aitools.workspace.application:scan-entry-size entry))))
                   (when (= (entry-depth start-relative relative) 1)
                     (push
                      (json-object-from-alist
                       (list
                        (cons "name"
                              (aitools.workspace.application:scan-entry-name entry))
                        (cons "kind"
                              (kind-name kind))))
                      entries))
                   nil))
         :on-error
         (lambda (reason path)
           (aitools.kernel.domain:flow-call
               (scan-error "overview" reason path)
             (:on-error (code message &key repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code message :repairs repairs))))
         :on-complete
         (lambda (source stopped)
           (declare (ignore stopped))
           (let* ((rows (language-tally-rows tally))
                  (total (length rows))
                  (truncated (> total limit))
                  (fields
                    (list
                     (cons "root" (%root-path session))
                     (cons "path" start-relative)
                     (cons "ignore_source" (%ignore-source-name source))
                     (cons
                      "languages"
                      (loop for (language files lines bytes) in rows
                            repeat limit
                            collect
                            (json-object-from-alist
                             (list (cons "lang" language)
                                   (cons "files" files)
                                   (cons "lines" lines)
                                   (cons "bytes" bytes)))))
                     (cons "languages_total" total)
                     (cons "build_files" (nreverse build-files))
                     (cons "entries" (nreverse entries))
                     (cons "truncated" (json-boolean truncated)))))
             (if truncated
                 (aitools.kernel.domain:flow-continue
                  :on-partial
                  (%result-fields
                   fields
                   (list
                    (command-line
                     (append
                      (list "aitools" "overview")
                      (when path (list path))
                      (list "--limit" (princ-to-string total))
                      (when tx (list "--tx" tx)))))))
                 (aitools.kernel.domain:flow-continue
                  :on-ok (%result-fields fields))))))))))

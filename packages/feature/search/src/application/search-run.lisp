(in-package #:aitools.search.application)

(aitools.kernel.domain:define-flow
    (%run-search session request matcher scan-options &key on-ok on-partial on-error)
  (destructuring-bind (&key paths output before after limit &allow-other-keys) request
    (let* ((programs (matcher-programs matcher))
           (with-index (> (length programs) 1))
           (items '()) (remaining limit) (truncated nil) (characters 0)
           (total 0) (entries 0) (files-scanned 0) (skipped '()))
      (flet ((skip (path reason)
               (push (json-object-from-alist
                      (list (cons "path" path) (cons "reason" reason)))
                     skipped))
             (entry-item (item)
               (incf entries)
               (if (plusp remaining)
                   (progn (push item items) (decf remaining))
                   (setf truncated t))))
        (apply #'aitools.workspace.application:call-with-workspace-scan/k
               (%host session) (session-root session)
               :paths (%start-paths session paths)
               :work (%search-worker session matcher output limit (%run-deadline))
               :on-skip (lambda (entry reason)
                          (skip
                           (aitools.workspace.application:scan-entry-path entry)
                           (if (eq reason :too-large) "too-large" "unreadable")))
               :on-error
               (lambda (reason path)
                 (aitools.kernel.domain:flow-call
                     (scan-error "search" reason path)
                   (:on-error (code message &key repairs)
                     (aitools.kernel.domain:flow-continue
                      :on-error code message :repairs repairs))))
               :emit
               (lambda (entry result)
                 (let ((path (aitools.workspace.application:scan-entry-path entry)))
                   (cond
                     ((null result))
                     ((and (consp result) (eq (first result) :skipped))
                      (skip path (second result)))
                     (t
                      (incf files-scanned)
                      (let ((selected (file-outcome-selected-count result)))
                        (ecase output
                          (:blocks
                           (incf total selected)
                           (when (plusp selected)
                             (let ((taken (min remaining selected)))
                               (when (< taken selected) (setf truncated t))
                               (when (plusp taken)
                                 (multiple-value-bind (blocks count)
                                     (build-blocks
                                      path
                                      (file-outcome-octets result)
                                      (subseq (file-outcome-selected result) 0 taken)
                                      before
                                      after)
                                   (incf characters count)
                                   (setf items (revappend blocks items))
                                   (decf remaining taken))))))
                          (:matches
                           (let ((count (file-outcome-match-count result)))
                             (incf total count)
                             (let ((taken (min remaining count)))
                               (when (< taken count) (setf truncated t))
                               (loop for match in (file-outcome-matches result)
                                     repeat taken
                                     do (multiple-value-bind (object count)
                                            (render-match
                                             path
                                             (file-outcome-octets result)
                                             match
                                             programs
                                             with-index)
                                          (incf characters count)
                                          (push object items)))
                               (decf remaining taken))))
                          (:count
                           (incf total selected)
                           (when (plusp selected)
                             (entry-item
                              (json-object-from-alist
                               (list (cons "path" path) (cons "count" selected))))))
                          (:files
                           (incf total selected)
                           (when (plusp selected) (entry-item path)))
                          (:files-without-match
                           (incf total selected)
                           (when (zerop selected) (entry-item path))))))))
                 nil)
               :on-complete
               (lambda (source stopped)
                 (declare (ignore stopped))
                 (let ((fields
                         (append
                          (list (cons "mode" (%mode-name output))
                                (cons (ecase output
                                        (:blocks "blocks")
                                        (:matches "matches")
                                        (:count "counts")
                                        ((:files :files-without-match) "paths"))
                                      (nreverse items))
                                (cons "total_matches" total))
                          (when (member output '(:count :files :files-without-match))
                            (list (cons "total" entries)))
                          (list (cons "files_scanned" files-scanned)
                                (cons "skipped" (nreverse skipped))
                                (cons "ignore_source" (%ignore-source-name source))
                                (cons "truncated" (json-boolean truncated))
                                (cons "approx_tokens"
                                      (aitools.kernel.domain:approx-token-count
                                       characters))))))
                   (if truncated
                       (aitools.kernel.domain:flow-continue
                        :on-partial
                        (%result-fields
                         fields
                         (list
                          (%search-command-line
                           request
                           :limit
                           (if (member output '(:blocks :matches))
                               total
                               entries)))))
                       (aitools.kernel.domain:flow-continue
                        :on-ok
                        (%result-fields fields)))))
               scan-options)))))

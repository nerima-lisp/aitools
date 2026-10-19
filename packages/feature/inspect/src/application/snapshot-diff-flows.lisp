(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow (%snapshot-missing context id &key on-error)
                                   (let ((ids (%existing-snapshot-ids context)))
                                     (aitools.kernel.domain:flow-continue
                                      :on-error
                                      "input.not-found"
                                      (format nil
                                              "snapshot ~A does not exist"
                                              id)
                                      :candidates
                                      (mapcar
                                       (lambda (existing)
                                         (json-object "snapshot_id" existing))
                                       (rank-similar id ids :count 5))
                                      :repairs
                                      (append
                                       (when ids
                                         (list
                                          (repair "diff-latest"
                                                  "Compare with the newest snapshot."
                                                  (command-line context
                                                                (list "snapshot"
                                                                      "diff"
                                                                      (car
                                                                       (last
                                                                        ids)))
                                                                :tx
                                                                :none))))
                                       (list
                                        (repair "create"
                                                "Take a new snapshot."
                                                (command-line context
                                                              (list "snapshot"
                                                                    "create")
                                                              :tx
                                                              :none)))))))

(aitools.kernel.domain:define-flow
 (%load-snapshot/k context id &key on-snapshot on-error)
 (let* ((store (context-store context))
        (path (and (valid-snapshot-id-p id) (%snapshot-path context id))))
   (if (or (null path)
           (not (eq (funcall (%store-io store #'store-io-lstat) path) :file)))
       (aitools.kernel.domain:flow-call
        (%snapshot-missing context id)
        (:on-error
         (code message &key candidates repairs)
         (aitools.kernel.domain:flow-continue
          :on-error
          code
          message
          :candidates
          candidates
          :repairs
          repairs)))
     (let ((octets (funcall (%store-io store #'store-io-read-file) path)))
       (decode-utf8-strict/k octets
                             :on-decoded
                             (lambda (text)
                               (aitools.kernel.domain:flow-call
                                (decode-snapshot/k text id)
                                (:on-snapshot (snapshot)
                                              (aitools.kernel.domain:flow-continue
                                               :on-snapshot
                                               snapshot))
                                (:on-invalid ()
                                             (aitools.kernel.domain:flow-call
                                              (%snapshot-missing context id)
                                              (:on-error
                                               (code message
                                                     &key
                                                     candidates
                                                     repairs)
                                               (aitools.kernel.domain:flow-continue
                                                :on-error
                                                code
                                                message
                                                :candidates
                                                candidates
                                                :repairs
                                                repairs))))))
                             :on-invalid
                             (lambda (position)
                               (declare (ignore position))
                               (aitools.kernel.domain:flow-call
                                (%snapshot-missing context id)
                                (:on-error
                                 (code message &key candidates repairs)
                                 (aitools.kernel.domain:flow-continue
                                  :on-error
                                  code
                                  message
                                  :candidates
                                  candidates
                                  :repairs
                                  repairs)))))))))

(defun %limit-list (items limit)
  (if (> (length items) limit)
      (subseq items 0 limit)
      items))

(defun %diff-snapshot (context snapshot limit)
  (let ((current
         (%scan-files context
                      (list :glob
                            (snapshot-glob snapshot)
                            :lang
                            (snapshot-lang snapshot)
                            :no-ignore
                            (snapshot-no-ignore snapshot)
                            :skip-larger-than
                            (snapshot-skip-larger-than snapshot)
                            :newer
                            (snapshot-newer snapshot)))))
    (multiple-value-bind (added removed suspects)
        (compare-snapshot snapshot current)
      (let* ((modified
              (loop for (old . new) in suspects
                    for octets = (source-read-octets (context-source context)
                                                     (join-path
                                                      (workspace-root-path
                                                       (inspect-context-root
                                                        context))
                                                      (snapshot-file-path new)))
                    when (or (null octets)
                             (string/= (content-hash octets)
                                       (snapshot-file-hash old)))
                      collect (snapshot-file-path new)))
             (truncated
              (some
               (lambda (items)
                 (> (length items) limit))
               (list added removed modified))))
        (values
         (list (cons "snapshot_id" (snapshot-id snapshot))
               (cons "added" (%limit-list added limit))
               (cons "removed" (%limit-list removed limit))
               (cons "modified" (%limit-list modified limit))
               (cons "truncated" (json-bool truncated)))
         truncated)))))

(aitools.kernel.domain:define-flow
 (snapshot-diff-flow ports
                     id
                     &key
                     root
                     lock-timeout
                     (limit 100)
                     on-ok
                     on-partial
                     on-error)
 "`snapshot diff`. Calls ON-OK or ON-PARTIAL (fields), or ON-ERROR."
 (aitools.kernel.domain:flow-call
  (inspect-context-flow ports :root root :lock-timeout lock-timeout)
  (:on-ready (context)
             (aitools.kernel.domain:flow-call
              (%load-snapshot/k context id)
              (:on-snapshot (snapshot)
                            (multiple-value-bind (fields
                                                  truncated)
                                (%diff-snapshot
                                 context
                                 snapshot
                                 limit)
                              (if truncated
                                  (aitools.kernel.domain:flow-continue
                                   :on-partial
                                   fields)
                                (aitools.kernel.domain:flow-continue
                                 :on-ok
                                 fields))))
              (:on-error
               (code message
                     &key
                     candidates
                     repairs)
               (aitools.kernel.domain:flow-continue
                :on-error
                code
                message
                :candidates
                candidates
                :repairs
                repairs))))
  (:on-error (code message &key repairs)
             (aitools.kernel.domain:flow-continue :on-error
                                                  code
                                                  message
                                                  :repairs
                                                  repairs))))

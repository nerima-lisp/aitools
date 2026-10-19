(in-package #:aitools.inspect.application)

(defun %blob-or-nil (store hash)
  (handler-case (read-blob store hash)
    (error ()
      nil)))

(defun %after-content (context change)
  "The bytes a journal change left behind: its blob when one survives, or
the file on disk when that still has the recorded hash."
  (let ((after (change-result-after change)))
    (when (eq (entry-state-kind after) :file)
      (let ((store (context-store context)))
        (or (%blob-or-nil store (entry-state-hash after))
            (let ((disk
                   (source-read-octets (context-source context)
                                       (join-path (context-root-real context)
                                                  (change-result-path change)))))
              (and disk
                   (string= (content-hash disk) (entry-state-hash after))
                   disk)))))))

(defun %op-change (context change)
  (let* ((before (change-result-before change))
         (before-content
          (and (eq (entry-state-kind before) :file)
               (%blob-or-nil (context-store context) (entry-state-hash before))))
         (after-content (%after-content context change))
         (available
          (and (or before-content (not (eq (entry-state-kind before) :file)))
               (or after-content
                   (not
                    (eq (entry-state-kind (change-result-after change)) :file)))))
         (diff
          (and available
               (change-diff
                (make-change-result :path
                                    (change-result-path change)
                                    :action
                                    (change-result-action change)
                                    :from
                                    (change-result-from change)
                                    :before
                                    before
                                    :after
                                    (change-result-after change)
                                    :before-content
                                    before-content
                                    :after-content
                                    after-content)))))
    (json-object-from-pairs
     (append
      (list (cons "path" (change-result-path change))
            (cons "action" (action-name (change-result-action change))))
      (when (change-result-from change)
        (list (cons "from" (change-result-from change))))
      (cond
        (diff (list (cons "diff" diff)))
        ((not available) (list (cons "content_available" (json-false)))))))))

(defun %op-diff (context op limit on-ok on-partial on-error)
  (let ((entry (find-journal-entry (context-store context) op)))
    (if (null entry)
        (fail on-error
              "input.not-found"
              (format nil "op ~A is not in the journal" op)
              :repairs
              (list
               (repair "history" "List the recorded ops." "aitools history")))
        (multiple-value-bind (shown truncated)
            (%limited (journal-entry-changes entry) limit)
          (%finish on-ok
                   on-partial
                   truncated
                   (list (cons "mode" "op")
                         (cons "op_id" (journal-entry-op-id entry))
                         (cons "argv" (journal-entry-argv entry))
                         (cons "changes"
                               (mapcar
                                (lambda (change)
                                  (%op-change context change))
                                shown))))))))

(aitools.kernel.domain:define-flow
 (diff-flow ports
            &key
            a
            b
            op
            root
            tx
            lock-timeout
            (context 3)
            (output "unified")
            ignore-whitespace
            ignore-eol
            (limit 100)
            on-ok
            on-partial
            on-error)
 "`diff`. Calls ON-OK or ON-PARTIAL (fields), or ON-ERROR."
 (declare (type function on-ok on-partial on-error))
 (labels ((report-ok (fields)
            (aitools.kernel.domain:flow-continue :on-ok fields))
          (report-partial (fields)
            (aitools.kernel.domain:flow-continue :on-partial fields))
          (report-error (code message &key candidates diagnostics conflicts repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates
             :diagnostics diagnostics
             :conflicts conflicts
             :repairs repairs)))
   (let ((lines-context context))
     (flet ((invalid (message)
              (aitools.kernel.domain:flow-continue :on-error
                                                   "argument.invalid"
                                                   message
                                                   :repairs
                                                   (list
                                                    (repair "diff-files"
                                                            "Compare two files."
                                                            "aitools diff a.txt b.txt")
                                                    (repair "diff-op"
                                                            "Show a journal op's changes."
                                                            "aitools history")))))
       (cond
         ((and op (or a b)) (invalid "--op takes no paths"))
         ((and (not op) (not (and a b)))
          (invalid "diff needs two paths, or --op <op_id>"))
         (t
          (aitools.kernel.domain:flow-call
           (inspect-context-flow ports
                                 :root
                                 root
                                 :tx
                                 tx
                                 :lock-timeout
                                 lock-timeout)
           (:on-ready (inspect-context)
                      (if op
                          (%op-diff inspect-context
                                    op
                                    limit
                                    #'report-ok
                                    #'report-partial
                                    #'report-error)
                          (%compare-paths inspect-context
                                          a
                                          b
                                          (list :context
                                                lines-context
                                                :output
                                                output
                                                :ignore-whitespace
                                                ignore-whitespace
                                                :ignore-eol
                                                ignore-eol
                                                :limit
                                                limit)
                                          #'report-ok
                                          #'report-partial
                                          #'report-error)))
           (:on-error (code message &key repairs)
                      (aitools.kernel.domain:flow-continue :on-error
                                                           code
                                                           message
                                                           :repairs
                                                           repairs)))))))))

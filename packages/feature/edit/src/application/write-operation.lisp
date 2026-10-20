(in-package #:aitools.edit.application)

(defun %extend-change (change per-change)
  (let* ((members (aitools.protocol.domain:json-object-members change))
         (more (cdr (assoc (cdr (assoc "path" members :test #'string=)) per-change :test #'equal))))
    (if more (aitools.protocol.domain:json-object-from-alist (append members more)) change)))

(defun %record-options (write-plan paths)
  (let ((options (write-plan-record-options write-plan)))
    (if (functionp options) (funcall options paths) options)))

(defun %with-extra-fields (fields extra)
  "FIELDS (from WRITE-RESULT-FIELDS) with the plan's EXTRA string-keyed
fields inserted before `next_commands`. Two keyword entries adjust
`changes` itself: (:PER-CHANGE . ((path . members) ...)) appends members to
that path's change, (:LIMIT-CHANGES . N) keeps the first N changes, and
(:TOTAL-CHANGES . T) adds `total`, the count before that limit."
  (let* ((per-change (cdr (assoc :per-change extra)))
         (limit (cdr (assoc :limit-changes extra)))
         (total (and (assoc :total-changes extra)
                     (list (cons "total" (length (cdr (assoc "changes" fields :test #'string=)))))))
         (plain (append (remove-if (lambda (pair) (keywordp (car pair))) extra) total))
         (fields (mapcar (lambda (pair)
                           (if (string= (car pair) "changes")
                               (let ((changes (mapcar (lambda (change) (%extend-change change per-change)) (cdr pair))))
                                 (cons "changes" (if limit (subseq changes 0 (min limit (length changes))) changes)))
                               pair))
                         fields))
         (next (assoc "next_commands" fields :test #'string=)))
    (append (remove next fields) plain (and next (list next)))))

(defun %catching-refusals (reject thunk)
  "Call THUNK; turn the domain's and the kits' input refusals into REJECT."
  (handler-case (call-with-regex-refusals thunk)
    (aitools.edit.domain:edit-refusal (condition)
      (funcall reject (aitools.edit.domain:edit-refusal-code condition)
               (aitools.edit.domain:edit-refusal-detail condition)))
    (aitools.text.domain:archive-unsupported (condition)
      (funcall reject "input.unsupported-format" (princ-to-string condition)))
    (aitools.text.domain:archive-error (condition)
      (funcall reject "input.syntax-error" (princ-to-string condition)))
    (aitools.store.application:store-io-error (condition)
      (funcall reject "environment.io" (princ-to-string condition)))))

(defun run-plan (context plan commit reject)
  "Call PLAN (from a WRITE-PLAN) against CONTEXT. COMMIT is called with the
requests and the extra fields the plan passed; REJECT as in a store
VALIDATE continuation."
  (declare (type function plan commit reject))
  (%catching-refusals reject
                      (lambda ()
                        (funcall plan context
                                 (lambda (requests &optional extra) (funcall commit requests extra))
                                 reject))))

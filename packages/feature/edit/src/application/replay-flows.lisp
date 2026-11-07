(in-package #:aitools.edit.application)

(defparameter +replayable-commands+
  '("edit" "insert"
    "replace"
    "apply"
    "transform"
    "move-lines"
    "doc.set"
    "doc.delete"
    "doc.merge"
    "doc.patch"
    "doc.fmt")
  "Commands whose ops `tx rebase` may re-run after their plans are prepared without a workspace environment.")

(aitools.kernel.domain:define-flow
    (%replay-edit-op/k argv view &key serializer on-commit on-reject)
  (aitools.kernel.domain:flow-call
      (parse-recorded-argv argv)
    (:on-invalid (message)
      (aitools.kernel.domain:flow-continue
       :on-reject "argument.invalid" message))
    (:on-parsed (name positionals options)
      (let ((preparer (gethash name *preparers*)))
        (if (not (member name +replayable-commands+ :test #'string=))
            (aitools.kernel.domain:flow-continue
             :on-reject "argument.invalid"
             (format nil "~A cannot be replayed" name))
            (funcall preparer
                     (and serializer (make-edit-ports :serialize-document serializer))
                     nil positionals options
                     :on-plan
                     (lambda (plan)
                       (if (not (write-plan-replayable plan))
                           (aitools.kernel.domain:flow-continue
                            :on-reject "refusal.target-changed"
                            (format nil "~A is position-based and is not replayed" name))
                           (aitools.kernel.domain:flow-call
                               (run-plan
                                (make-write-context
                                 :view view
                                 :paths (mapcar #'write-target-path (write-plan-targets plan))
                                 :command name
                                 :expect-count (parse-count (getf options :expect-count)))
                                (write-plan-plan plan))
                             (:on-committed (requests extra)
                               (declare (ignore extra))
                               (aitools.kernel.domain:flow-continue
                                :on-commit requests))
                             (:on-rejected (code message &key candidates diagnostics conflicts repairs path)
                               (declare (ignore path))
                               (aitools.kernel.domain:flow-continue
                                :on-reject code message
                                :candidates candidates
                                :diagnostics diagnostics
                                :conflicts conflicts
                                :repairs repairs)))))
                     :on-error
                     (lambda (code message &key candidates diagnostics conflicts repairs path)
                       (aitools.kernel.domain:flow-continue
                        :on-reject code message
                        :candidates candidates
                        :diagnostics diagnostics
                        :conflicts conflicts
                        :repairs repairs
                        :path path)))))))
) (defun replay-edit-op (argv view commit reject &key serializer)
  "Adapt the journal callback; SERIALIZER supplies YAML/TOML replay output."
  (declare (type function commit reject))
  (%replay-edit-op/k argv
                     view
                     :serializer serializer
                     :on-commit commit
                     :on-reject
                     (lambda (code message &key candidates diagnostics conflicts repairs path)
                       (funcall reject code message
                                :candidates candidates
                                :diagnostics diagnostics
                                :conflicts conflicts
                                :repairs repairs
                                :path path))))

(progn
  (defun register-format-document-replayers (serializer)
    (dolist (name
             (list "doc.set" "doc.delete" "doc.merge" "doc.patch" "doc.fmt"))
      (aitools.journal.application:register-tx-replayer
       name
       (lambda (argv view commit reject)
         (replay-edit-op argv view commit reject :serializer serializer))))
    t)
  (dolist (name +replayable-commands+)
    (aitools.journal.application:register-tx-replayer
     name (function replay-edit-op))))

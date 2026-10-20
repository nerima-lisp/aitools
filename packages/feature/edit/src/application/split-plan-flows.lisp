(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
  (%split-plan context piece-function prefix digits &key on-error on-committed)
  (let ((path (context-path context)))
    (aitools.kernel.domain:flow-call
      (read-file-octets/k context path)
      (:on-error (code message &key candidates diagnostics conflicts repairs path)
        (flow-continue :on-error
                       code
                       message
                       :candidates
                       candidates
                       :diagnostics
                       diagnostics
                       :conflicts
                       conflicts
                       :repairs
                       repairs
                       :path
                       path))
      (:on-octets (octets mode)
        (flet ( (cut (document)
                 (let ((pieces (funcall piece-function octets document))
                       (requests '())
                       (per-change '())
                       (failure nil))
                   (if (>= (length pieces) (expt 10 digits))
                       (aitools.kernel.domain:flow-continue
                         :on-error
                         "argument.invalid"
                         (format
                           nil
                           "~D pieces need more than --suffix-digits ~D"
                           (length pieces)
                           digits))
                       (progn
                         (loop for piece in pieces
                               for index from 1
                               unless failure
                                 do (aitools.kernel.domain:flow-call
                                      (resolve-extra-path/k context (split-piece-name prefix index digits) :base :cwd)
                                      (:on-outside (message)
                                        (setf failure (list
                                                        "refusal.outside-workspace"
                                                        message)))
                                      (:on-inside (target)
                                        (if (aitools.store.domain:entry-state-absent-p
                                             (aitools.store.application:view-path-state (write-context-view context)
                                                                                        target))
                                            (progn
                                              (push
                                               (aitools.store.domain:write-file-request target
                                                                                        (subseq octets
                                                                                                (split-piece-start
                                                                                                 piece)
                                                                                                (split-piece-end piece))
                                                                                        :mode
                                                                                        mode)
                                               requests)
                                              (push
                                               (cons
                                                 target
                                                 (list
                                                   (cons
                                                     "start_line"
                                                     (split-piece-start-line piece))
                                                   (cons
                                                     "lines"
                                                     (split-piece-lines piece))))
                                               per-change))
                                            (setf failure (list
                                                            "refusal.exists"
                                                            (format
                                                              nil
                                                              "~A already exists; nothing was written"
                                                              target)))))))
                         (if failure
                             (flow-continue
                               :on-error
                               (first failure)
                               (second failure))
                             (aitools.kernel.domain:flow-continue
                               :on-committed
                               (nreverse requests)
                               (list
                                (cons :per-change per-change)))))))))
                     (if (eq (aitools.text.domain:line-ending-style octets) :none)
                     (cut (make-text-document ""))
                     (aitools.kernel.domain:flow-call
                       (decode-text-document/k octets)
                       (:on-decoded (document)
                         (cut document))
                       (:on-binary ()
                         (cut (make-text-document "")))
                       (:on-invalid (offset)
                         (declare (ignore offset))
                         (cut (make-text-document ""))))))))))

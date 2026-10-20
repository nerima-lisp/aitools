(in-package #:aitools.edit.application)

(defun %split-plan (context commit reject piece-function prefix digits)
(let ((path (context-path context)))
(read-file-octets/k context
                    path
                    reject
                    (lambda (octets mode)
                    (flet ((cut (document)
                           (let ((pieces
                                  (funcall piece-function octets document)))
                           (when (>= (length pieces) (expt 10 digits))
                           (return-from %split-plan
                           (funcall reject
                                    "argument.invalid"
                                    (format nil
                                            "~D pieces need more than --suffix-digits ~D"
                                            (length pieces)
                                            digits))))
                           (let ((requests '())
                                 (per-change '()))
                           (loop for piece in pieces
                                 for index from 1
                                 do (resolve-extra-path/k context
                                                          (split-piece-name
                                                           prefix
                                                           index
                                                           digits)
                                                          :base
                                                          :cwd
                                                          :on-outside
                                                          (lambda (message)
                                                          (return-from
                                                          %split-plan
                                                          (funcall reject
                                                                   "refusal.outside-workspace"
                                                                   message)))
                                                          :on-inside
                                                          (lambda (target)
                                                          (unless
                                                          (aitools.store.domain:entry-state-absent-p
                                                           (aitools.store.application:view-path-state
                                                            (write-context-view
                                                             context)
                                                            target))
                                                          (return-from
                                                          %split-plan
                                                          (funcall reject
                                                                   "refusal.exists"
                                                                   (format nil
                                                                           "~A already exists; nothing was written"
                                                                           target))))
                                                          (push
                                                           (aitools.store.domain:write-file-request
                                                            target
                                                            (subseq octets
                                                                    (split-piece-start
                                                                     piece)
                                                                    (split-piece-end
                                                                     piece))
                                                            :mode
                                                            mode)
                                                           requests)
                                                          (push
                                                           (cons target
                                                                 (list
                                                                  (cons
                                                                   "start_line"
                                                                   (split-piece-start-line
                                                                    piece))
                                                                  (cons "lines"
                                                                        (split-piece-lines
                                                                         piece))))
                                                           per-change))))
                           (funcall commit
                                    (nreverse requests)
                                    (list (cons :per-change per-change)))))))
                    (if (eq (aitools.text.domain:line-ending-style octets)
                            :none)
                    (cut (make-text-document ""))
                    (decode-text-document/k octets
                                            :on-decoded
                                            #'cut
                                            :on-binary
                                            (lambda ()
                                            (cut (make-text-document "")))
                                            :on-invalid
                                            (lambda (offset)
                                            (declare (ignore offset))
                                            (cut (make-text-document ""))))))))))


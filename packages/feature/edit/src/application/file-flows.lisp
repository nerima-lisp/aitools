;;;;
;;;; File operations: move, copy, delete, mkdir, chmod,
;;;; link and touch. mktemp, which alone bypasses the journal, is in
;;;; mktemp.lisp.
(in-package #:aitools.edit.application)

(defun %state (context path)
  (aitools.store.application:view-path-state (write-context-view context) path))

(defun %kind (context path)
  (aitools.store.domain:entry-state-kind (%state context path)))

(defun %walk-tree (context directory)
  "Entries below DIRECTORY as (relative-path . kind), parents before children."
  (loop for entry in (aitools.store.application:view-directory-entries
                      (write-context-view context)
                      directory)
        for path = (%child directory (car entry))
        append (cons (cons path (cdr entry))
                     (when (eq (cdr entry) :directory)
                       (%walk-tree context path)))))

(defun %replace-rebase (path from to)
  "PATH (below FROM, \"\" for the workspace root) moved below TO."
  (if (zerop (length from))
      (%child to path)
      (concatenate 'string to (subseq path (length from)))))

(define-write-command "rm"
                      (ports env positionals options &key on-plan on-error)
                      (let ((max-entries
                             (parse-count
                              (or (getf options :max-entries) "1000")))
                            (max-bytes
                             (handler-case (aitools.kernel.domain:size-bytes
                                            (aitools.kernel.domain:parse-size
                                             (or (getf options :max-bytes)
                                                 "64MiB")))
                               (error ()
                                 nil)))
                            (recursive (getf options :recursive)))
                        (cond
                          ((/= (length positionals) 1)
                           (flow-continue :on-error
                                          "argument.invalid"
                                          "rm takes exactly one path"))
                          ((or (null max-entries) (zerop max-entries))
                           (flow-continue :on-error
                                          "argument.invalid"
                                          "--max-entries must be a positive integer"))
                          ((null max-bytes)
                           (flow-continue :on-error
                                          "argument.invalid"
                                          "--max-bytes must be a size"))
                          (t
                           (flow-continue :on-plan
                                          (make-write-plan :command
                                                           "rm"
                                                           :targets
                                                           (list
                                                            (make-write-target
                                                             (first positionals)
                                                             :follow
                                                             nil))
                                                           :guard-requirements
                                                           (when recursive
                                                             '((:expect-count)))
                                                           :expect-count
                                                           (getf options
                                                                 :expect-count)
                                                           :expect-hashes
                                                           (getf options
                                                                 :expect-hash)
                                                           :plan
                                                           (lambda
                                                               (context &key
                                                                        on-committed
                                                                        on-rejected)
                                                             (aitools.kernel.domain:flow-call
                                                              (%rm-requests/k
                                                               context
                                                               recursive
                                                               max-entries
                                                               max-bytes)
                                                              (:on-error
                                                               (code message
                                                                     &key
                                                                     candidates
                                                                     diagnostics
                                                                     conflicts
                                                                     repairs
                                                                     path)
                                                               (declare (ignore
                                                                         path))
                                                               (funcall
                                                                on-rejected
                                                                code
                                                                message
                                                                :candidates
                                                                candidates
                                                                :diagnostics
                                                                diagnostics
                                                                :conflicts
                                                                conflicts
                                                                :repairs
                                                                repairs))
                                                              (:on-committed
                                                               (requests fields)
                                                               (funcall
                                                                on-committed
                                                                requests
                                                                fields))))
                                                           :record-options
                                                           options
                                                           :record-positionals
                                                           (lambda (paths)
                                                             paths)))))))

(define-write-command "delete"
                      (ports env positionals options &key on-plan on-error)
                      (if (/= (length positionals) 1)
                          (flow-continue :on-error
                                         "argument.invalid"
                                         "delete takes exactly one path")
                          (flow-continue :on-plan
                                         (make-write-plan :command
                                                          "delete"
                                                          :targets
                                                          (list
                                                           (make-write-target
                                                            (first positionals)
                                                            :follow
                                                            nil))
                                                          :expect-hashes
                                                          (getf options
                                                                :expect-hash)
                                                          :plan
                                                          (lambda
                                                              (context &key
                                                                       on-committed
                                                                       on-rejected)
                                                            (let ((path
                                                                   (context-path
                                                                    context)))
                                                              (if (eq
                                                                   (%kind
                                                                    context
                                                                    path)
                                                                   :absent)
                                                                  (aitools.kernel.domain:flow-call
                                                                   (%not-found
                                                                    context
                                                                    path)
                                                                   (:on-error
                                                                    (code
                                                                     message
                                                                     &key
                                                                     candidates
                                                                     diagnostics
                                                                     conflicts
                                                                     repairs
                                                                     path)
                                                                    (declare (ignore
                                                                              path))
                                                                    (funcall
                                                                     on-rejected
                                                                     code
                                                                     message
                                                                     :candidates
                                                                     candidates
                                                                     :diagnostics
                                                                     diagnostics
                                                                     :conflicts
                                                                     conflicts
                                                                     :repairs
                                                                     repairs)))
                                                                  (funcall
                                                                   on-committed
                                                                   (list
                                                                    (aitools.store.domain:delete-request
                                                                     path))))))
                                                          :record-options
                                                          options
                                                          :record-positionals
                                                          (lambda (paths)
                                                            paths)))))

(define-write-command "mkdir"
                      (ports env positionals options &key on-plan on-error)
                      (if (/= (length positionals) 1)
                          (flow-continue :on-error
                                         "argument.invalid"
                                         "mkdir takes exactly one path")
                          (flow-continue :on-plan
                                         (make-write-plan :command
                                                          "mkdir"
                                                          :targets
                                                          (list
                                                           (make-write-target
                                                            (first positionals)))
                                                          :plan
                                                          (lambda
                                                              (context &key
                                                                       on-committed
                                                                       on-rejected)
                                                            (declare (ignore
                                                                      on-rejected))
                                                            (funcall
                                                             on-committed
                                                             (list
                                                              (aitools.store.domain:mkdir-request
                                                               (context-path
                                                                context)))))
                                                          :record-options
                                                          options
                                                          :record-positionals
                                                          (lambda (paths)
                                                            paths)))))

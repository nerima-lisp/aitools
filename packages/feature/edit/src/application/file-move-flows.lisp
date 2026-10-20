(in-package #:aitools.edit.application)

(defun %per-path-move-requests (context source destination)
  "A directory move inside a tx, recorded per path: recreate every
entry below DESTINATION, then delete the originals, children first."
  (let ((creates (list (aitools.store.domain:mkdir-request destination)))
        (deletes (list (aitools.store.domain:delete-request source)))
        (view (write-context-view context)))
    (dolist (entry (%walk-tree context source))
      (let ((path (car entry))
            (kind (cdr entry)))
        (let ((target (%replace-rebase path source destination))
              (state (aitools.store.application:view-path-state view path)))
          (push (aitools.store.domain:delete-request path) deletes)
          (push
           (ecase kind
             (:directory (aitools.store.domain:mkdir-request target))
             (:file
              (aitools.store.domain:write-file-request target
                                                       (aitools.store.application:view-read-file
                                                        view
                                                        path)
                                                       :mode
                                                       (aitools.store.domain:entry-state-mode
                                                        state)))
             (:symlink
              (aitools.store.domain:symlink-request target
                                                    (aitools.store.domain:entry-state-target
                                                     state))))
           creates))))
    (append (nreverse creates) deletes)))

(define-write-command "move"
                      (ports env positionals options &key on-plan on-error)
                      (if (/= (length positionals) 2)
                          (flow-continue :on-error
                                         "argument.invalid"
                                         "move takes SRC and DST")
                          (flow-continue :on-plan
                                         (make-write-plan :command
                                                          "move"
                                                          :targets
                                                          (list
                                                           (make-write-target
                                                            (first positionals)
                                                            :follow
                                                            nil)
                                                           (make-write-target
                                                            (second positionals)
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
                                                            (let* ((source
                                                                    (context-path
                                                                     context
                                                                     0))
                                                                   (destination
                                                                    (context-path
                                                                     context
                                                                     1))
                                                                   (kind
                                                                    (%kind
                                                                     context
                                                                     source)))
                                                              (if (eq kind
                                                                      :absent)
                                                                  (aitools.kernel.domain:flow-call
                                                                   (%not-found
                                                                    context
                                                                    source)
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
                                                                  (aitools.kernel.domain:flow-call
                                                                   (%destination-check/k
                                                                    context
                                                                    destination
                                                                    kind
                                                                    (getf
                                                                     options
                                                                     :overwrite))
                                                                   (:on-rejected
                                                                    (code
                                                                     message
                                                                     &key
                                                                     candidates
                                                                     diagnostics
                                                                     conflicts
                                                                     repairs
                                                                     path)
                                                                    (funcall on-rejected code message :candidates candidates :diagnostics diagnostics :conflicts conflicts :repairs repairs :path path))
                                                                   (:on-ok ()
                                                                           (funcall
                                                                            on-committed
                                                                            (if (and
                                                                                 (eq
                                                                                  kind
                                                                                  :directory)
                                                                                 (write-context-tx
                                                                                  context))
                                                                                (%per-path-move-requests
                                                                                 context
                                                                                 source
                                                                                 destination)
                                                                                (list
                                                                                 (aitools.store.domain:move-request
                                                                                  source
                                                                                  destination)))))))))
                                                          :record-options
                                                          options
                                                          :record-positionals
                                                          (lambda (paths)
                                                            paths)))))

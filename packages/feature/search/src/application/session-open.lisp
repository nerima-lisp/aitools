(in-package #:aitools.search.application)

;;; ------------------------------------------------------------ session setup

(aitools.kernel.domain:define-flow (%unavailable command &key on-error)
  (aitools.kernel.domain:flow-continue :on-error "internal.unexpected"
   "the search context is missing a required capability"
   :repairs (list (repair "inspect-schema" "Show this command's arguments and rules."
                          (format nil "aitools schema ~A" command)))))

(aitools.kernel.domain:define-flow
    (%open-tx/k boundaries workspace-root tx command &key on-session on-error)
  (let ((open-store (cl-boundary-kit:boundary-context-require boundaries :search-open-store)))
    (if (null open-store)
        (aitools.kernel.domain:flow-call (%unavailable command)
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs)))
        (let ((store (funcall open-store (aitools.workspace.application:workspace-root-real workspace-root)))
              (staged (make-hash-table :test 'equal)))
          (flet ((missing ()
                   (aitools.kernel.domain:flow-continue :on-error
                    "input.not-found" (format nil "tx ~A does not exist" tx)
                    :repairs (list (repair "list-tx" "List the open transactions." "aitools tx status"))))
                 (with-view (view)
                   (aitools.kernel.domain:flow-continue :on-session
                    (%make-session boundaries workspace-root (%make-overlay view staged (%now boundaries))
                                   view staged))))
            (aitools.kernel.domain:flow-call
             (aitools.store.application:tx-status/k
              store
              tx)
             (:on-not-found ()
               (funcall #'missing))
             (:on-status (status)
               (dolist (entry (aitools.store.application:tx-status-paths status))
                 (setf (gethash
                        (aitools.store.domain:tx-path-path entry)
                        staged)
                       (aitools.store.domain:tx-path-staged entry)))
               (aitools.kernel.domain:flow-call
                (aitools.store.application:call-with-tx-view/k
                 store
                 tx)
                (:on-view (view)
                  (funcall #'with-view view))
                (:on-not-found ()
                  (funcall #'missing))))))))))

(aitools.kernel.domain:define-flow
    (call-with-session/k boundaries command &key root tx on-session on-error)
  "Resolve the workspace root by --root, git top, then working directory. Open
TX's read-through view when given and continue with the session or an error.
COMMAND names the command used to build repairs."
  (let ((host (cl-boundary-kit:boundary-context-require boundaries :search-workspace-host)))
    (if (not (and host (cl-boundary-kit:boundary-context-require boundaries :search-text-context)))
        (aitools.kernel.domain:flow-call (%unavailable command)
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs)))
        (aitools.workspace.application:call-with-resolved-root/k
         host :root root
         :on-error (lambda (reason path)
                     (aitools.kernel.domain:flow-continue :on-error
                      (if (eq reason :not-found) "input.not-found" "argument.invalid")
                      (format nil "workspace root ~A ~A" path
                              (if (eq reason :not-found) "does not exist" "is not a directory"))
                      :repairs
                      (list (repair "use-default-root" "Run from the workspace without --root."
                                    (format nil "aitools ~A" command)))))
         :on-resolved (lambda (workspace-root)
                        (if tx
                            (aitools.kernel.domain:flow-call
                                (%open-tx/k boundaries workspace-root tx command)
                              (:on-session (session)
                                (aitools.kernel.domain:flow-continue :on-session session))
                              (:on-error (code message &key repairs)
                                (aitools.kernel.domain:flow-continue
                                 :on-error code message :repairs repairs)))
                            (aitools.kernel.domain:flow-continue
                             :on-session (%make-session boundaries workspace-root nil nil nil))))))))

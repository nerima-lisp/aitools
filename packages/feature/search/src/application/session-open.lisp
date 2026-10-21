(in-package #:aitools.search.application)

;;; ------------------------------------------------------------ session setup

(defun %unavailable (on-error command)
  (funcall on-error "internal.unexpected"
           "the search context was built without its workspace host or text source"
           :repairs (list (repair "inspect-schema" "Show this command's arguments and rules."
                                   (format nil "aitools schema ~A" command)))))

(defun %open-tx/k (ports workspace-root tx command on-session on-error)
  "Open TX's read-through view of WORKSPACE-ROOT and call ON-SESSION with a
session carrying the tx overlay, or ON-ERROR."
  (let ((open-store (search-ports-open-store ports)))
    (if (null open-store)
        (%unavailable on-error command)
        (let ((store (funcall open-store (aitools.workspace.application:workspace-root-real workspace-root)))
              (staged (make-hash-table :test (function equal))))
          (flet ((missing ()
                   (funcall on-error "input.not-found" (format nil "tx ~A does not exist" tx)
                            :repairs (list (repair "list-tx" "List the open transactions." "aitools tx status"))))
                 (with-view (view)
                   (funcall on-session
                            (%make-session ports workspace-root (%make-overlay view staged (%now ports))
                                           view staged))))
            (aitools.kernel.domain:flow-call
             (aitools.store.application:tx-status/k store tx)
             (:on-not-found ()
               (funcall (function missing)))
             (:on-status (status)
               (dolist (entry (aitools.store.application:tx-status-paths status))
                 (setf (gethash (aitools.store.domain:tx-path-path entry) staged)
                       (aitools.store.domain:tx-path-staged entry)))
               (aitools.kernel.domain:flow-call
                (aitools.store.application:call-with-tx-view/k store tx)
                (:on-view (view)
                  (funcall (function with-view) view))
                (:on-not-found ()
                  (funcall (function missing)))))))))))


(defun call-with-session/k (ports command &key root tx on-session on-error)
  "Resolve the workspace root (the global `--root` ROOT, else git's top
level, else the working directory), open TX's read-through view when TX is
given, and call ON-SESSION (session), or ON-ERROR with an error code,
message, and repairs. COMMAND names the command for repairs."
  (declare (type function on-session on-error))
  (let ((host (search-ports-workspace-host ports)))
    (if (not (and host (search-ports-text-source ports)))
        (%unavailable on-error command)
        (aitools.workspace.application:call-with-resolved-root/k
         host
         :root root
         :on-error (lambda (reason path)
                     (funcall on-error (if (eq reason :not-found) "input.not-found" "argument.invalid")
                              (format nil "workspace root ~A ~A" path
                                      (if (eq reason :not-found) "does not exist" "is not a directory"))
                              :repairs (list (repair "use-default-root" "Run from the workspace without --root."
                                                      (format nil "aitools ~A" command)))))
         :on-resolved (lambda (workspace-root)
                        (if tx
                            (%open-tx/k ports workspace-root tx command on-session on-error)
                            (funcall on-session (%make-session ports workspace-root nil nil nil))))))))

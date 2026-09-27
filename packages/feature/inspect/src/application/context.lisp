;;;; packages/feature/inspect/src/application/context.lisp
;;;;
;;;; What every inspect flow needs before it touches a file: the workspace
;;;; root (`--root`, git top, or cwd), the working directory that
;;;; relative arguments are resolved against, and for `--tx` the tx's view
;;;; of the workspace (disk plus the tx's staged changes). Also the error and command-text helpers every
;;;; flow shares, so each error carries a complete `repairs[].command`.
(in-package #:aitools.inspect.application)

(defstruct (inspect-context (:constructor %make-inspect-context) (:copier nil))
  (ports nil :type inspect-ports :read-only t)
  (root nil :read-only t)
  (cwd "" :type string :read-only t)
  ;; The `--root` argument as typed, repeated in generated commands.
  (root-argument nil :read-only t)
  (tx nil :type (or null string) :read-only t)
  (store nil :read-only t)
  ;; A STORE-VIEW of the tx, or NIL without `--tx`.
  (view nil :read-only t)
  (lock-timeout-ms 0 :type integer :read-only t))

(defun repair (action detail command)
  (list :action action :detail detail :command command))

(defun fail (on-error code message &rest keys &key repairs candidates diagnostics conflicts)
  "Call ON-ERROR for CODE with KEYS. Every caller names at least one repair."
  (declare (ignore repairs candidates diagnostics conflicts))
  (apply on-error code message keys))

(defun context-host (context)
  (inspect-ports-workspace-host (inspect-context-ports context)))

(defun context-source (context)
  (inspect-ports-text-source (inspect-context-ports context)))

(defun context-root-real (context)
  (workspace-root-real (inspect-context-root context)))

(defun command-line (context words &key tx)
  "The complete command `aitools [--root R] WORDS... [--tx T]`, each word
shell-quoted. TX is the context's tx unless the keyword TX is :NONE."
  (let ((tx (and (not (eq tx :none)) (inspect-context-tx context)))
        (root (inspect-context-root-argument context)))
    (format nil "aitools~@[ --root ~A~]~{ ~A~}~@[ --tx ~A~]"
            (and root (shell-quote root))
            (mapcar #'shell-quote words)
            (and tx (shell-quote tx)))))

(defun %lock-timeout-ms (text)
  "Milliseconds for a `--lock-timeout` TEXT (NIL for the store default), or
NIL when TEXT is not a duration."
  (if (null text)
      +default-lock-timeout-ms+
      (handler-case (duration-milliseconds (parse-duration text))
        (error () nil))))

(defun call-with-inspect-context/k (ports &key root tx lock-timeout on-ready on-error)
  "Resolve the root and, with TX, the tx view; call ON-READY (context) or
report the failure through ON-ERROR (the command-result error continuation)."
  (declare (type function on-ready on-error))
  (let ((host (inspect-ports-workspace-host ports))
        (timeout (%lock-timeout-ms lock-timeout)))
    (if (null timeout)
        (fail on-error "argument.invalid" (format nil "--lock-timeout ~S is not a duration" lock-timeout)
              :repairs (list (repair "fix-argument" "Give a duration such as 10s." "aitools schema")))
        (call-with-resolved-root/k
         host
         :root root
         :on-error (lambda (reason path)
                     (fail on-error "input.not-found"
                           (format nil "workspace root ~A ~A" path
                                   (if (eq reason :not-a-directory) "is not a directory" "does not exist"))
                           :repairs (list (repair "use-default-root" "Omit --root to use the git root or the working directory."
                                                  "aitools info ."))))
         :on-resolved
         (lambda (resolved)
           (let ((cwd (normalize-path (host-current-directory host))))
             (flet ((ready (store view)
                      (funcall on-ready (%make-inspect-context :ports ports :root resolved :cwd cwd
                                                               :root-argument root :tx tx :store store
                                                               :view view :lock-timeout-ms timeout))))
               (if (null tx)
                   (ready nil nil)
                   (let ((store (funcall (inspect-ports-open-store ports) (workspace-root-real resolved))))
                     (call-with-tx-view/k
                      store tx
                      :on-view (lambda (view) (ready store view))
                      :on-not-found (lambda ()
                                      (fail on-error "input.not-found" (format nil "tx ~A does not exist" tx)
                                            :repairs (list (repair "list-tx" "List the open transactions."
                                                                   "aitools tx status"))))))))))))))

(defun context-store (context)
  "The workspace's STORE, opened on first use."
  (or (inspect-context-store context)
      (funcall (inspect-ports-open-store (inspect-context-ports context)) (context-root-real context))))

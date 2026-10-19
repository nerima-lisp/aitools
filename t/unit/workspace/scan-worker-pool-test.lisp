(in-package #:aitools.workspace.test)

(describe "aitools.workspace.application scan: worker pool lifecycle"
  (it "does not create a worker pool for a one-file scan"
    (let* ((base (repo-host '(("a.txt" . ""))))
           (host (make-workspace-host
                  :list-directory (lambda (path) (host-list-directory base path))
                  :stat (lambda (path) (host-stat base path))
                  :read-link (lambda (path) (host-read-link base path))
                  :read-octets (lambda (path) (host-read-octets base path))
                  :getenv (lambda (name) (host-getenv base name))
                  :home-directory (lambda () (host-home-directory base))
                  :current-directory (lambda () (host-current-directory base))
                  :call-with-ordered-mapper
                  #'aitools.workspace.infrastructure:call-with-ordered-mapper))
           (calls 0)
           (scan-thread (cl-concurrent-kit:current-thread))
           (make-executor #'cl-concurrent-kit:make-executor))
      ;; Other specs can create pools in parallel; count only this scan.
      (cl-weave:with-mocked-functions
          (((symbol-function 'cl-concurrent-kit:make-executor)
            (lambda (&rest arguments)
              (when (eq (cl-concurrent-kit:current-thread) scan-thread)
                (incf calls))
              (apply make-executor arguments))))
        (expect (scan-paths host (resolved-root host) :work #'scan-entry-path)
                :to-equal '("a.txt"))
        (expect calls :to-be 0))))
)

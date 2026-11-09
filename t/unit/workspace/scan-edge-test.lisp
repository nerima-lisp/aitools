(in-package #:aitools.workspace.test)

(describe "aitools.workspace.application scan boundary callbacks"
  (it "reports an unreadable directory through ON-SKIP"
    (let* ((base (repo-host '(("locked/child" . ""))))
           (host
             (make-workspace-boundaries
              :list-directory
              (lambda (path)
                (if (string= path "/r/locked")
                    (values nil nil)
                    (host-list-directory base path)))
              :stat (lambda (path) (host-stat base path))
              :read-link (lambda (path) (host-read-link base path))
              :read-octets (lambda (path) (host-read-octets base path))
              :environment (cl-boundary-kit:boundary-context-require base :environment)
              :working-directory (cl-boundary-kit:boundary-context-require base :working-directory)
              :home-directory (lambda () (host-home-directory base))))
           (skipped '()))
      (expect (scan-paths host (resolved-root host)
                          :on-skip (lambda (entry reason)
                                     (push (list (scan-entry-path entry) reason) skipped)))
              :to-equal '("locked"))
      (expect skipped :to-equal '(("locked" :unreadable)))))
  (it "returns the entry mode through the scan accessor"
    (let* ((entry (make-workspace-entry :name "mode" :mode #o640))
           (candidate (aitools.workspace.application::%make-scan-entry "mode" "/r/mode" entry)))
      (expect (scan-entry-mode candidate) :to-be #o640)))
  (it "never classifies a path outside the root as ignored"
    (let* ((host (repo-host '(("a.log" . ""))))
           (root (resolved-root host)))
      (expect (multiple-value-list (workspace-path-ignored-p host root "/elsewhere"))
              :to-equal '(nil :gitignore)))))

(describe "workspace scan exit and default limit"
  (it "stops a work batch after the first emitted result"
    (let* ((host (repo-host '(("a" . "") ("b" . ""))))
           (completed nil)
           (seen '()))
      (call-with-workspace-scan/k
       host (resolved-root host)
       :work #'scan-entry-path
       :emit (lambda (entry result)
               (push (list (scan-entry-path entry) result) seen)
               :stop)
       :on-complete (lambda (source stopped)
                      (setf completed (list source stopped)))
       :on-error (constantly nil))
      (expect seen :to-equal '(("a" "a")))
      (expect completed :to-equal '(:gitignore t))))
  (it "stops later explicit starts after a nested emit"
    (let* ((host (repo-host '(("a/child" . "") ("b" . ""))))
           (completed nil)
           (seen '()))
      (call-with-workspace-scan/k
       host (resolved-root host) :paths '("a" "b")
       :emit (lambda (entry result)
               (declare (ignore result))
               (push (scan-entry-path entry) seen)
               :stop)
       :on-complete (lambda (source stopped)
                      (setf completed (list source stopped)))
       :on-error (constantly nil))
      (expect seen :to-equal '("a/child"))
      (expect completed :to-equal '(:gitignore t))))
  (it "uses the default large-file limit"
    (let* ((base (repo-host '(("large" . ""))))
           (host
             (make-workspace-boundaries
              :list-directory
              (lambda (path)
                (multiple-value-bind (entries readable)
                    (host-list-directory base path)
                  (values
                   (mapcar (lambda (entry)
                             (if (string= (workspace-entry-name entry) "large")
                                 (make-workspace-entry
                                  :name "large" :kind :file
                                  :size (1+ +default-skip-larger-than+))
                                 entry))
                           entries)
                   readable)))
              :stat (lambda (path) (host-stat base path))
              :read-link (lambda (path) (host-read-link base path))
              :read-octets (lambda (path) (host-read-octets base path))
              :environment (cl-boundary-kit:boundary-context-require base :environment)
              :working-directory (cl-boundary-kit:boundary-context-require base :working-directory)
              :home-directory (lambda () (host-home-directory base))))
           (skipped nil))
      (expect (scan-paths host (resolved-root host)
                          :on-skip (lambda (entry reason)
                                     (push (list (scan-entry-path entry) reason) skipped)))
              :to-equal nil)
      (expect skipped :to-equal '(("large" :too-large))))))

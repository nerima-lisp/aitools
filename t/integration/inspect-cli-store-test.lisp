;;;; t/integration/inspect-cli-store-test.lisp
;;;;
;;;; Inspect commands against a real store: `--tx` reads that cannot record
;;;; the read set, `--tx` reads of staged kinds and modes, `diff --op` when a
;;;; change's content is gone, and snapshots without a state-directory
;;;; function. Workspace and store helpers come from inspect-cli-test.lisp.
(in-package #:aitools.integration.inspect-cli-test)

(defun begin-tx (store)
  (aitools.store.application:tx-begin/k store :on-begun (lambda (id name created) (declare (ignore name created)) id)
                                              :on-busy (lambda () (fail "busy"))))

(defun delegating-host (&key on-stat)
  "The production workspace host, calling ON-STAT (path) before each stat."
  (let ((host (aitools.workspace.infrastructure:make-host-workspace-host)))
    (aitools.workspace.application:make-workspace-host
     :list-directory (lambda (path) (aitools.workspace.application:host-list-directory host path))
     :stat (lambda (path) (funcall on-stat path) (aitools.workspace.application:host-stat host path))
     :read-link (lambda (path) (aitools.workspace.application:host-read-link host path))
     :read-octets (lambda (path) (aitools.workspace.application:host-read-octets host path))
     :getenv (lambda (name) (aitools.workspace.application:host-getenv host name))
     :home-directory (lambda () (aitools.workspace.application:host-home-directory host))
     :current-directory (lambda () (aitools.workspace.application:host-current-directory host)))))

(describe "--tx reads that cannot record the read set"
  (it "answers environment.busy with a retry command while another thread holds the tx lock"
    (with-workspace (root)
      (let* ((file (ws-file root "f.txt" (format nil "x~%")))
             (store (open-store root))
             (tx (begin-tx store))
             (held (sb-thread:make-semaphore))
             (release (sb-thread:make-semaphore))
             (holder (sb-thread:make-thread
                      (lambda ()
                        (aitools.store.application:call-with-tx-lock/k
                         store tx 5000
                         :on-acquired (lambda () (sb-thread:signal-semaphore held) (sb-thread:wait-on-semaphore release))
                         :on-timeout (lambda () (sb-thread:signal-semaphore held))
                         :on-not-found (lambda () (sb-thread:signal-semaphore held)))))))
        (unwind-protect
             (progn
               (sb-thread:wait-on-semaphore held)
               (multiple-value-bind (code envelope)
                   (run-aitools "--root" root "--lock-timeout" "50ms" "read" file "--tx" tx)
                 (expect code :to-be 1)
                 (expect (value-at envelope "error" "code") :to-equal "environment.busy")
                 (expect (value-at envelope "error" "repairs" 0 "command")
                         :to-equal (format nil "aitools --root ~A read ~A --tx ~A" root file tx))))
          (sb-thread:signal-semaphore release)
          (sb-thread:join-thread holder)))))

  (it "answers input.not-found when the tx is aborted between opening its view and recording the read"
    (with-workspace (root)
      (let* ((file (ws-file root "f.txt" (format nil "x~%")))
             (store (open-store root))
             (tx (begin-tx store))
             (aborted nil)
             (ports (aitools.inspect.application:make-inspect-ports
                     :workspace-host (delegating-host
                                      :on-stat (lambda (path)
                                                 (when (and (not aborted) (string= path (string-right-trim "/" file)))
                                                   (setf aborted t)
                                                   (aitools.store.application:tx-abort/k
                                                    store tx :on-aborted #'identity
                                                             :on-not-found (lambda () (fail "tx vanished early"))
                                                             :on-busy (lambda () (fail "busy"))))))
                     :text-source (aitools.text.infrastructure:make-host-text-source)
                     :open-store #'open-store
                     :state-directory-function (constantly nil)))
             (result (aitools.protocol.application:call-with-command-result/k
                      (lambda (&rest continuations)
                        (apply #'aitools.inspect.application:read-flow ports file :root root :tx tx continuations)))))
        (expect aborted :to-be t)
        (expect (aitools.protocol.application:command-result-kind result) :to-be :error)
        (let ((fields (aitools.protocol.application:command-result-fields result)))
          (expect (getf fields :code) :to-equal "input.not-found")
          (expect (getf fields :message) :to-equal (format nil "tx ~A does not exist" tx))
          (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools tx status"))))))

(defun stage (store tx argv &rest requests)
  (aitools.store.application:tx-stage/k
   store tx argv
   (lambda (view commit reject)
     (declare (ignore view reject))
     (funcall commit requests))
   :on-staged (lambda (tx-op results) (declare (ignore results)) tx-op)
   :on-rejected (lambda (code message &rest keys) (declare (ignore keys)) (fail (format nil "~A ~A" code message)))
   :on-not-found (lambda () (fail "tx not found"))
   :on-busy (lambda () (fail "busy"))))

(describe "--tx reads see the tx's staged kinds and modes"
  (it "reads a staged deletion as missing, a staged new directory as a directory, and a staged write's mode"
    (with-workspace (root)
      (ws-file root "gone.txt" (format nil "g~%"))
      (let ((cfg (ws-file root "cfg.txt" (format nil "c~%"))))
        (sb-posix:chmod cfg #o644)
        (let* ((store (open-store root))
               (tx (begin-tx store)))
          (stage store tx (list "rm" "gone.txt") (aitools.store.domain:delete-request "gone.txt"))
          (stage store tx (list "write" "sub/new.txt")
                 (aitools.store.domain:write-file-request "sub/new.txt" (sb-ext:string-to-octets "n" :external-format :utf-8)))
          (stage store tx (list "write" "cfg.txt")
                 (aitools.store.domain:write-file-request "cfg.txt" (sb-ext:string-to-octets "staged" :external-format :utf-8)
                                                          :mode #o600))
          (multiple-value-bind (code envelope) (run-aitools "--root" root "read" (concatenate 'string root "gone.txt") "--tx" tx)
            (expect code :to-be 1)
            (expect (value-at envelope "error" "code") :to-equal "input.not-found"))
          (multiple-value-bind (code envelope) (run-aitools "--root" root "read" (concatenate 'string root "sub") "--tx" tx)
            (expect code :to-be 1)
            (expect (value-at envelope "error" "code") :to-equal "refusal.not-a-file")
            (expect (value-at envelope "error" "message") :to-contain "is a directory"))
          (multiple-value-bind (code envelope) (run-aitools "--root" root "info" cfg "--tx" tx)
            (expect code :to-be 0)
            (expect (value-at envelope "mode") :to-equal "0600"))
          (multiple-value-bind (code envelope) (run-aitools "--root" root "info" cfg)
            (expect code :to-be 0)
            (expect (value-at envelope "mode") :to-equal "0644")))))))

(describe "diff --op when a change's content is gone"
  (it "reports content_available false instead of a diff"
    (with-workspace (root)
      (let* ((store (open-store root))
             (first-op (commit-write store "f.txt" (format nil "one~%")))
             (second-op (commit-write store "f.txt" (format nil "one~%two~%"))))
        (declare (ignore first-op))
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname
          (concatenate 'string (aitools.store.application:store-state-directory store) "/blobs"))
         :validate t)
        (write-bytes (concatenate 'string root "f.txt") (format nil "rewritten~%"))
        (multiple-value-bind (code envelope) (run-aitools "--root" root "diff" "--op" second-op)
          (expect code :to-be 0)
          (expect (value-at envelope "changes" 0 "path") :to-equal "f.txt")
          (expect (value-at envelope "changes" 0 "content_available") :to-be json-kit:+json-false+)
          (expect (value-at envelope "changes" 0 "diff") :to-be nil))))))

(describe "snapshot flows without a state-directory function"
  (it "keeps snapshots in the store's state directory"
    (with-workspace (root)
      (ws-file root "a.txt" "a")
      (let* ((ports (aitools.inspect.application:make-inspect-ports
                     :workspace-host (aitools.workspace.infrastructure:make-host-workspace-host)
                     :text-source (aitools.text.infrastructure:make-host-text-source)
                     :open-store #'open-store
                     :state-directory-function (constantly nil)))
             (created (aitools.protocol.application:call-with-command-result/k
                       (lambda (&rest continuations)
                         (apply #'aitools.inspect.application:snapshot-create-flow ports :root root continuations))))
             (id (cdr (assoc "snapshot_id" (aitools.protocol.application:command-result-fields created) :test #'string=))))
        (expect (probe-file (format nil "~A/snapshots/~A.json"
                                    (aitools.store.application:store-state-directory (open-store root)) id))
                :to-be-truthy)))))

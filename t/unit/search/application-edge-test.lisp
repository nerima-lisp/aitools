(in-package #:aitools.search.test)

(describe "aitools.search.application edge paths"
  (it "forwards a missing boundary through each code flow"
    (let ((boundaries
            (cl-boundary-kit:make-boundary-context
             :search-workspace-host nil
             :search-text-context nil
             :search-open-store nil
             :search-unix-now nil
             :search-read-stdin-octets nil)))
      (dolist (flow (list #'code-outline/k #'code-defs/k #'code-refs/k))
        (multiple-value-bind (kind fields) (run-flow flow boundaries)
          (expect kind :to-be :error)
          (expect (getf fields :code) :to-equal "internal.unexpected")
          (expect (getf fields :message) :to-contain "missing a required capability")))))

  (it "skips an explicitly selected symlink and a file that disappears before reading"
    (let ((boundaries
            (make-fake-search-boundaries
             :files '(("/w/target.lisp" "(defun hidden ())"))
             :symlinks '(("/w/link.lisp" . "target.lisp")))))
      (multiple-value-bind (kind fields)
          (run-flow #'code-defs/k boundaries :name "hidden" :path "link.lisp")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 0)))
    (let ((boundaries
            (make-fake-search-boundaries
             :files '(("/w/gone.lisp" "(defun hidden ())"))
             :unreadable '("/w/gone.lisp"))))
      (multiple-value-bind (kind fields)
          (run-flow #'code-defs/k boundaries :name "hidden")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 0))))

  (it "turns an expired code scan regex budget into no definitions"
    (let ((aitools.search.domain::*regex-run-deadline* 0))
      (multiple-value-bind (kind fields)
          (run-flow #'code-defs/k
                    (make-fake-search-boundaries
                     :files '(("/w/a.lisp" "(defun hidden ())")))
                    :name "hidden")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 0))))

  (it "forwards an unsupported language from find's scan options"
    (multiple-value-bind (kind fields)
        (run-flow #'find/k
                  (make-fake-search-boundaries :directories '("/w"))
                  :lang "cobol")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "argument.invalid")))

  (it "keeps an unreadable source out of overview language totals"
    (multiple-value-bind (kind fields)
        (run-flow #'overview/k
                  (make-fake-search-boundaries
                   :files '(("/w/a.lisp" "(defun hidden ())"))
                   :unreadable '("/w/a.lisp")))
      (expect kind :to-be :ok)
      (expect (field fields "languages") :to-equal '())))

  (it "uses an empty standard-input pattern when the reader returns no octets"
    (multiple-value-bind (kind fields)
        (run-flow #'search/k
                  (make-fake-search-boundaries
                   :files (list (list "/w/a.txt" (format nil "first~%second~%")))
                   :stdin "")
                  :stdin t :output :count)
      (expect kind :to-be :ok)
      (expect (field fields "total_matches") :to-be 2)))

  (it "merges staged directories and deletions into the workspace overlay"
    (let* ((staged (make-hash-table :test 'equal))
           (disk-root
             (list (aitools.workspace.application:make-workspace-entry
                    :name "existing" :kind :directory :size 0 :mtime 10 :mode #o755)
                   (aitools.workspace.application:make-workspace-entry
                    :name "gone" :kind :file :size 1 :mtime 10 :mode #o644)))
           (disk-existing
             (list (aitools.workspace.application:make-workspace-entry
                    :name "mode" :kind :directory :size 0 :mtime 20 :mode #o711))))
      (setf (gethash "gone" staged) (aitools.store.domain::absent-state)
            (gethash "new/deep" staged) (aitools.store.domain::directory-state)
            (gethash "existing/deep" staged) (aitools.store.domain::directory-state)
            (gethash "existing/mode" staged) (aitools.store.domain::directory-state))
      (let* ((overlay (aitools.search.application::%make-overlay nil staged 100))
             (list-directory
               (aitools.workspace.application::workspace-overlay-list-directory overlay))
             (root-entries (funcall list-directory "" disk-root))
             (existing-entries (funcall list-directory "existing" disk-existing))
             (read-octets
               (aitools.workspace.application::workspace-overlay-read-octets overlay)))
        (expect (find "gone" root-entries
                      :key #'aitools.workspace.application:workspace-entry-name
                      :test #'string=)
                :to-be nil)
        (expect (aitools.workspace.application:workspace-entry-kind
                 (find "new" root-entries
                       :key #'aitools.workspace.application:workspace-entry-name
                       :test #'string=))
                :to-be :directory)
        (expect (aitools.workspace.application:workspace-entry-mode
                 (find "mode" existing-entries
                       :key #'aitools.workspace.application:workspace-entry-name
                       :test #'string=))
                :to-be #o711)
        (multiple-value-bind (override octets) (funcall read-octets "existing/deep")
          (expect override :to-be t)
          (expect octets :to-be nil)))))

  (it "reports a transaction removed between status and view reads"
    (let ((boundaries
            (make-fake-search-boundaries
             :directories '("/w")
             :open-store (lambda (root) (declare (ignore root)) :store))))
      (cl-weave:with-mocked-functions
          (((symbol-function 'aitools.store.application:tx-status/k)
            (lambda (store tx-id &key on-status on-not-found)
              (declare (ignore store tx-id on-not-found))
              (funcall on-status
                       (aitools.store.application::make-tx-status
                        :id "vanished" :created ""))))
           ((symbol-function 'aitools.store.application:call-with-tx-view/k)
            (lambda (store tx-id &key on-view on-not-found)
              (declare (ignore store tx-id on-view))
              (funcall on-not-found))))
        (let ((result nil))
          (aitools.search.application::call-with-session/k
           boundaries "search" :tx "vanished"
           :on-session
           (lambda (session)
             (declare (ignore session))
             (setf result :session))
           :on-error
           (lambda (code message &key repairs)
             (declare (ignore repairs))
             (setf result (list code message))))
          (expect (first result) :to-equal "input.not-found")
          (expect (second result) :to-equal "tx vanished does not exist"))))))


;;;; t/integration/journal-tx-failure-test.lisp
;;;;
;;;; Journal and tx flows when the store fails (environment.io, via injected
;;;; I/O faults), when the tx lock is contended (environment.busy), and the
;;;; repairs they name without global options. Append helpers come from
;;;; journal-tx-test.lisp.
(in-package #:aitools.journal.test)

(defun call-with-io-fault-at (point thunk)
  "Call THUNK with the store's fault hook signalling one STORE-IO-ERROR, the
condition a failing rename or fsync raises, at the first POINT."
  (let* ((fired nil)
         (aitools.store.application:*fault-hook*
           (lambda (name &rest details)
             (declare (ignore details))
             (when (and (eq name point) (not fired))
               (setf fired t)
               (error 'aitools.store.application:store-io-error
                      :operation "rename" :path "injected" :detail "injected I/O failure")))))
    (multiple-value-prog1 (funcall thunk)
      (assert fired () "the fault point ~S was never reached" point))))

(defun run-with (ports context flow &rest arguments)
  "Like RUN, with explicit PORTS and CONTEXT."
  (let ((result (aitools.protocol.application:call-with-command-result/k
                 (lambda (&rest continuations)
                   (apply flow ports context (append arguments continuations))))))
    (values (aitools.protocol.application:command-result-kind result)
            (aitools.protocol.application:command-result-fields result))))

(defun open-tx-with-append (store)
  "A tx staging a.txt from \"a\" to \"a+1\"; returns its id."
  (put-file store "a.txt" "a")
  (let ((tx (begin store)))
    (stage-append store tx "a.txt" "+1")
    tx))

(describe "aitools.journal store failures (environment.io)"
  (it-each ((:after-validate) (:after-prepare))
      "rejects tx commit with environment.io and writes nothing when the store fails at ~S"
      (point)
    (with-temp-store (store)
      (let ((tx (open-tx-with-append store)))
        (multiple-value-bind (kind error)
            (call-with-io-fault-at point (lambda () (run #'tx-commit-flow store tx)))
          (expect kind :to-be :error)
          (expect (getf error :code) :to-equal "environment.io")
          (expect (search "injected I/O failure" (getf error :message)) :to-be-truthy)
          (expect (getf error :diagnostics) :to-be nil)
          (expect (repair-commands error) :to-equal '("aitools schema tx commit")))
        (expect (disk-text store "a.txt") :to-equal "a")
        (expect (tx-text store tx "a.txt") :to-equal "a+1"))))

  (it "rejects undo with environment.io and an undo schema repair when the store fails before the commit point"
    (with-temp-store (store)
      (put-file store "a.txt" "before")
      (let ((op (write-op store "a.txt" "after")))
        (multiple-value-bind (kind error)
            (call-with-io-fault-at :after-prepare (lambda () (undo store op)))
          (expect kind :to-be :error)
          (expect (getf error :code) :to-equal "environment.io")
          (expect (repair-commands error) :to-equal '("aitools schema undo")))
        (expect (disk-text store "a.txt") :to-equal "after"))))

  (it-each ((:tx-commit) (:undo))
      "reports a failure after the commit point of ~S as environment.io naming the op, and recovery rolls it forward"
      (operation)
    (with-temp-store (store)
      (multiple-value-bind (thunk expected-text)
          (ecase operation
            (:tx-commit (let ((tx (open-tx-with-append store)))
                          (values (lambda () (run #'tx-commit-flow store tx)) "a+1")))
            (:undo (put-file store "a.txt" "before")
                   (let ((op (write-op store "a.txt" "after")))
                     (values (lambda () (undo store op)) "before"))))
        (multiple-value-bind (kind error) (call-with-io-fault-at :after-apply thunk)
          (expect kind :to-be :error)
          (expect (getf error :code) :to-equal "environment.io")
          (expect (length (getf error :diagnostics)) :to-be 1)
          (let* ((diagnostic (json-alist (first (getf error :diagnostics))))
                 (op-id (cdr (assoc "op_id" diagnostic :test #'string=))))
            (expect (mapcar #'car diagnostic) :to-equal '("op_id" "recovery"))
            (expect (aitools.store.domain:valid-op-id-p op-id) :to-be-truthy)
            (expect (cdr (assoc "recovery" diagnostic :test #'string=)) :to-equal "pending")
            (expect (search op-id (getf error :message)) :to-be-truthy)
            (expect (repair-commands error)
                    :to-equal (list (format nil "aitools --root ~A history" (aitools.store.application:store-root store))))
            ;; The steps were applied before the fault; only the journal entry is missing until recovery.
            (expect (disk-text store "a.txt") :to-equal expected-text)
            (expect (aitools.store.application:recover/k store
                                                          :on-rolled-forward #'identity
                                                          :on-discarded #'identity
                                                          :on-none (constantly :none)
                                                          :on-busy (constantly :busy)
                                                          :on-failed (constantly :failed))
                    :to-equal (list (cons op-id "rolled-forward")))
            (expect (member-value (first (field (nth-value 1 (run #'history-flow store)) "items")) "op_id")
                    :to-equal op-id)
            (expect (disk-text store "a.txt") :to-equal expected-text))))))

  (it-each (("store-io-error" aitools.store.application:store-io-error
                              (:operation "open" :path "state" :detail "disk gone") nil)
            ("store-format-error" aitools.store.domain:store-format-error (:detail "bad intent") nil)
            ("store-committed-error" aitools.store.application:store-committed-error
                                     (:op-id "op-20260101T000000Z-0000000a" :operation "rename" :path "a.txt") t))
      "reports a ~A escaping the store as environment.io"
      (label condition-type initargs committed)
    (declare (ignore label))
    (with-temp-store (store)
      (let ((ports (make-journal-ports
                    :workspace-host (aitools.workspace.infrastructure:make-host-workspace-host
                                     :current-directory (constantly (aitools.store.application:store-root store))
                                     :getenv (constantly nil))
                    :open-store (lambda (real-root)
                                  (declare (ignore real-root))
                                  (apply #'error condition-type initargs)))))
        (multiple-value-bind (kind error) (run-with ports (context-for store) #'tx-status-flow nil)
          (expect kind :to-be :error)
          (expect (getf error :code) :to-equal "environment.io")
          (expect (getf error :message)
                  :to-equal (princ-to-string (apply #'make-condition condition-type initargs)))
          (if committed
              (progn
                (expect (mapcar #'json-alist (getf error :diagnostics))
                        :to-equal '((("op_id" . "op-20260101T000000Z-0000000a") ("recovery" . "pending"))))
                (expect (repair-commands error)
                        :to-equal (list (format nil "aitools --root ~A history"
                                                (aitools.store.application:store-root store)))))
              (progn
                (expect (getf error :diagnostics) :to-be nil)
                (expect (repair-commands error) :to-equal '("aitools schema tx status"))))))))

  (it-each ((tx-begin-flow "tx begin" ())
            (tx-status-flow "tx status" (nil))
            (tx-commit-flow "tx commit" ("tx-20260101T000000Z-00000000"))
            (history-flow "history" ()))
      "refuses an unresolvable --root for ~S with argument.invalid and the ~A schema as the repair"
      (flow words arguments)
    (with-temp-store (store)
      (let ((missing (concatenate 'string (aitools.store.application:store-root store) "/no/such/dir")))
        (multiple-value-bind (kind error)
            (apply #'run-with (ports-for store) (make-journal-context :root missing) flow arguments)
          (expect kind :to-be :error)
          (expect (getf error :code) :to-equal "argument.invalid")
          (expect (search (format nil "workspace root ~A" missing) (getf error :message)) :to-be 0)
          (expect (repair-commands error) :to-equal (list (format nil "aitools schema ~A" words))))))))

(describe "aitools.journal tx lock contention (environment.busy)"
  (it-each ((tx-begin-flow "tx begin --name demo")
            (tx-drop-flow "tx drop ~A 1")
            (tx-rebase-flow "tx rebase ~A")
            (tx-commit-flow "tx commit ~A")
            (tx-abort-flow "tx abort ~A"))
      "answers ~S with environment.busy and `~A` as the retry"
      (flow words)
    (with-temp-store (store)
      (let* ((tx (open-tx-with-append store))
             (arguments (ecase flow
                          (tx-begin-flow (list :name "demo"))
                          (tx-drop-flow (list tx "1"))
                          ((tx-rebase-flow tx-commit-flow tx-abort-flow) (list tx))))
             (pid (hold-lock-in-child store 3)))
        (unwind-protect
             (multiple-value-bind (kind error)
                 (apply #'run-with (ports-for store) (context-for store :lock-timeout "100ms") flow arguments)
               (expect kind :to-be :error)
               (expect (getf error :code) :to-equal "environment.busy")
               (expect (repair-commands error)
                       :to-equal (list (format nil "aitools --root ~A --lock-timeout 100ms ~?"
                                               (aitools.store.application:store-root store) words (list tx)))))
          (kill-child pid))
        (expect (tx-text store tx "a.txt") :to-equal "a+1")
        (expect (disk-text store "a.txt") :to-equal "a")))))

(describe "aitools.journal repairs without global options"
  (it "names the plain command when the workspace came from the working directory"
    (with-temp-store (store)
      ;; Without --root the workspace is found from the working directory,
      ;; whichever .git lies above it; the store is STORE either way.
      (multiple-value-bind (kind error)
          (run-with (make-journal-ports
                     :workspace-host (aitools.workspace.infrastructure:make-host-workspace-host
                                      :current-directory (constantly (aitools.store.application:store-root store))
                                      :getenv (constantly nil))
                     :open-store (lambda (real-root) (declare (ignore real-root)) store))
                    (make-journal-context) #'tx-status-flow "tx-20260101T000000Z-00000000")
        (expect kind :to-be :error)
        (expect (getf error :code) :to-equal "input.not-found")
        (expect (repair-commands error) :to-equal '("aitools tx status"))))))

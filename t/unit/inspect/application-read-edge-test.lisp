(in-package #:aitools.inspect.test)

(defun %application-edge-context (&key view store files text-context open-store)
  (aitools.inspect.application::%make-inspect-context
   :boundaries (make-test-boundaries :files files
                                    :text-context text-context
                                    :open-store open-store)
   :root (aitools.workspace.domain:make-workspace-root
          :path "/work" :real "/work")
   :view view
   :store store))

(defun %application-edge-target (&key (absolute "/work/a.txt")
                                      (real "/work/a.txt")
                                      (relative "a.txt")
                                      (argument "a.txt"))
  (aitools.inspect.application::%make-file-target
   :argument argument :absolute absolute :real real :relative relative
   :kind :file))

(describe "inspect application read edges"
  (it "uses the staged path kind when probing a file through a tx view"
    (let* ((context (%application-edge-context
                     :view t :files '(("/work/a.txt" . "disk"))))
           (target nil))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.store.application:view-path-state))
            (lambda (view path)
              (declare (ignore view path))
              (aitools.store.domain:file-state "staged" #o600))))
        (setf target (aitools.inspect.application::probe-target context "a.txt")))
      (expect (aitools.inspect.application::file-target-kind target) :to-be :file)))

  (it "treats a staged symlink as absent in the tx view"
    (let* ((context (%application-edge-context
                     :view t :files '(("/work/a.txt" . "disk"))))
           (target nil))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.store.application:view-path-state))
            (lambda (view path)
              (declare (ignore view path))
              (aitools.store.domain:symlink-state "elsewhere"))))
        (setf target (aitools.inspect.application::probe-target context "a.txt")))
      (expect (aitools.inspect.application::file-target-kind target) :to-be :absent)))

  (it "uses staged stat fields and leaves missing targets without metadata"
    (let ((context (%application-edge-context :view t :store :mock-store
                                              :files '(("/work/a.txt" . "disk"))))
          (target (%application-edge-target)))
      (cl-weave:with-mocked-functions
          (((symbol-function (quote aitools.store.application:view-path-state))
            (lambda (view path)
              (declare (ignore view path))
              (aitools.store.domain:file-state "staged" #o600 1700000001)))
           ((symbol-function (quote aitools.store.application:workspace-state))
            (lambda (store path)
              (declare (ignore store path))
              (aitools.store.domain:file-state "disk" #o644))))
        (multiple-value-bind (mode mtime)
            (aitools.inspect.application::%stat-fields context target)
          (expect mode :to-be #o600)
          (expect mtime :to-be 1700000001)))
      (multiple-value-bind (mode mtime)
          (aitools.inspect.application::%stat-fields
           (%application-edge-context)
           (%application-edge-target :real nil :relative nil))
        (expect mode :to-be nil)
        (expect mtime :to-be nil))))

  (it "clamps tx-view range reads and reports the whole size"
    (let ((context (%application-edge-context :view t))
          (target (%application-edge-target)))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.store.application:view-read-file))
            (lambda (view path)
              (declare (ignore view path))
              (%as-octets "abcdef"))))
        (multiple-value-bind (octets size)
            (aitools.inspect.application::read-target-range context target 4 20)
          (expect octets :to-equalp (%as-octets "ef"))
          (expect size :to-be 6)))))

  (it "reports a vanished tx-view range as missing"
    (let ((context (%application-edge-context :view t))
          (target (%application-edge-target)))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.store.application:view-read-file))
            (lambda (view path) (declare (ignore view path)) nil)))
        (multiple-value-bind (octets problem)
            (aitools.inspect.application::read-target-range context target 0 4)
          (expect octets :to-be nil)
          (expect problem :to-be nil)))))

  (it "does not invoke a chunk consumer for a missing tx-view file"
    (let ((context (%application-edge-context :view t))
          (target (%application-edge-target))
          (called nil))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.store.application:view-read-file))
            (lambda (view path) (declare (ignore view path)) nil)))
        (multiple-value-bind (size problem)
            (aitools.inspect.application::map-target-chunks
             context target (lambda (chunk) (declare (ignore chunk)) (setf called t)))
          (expect size :to-be nil)
          (expect problem :to-be nil)
          (expect called :to-be nil)))))

  (it "returns the text boundary failure when a chunk read cannot start"
    (let* ((text-context
             (cl-boundary-kit:make-boundary-context
              :text-file-size (lambda (path) (declare (ignore path)) (values nil :unreadable))
              :text-call-with-chunks
              (lambda (path size function)
                (declare (ignore path size function))
                (fail "chunk reader should not run without a file size"))))
           (context (%application-edge-context :text-context text-context))
           (target (%application-edge-target :relative nil)))
      (multiple-value-bind (size problem)
          (aitools.inspect.application::map-target-chunks context target #'identity)
        (expect size :to-be nil)
        (expect problem :to-be :unreadable))))

  (it "ignores a failed chunk stream after the size probe"
    (let* ((seen nil)
           (text-context
             (cl-boundary-kit:make-boundary-context
              :text-file-size (lambda (path) (declare (ignore path)) 4)
              :text-call-with-chunks
              (lambda (path size function)
                (declare (ignore path size))
                (funcall function (%as-octets "data"))
                nil)))
           (context (%application-edge-context :text-context text-context))
           (target (%application-edge-target :relative nil)))
      (multiple-value-bind (size problem)
          (aitools.inspect.application::map-target-chunks
           context target (lambda (chunk) (setf seen chunk)))
        (expect seen :to-equalp (%as-octets "data"))
        (expect size :to-be nil)
        (expect problem :to-be nil))))

  (it "keeps the target's lexical path when it has no resolved path"
    (let ((target (%application-edge-target :real nil :relative nil
                                            :absolute "/work/missing.txt")))
      (expect (aitools.inspect.application::target-display-path target)
              :to-equal "/work/missing.txt")))

  (it "returns no suggestions if the workspace scan fails"
    (let ((context (%application-edge-context)))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.workspace.application:call-with-workspace-scan/k))
            (lambda (host root &rest arguments &key on-error &allow-other-keys)
              (declare (ignore host root arguments))
              (funcall on-error :not-found "/work"))))
        (expect (aitools.inspect.application::missing-path-candidates
                 context "/work/misspelled.txt")
                :to-be nil))))

  (it "reports transaction read failures through info and check"
    (let ((boundaries
            (make-test-boundaries
             :files '(("/work/a.json" . "{}"))
             :open-store (lambda (root) (declare (ignore root)) :mock-store))))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.store.application:call-with-tx-view/k))
            (lambda (store tx &key on-view on-not-found)
              (declare (ignore store tx on-not-found))
              (funcall on-view :mock-view)))
           ((symbol-function
             (quote aitools.store.application:view-path-state))
            (lambda (view path)
              (declare (ignore view path))
              (aitools.store.domain:file-state "disk" #o644)))
           ((symbol-function
             (quote aitools.store.application:tx-record-read/k))
            (lambda (store tx path &key lock-timeout-ms on-recorded on-not-found on-busy)
              (declare (ignore store tx path lock-timeout-ms on-recorded on-busy))
              (funcall on-not-found))))
        (multiple-value-bind (kind fields)
            (run-flow #'aitools.inspect.application:info-flow
                      boundaries "a.json" :tx "tx-1")
          (expect kind :to-be :error)
          (expect (error-code fields) :to-equal "input.not-found")
          (expect (getf fields :message) :to-equal "tx tx-1 does not exist")
          (expect (getf (first (getf fields :repairs)) :command)
                  :to-equal "aitools tx status"))
        (multiple-value-bind (kind fields)
            (run-flow #'aitools.inspect.application:check-flow
                      boundaries "a.json" :format "json" :tx "tx-1")
          (expect kind :to-be :error)
          (expect (error-code fields) :to-equal "input.not-found")
          (expect (getf fields :message) :to-equal "tx tx-1 does not exist")
          (expect (getf (first (getf fields :repairs)) :command)
                  :to-equal "aitools tx status")))))

  (it "omits an op diff when both file blobs and matching disk bytes are unavailable"
    (let* ((context (%application-edge-context :store :mock-store))
           (change
             (aitools.store.domain:make-change-result
              :path "a.txt" :action :modified
              :before (aitools.store.domain:file-state "old-hash" #o644)
              :after (aitools.store.domain:file-state "new-hash" #o644)))
           (fields nil))
      (cl-weave:with-mocked-functions
          (((symbol-function (quote aitools.store.application:read-blob))
            (lambda (store hash &rest options)
              (declare (ignore store hash options))
              (error "blob is unavailable"))))
        (setf fields (aitools.inspect.application::%op-change context change)))
      (expect (json-false-value-p (json-object-get fields "content_available"))
              :to-be t)
      (expect (json-object-get fields "diff") :to-be nil)))

  (it "rejects a file diff against a directory"
    (multiple-value-bind (kind fields)
        (run-flow #'aitools.inspect.application:diff-flow
                  (make-test-boundaries :files '(("/work/a.txt" . "text")
                                                 ("/work/sub/b.txt" . "child")))
                  :a "a.txt" :b "sub")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")))

  (it "forwards partial outcomes from ranged text selections"
    (let ((long-line (make-string 20000 :initial-element #\a)))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.inspect.application:read-flow
                    (make-test-boundaries :files `(("/work/long.txt" . ,long-line)))
                    "long.txt" :range "1")
        (expect kind :to-be :partial)
        (expect (field fields "cut_lines") :to-equal '(1))
        (expect (field fields "start_line") :to-be 1))))

  (it "preserves invert when a truncated match selection can continue"
    (multiple-value-bind (kind fields)
        (run-flow #'aitools.inspect.application:read-flow
                  (make-test-boundaries
                   :files `(("/work/m.txt" . ,(format nil "hit~%keep~%more~%"))))
                  "m.txt" :match "hit" :invert t :max-lines 1)
      (expect kind :to-be :partial)
      (expect (field fields "line_numbers") :to-equal '(2))
      (expect (search "--invert" (first (field fields "next_commands")))
              :to-be-truthy)))
)

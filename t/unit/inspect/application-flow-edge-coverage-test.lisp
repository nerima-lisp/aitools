(in-package #:aitools.inspect.test)

(defun %application-edge-inspect-context (boundaries &key view store)
  (aitools.inspect.application::%make-inspect-context
   :boundaries boundaries
   :root (aitools.workspace.domain:make-workspace-root
          :path "/work" :real "/work")
   :view view
   :store store))

(describe "inspect application edge coverage"
  (it "uses disk state at the workspace root and staged directory state below it"
    (let* ((context
             (%application-edge-inspect-context
              (make-test-boundaries :files '(("/work/sub" . "disk-file")))
              :view :mock))
           (root-target
             (aitools.inspect.application::probe-target context "."))
           (staged-target nil))
      (cl-weave:with-mocked-functions
          (((symbol-function (quote aitools.store.application:view-path-state))
            (lambda (view path)
              (declare (ignore view path))
              (aitools.store.domain:directory-state))))
        (setf staged-target
              (aitools.inspect.application::probe-target context "sub")))
      (expect (aitools.inspect.application::file-target-kind root-target)
              :to-be :directory)
      (expect (aitools.inspect.application::file-target-relative root-target)
              :to-equal "")
      (expect (aitools.inspect.application::file-target-kind staged-target)
              :to-be :directory)))

  (it "treats an unresolved symlink loop as absent"
    (let* ((context
             (%application-edge-inspect-context
              (make-test-boundaries :symlinks '(("/work/loop" . "loop")))))
           (target (aitools.inspect.application::probe-target context "loop"))) (expect (aitools.inspect.application::file-target-kind target) :to-be :absent) (expect (aitools.inspect.application::file-target-real target) :to-be nil) (expect (aitools.inspect.application::target-display-path target)
              :to-equal "/work/loop")))

  (it "classifies a host entry that is neither a file nor a directory"
    (let* ((context
             (%application-edge-inspect-context (make-test-boundaries)))
           (special-entry
             (aitools.workspace.application:make-workspace-entry
              :name "special" :kind :other :size 1 :mtime 1700000000 :mode #o600))
           (target nil))
      (cl-weave:with-mocked-functions
          (((symbol-function (quote aitools.workspace.application:host-stat))
            (lambda (host path)
              (declare (ignore host))
              (when (string= path "/work/special") special-entry))))
        (setf target
              (aitools.inspect.application::probe-target context "special")))
      (expect (aitools.inspect.application::file-target-kind target) :to-be :other)))

  (it "ranks an outside missing path by its basename"
    (let* ((boundaries
             (make-test-boundaries
              :files '(("/work/target-near.txt" . "candidate"))))
           (context (%application-edge-inspect-context boundaries))
           (candidates
             (aitools.inspect.application::missing-path-candidates
              context "/outside/target-near.txt")))
      (expect (json-object-get (first candidates) "path")
              :to-equal "target-near.txt")))

  (it "dispatches a TOML document through doc get"
    (let ((boundaries
            (make-test-boundaries
             :files '(("/work/doc.toml" . "name = \"Ada\"")))))
      (multiple-value-bind (kind fields)
          (run-flow #'doc-inspect-flow
                    boundaries "get"
                    (list "doc.toml")
                    (list "doc.toml" "/name")
                    nil
                    (list :max-bytes "16KiB"))
        (expect kind :to-be :ok)
        (expect (field fields "value") :to-equal "Ada"))))

  (it "rejects malformed byte spans with missing or nonnumeric starts"
    (let ((boundaries
            (make-test-boundaries
             :files `(("/work/bytes.bin" . ,(%as-octets #(0 1 2 3)))))))
      (dolist (span '(":4" "a:4"))
        (multiple-value-bind (kind fields)
            (run-flow #'read-flow boundaries "bytes.bin"
                      :as "hex" :bytes span)
          (expect kind :to-be :error)
          (expect (error-code fields) :to-equal "argument.invalid")))))

  (it "forwards a partial selected range after decoded line cutting"
    (let* ((line (make-string 20000 :initial-element #\a))
           (boundaries
             (make-test-boundaries :files `(("/work/long.txt" . ,line)))))
      (multiple-value-bind (kind fields)
          (run-flow #'read-flow boundaries "long.txt"
                    :range "1" :escape-invisible t)
        (expect kind :to-be :partial)
        (expect (field fields "cut_lines") :to-equal '(1)))))

  (it "does not report a text diff for an available mode-change entry"
    (let* ((context
             (%application-edge-inspect-context nil :store :mock-store))
           (change
             (aitools.store.domain:make-change-result
              :path "mode.txt" :action :mode-changed
              :before (aitools.store.domain:file-state "same-hash" #o644)
              :after (aitools.store.domain:file-state "same-hash" #o600)))
           (fields nil))
      (cl-weave:with-mocked-functions
          (((symbol-function (quote aitools.store.application:read-blob))
            (lambda (store hash &rest options)
              (declare (ignore store hash options))
              (%as-octets "same text"))))
        (setf fields (aitools.inspect.application::%op-change context change)))
      (expect (json-object-get fields "diff") :to-be nil)
      (expect (json-object-get fields "content_available") :to-be nil)))
)

(in-package #:aitools.inspect.test)

(describe "inspect application flow coverage"
  (it "returns no collected files after the workspace scan reports an error"
    (let* ((boundaries (make-test-boundaries))
           (context
             (aitools.inspect.application::%make-inspect-context
              :boundaries boundaries
              :root (aitools.workspace.domain:make-workspace-root
                     :path "/work" :real "/work")))
           (files nil)
           (source nil))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.workspace.application:call-with-workspace-scan/k))
            (lambda (host root &rest arguments &key on-error &allow-other-keys)
              (declare (ignore host root arguments))
              (funcall on-error :unreadable "/work/locked")
              nil)))
        (multiple-value-setq (files source)
          (aitools.inspect.application::%scan-files context nil)))
      (expect files :to-be nil)
      (expect source :to-be nil)))

  (it "uses the default archive list limit of 200 members"
    (let* ((members
             (loop for index below 201
                   collect (archive-member (format nil "item-~3,'0D.txt" index) "x")))
           (zip (aitools.text.domain:write-zip members)))
      (multiple-value-bind (kind fields)
          (run-flow #'archive-list-flow
                    (make-test-boundaries :files `(("/work/many.zip" . ,zip)))
                    "many.zip")
        (expect kind :to-be :partial)
        (expect (length (field fields "items")) :to-be 200)
        (expect (field fields "total") :to-be 201))))

  (it "uses text mode and the 80-line default for archive reads"
    (let* ((text
             (format nil "~{~A~%~}"
                     (loop for index from 1 to 81
                           collect (format nil "line~D" index))))
           (zip (aitools.text.domain:write-zip
                 (list (archive-member "long.txt" text)))))
      (multiple-value-bind (kind fields)
          (run-flow #'archive-read-flow
                    (make-test-boundaries :files `(("/work/long.zip" . ,zip)))
                    "long.zip" "long.txt")
        (expect kind :to-be :partial)
        (expect (field fields "mode") :to-equal "text")
        (expect (length (field fields "lines")) :to-be 80)
        (expect (field fields "total_lines") :to-be 81))))

  (it "reports an unresolved non-file target without stat metadata"
    (let* ((target
             (aitools.inspect.application::%make-file-target
              :argument "special" :absolute "/work/special"
              :real nil :relative nil :kind :other))
           (boundaries (make-test-boundaries)))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.inspect.application::probe-target))
            (lambda (context path)
              (declare (ignore context path))
              target)))
        (multiple-value-bind (kind fields)
            (run-flow #'info-flow boundaries "special")
          (expect kind :to-be :ok)
          (expect (field fields "kind") :to-equal "other")
          (expect (field fields "size") :to-be 0)
          (expect (json-null-value-p (field fields "mode")) :to-be t)))))

  (it "renders an op diff when the before and after blobs are available"
    (let* ((context
             (aitools.inspect.application::%make-inspect-context :store :mock-store))
           (change
             (aitools.store.domain:make-change-result
              :path "same.txt" :action :modified
              :before (aitools.store.domain:file-state "same-hash" #o644)
              :after (aitools.store.domain:file-state "same-hash" #o644)))
           (fields nil))
      (cl-weave:with-mocked-functions
          (((symbol-function (quote aitools.store.application:read-blob))
            (lambda (store hash &rest options)
              (declare (ignore store hash options))
              (%as-octets "same text"))))
        (setf fields (aitools.inspect.application::%op-change context change)))
      (expect (json-object-get fields "diff") :to-equal "")
      (expect (json-object-get fields "content_available") :to-be nil))))

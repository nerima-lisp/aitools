(in-package #:aitools.inspect.test)

(describe "JSON document flow skeletons"
  (it "expands shared error forwarding around the flow body"
    (let ((expansion
            (macroexpand-1
             '(aitools.inspect.application::%with-json-flow-error-forwarder
               (marker)))))
      (expect (first expansion) :to-be 'labels)
      (expect (first (first (second expansion)))
              :to-be 'aitools.inspect.application::report-error)
      (expect (third expansion) :to-equal '(marker))))

  (it "expands document commands with their declared constructor inputs"
    (let* ((expansion
             (macroexpand-1
              '(aitools.inspect.presentation::%define-doc-command
                doc-command "get"
                :positionals positionals
                :options options
                :handler handler)))
           (constructor (fourth expansion))
           (description (getf (rest constructor) :description)))
      (expect (first expansion) :to-be 'defun)
      (expect (second expansion) :to-be 'doc-command)
      (expect (first constructor) :to-be 'cl-cli:make-command)
      (expect (getf (rest constructor) :name) :to-equal "get")
      (expect (second description) :to-equal "doc.get")
      (expect (getf (rest constructor) :positionals) :to-be 'positionals)
      (expect (getf (rest constructor) :options) :to-be 'options)
      (expect (getf (rest constructor) :handler) :to-be 'handler)))

  (it "expands the shared document reader with both result continuations"
    (let ((expansion
            (macroexpand-1
             '(aitools.inspect.application::%call-document-reader
               (read-document)))))
      (expect (first expansion) :to-be 'aitools.kernel.domain:flow-call)
      (expect (second expansion) :to-equal '(read-document))
      (expect (first (third expansion)) :to-be :on-document)
      (expect (first (fourth expansion)) :to-be :on-error)))

  (it "returns the entire JSON text when it fits within the byte limit"
    (expect (aitools.inspect.application::%utf8-prefix "short" 5)
            :to-equal "short")))

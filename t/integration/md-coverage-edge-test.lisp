(in-package #:aitools.integration.md-command-test)
(describe "Markdown adapter coverage edges"
  (it "leaves malformed YAML front matter for the dedicated YAML reader"
    (let ((octets
            (sb-ext:string-to-octets
             (format nil "---~%title: \"bad\\q\"~%---~%# Heading~%")
             :external-format :utf-8)))
      (multiple-value-bind (document code message offset)
          (aitools.md.infrastructure:parse-md-octets octets)
        (declare (ignore message offset))
        (expect code :to-be nil)
        (expect (aitools.md.domain:md-heading-heading
                 (aitools.md.domain:md-document-heading document 1))
                :to-equal "Heading"))))
  (it "maps a Markdown kit parser error to the syntax error code"
    (let ((octets (sb-ext:string-to-octets "# Heading" :external-format :utf-8)))
      (cl-weave:with-mocked-functions
          (((symbol-function (find-symbol "PARSE-STRING" "CL-MARKDOWN-KIT"))
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (error
               (make-condition
                (find-symbol "MARKDOWN-KIT-ERROR" "CL-MARKDOWN-KIT")
                :message "invalid front matter")))))
        (multiple-value-bind (document code message offset)
            (aitools.md.infrastructure:parse-md-octets octets)
          (declare (ignore document))
          (expect code :to-equal "input.syntax-error")
          (expect message :to-equal "invalid front matter")
          (expect offset :to-be nil)))))
  (it "preserves the false stdin option from a parsed invocation"
    (let* ((command
             (aitools.md.presentation::%replace-command
              (%make-md-test-boundaries)))
           (invocation
             (cl-cli::%make-invocation
              :app nil
              :command command
              :command-path (list command)
              :action :dispatch
              :argv0 "aitools"
              :raw-argv '("md" "replace" "guide.md" "1" "--expect-hash" "current")
              :global-options nil
              :command-options '(:expect-hash "current" :stdin nil)
              :positionals '(:path "guide.md" :section "1")
              :option-sources nil
              :stdout nil
              :stderr nil))
           (result
             (funcall (cl-cli:command-handler command) invocation)))
      (expect (cl-cli:option-value invocation :stdin) :to-be nil)
      (expect (getf (aitools.protocol.application:command-result-fields result) :code)
              :to-equal "argument.invalid"))))

(in-package #:aitools.integration.dispatch-test)

(describe "dispatch group format repairs"
  (it "routes bare and retired group repairs with their operands"
    (aitools.test.support:with-workspace (*root* state-home)
      (declare (ignore state-home))
      (multiple-value-bind (code envelope)
          (run-aitools "get" "f.json" "/a")
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "argument.invalid")
        (expect (repair-commands envelope)
                :to-equal
                (list (format nil "aitools --root ~A doc get f.json /a" *root*))))
      (multiple-value-bind (code envelope)
          (run-plain "status")
        (expect code :to-be 1)
        (expect (repair-commands envelope)
                :to-equal
                (list "aitools bg status" "aitools tx status")))
      (multiple-value-bind (code envelope)
          (run-plain "uuid")
        (expect code :to-be 1)
        (expect (repair-commands envelope) :to-equal '("aitools util uuid")))
      (dolist (group (list "json" "yaml" "toml"))
        (dolist (verb (list "get" "select" "diff" "set" "delete" "merge" "patch" "fmt"))
          (multiple-value-bind (code envelope)
              (run-aitools group verb)
            (expect code :to-be 1)
            (expect (value envelope "error" "code") :to-equal "argument.invalid")
            (expect (repair-commands envelope)
                    :to-equal (list (format nil "aitools doc ~A" verb))))))))
  (it "recognizes retired format commands after a value-taking global option"
    (multiple-value-bind (code envelope)
        (run-plain "--root" "json" "json" "get")
      (expect code :to-be 1)
      (expect (value envelope "error" "code") :to-equal "argument.invalid")
      (expect (repair-commands envelope) :to-equal (list "aitools doc get"))))
  (it "uses the generic repair for an unknown retired-format verb"
    (multiple-value-bind (code envelope)
        (run-plain "yaml" "frobnicate")
      (expect code :to-be 1)
      (expect (value envelope "error" "code") :to-equal "argument.invalid")
      (expect (repair-commands envelope) :to-equal (list "aitools schema")))))

(describe "dispatch repairs with a supplied app"
  (it "uses the custom app global options when recognizing retired format names"
    (multiple-value-bind (app registry) (aitools/cli:build-app)
      (let ((custom-app
              (cl-cli:make-app
               :name (cl-cli:app-name app)
               :version (cl-cli:app-version app)
               :global-options (list (cl-cli:make-option :name "root" :kind :flag))
               :commands (cl-cli:app-commands app))))
        (multiple-value-bind (code envelope)
            (dispatch-envelope custom-app registry (list "--root" "json" "get"))
          (expect code :to-be 1)
          (expect (value envelope "error" "code") :to-equal "argument.invalid")
          (expect (repair-commands envelope) :to-equal (list "aitools doc get")))))))

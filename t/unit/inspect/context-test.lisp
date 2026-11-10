(in-package #:aitools.inspect.test)

(describe "inspect context boundary"
          (it "preserves root error details when candidates are empty"
              (multiple-value-bind (kind fields)
                  (run-flow #'info-flow
                            (make-test-boundaries :files nil)
                            "a.txt"
                            :root
                            "/missing")
                (expect kind :to-be :error)
                (expect (error-code fields) :to-equal "input.not-found")
                (expect (getf fields :message)
                        :to-equal
                        "workspace root /missing does not exist")
                (expect (getf fields :candidates) :to-be nil)
                (expect (getf (first (getf fields :repairs)) :command)
                        :to-equal
                        "aitools info ."))))

(describe "migrated Inspect flows preserve missing-root errors"
  (it "returns a missing-root error through check"
    (multiple-value-bind (kind fields)
        (run-flow (function check-flow) (make-test-boundaries :files nil)
                  "a.txt" :root "/missing")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message)
              :to-equal "workspace root /missing does not exist")
      (expect (getf fields :candidates) :to-be nil)
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools info .")))

  (it "returns a missing-root error through diff"
    (multiple-value-bind (kind fields)
        (run-flow (function diff-flow) (make-test-boundaries :files nil)
                  :a "a.txt" :b "b.txt" :root "/missing")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message)
              :to-equal "workspace root /missing does not exist")
      (expect (getf fields :candidates) :to-be nil)
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools info .")))

  (it "returns a missing-root error through snapshot create"
    (multiple-value-bind (kind fields)
        (run-flow (function snapshot-create-flow) (make-test-boundaries :files nil)
                  :root "/missing")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message)
              :to-equal "workspace root /missing does not exist")
      (expect (getf fields :candidates) :to-be nil)
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools info .")))

  (it "returns a missing-root error through snapshot diff"
    (multiple-value-bind (kind fields)
        (run-flow (function snapshot-diff-flow) (make-test-boundaries :files nil)
                  "snap-20231114T221320Z-0a1b2c3d" :root "/missing")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message)
              :to-equal "workspace root /missing does not exist")
      (expect (getf fields :candidates) :to-be nil)
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools info ."))))
(describe "inspect context edges"
  (it "reports when --root exists as a file instead of a directory"
    (multiple-value-bind (kind fields)
        (run-flow (function info-flow)
                  (make-test-boundaries
                   :files (list (cons "/work/not-a-root.txt" "x")))
                  "a.txt" :root "/work/not-a-root.txt")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message)
              :to-equal "workspace root /work/not-a-root.txt is not a directory")
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools info .")))
  (it "maps each composition dependency to its context-specific boundary key"
    (let* ((workspace-host (gensym "HOST"))
           (text-context (gensym "TEXT"))
           (open-store (gensym "STORE"))
           (state-directory-function (gensym "STATE"))
           (boundaries
             (aitools.inspect.composition::make-inspect-dependencies
              :workspace-host workspace-host
              :text-source text-context
              :open-store open-store
              :state-directory-function state-directory-function)))
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :inspect-workspace-host)
              :to-be workspace-host)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :inspect-text-context)
              :to-be text-context)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :inspect-open-store)
              :to-be open-store)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :inspect-state-directory)
              :to-be state-directory-function))))

(in-package #:aitools.inspect.test)

(defun %archive-octets ()
  (aitools.text.domain:write-zip
   (list (aitools.text.domain:make-archive-member :name "m.txt" :kind :file :mode #o644 :mtime 0
                                                  :data (%as-octets "m")))))

(defparameter *single-file-flows*
  `(("table read" ,#'table-read-flow ())
    ("table agg" ,#'table-agg-flow ())
    ("json get" ,#'json-get-flow (""))
    ("json select" ,#'json-select-flow (""))
    ("archive list" ,#'archive-list-flow ())
    ("archive read" ,#'archive-read-flow ("m.txt"))
    ("read" ,#'read-flow ())
    ("read --encoding" ,#'read-flow (:encoding "utf-8"))
    ("read --as hex" ,#'read-flow (:as "hex"))
    ("check" ,#'check-flow (:format "json")))
  "(label flow extra-arguments) for each single-file flow that reads its
target's bytes after probing it; EXTRA-ARGUMENTS follow the path.")

(describe "single-file flows on a file that cannot be read (contract-F1)"
  (it-each (("table read") ("table agg") ("json get") ("json select") ("archive list") ("archive read")
            ("read") ("read --encoding") ("read --as hex") ("check"))
      "~A reports environment.io for a file denied by permission"
      (label)
    (destructuring-bind (flow extra) (rest (assoc label *single-file-flows* :test #'string=))
      (multiple-value-bind (kind fields)
          (apply #'run-flow flow (make-test-ports :unreadable `(("/work/f.json" . ,(%archive-octets)))) "f.json" extra)
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "environment.io")
        (expect (getf fields :message) :to-equal "file f.json cannot be read: permission denied")
        (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools info f.json"))))

  (it-each (("table read") ("table agg") ("json get") ("json select") ("archive list") ("archive read")
            ("read") ("read --encoding") ("read --as hex") ("check"))
      "~A reports input.not-found for a file that vanished after it was probed"
      (label)
    (destructuring-bind (flow extra) (rest (assoc label *single-file-flows* :test #'string=))
      ;; The host still lists the file; the text source no longer has it.
      (let* ((present (make-fake-filesystem :files `(("/work/f.json" . ,(%archive-octets))) :directories '("/work")))
             (gone (make-fake-filesystem :directories '("/work")))
             (ports (make-inspect-ports :workspace-host (make-fake-host present) :text-source (make-fake-source gone)
                                        :open-store (lambda (root) (fail (format nil "open-store ~A" root)))
                                        :state-directory-function (constantly nil))))
        (multiple-value-bind (kind fields) (apply #'run-flow flow ports "f.json" extra)
          (expect kind :to-be :error)
          (expect (error-code fields) :to-equal "input.not-found")
          (expect (getf fields :message) :to-equal "file f.json does not exist")
          (expect (json-object-get (first (getf fields :candidates)) "path") :to-equal "f.json"))))))

(describe "an existing but unreadable file reports environment.io (contract-F1)"
  (flet ((ports () (make-test-ports :unreadable '(("/work/secret.txt" . "hidden")
                                                  ("/work/other.txt" . "hidden too"))
                                    :files '(("/work/readable.txt" . "1
2")))))
    (it "info reports environment.io, not input.not-found"
      (expect (error-code (nth-value 1 (run-flow #'info-flow (ports) "secret.txt"))) :to-equal "environment.io"))
    (it "check reports environment.io"
      (expect (error-code (nth-value 1 (run-flow #'check-flow (ports) "secret.txt" :format "json")))
              :to-equal "environment.io"))
    (it "diff reports environment.io when either side is unreadable"
      (expect (error-code (nth-value 1 (run-flow #'diff-flow (ports) :a "secret.txt" :b "other.txt")))
              :to-equal "environment.io")
      (expect (error-code (nth-value 1 (run-flow #'diff-flow (ports) :a "readable.txt" :b "other.txt")))
              :to-equal "environment.io"))
    (it "a genuinely missing path is still input.not-found"
      (expect (error-code (nth-value 1 (run-flow #'info-flow (ports) "nope.txt"))) :to-equal "input.not-found"))))

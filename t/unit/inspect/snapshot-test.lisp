;;;; t/unit/inspect/snapshot-test.lisp
;;;;
;;;; Snapshot records and comparison rules (the flows run
;;;; against a real directory in t/integration/inspect-snapshot-test.lisp).
(in-package #:aitools.inspect.test)

(defun snap-file (path size mtime &optional (hash "h"))
  (make-snapshot-file path size mtime hash))

(describe "snapshot ids"
  (it "makes ids it also accepts, and rejects anything path-like"
    (let ((id (snapshot-id-from-time 1700000000 "0a1b2c3d")))
      (expect id :to-equal "snap-20231114T221320Z-0a1b2c3d")
      (expect (valid-snapshot-id-p id) :to-be-truthy))
    (expect (valid-snapshot-id-p "../../etc/passwd") :to-be nil)
    (expect (valid-snapshot-id-p "snap-20231114T221320Z-0A1B2C3D") :to-be nil)
    (expect (valid-snapshot-id-p "snap-2023111XT221320Z-0a1b2c3d") :to-be nil)))

(describe "snapshot records"
  (it "round-trips a record with its scan options"
    (let* ((snapshot (make-snapshot "snap-20231114T221320Z-0a1b2c3d" "2023-11-14T22:13:20Z"
                                    (list (snap-file "a.txt" 3 100 "aa") (snap-file "b/c.txt" 0 200 "bb"))
                                    :glob '("*.txt") :lang "markdown" :no-ignore t :skip-larger-than 1024 :newer 50))
           (decoded (decode-snapshot/k (encode-snapshot snapshot) (snapshot-id snapshot)
                                       :on-snapshot #'identity :on-invalid (constantly :invalid))))
      (expect (mapcar #'snapshot-file-path (snapshot-files decoded)) :to-equal '("a.txt" "b/c.txt"))
      (expect (snapshot-file-hash (second (snapshot-files decoded))) :to-equal "bb")
      (expect (snapshot-glob decoded) :to-equal '("*.txt"))
      (expect (snapshot-lang decoded) :to-equal "markdown")
      (expect (snapshot-no-ignore decoded) :to-be t)
      (expect (snapshot-skip-larger-than decoded) :to-be 1024)
      (expect (snapshot-newer decoded) :to-be 50)))

  (it "rejects a record for another id, malformed JSON, or a wrong shape"
    (let ((text (encode-snapshot (make-snapshot "snap-20231114T221320Z-0a1b2c3d" "t" '()))))
      (flet ((decode (text id) (decode-snapshot/k text id :on-snapshot (constantly :ok) :on-invalid (constantly :invalid))))
        (expect (decode text "snap-20231114T221320Z-0a1b2c3d") :to-be :ok)
        (expect (decode text "snap-20231114T221320Z-ffffffff") :to-be :invalid)
        (expect (decode "{" "snap-20231114T221320Z-0a1b2c3d") :to-be :invalid)
        (expect (decode "{\"snapshot_id\":\"snap-20231114T221320Z-0a1b2c3d\",\"created\":\"t\",\"options\":{\"glob\":[],\"no_ignore\":false,\"lang\":null,\"skip_larger_than\":null,\"newer\":null},\"files\":[[\"a\",-1,0,\"h\"]]}"
                        "snap-20231114T221320Z-0a1b2c3d")
                :to-be :invalid)))))

(describe "snapshot comparison"
  (it "classifies added and removed paths and flags size or mtime changes only"
    (let ((snapshot (make-snapshot "snap-20231114T221320Z-0a1b2c3d" "t"
                                   (list (snap-file "same" 1 1) (snap-file "gone" 1 1)
                                         (snap-file "touched" 1 1) (snap-file "grown" 1 1)))))
      (multiple-value-bind (added removed suspects)
          (compare-snapshot snapshot (list (snap-file "grown" 2 1 "") (snap-file "new" 1 1 "")
                                           (snap-file "same" 1 1 "") (snap-file "touched" 1 9 "")))
        (expect added :to-equal '("new"))
        (expect removed :to-equal '("gone"))
        (expect (mapcar (lambda (pair) (snapshot-file-path (car pair))) suspects) :to-equal '("grown" "touched"))))))

(describe "snapshot create scan options"
  (it "rejects an unknown --lang before scanning with the current language list"
    ;; OPEN-STORE fails the test if called: validation must not touch the store.
    (multiple-value-bind (kind fields)
        (run-flow #'snapshot-create-flow (make-test-ports) :lang "klingon")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (getf fields :message)
              :to-equal (format nil "unknown --lang ~S; known: ~{~A~^, ~}" "klingon"
                                (aitools.text.domain:language-names)))
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools schema snapshot create")))

  (it-each (("an unparsable --skip-larger-than" (:skip-larger-than "huge") "argument.invalid"
             "--skip-larger-than \"huge\" is not a size" "aitools schema snapshot create")
            ("a --newer that is neither a duration nor a path" (:newer "no-such-file") "argument.invalid"
             "--newer no-such-file is neither a duration nor an existing path" "aitools snapshot create --newer 1h"))
      "rejects ~A before scanning"
      (label options code message repair)
    (declare (ignore label))
    ;; OPEN-STORE fails the test if called: validation must not touch the store.
    (multiple-value-bind (kind fields) (apply #'run-flow #'snapshot-create-flow (make-test-ports) options)
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal code)
      (expect (getf fields :message) :to-equal message)
      (expect (getf (first (getf fields :repairs)) :command) :to-equal repair))))

(defun snapshot-record-text (options)
  (format nil "{\"snapshot_id\":\"snap-20231114T221320Z-0a1b2c3d\",\"created\":\"t\",\"options\":~A,\"files\":[]}"
          options))

(describe "snapshot record shapes"
  (flet ((decode (text)
           (decode-snapshot/k text "snap-20231114T221320Z-0a1b2c3d" :on-snapshot (constantly :ok) :on-invalid (constantly :invalid))))
    (it "rejects a record that is an array instead of an object"
      (expect (decode "[]") :to-be :invalid))

    (it-each (("a glob that is not a list of strings" "{\"glob\":[1],\"no_ignore\":false,\"lang\":null,\"skip_larger_than\":null,\"newer\":null}")
              ("a no_ignore that is not a boolean" "{\"glob\":[],\"no_ignore\":1,\"lang\":null,\"skip_larger_than\":null,\"newer\":null}")
              ("a lang that is not a string" "{\"glob\":[],\"no_ignore\":false,\"lang\":3,\"skip_larger_than\":null,\"newer\":null}")
              ("a newer that is not an integer" "{\"glob\":[],\"no_ignore\":false,\"lang\":null,\"skip_larger_than\":null,\"newer\":\"x\"}"))
        "rejects options with ~A"
        (label options)
      (declare (ignore label))
      (expect (decode (snapshot-record-text options)) :to-be :invalid))

    (it "accepts the same record with well-typed options"
      (expect (decode (snapshot-record-text "{\"glob\":[\"*\"],\"no_ignore\":true,\"lang\":\"markdown\",\"skip_larger_than\":5,\"newer\":7}"))
              :to-be :ok))

    (it-each (("snap-20231114X221320Z-0a1b2c3d") ("snap-20231114T2213a0Z-0a1b2c3d") ("snap-20231114T221320Y-0a1b2c3d")
              ("snip-20231114T221320Z-0a1b2c3d"))
        "rejects the malformed id ~A"
        (id)
      (expect (valid-snapshot-id-p id) :to-be nil))))

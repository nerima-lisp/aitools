;;;; t/unit/search/find-flow-test.lisp
;;;;
;;;; `find` through the flow.
(in-package #:aitools.search.test)

(defparameter *find-files*
  (list (list "/w/README.md" "readme" :mtime 3000)
        (list "/w/src/main.lisp" "(defun main ())" :mtime 2000)
        (list "/w/src/util.lisp" (make-string 300 :initial-element #\x) :mtime 1500)
        (list "/w/src/deep/more.lisp" "" :mtime 1200)
        (list "/w/bin/run.sh" "#!/bin/sh" :mode #o755 :mtime 1100)
        (list "/w/docs/guide.md" "guide" :mtime 900)))

(defun find-in (&rest arguments)
  (apply #'run-flow #'find/k
         (make-fake-ports :files *find-files* :directories '("/w/empty")
                          :symlinks '(("/w/link" . "src/main.lisp")))
         arguments))

(defun item-paths (fields)
  (mapcar (lambda (item) (jfield item "path")) (field fields "items")))

(describe "aitools.search.application find/k"
  (it "matches a pattern without / against the name and one with / against the path"
    (expect (item-paths (nth-value 1 (find-in :pattern "*.lisp")))
            :to-equal '("src/deep/more.lisp" "src/main.lisp" "src/util.lisp"))
    (expect (item-paths (nth-value 1 (find-in :pattern "main")))
            :to-equal '("src/main.lisp"))
    (expect (item-paths (nth-value 1 (find-in :pattern "src/*.lisp")))
            :to-equal '("src/main.lisp" "src/util.lisp")))

  (it "limits --depth to entries at most N levels below the start"
    (expect (item-paths (nth-value 1 (find-in :depth 1)))
            :to-equal '("README.md" "bin" "docs" "empty" "link" "src"))
    (expect (item-paths (nth-value 1 (find-in :path "src" :depth 1)))
            :to-equal '("src/deep" "src/main.lisp" "src/util.lisp")))

  (it "sorts by mtime newest first and by size largest first"
    (expect (item-paths (nth-value 1 (find-in :type :file :sort :mtime)))
            :to-equal '("README.md" "src/main.lisp" "src/util.lisp" "src/deep/more.lisp" "bin/run.sh" "docs/guide.md"))
    (expect (first (item-paths (nth-value 1 (find-in :type :file :sort :size)))) :to-equal "src/util.lisp"))

  (it "honours --newer, --min-size, and --max-size"
    (expect (item-paths (nth-value 1 (find-in :type :file :newer "/w/src/util.lisp")))
            :to-equal '("README.md" "src/main.lisp"))
    (expect (item-paths (nth-value 1 (find-in :type :file :min-size "100"))) :to-equal '("src/util.lisp"))
    (expect (item-paths (nth-value 1 (find-in :type :file :max-size "0"))) :to-equal '("src/deep/more.lisp")))

  (it "selects empty files and directories, executables, and symlinks"
    (expect (item-paths (nth-value 1 (find-in :empty t))) :to-equal '("empty" "src/deep/more.lisp"))
    (expect (item-paths (nth-value 1 (find-in :executable t))) :to-equal '("bin/run.sh"))
    (let ((items (field (nth-value 1 (find-in :type :symlink)) "items")))
      (expect (mapcar (lambda (item) (jfield item "kind")) items) :to-equal '("symlink"))
      (expect (jfield (first items) "path") :to-equal "link")))

  (it "gives items path, kind, size, octal mode, and UTC mtime"
    (let ((item (first (field (nth-value 1 (find-in :pattern "run.sh")) "items"))))
      (expect (jfield item "kind") :to-equal "file")
      (expect (jfield item "size") :to-be 9)
      (expect (jfield item "mode") :to-equal "0755")
      (expect (jfield item "mtime") :to-equal "1970-01-01T00:18:20Z")))

  (it "sums the sizes below each directory with --sizes"
    (let ((items (field (nth-value 1 (find-in :type :directory :sizes t)) "items")))
      (flet ((size-of (path) (jfield (find path items :key (lambda (item) (jfield item "path")) :test #'string=) "size")))
        (expect (size-of "src") :to-be (+ 15 300 0))
        (expect (size-of "src/deep") :to-be 0)
        (expect (size-of "bin") :to-be 9)))
    (expect (jfield (first (field (nth-value 1 (find-in :type :directory)) "items")) "size")
            :to-be json-kit:+json-null+))

  (it "nests --output tree and counts entries past --limit as omitted"
    (multiple-value-bind (kind fields) (find-in :path "src" :output :tree :limit 2)
      (expect kind :to-be :partial)
      (let* ((tree (field fields "tree"))
             (children (jfield tree "children")))
        (expect (jfield tree "name") :to-equal "src")
        (expect (mapcar (lambda (node) (jfield node "name")) children) :to-equal '("deep"))
        (expect (mapcar (lambda (node) (jfield node "name")) (jfield (first children) "children"))
                :to-equal '("more.lisp"))
        (expect (jfield tree "omitted") :to-be 2))
      (expect (field fields "total") :to-be 4)))

  (it "is partial past --limit with the total and a next command"
    (multiple-value-bind (kind fields) (find-in :limit 2)
      (expect kind :to-be :partial)
      (expect (length (field fields "items")) :to-be 2)
      (expect (first (field fields "next_commands")) :to-contain "--limit 12"))))

(describe "aitools.search.application find/k next commands and errors"
  (it "reproduces the filters and scan options of a cut-short listing"
    (multiple-value-bind (kind fields)
        (find-in :pattern "*.lisp" :path "src" :type :file :depth 3 :sort :size :min-size "0" :max-size "1MiB"
                 :glob '("*.lisp") :lang "common-lisp" :no-ignore t :skip-larger-than "1MiB" :newer "1000h" :limit 1)
      (expect kind :to-be :partial)
      (expect (field fields "next_commands")
              :to-equal (list (format nil "aitools find '*.lisp' src --type file --depth 3 --sort size --min-size 0 ~
                                           --max-size 1MiB --limit 3 --glob '*.lisp' --lang common-lisp --no-ignore ~
                                           --skip-larger-than 1MiB --newer 1000h")))))

  (it "reproduces --empty, --sizes, --output tree, and --type dir"
    (multiple-value-bind (kind fields) (find-in :type :directory :empty t :sizes t :output :tree :limit 0)
      (expect kind :to-be :partial)
      (expect (field fields "next_commands")
              :to-equal '("aitools find --type dir --empty --output tree --sizes --limit 1"))))

  (it "reproduces --executable and --type symlink"
    (multiple-value-bind (kind fields)
        (run-flow #'find/k (make-fake-ports :files '(("/w/a" "x" :mode #o755) ("/w/b" "y" :mode #o755)))
                  :executable t :limit 1)
      (expect kind :to-be :partial)
      (expect (field fields "next_commands") :to-equal '("aitools find --executable --limit 2")))
    (multiple-value-bind (kind fields) (find-in :type :symlink :limit 0)
      (expect kind :to-be :partial)
      (expect (field fields "next_commands") :to-equal '("aitools find --type symlink --limit 1"))))

  (it-each ((:min-size "big" "--min-size: not a size: big") (:max-size "-1" "--max-size: not a size: -1"))
      "rejects ~S ~S as argument.invalid"
      (option value message)
    (multiple-value-bind (kind fields) (find-in option value)
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "argument.invalid")
      (expect (getf fields :message) :to-equal message)))

  (it "reports a start path that does not exist as input.not-found"
    (expect (getf (nth-value 1 (find-in :path "nope")) :code) :to-equal "input.not-found"))

  (it "roots --output tree at a file start"
    (multiple-value-bind (kind fields) (find-in :path "README.md" :output :tree)
      (expect kind :to-be :ok)
      (expect (list (jfield (field fields "tree") "name") (jfield (field fields "tree") "kind"))
              :to-equal '("README.md" "file"))))

  (it "counts an omitted entry whose directories are not in the tree toward the root"
    (let ((tree (field (nth-value 1 (run-flow #'find/k (make-fake-ports :files '(("/w/README.md" "x") ("/w/src/deep/a.lisp" "y")))
                                               :type :file :output :tree :limit 1))
                       "tree")))
      (expect (mapcar (lambda (node) (jfield node "name")) (jfield tree "children")) :to-equal '("README.md"))
      (expect (jfield tree "omitted") :to-be 1)))

  (it "names every entry kind, including other"
    (expect (mapcar #'kind-name '(:file :directory :symlink :other)) :to-equal '("file" "dir" "symlink" "other"))))

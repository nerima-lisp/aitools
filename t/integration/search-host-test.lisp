;;;; t/integration/search-host-test.lisp
;;;;
;;;; The search context over the real filesystem: the production workspace
;;;; host (a CPU-sized worker pool) and text source, a real store for
;;;; `--tx`, and the rule that scanning never
;;;; starts a git process.
(in-package #:aitools.search.test)

(defun %write-file (path content)
  (ensure-directories-exist (sb-ext:parse-native-namestring path))
  (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :if-exists :supersede
                                                             :element-type '(unsigned-byte 8))
    (write-sequence (%bytes content) out)))

(defun %real-root (directory)
  (string-right-trim "/" (namestring (truename (sb-ext:parse-native-namestring
                                                (concatenate 'string directory "/"))))))

(defun %host-ports (root &key open-store)
  (make-search-ports
   :workspace-host (aitools.workspace.infrastructure:make-host-workspace-host
                    :getenv (constantly nil)
                    :home-directory (constantly "/nonexistent-home")
                    :current-directory (constantly root))
   :text-source (aitools.text.infrastructure:make-host-text-source)
   :open-store open-store
   :unix-now (constantly 0)))

(defmacro %with-temp-workspace ((root) &body body)
  `(let ((,root (%real-root (sb-posix:mkdtemp (format nil "~A/aitools-search-XXXXXX"
                                                      (string-right-trim "/" (or (sb-posix:getenv "TMPDIR") "/tmp")))))))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree (sb-ext:parse-native-namestring (concatenate 'string ,root "/"))
                                   :validate (lambda (path) (search "aitools-search-" (namestring path)))))))

(defun %block-paths (fields)
  (mapcar (lambda (block) (jfield block "path")) (field fields "blocks")))

(describe "aitools.search on the real filesystem"
  (it "returns path-ordered, byte-identical results from the parallel worker pool"
    (%with-temp-workspace (root)
      (let ((expected '()))
        ;; Names chosen so creation order, directory order, and path order differ.
        (loop for d in '("zeta" "alpha" "mid" "Beta")
              do (loop for f in '("9.txt" "10.txt" "a.txt" "_.txt" "Z.txt")
                       for path = (format nil "~A/~A" d f)
                       do (%write-file (format nil "~A/~A" root path) (format nil "one~%hit ~A~%" path))
                          (push path expected)))
        (%write-file (format nil "~A/top.txt" root) "hit")
        (push "top.txt" expected)
        (let ((ports (%host-ports root)))
          (multiple-value-bind (kind fields) (run-flow #'search/k ports :patterns '("hit") :limit 100)
            (expect kind :to-be :ok)
            (expect (%block-paths fields) :to-equal (sort (copy-list expected) #'string<))
            (expect (multiple-value-call #'rendered (run-flow #'search/k ports :patterns '("hit") :limit 100))
                    :to-equal (rendered kind fields)))))))

  (it "reads through a tx: staged edits, additions, deletions, and the tx's .gitignore"
    (aitools.store.test-support:with-temp-store (store)
      (let* ((root (%real-root (aitools.store.application:store-root store)))
             (ports (%host-ports root :open-store (constantly store))))
        (%write-file (format nil "~A/.git/HEAD" root) (format nil "ref: refs/heads/main~%"))
        (%write-file (format nil "~A/a.txt" root) "old hit")
        (%write-file (format nil "~A/gone.txt" root) "hit")
        (%write-file (format nil "~A/secret.txt" root) "hit")
        (let ((tx (aitools.store.application:tx-begin/k store :on-begun (lambda (id name created)
                                                                           (declare (ignore name created))
                                                                           id)
                                                               :on-busy (lambda () (fail "tx begin busy")))))
          (aitools.store.application:tx-stage/k
           store tx '("test")
           (lambda (view commit reject)
             (declare (ignore view reject))
             (funcall commit (list (aitools.store.domain:write-file-request "a.txt" (%bytes "new hit"))
                                   (aitools.store.domain:write-file-request "new/added.txt" (%bytes "hit"))
                                   (aitools.store.domain:write-file-request ".gitignore" (%bytes (format nil "secret.txt~%")))
                                   (aitools.store.domain:delete-request "gone.txt"))))
           :on-staged (lambda (tx-op results) (declare (ignore results)) tx-op)
           :on-rejected (lambda (code &rest rest) (fail (format nil "stage rejected ~A ~S" code rest)))
           :on-not-found (lambda () (fail "tx not found"))
           :on-busy (lambda () (fail "tx busy")))
          (multiple-value-bind (kind fields) (run-flow #'search/k ports :patterns '("hit") :context 0 :tx tx)
            (expect kind :to-be :ok)
            (expect (%block-paths fields) :to-equal '("a.txt" "new/added.txt"))
            (expect (jfield (first (field fields "blocks")) "lines") :to-equal '("new hit")))
          (multiple-value-bind (kind fields) (run-flow #'search/k ports :patterns '("hit") :context 0)
            (expect kind :to-be :ok)
            (expect (%block-paths fields) :to-equal '("a.txt" "gone.txt" "secret.txt")))
          (multiple-value-bind (kind fields) (run-flow #'find/k ports :type :file :tx tx)
            (expect kind :to-be :ok)
            (expect (mapcar (lambda (item) (jfield item "path")) (field fields "items"))
                    :to-equal '(".gitignore" "a.txt" "new/added.txt")))
          (multiple-value-bind (kind fields) (run-flow #'find/k ports :type :file :tx "tx-missing")
            (declare (ignore kind))
            (expect (getf fields :code) :to-equal "input.not-found"))
          (expect (probe-file (sb-ext:parse-native-namestring (format nil "~A/new/added.txt" root))) :to-be nil)))))

  (it "never starts a process: the search context names no process or git kit"
    (let ((sources (directory (merge-pathnames (make-pathname :directory '(:relative "packages" "feature" "search" "src" :wild-inferiors)
                                                   :name :wild :type "lisp")
                                    (asdf:system-source-directory "aitools")))))
      (expect (length sources) :to-be-greater-than 10)
      (dolist (source sources)
        (let ((text (uiop:read-file-string source)))
          (dolist (needle '("process-kit:" "vcs-kit:" "run-program" "run-git"))
            (expect (search needle text :test #'char-equal) :to-be nil)))))))

(defun %begin-staged-tx (store requests)
  "A new tx of STORE with REQUESTS staged in it; its id."
  (let ((tx (aitools.store.application:tx-begin/k store :on-begun (lambda (id name created)
                                                                     (declare (ignore name created))
                                                                     id)
                                                         :on-busy (lambda () (fail "tx begin busy")))))
    (aitools.store.application:tx-stage/k
     store tx '("test")
     (lambda (view commit reject)
       (declare (ignore view reject))
       (funcall commit requests))
     :on-staged (lambda (tx-op results) (declare (ignore results)) tx-op)
     :on-rejected (lambda (code &rest rest) (fail (format nil "stage rejected ~A ~S" code rest)))
     :on-not-found (lambda () (fail "tx not found"))
     :on-busy (lambda () (fail "tx busy")))
    tx))

(describe "aitools.search read-through of a tx on the real filesystem"
  (it "carries --tx into every flow's next command and reads staged, deleted, and binary paths from the tx"
    (aitools.store.test-support:with-temp-store (store)
      (let* ((root (%real-root (aitools.store.application:store-root store)))
             (ports (%host-ports root :open-store (constantly store))))
        (%write-file (format nil "~A/.git/HEAD" root) (format nil "ref: refs/heads/main~%"))
        (%write-file (format nil "~A/a.lisp" root) "(defun old ())")
        ;; Read from disk through the overlay: the tx does not stage it.
        (%write-file (format nil "~A/.gitignore" root) (format nil "ignored.lisp~%"))
        (%write-file (format nil "~A/ignored.lisp" root) (format nil "(parse)~%"))
        (%write-file (format nil "~A/sub/.gitignore" root) (format nil "x.lisp~%"))
        (%write-file (format nil "~A/sub/x.lisp" root) (format nil "(parse)~%"))
        (%write-file (format nil "~A/gone.lisp" root) (format nil "(defun parse ())~%"))
        (let* ((tx (%begin-staged-tx
                    store
                    (list (aitools.store.domain:write-file-request "a.lisp" (%bytes (format nil "(defun parse ())~%(parse)~%")))
                          (aitools.store.domain:write-file-request "sub/b.lisp" (%bytes (format nil "(defun parse-b ())~%")))
                          (aitools.store.domain:write-file-request "fresh/c.py" (%bytes (format nil "parse = 1~%")))
                          (aitools.store.domain:write-file-request "bin.lisp" (coerce #(40 0 41) '(vector (unsigned-byte 8))))
                          (aitools.store.domain:delete-request "sub/.gitignore")
                          (aitools.store.domain:delete-request "gone.lisp"))))
               (suffix (format nil "--tx ~A" tx)))
          (flet ((next (flow &rest arguments)
                   (multiple-value-bind (kind fields) (apply #'run-flow flow ports :tx tx arguments)
                     (expect kind :to-be :partial)
                     (first (field fields "next_commands")))))
            (expect (next #'search/k :patterns '("parse") :limit 1)
                    :to-equal (format nil "aitools search --pattern parse --before 2 --after 2 --limit 5 ~A" suffix))
            (expect (next #'code-defs/k :name "parse" :prefix t :limit 1)
                    :to-equal (format nil "aitools code defs parse --prefix --limit 2 ~A" suffix))
            (expect (next #'code-refs/k :name "parse" :limit 1)
                    :to-equal (format nil "aitools code refs parse --limit 4 ~A" suffix))
            (expect (next #'overview/k :limit 1) :to-equal (format nil "aitools overview --limit 2 ~A" suffix))
            (expect (next #'find/k :type :file :limit 1) :to-equal (format nil "aitools find --type file --limit 6 ~A" suffix)))
          (multiple-value-bind (kind fields) (run-flow #'search/k ports :patterns '("parse") :context 0 :tx tx :limit 50)
            (expect kind :to-be :ok)
            ;; sub/x.lisp shows because the tx deletes the .gitignore that hid it.
            (expect (%block-paths fields) :to-equal '("a.lisp" "fresh/c.py" "sub/b.lisp" "sub/x.lisp"))
            (expect (mapcar (lambda (entry) (json-alist-value entry "reason")) (field fields "skipped")) :to-equal '("binary")))
          (multiple-value-bind (kind fields) (run-flow #'code-outline/k ports :path "gone.lisp" :tx tx)
            (expect kind :to-be :error)
            (expect (getf fields :code) :to-equal "input.not-found"))
          (multiple-value-bind (kind fields) (run-flow #'find/k ports :path "fresh" :tx tx)
            (expect kind :to-be :ok)
            (expect (mapcar (lambda (item) (jfield item "path")) (field fields "items")) :to-equal '("fresh/c.py"))))))))

(defmacro %with-stdin-from ((path) &body body)
  "Run BODY with file descriptor 0 reading PATH, restoring the original
standard input afterwards."
  (let ((saved (gensym "SAVED")) (fd (gensym "FD")))
    `(let ((,saved (sb-posix:dup 0))
           (,fd (sb-posix:open ,path sb-posix:o-rdonly)))
       (unwind-protect (progn (sb-posix:dup2 ,fd 0) ,@body)
         (sb-posix:dup2 ,saved 0)
         (sb-posix:close ,saved)
         (sb-posix:close ,fd)))))

(defun %production-stdin (limit)
  "The outcome of the production search READ-STDIN-OCTETS port with LIMIT."
  (funcall (aitools.search.application::search-ports-read-stdin-octets
            (aitools.search.infrastructure:make-production-search-ports))
           limit
           :on-octets (lambda (octets) (list :octets (length octets) (and (plusp (length octets)) (aref octets 0))))
           :on-too-large (lambda () (list :too-large))
           :on-failure (lambda (message) (list :failure (stringp message)))))

(describe "aitools.search production ports"
  (it "reads standard input as raw octets up to the limit, over several reads"
    (%with-temp-workspace (root)
      (let ((path (format nil "~A/in" root)))
        (%write-file path (make-array 10000 :element-type '(unsigned-byte 8) :initial-element 255))
        (%with-stdin-from (path) (expect (%production-stdin 10000) :to-equal '(:octets 10000 255)))
        (%with-stdin-from (path) (expect (%production-stdin 9999) :to-equal '(:too-large)))
        (%write-file path "")
        (%with-stdin-from (path) (expect (%production-stdin 10) :to-equal '(:octets 0 nil))))))

  (it "reports a standard input that cannot be read as a failure"
    (%with-temp-workspace (root)
      (%with-stdin-from (root) (expect (%production-stdin 10) :to-equal '(:failure t)))))

  (it "tells the Unix time in seconds"
    (let ((now (funcall (aitools.search.application::search-ports-unix-now
                         (aitools.search.infrastructure:make-production-search-ports))))
          (expected (aitools.kernel.domain:universal-time-to-unix-seconds (get-universal-time))))
      (expect (<= (abs (- now expected)) 2) :to-be t))))

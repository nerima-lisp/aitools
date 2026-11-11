(in-package #:aitools.workspace.test)

(describe "workspace value construction edges"
  (it "accepts the workspace value alternatives"
    (dolist (kind '(:file :directory :symlink :other))
      (expect (workspace-entry-kind
               (make-workspace-entry :kind kind :mtime -1))
              :to-be kind))
    (let* ((repository
             (make-git-repository :top "/repo"
                                  :git-dir "/repo/.git"
                                  :common-dir "/repo/.git"))
           (root (make-workspace-root :repository repository)))
      (expect (workspace-root-repository root) :to-be repository))
    (dolist (source '(:none :builtin :gitignore))
      (expect (aitools.workspace.application:ignore-context-source
               (aitools.workspace.application::make-ignore-context :source source))
              :to-be source))
    (let* ((bytes #(65 66))
           (overlay
             (make-workspace-overlay
              :read-octets (lambda (path)
                             (declare (ignore path))
                             (values t bytes)))))
      (multiple-value-bind (override-p actual)
          (funcall
           (aitools.workspace.application::workspace-overlay-read-octets overlay)
           "file")
        (expect override-p :to-be-truthy)
        (expect actual :to-equalp bytes)))))

(describe "workspace Git boundary predicates"
  (it "matches .git path components and metadata path prefixes"
    (expect (git-path-has-dot-git-component-p "/repo/.git/config")
            :to-be-truthy)
    (expect (git-path-has-dot-git-component-p "/repo/.GIT/config")
            :to-be-truthy)
    (expect (git-path-has-dot-git-component-p "/repo/.gitignore/config")
            :to-be-falsy)
    (expect (git-metadata-path-inside-p "/bare/.git" "/bare/.git/config")
            :to-be-truthy)
    (expect (git-metadata-path-inside-p "/bare/.git" "/bare/.git")
            :to-be-truthy)
    (expect (git-metadata-path-inside-p "/bare/.git" "/bare/.gitconfig")
            :to-be-falsy)
    (expect (git-metadata-path-inside-p "/bare/.git" "/other/.git/config")
            :to-be-falsy))
  (it "recognizes common-root metadata entries and concrete lock files"
    (expect (git-common-root-entry-name-p "objects") :to-be-truthy)
    (expect (git-common-root-entry-name-p "config.lock") :to-be-truthy)
    (expect (git-common-root-entry-name-p "CUSTOM_HEAD") :to-be-truthy)
    (expect (git-common-root-entry-name-p "CUSTOM_HEAD.lock") :to-be-truthy)
    (expect (git-common-root-entry-name-p "FETCH_HEAD.lock") :to-be-falsy)
    (expect (git-common-root-entry-name-p "unknown.lock") :to-be-falsy)
    (expect (git-common-root-entry-name-p nil) :to-be-falsy)
    (expect
     (git-common-root-entry-name-p
      (concatenate 'string "sharedindex."
                   (make-string 40 :initial-element #\a)))
     :to-be-truthy)
    (expect
     (git-common-root-entry-name-p
      (concatenate 'string "sharedindex."
                   (make-string 64 :initial-element #\a)))
     :to-be-truthy)
    (expect
     (git-common-root-entry-name-p
      (concatenate 'string "sharedindex."
                   (make-string 39 :initial-element #\a)))
     :to-be-falsy)
    (expect
     (git-common-root-entry-name-p
      (concatenate 'string "sharedindex."
                   (make-string 40 :initial-element #\g)))
     :to-be-falsy)
    (expect (git-common-root-entry-name-p "bad name_HEAD") :to-be-falsy))
  (it "matches only known entries below a common root"
    (expect (git-common-root-metadata-path-p "/bare" "/bare/objects/pack")
            :to-be-truthy)
    (expect (git-common-root-metadata-path-p "/bare" "/bare/.git/config")
            :to-be-truthy)
    (expect (git-common-root-metadata-path-p "/bare" "/bare/ordinary/file")
            :to-be-falsy)
    (expect (git-common-root-metadata-path-p "/bare" "/barely/objects")
            :to-be-falsy)))

(describe "workspace ignore path and parser boundaries"
  (it "returns the path remainder only below the ignore base"
    (expect (aitools.workspace.domain::%path-under-base "file" "" nil)
            :to-equal "file")
    (expect (aitools.workspace.domain::%path-under-base "repo/sub/file" "repo/sub" nil)
            :to-equal "file")
    (expect (aitools.workspace.domain::%path-under-base "Repo/Sub/File" "repo/sub" t)
            :to-equal "File")
    (expect (aitools.workspace.domain::%path-under-base "repo/sub" "repo/sub" nil)
            :to-be nil)
    (expect (aitools.workspace.domain::%path-under-base "repo/subdir/file" "repo/sub" nil)
            :to-be nil)
    (expect (aitools.workspace.domain::%path-under-base "Repo/sub/file" "repo" nil)
            :to-be nil))
  (it "rejects a gitdir marker without a path"
    (expect (aitools.workspace.domain::parse-gitdir-file "gitdir:")
            :to-be nil))
  (it "finishes a read after a full-sized stream chunk"
    (let ((calls 0))
      (multiple-value-bind (octets status)
          (aitools.workspace.infrastructure::%read-stream-octets
           nil 65536 65536
           :reader (lambda (buffer stream)
                     (declare (ignore stream))
                     (incf calls)
                     (if (= calls 1)
                         (progn (fill buffer #x2A) (length buffer))
                         0)))
        (expect status :to-be nil)
        (expect (length octets) :to-be 65536)
        (expect (aref octets 0) :to-be #x2A)
        (expect calls :to-be 2)))))

(describe "workspace direct coverage calls"
  (it "executes the scan flow with the default size limit"
    (let* ((host
             (make-fake-workspace-boundaries
              :files '(("/r/.git/HEAD" . "ref: refs/heads/main")
                       ("/r/sample" . ""))
              :cwd "/r"))
           (root (resolved-root host))
           (seen nil))
      (expect
       (call-with-workspace-scan/k
        host root
        :emit (lambda (entry result)
                (declare (ignore result))
                (push (scan-entry-path entry) seen)
                nil)
        :on-complete (lambda (source stopped) (list source stopped))
        :on-error (lambda (&rest arguments)
                    (declare (ignore arguments))
                    :error))
       :to-equal '(:gitignore nil))
      (expect (nreverse seen) :to-equal '("sample"))))
  (it "calls the index and ignore parsers directly"
    (expect
     (coerce
      (parse-git-index-paths
       (make-array 12 :element-type '(unsigned-byte 8)
                   :initial-contents '(68 73 82 67 0 0 0 2 0 0 0 0)))
      'list)
     :to-equal nil)
    (let ((lines (parse-ignore-lines '("skip.tmp")))
          (octets (parse-ignore-octets (string-bytes "skip.tmp"))))
      (expect (ignore-list-verdict lines "skip.tmp" nil)
              :to-be :ignored)
      (expect (ignore-list-verdict octets "skip.tmp" nil)
              :to-be :ignored)))
  (it "evaluates workspace root and ignore context value paths"
    (let* ((repository (make-git-repository))
           (root (make-workspace-root :repository repository))
           (context (apply #'aitools.workspace.application::make-ignore-context nil)))
      (expect (workspace-root-repository root) :to-be repository)
      (expect (aitools.workspace.application:ignore-context-source context)
              :to-be :none)
      (expect (aitools.workspace.application::ignore-context-base-stack context)
              :to-be nil))
    (dolist (source '(:option :git :cwd))
      (expect (aitools.workspace.domain::workspace-root-source
               (make-workspace-root :source source))
              :to-be source))
    (dolist (source '(:none :builtin :gitignore))
      (expect (aitools.workspace.application:ignore-context-source
               (aitools.workspace.application::make-ignore-context :source source))
              :to-be source)))
  (it "covers a positive count and the nonpositive fallback"
    (expect (plusp (aitools.workspace.infrastructure::processor-count))
            :to-be-truthy)
    (cl-weave:with-mocked-functions
        (((symbol-function
           (find-symbol "%SYSCONF-ONLINE-PROCESSORS"
                        "AITOOLS.WORKSPACE.INFRASTRUCTURE"))
          (lambda () -1)))
      (expect (aitools.workspace.infrastructure::processor-count)
              :to-be
              1))))

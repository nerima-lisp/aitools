(in-package #:aitools.workspace.test)

(describe "workspace value construction edges"
  (it "constructs workspace entries, ignore contexts, and overlays"
    (dolist (kind '(:file :directory :symlink :other))
      (expect (workspace-entry-kind
               (make-workspace-entry :kind kind
                                     :size 1
                                     :mtime -1
                                     :mode #o7777
                                     :device 0
                                     :inode 0))
              :to-be kind))
    (let* ((repository
             (make-git-repository :top "/repo"
                                  :git-dir "/repo/.git"
                                  :common-dir "/repo/.git"))
           (root (make-workspace-root :repository repository)))
      (expect (workspace-root-repository root) :to-be repository))
    (dolist (source '(:none :builtin :gitignore))
      (let* ((base-stack
               (list (aitools.workspace.domain:parse-ignore-lines '("skip.tmp"))))
             (context
               (aitools.workspace.application::make-ignore-context
                :source source
                :base-stack base-stack)))
        (expect (aitools.workspace.application:ignore-context-source context)
                :to-be source)
        (expect (aitools.workspace.application::ignore-context-base-stack context)
                :to-be base-stack)))
    (let* ((bytes #(65 66))
           (overlay
             (aitools.workspace.application:make-workspace-overlay
              :list-directory (lambda (path entries)
                                (list path (length entries)))
              :read-octets (lambda (path)
                             (declare (ignore path))
                             (values t bytes)))))
      (expect
       (funcall
        (aitools.workspace.application::workspace-overlay-list-directory overlay)
        "dir" '(entry))
       :to-equal '("dir" 1))
      (multiple-value-bind (override-p actual)
          (funcall
           (aitools.workspace.application::workspace-overlay-read-octets overlay)
           "file")
        (progn
          (expect override-p :to-be-truthy)
          (let ((default-overlay
                  (aitools.workspace.application:make-workspace-overlay)))
            (expect
             (aitools.workspace.application::workspace-overlay-list-directory default-overlay)
             :to-be nil)
            (expect
             (aitools.workspace.application::workspace-overlay-read-octets default-overlay)
             :to-be nil)))
        (expect actual :to-equalp bytes)))
    (signals type-error (make-workspace-entry :kind (read-from-string ":unknown")))
    (signals type-error (make-workspace-entry :size (read-from-string "-1")))
    (signals type-error (make-workspace-entry :mode (read-from-string "4096")))
    (signals type-error
      (aitools.workspace.application::make-ignore-context :source (read-from-string ":unknown")))
    (signals type-error
      (aitools.workspace.application:make-workspace-overlay
       :list-directory (read-from-string ":invalid")))
    (signals type-error
      (aitools.workspace.domain:make-workspace-root :source (read-from-string ":unknown")))))

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
  (it "executes scan and parser entry points"
    (let* ((host
             (make-fake-workspace-boundaries
              :files '(("/r/.git/HEAD" . "ref: refs/heads/main")
                       ("/r/sample" . ""))
              :cwd "/r"))
           (root (resolved-root host))
           (seen nil))
      (expect
       (aitools.workspace.application:call-with-workspace-scan/k
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
      (expect (nreverse seen) :to-equal '("sample")))
    (let ((empty-index
            (make-array 12 :element-type '(unsigned-byte 8)
                        :initial-contents '(68 73 82 67 0 0 0 2 0 0 0 0)))
          (one-entry-index
            (make-array 96 :element-type '(unsigned-byte 8) :initial-element 0)))
      (replace one-entry-index #(68 73 82 67 0 0 0 2 0 0 0 1))
      (setf (aref one-entry-index 74) 97)
      (expect
       (coerce
        (aitools.workspace.domain:parse-git-index-paths empty-index)
        'list)
       :to-equal nil)
      (expect
       (coerce
        (aitools.workspace.domain:parse-git-index-paths one-entry-index)
        'list)
       :to-equal '("a")))
    (signals aitools.workspace.domain:git-index-error
      (aitools.workspace.domain:parse-git-index-paths #(0)))
    (let ((lines
            (apply #'aitools.workspace.domain:parse-ignore-lines
                   (list '("skip.tmp"))))
          (octets
            (apply #'aitools.workspace.domain:parse-ignore-octets
                   (list (string-bytes "skip.tmp")))))
      (expect
       (aitools.workspace.domain:ignore-list-verdict lines "skip.tmp" nil)
       :to-be :ignored)
      (expect
       (aitools.workspace.domain:ignore-list-verdict octets "skip.tmp" nil)
       :to-be :ignored)))
  (it "covers workspace-root sources and gitdir marker alternatives"
    (let* ((repository (aitools.workspace.domain:make-git-repository))
           (root (aitools.workspace.domain:make-workspace-root
                  :repository repository))
           (context
             (apply #'aitools.workspace.application::make-ignore-context nil)))
      (expect (aitools.workspace.domain:workspace-root-repository root)
              :to-be repository)
      (expect (aitools.workspace.application:ignore-context-source context)
              :to-be :none)
      (expect
       (aitools.workspace.application::ignore-context-base-stack context)
       :to-be nil))
    (dolist (source '(:option :git :cwd))
      (let ((root
              (apply #'aitools.workspace.domain:make-workspace-root
                     (list :source source))))
        (expect (aitools.workspace.domain::workspace-root-source root)
                :to-be source)))
    (expect (aitools.workspace.domain::parse-gitdir-file "gitdir:")
            :to-be nil)
    (expect
     (aitools.workspace.domain::parse-gitdir-file "gitdir: /worktrees/active")
     :to-equal "/worktrees/active")))

(in-package #:aitools.workspace.test)

(describe "workspace value defaults and type contracts"
  (it "sets workspace value defaults"
    (let ((entry (make-workspace-entry))
          (repository (make-git-repository))
          (root (make-workspace-root))
          (context (aitools.workspace.application::make-ignore-context)))
      (expect (workspace-entry-name entry) :to-equal "")
      (expect (workspace-entry-kind entry) :to-be :file)
      (expect (workspace-entry-size entry) :to-be 0)
      (expect (workspace-entry-mtime entry) :to-be 0)
      (expect (workspace-entry-mode entry) :to-be 0)
      (expect (aitools.workspace.domain::entry-order-key entry) :to-equal "")
      (expect (git-repository-top repository) :to-equal "")
      (expect (aitools.workspace.domain::workspace-root-source root) :to-be :cwd)
      (expect (workspace-root-repository root) :to-be-falsy)
      (expect (aitools.workspace.application::ignore-context-source context) :to-be :none)
      (expect (aitools.workspace.application::ignore-context-base-stack context) :to-be nil)))
  (it "rejects invalid typed slot values"
    (let ((invalid (intern "INVALID" "KEYWORD"))
          (values (make-hash-table :test (function eq))))
      (setf (gethash :kind values) invalid
            (gethash :name values) 1
            (gethash :size values) -1
            (gethash :mode values) 4096
            (gethash :repository-top values) 1
            (gethash :source values) invalid
            (gethash :ignore-source values) invalid)
      (locally (declare (optimize (safety 3)))
        (expect (handler-case (progn (make-workspace-entry :kind (gethash :kind values)) nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case (progn (make-workspace-entry :name (gethash :name values)) nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case (progn (make-workspace-entry :size (gethash :size values)) nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case (progn (make-workspace-entry :mode (gethash :mode values)) nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case (progn (make-git-repository :top (gethash :repository-top values)) nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case (progn (make-workspace-root :source (gethash :source values)) nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case
                    (progn (aitools.workspace.application::make-ignore-context
                            :source (gethash :ignore-source values))
                           nil)
                  (type-error () t))
                :to-be-truthy)))))

(describe "workspace repository pointers and entry ordering"
  (it "parses a gitdir file's first line and rejects empty or invalid targets"
    (expect (aitools.workspace.domain::parse-gitdir-file
             (format nil "gitdir:  /worktrees/wt  ~%ignored"))
            :to-equal "/worktrees/wt")
    (expect (aitools.workspace.domain::parse-gitdir-file "gitdir: /without-newline")
            :to-equal "/without-newline")
    (expect (aitools.workspace.domain::parse-gitdir-file "gitdir: ")
            :to-be nil)
    (expect (aitools.workspace.domain::parse-gitdir-file "other: /path")
            :to-be nil)
    (expect (aitools.workspace.domain::parse-gitdir-file "")
            :to-be nil))
  (it "sorts entries using a slash suffix for directories"
    (let ((directory (make-workspace-entry :name "tree" :kind :directory))
          (file (make-workspace-entry :name "tree.c" :kind :file)))
      (expect (aitools.workspace.domain::entry-order-key directory)
              :to-equal "tree/")
      (expect (mapcar #'workspace-entry-name
                      (aitools.workspace.domain::sort-entries (list directory file)))
              :to-equal (list "tree.c" "tree")))))

(describe "workspace value type boundaries"
  (it "rejects nonnumeric metadata and invalid repository or context slots"
    (let ((bad (make-hash-table :test (function eq)))
          (invalid (intern "INVALID" "KEYWORD")))
      (setf (gethash :size bad) "large"
            (gethash :mtime bad) "now"
            (gethash :mode bad) "executable"
            (gethash :git-dir bad) 1
            (gethash :root-real bad) 1
            (gethash :root-repository bad) invalid
            (gethash :ignore-casefold bad) 1
            (gethash :ignore-stack bad) invalid)
      (locally (declare (optimize (safety 3)))
        (expect (handler-case (progn (make-workspace-entry :size (gethash :size bad)) nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case (progn (make-workspace-entry :mtime (gethash :mtime bad)) nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case (progn (make-workspace-entry :mode (gethash :mode bad)) nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case (progn (make-git-repository :git-dir (gethash :git-dir bad)) nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case (progn (make-workspace-root :real (gethash :root-real bad)) nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case (progn (make-workspace-root :repository (gethash :root-repository bad)) nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case
                    (progn (aitools.workspace.application::make-ignore-context
                            :casefold (gethash :ignore-casefold bad))
                           nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case
                    (progn (aitools.workspace.application::make-ignore-context
                            :base-stack (gethash :ignore-stack bad))
                           nil)
                  (type-error () t))
                :to-be-truthy)))))

(describe "workspace value constructor boundaries"
  (it "covers root sources and rejects invalid callbacks and ignore stacks"
    (let ((invalid (make-hash-table :test (function eq))))
      (setf (gethash :callback invalid) "not a function"
            (gethash :stack invalid) "not a list")
      (dolist (source '(:option :git :cwd))
        (expect (aitools.workspace.domain::workspace-root-source
                 (make-workspace-root :source source))
                :to-be source))
      (let ((entry (make-workspace-entry :size 1 :mode #o7777)))
        (expect (workspace-entry-size entry) :to-be 1)
        (expect (workspace-entry-mode entry) :to-be #o7777))
      (locally (declare (optimize (safety 3)))
        (expect (handler-case
                    (progn (make-workspace-overlay
                            :list-directory (gethash :callback invalid))
                           nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case
                    (progn (make-workspace-overlay
                            :read-octets (gethash :callback invalid))
                           nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case
                    (progn (aitools.workspace.application::make-ignore-context
                            :base-stack (gethash :stack invalid))
                           nil)
                  (type-error () t))
                :to-be-truthy)))))

(describe "workspace value defaults through dynamic calls"
  (it "evaluates default slots and rejects invalid values through APPLY"
    (let* ((context (apply #'aitools.workspace.application::make-ignore-context nil))
           (invalid (make-hash-table :test (function eq))))
      (expect (aitools.workspace.application:ignore-context-source context)
              :to-be :none)
      (expect (aitools.workspace.application::ignore-context-base-stack context)
              :to-be nil)
      (setf (gethash :size invalid) -1
            (gethash :mode invalid) 4096
            (gethash :callback invalid) "not a function")
      (locally (declare (optimize (safety 3)))
        (expect (handler-case
                    (progn (apply #'make-workspace-entry
                                  (list :size (gethash :size invalid)))
                           nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case
                    (progn (apply #'make-workspace-entry
                                  (list :mode (gethash :mode invalid)))
                           nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case
                    (progn (apply #'make-workspace-overlay
                                  (list :list-directory (gethash :callback invalid)))
                           nil)
                  (type-error () t))
                :to-be-truthy)
        (expect (handler-case
                    (progn (apply #'aitools.workspace.application::make-ignore-context
                                  (list :base-stack (gethash :callback invalid)))
                           nil)
                  (type-error () t))
                :to-be-truthy)))))

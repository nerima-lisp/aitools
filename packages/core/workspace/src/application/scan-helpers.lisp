(in-package #:aitools.workspace.application)

(defun %root-relative (context top-relative)
  "TOP-RELATIVE re-expressed relative to the workspace root, or NIL when it
lies above the root (between the repository top and the workspace root)."
  (let ((prefix (ignore-context-prefix context)))
    (cond ((zerop (length prefix)) top-relative)
          ((string= top-relative prefix) "")
          ((and (> (length top-relative) (length prefix))
                (string= prefix top-relative :end2 (length prefix))
                (char= (char top-relative (length prefix)) #\/))
           (subseq top-relative (1+ (length prefix))))
          (t nil))))

(defun %list-entries (host overlay absolute root-relative)
  "(VALUES sorted-entries readable-p) for one directory, overlay applied."
  (multiple-value-bind (entries readable) (host-list-directory host absolute)
    (let ((entries (if readable entries '()))
          (hook (and overlay root-relative (workspace-overlay-list-directory overlay))))
      (when hook
        (setf entries (funcall hook root-relative entries)))
      (values (sort-entries entries) (or readable (and hook t))))))

(defun %read-file-octets (host overlay absolute root-relative)
  (let ((hook (and overlay root-relative (workspace-overlay-read-octets overlay))))
    (multiple-value-bind (override-p octets) (if hook (funcall hook root-relative) (values nil nil))
      (if override-p octets (host-read-octets host absolute)))))

(defun %directory-ignore-list (host context overlay directory-top-relative entry)
  "The IGNORE-LIST of DIRECTORY's .gitignore given its listing ENTRY (or
NIL). A symlinked .gitignore is not read, as git refuses to follow it."
  (when (and entry (eq (workspace-entry-kind entry) :file) (eq (ignore-context-source context) :gitignore))
    (let* ((file-top-relative (join-path directory-top-relative ".gitignore"))
           (octets (%read-file-octets host overlay
                                      (join-path (ignore-context-top context) file-top-relative)
                                      (%root-relative context file-top-relative))))
      (and octets (parse-ignore-octets octets :base directory-top-relative :source file-top-relative)))))

(defun %gitignore-entry (host context overlay directory-top-relative)
  "The .gitignore listing entry of a directory the walk has not listed."
  (let* ((root-relative (%root-relative context directory-top-relative))
         (absolute (join-path (ignore-context-top context) directory-top-relative)))
    (find ".gitignore" (%list-entries host overlay absolute root-relative)
          :key #'workspace-entry-name :test #'string=)))

(defun %tracked-p (context top-relative directory-p)
  (let ((tracked (ignore-context-tracked context)))
    (and tracked
         (if directory-p
             (sorted-paths-have-prefix-p tracked (concatenate 'string top-relative "/"))
             (sorted-paths-contains-p tracked top-relative)))))

(defun %ignored-p (context stack top-relative directory-p)
  (and stack
       (eq :ignored (ignore-stack-verdict stack top-relative directory-p
                                          :casefold (ignore-context-casefold context)))))

(defun %always-skipped-name-p (name)
  (or (git-metadata-name-p name) (aitools-temporary-name-p name)))

(defun %stack-above (host context overlay top-relative)
  "The ignore stack in force for entries of the directory TOP-RELATIVE,
excluding that directory's own .gitignore: every ancestor's .gitignore from
the repository top down, then the base stack."
  (let ((stack (ignore-context-base-stack context))
        (directory ""))
    (when (eq (ignore-context-source context) :gitignore)
      (dolist (component (path-components top-relative))
        (let ((list (%directory-ignore-list host context overlay directory
                                            (%gitignore-entry host context overlay directory))))
          (when list (push list stack)))
        (setf directory (join-path directory component))))
    stack))

(defun %relative-start (host root path)
  "PATH (absolute, or relative to the root) as a normalized root-relative
path, or NIL when it is outside the root. An absolute PATH maps through
WORKSPACE-RELATIVE-PATH, so a start typed from a working directory reached
through the root's real path is inside."
  (if (absolute-path-p path)
      (workspace-relative-path host root (normalize-path path))
      (let ((normalized (normalize-path (join-path (workspace-root-path root) path))))
        (and (path-inside-p (workspace-root-path root) normalized)
             (path-relative-to (workspace-root-path root) normalized)))))

(defun %starts (host root paths)
  "(VALUES starts bad-path): sorted root-relative starting points with any
start nested below another removed, or the first path outside the root."
  (let ((relatives '()))
    (dolist (path (or paths '("")))
      (let ((relative (%relative-start host root path)))
        (unless relative (return-from %starts (values nil path)))
        (push relative relatives)))
    (let ((sorted (sort (remove-duplicates relatives :test #'string=) #'string<)))
      (values (remove-if (lambda (start)
                           (some (lambda (other)
                                   (and (string/= other start)
                                        (or (string= other "")
                                            (and (> (length start) (length other))
                                                 (string= other start :end2 (length other))
                                                 (char= (char start (length other)) #\/)))))
                                 sorted))
                         sorted)
              nil))))

(defun %lookup-entry (host context overlay root-relative)
  "The WORKSPACE-ENTRY for ROOT-RELATIVE as its parent's listing shows it
(overlay applied), a synthetic directory entry for the root, or NIL."
  (if (string= root-relative "")
      (make-workspace-entry :name "" :kind :directory)
      (let* ((parent (or (path-parent root-relative) ""))
             (absolute (join-path (ignore-context-root-path context) parent)))
        (find (path-basename root-relative) (%list-entries host overlay absolute parent)
              :key #'workspace-entry-name :test #'string=))))

;;;;
;;;; Compare file copy, move, deletion, metadata, temporary-file, and batch-rename behavior with shell tools.
(in-package #:aitools.e2e.test)

(defun tree-snapshot (workspace directory)
  "Every entry under DIRECTORY as (relative kind mode content-or-target),
sorted: an observer independent of both aitools and the shell tools."
  (let ((root (ws-path workspace directory)))
    (labels ((walk (path prefix)
               (loop for entry in (sorted (mapcar (lambda (p)
                                                     (let ((name (if (uiop:directory-pathname-p p)
                                                                     (car (last (pathname-directory p)))
                                                                     (file-namestring p))))
                                                       name))
                                                   (append (uiop:subdirectories path)
                                                           (uiop:directory-files path))))
                     for relative = (if prefix (format nil "~A/~A" prefix entry) entry)
                     for native-path = (format nil "~A~A" (native path) entry)
                     for stat = (sb-posix:lstat native-path)
                     for mode = (sb-posix:stat-mode stat)
                     for kind = (cond ((= (logand mode #o170000) #o120000) :symlink)
                                      ((= (logand mode #o170000) #o040000) :dir)
                                      (t :file))
                     collect (list relative kind (logand mode #o7777)
                                   (case kind
                                     (:symlink (sb-posix:readlink native-path))
                                     (:file (read-octets native-path))))
                     when (eq kind :dir)
                       append (walk (uiop:ensure-directory-pathname native-path) relative))))
      (if (probe-file root) (walk root nil) :absent))))

(defun put-twin-trees (workspace)
  (dolist (side '("a" "o"))
    (put workspace (format nil "~A/f.txt" side) (format nil "file~%") :mode #o640)
    (put workspace (format nil "~A/dir/g.txt" side) (format nil "g~%"))
    (put workspace (format nil "~A/dir/sub/h.txt" side) (format nil "h~%"))
    (ensure-directories-exist (ws-path workspace (format nil "~A/emptydir/" side)))))

(defmacro define-file-operation-case ((row name foreign) script arguments)
  "Run the shell SCRIPT in o/ and aitools ARGUMENTS (paths under a/) on twin
trees, then compare the two trees entry by entry."
  `(define-row-case (,row ,name :foreign ,foreign) (ws)
     (put-twin-trees ws)
     (oracle ws ',foreign (format nil "cd o && ~A" ,script) nil)
     (run-ok ws ,arguments)
     (expect (tree-snapshot ws "a/") :to-equalp (tree-snapshot ws "o/"))))

(define-file-operation-case (28 "cp is copy" ("cp"))
  "cp f.txt copy.txt" '("copy" "a/f.txt" "a/copy.txt"))

(define-file-operation-case (28 "cp -r is copy --recursive" ("cp"))
  "cp -r dir dir2" '("copy" "a/dir" "a/dir2" "--recursive"))

(define-file-operation-case (28 "mv is move" ("mv"))
  "mv f.txt moved.txt" '("move" "a/f.txt" "a/moved.txt"))

(define-file-operation-case (28 "mv dir new is move" ("mv"))
  "mv dir renamed" '("move" "a/dir" "a/renamed"))

(define-file-operation-case (28 "rm is delete" ("rm"))
  "rm f.txt" '("delete" "a/f.txt"))

(define-file-operation-case (28 "rmdir is delete on an empty directory" ("rmdir"))
  "rmdir emptydir" '("delete" "a/emptydir"))

(define-file-operation-case (29 "mkdir -p is mkdir" ("mkdir"))
  "mkdir -p new/deep/er" '("mkdir" "a/new/deep/er"))

(define-file-operation-case (29 "chmod +x is chmod --exec" ("chmod"))
  "chmod +x f.txt" '("chmod" "a/f.txt" "--exec"))

(define-file-operation-case (29 "chmod 644 is chmod --mode 0644" ("chmod"))
  "chmod 644 f.txt" '("chmod" "a/f.txt" "--mode" "0644"))

(define-file-operation-case (29 "ln -s is link" ("ln"))
  "ln -s f.txt f.link" '("link" "f.txt" "a/f.link"))

(define-row-case (29 "ln -sf on an existing symlink is link --overwrite" :foreign ("ln")) (ws)
  (put-twin-trees ws)
  (shell ws "ln -s f.txt a/l && ln -s f.txt o/l")
  (oracle ws '("ln") "cd o && ln -sf dir/g.txt l" nil)
  (run-ok ws (list "link" "dir/g.txt" "a/l" "--overwrite"
                   "--expect-hash" (format nil "a/l=~A" (info-hash ws "a/l"))))
  (expect (tree-snapshot ws "a/") :to-equalp (tree-snapshot ws "o/")))

(define-row-case (29 "touch creates what touch creates" :foreign ("touch")) (ws)
  (put-twin-trees ws)
  (oracle ws '("touch") "cd o && touch new.txt" nil)
  (run-ok ws '("touch" "a/new.txt"))
  (expect (tree-snapshot ws "a/") :to-equalp (tree-snapshot ws "o/")))

(define-row-case (29 "touch on an existing file moves its mtime forward as touch does" :foreign ("touch")) (ws)
  (put-twin-trees ws)
  (dolist (side '("a/f.txt" "o/f.txt"))
    (sb-posix:utimes (native (ws-path ws side)) 1577836800 1577836800))
  (oracle ws '("touch") "cd o && touch f.txt" nil)
  (run-ok ws '("touch" "a/f.txt"))
  (flet ((mtime (path) (sb-posix:stat-mtime (sb-posix:stat (native (ws-path ws path))))))
    (expect (> (mtime "a/f.txt") 1577836800) :to-be (> (mtime "o/f.txt") 1577836800))
    (expect (> (mtime "a/f.txt") 1577836800) :to-be t))
  (expect (tree-snapshot ws "a/") :to-equalp (tree-snapshot ws "o/")))

(define-row-case (29 "mktemp's empty private file is mktemp's" :foreign ("mktemp")) (ws)
  (let* ((expected (oracle ws '("mktemp" "stat")
                           "f=$(mktemp) && { stat -c '%a %s' \"$f\" 2>/dev/null || stat -f '%Lp %z' \"$f\"; } && rm \"$f\""
                           (format nil "600 0~%")))
         (path (jget (run-ok ws '("mktemp")) "path"))
         (stat (sb-posix:stat path)))
    (expect (format nil "~O ~D~%" (logand #o7777 (sb-posix:stat-mode stat)) (sb-posix:stat-size stat))
            :to-equal expected)))

(define-row-case (30 "rename 's/\\.txt$/.md/' is find, then batch --atomic of moves" :foreign ("rename")) (ws)
  (dolist (name '("a.txt" "b.txt" "keep.log"))
    (put ws (format nil "a/~A" name) name)
    (put ws (format nil "o/~A" name) name))
  ;; The fixed expectation is the renamed tree itself, so without the Perl
  ;; rename it is built in o/ directly.
  (if (and (tool-path "rename")
           (zerop (nth-value 1 (shell ws "rename --help 2>&1 | grep -q 'perlexpr\\|perl'" :expect-status :any))))
      (oracle ws '("rename") "cd o && rename 's/\\.txt$/.md/' *.txt" nil)
      (progn
        (note-oracle :fixed '("rename (Perl)"))
        (dolist (name '("a" "b"))
          (rename-file (ws-path ws (format nil "o/~A.txt" name)) (ws-path ws (format nil "o/~A.md" name))))))
  (let* ((found (mapcar (lambda (item) (jget item "path"))
                        (jlist (jget (run-ok ws '("find" "*.txt" "a")) "items"))))
         (moves (mapcar (lambda (path)
                          (vector "move" path (format nil "~A.md" (subseq path 0 (- (length path) 4)))))
                        found)))
    (expect (length found) :to-be 2)
    (run-ok ws '("batch" "--atomic" "--stdin")
            :stdin (with-output-to-string (stream) (json-kit:write-json (coerce moves 'vector) stream)))
    (expect (tree-snapshot ws "a/") :to-equalp (tree-snapshot ws "o/"))))

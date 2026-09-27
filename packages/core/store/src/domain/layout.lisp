;;;; packages/core/store/src/domain/layout.lisp
;;;;
;;;; The state directory layout, identifier formats, and the shape
;;;; rules for workspace-relative paths the store accepts. Paths are native
;;;; namestring strings joined with `/`, never CL pathnames: a file named
;;;; `a*[1].txt` must not be parsed as a wildcard.
(in-package #:aitools.store.domain)

(defun %strip-trailing-slash (string)
  (if (and (> (length string) 1) (char= (char string (1- (length string))) #\/))
      (subseq string 0 (1- (length string)))
      string))

(defun join-path (directory &rest components)
  (let ((result (%strip-trailing-slash directory)))
    (dolist (component components result)
      (unless (zerop (length component))
        (setf result (if (string= result "/")
                         (concatenate 'string "/" component)
                         (concatenate 'string result "/" component)))))))

(defun state-home (xdg-state-home home)
  "`$XDG_STATE_HOME/aitools`, else `~/.local/state/aitools`. The XDG
base directory specification says a relative value must be ignored, so only
an absolute XDG_STATE_HOME is honoured."
  (cond ((and xdg-state-home (plusp (length xdg-state-home)) (char= (char xdg-state-home 0) #\/))
         (join-path xdg-state-home "aitools"))
        ((and home (plusp (length home)) (char= (char home 0) #\/))
         (join-path home ".local/state/aitools"))
        (t (error "neither XDG_STATE_HOME nor HOME names an absolute directory"))))

(defun %identifier-char-p (char)
  (or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9) (find char "._-")))

(defun workspace-id (real-root)
  "`<dir name>-<first 16 hex digits of SHA-256(real root path)>`. The
name keeps the state directory readable to a person; the hash makes two
checkouts with the same directory name distinct. Characters outside
[A-Za-z0-9._-] become `_` so the id is always a single safe path component."
  (let* ((root (%strip-trailing-slash real-root))
         (slash (position #\/ root :from-end t))
         (name (if (and slash (< (1+ slash) (length root))) (subseq root (1+ slash)) "root"))
         (safe (map 'string (lambda (char) (if (%identifier-char-p char) char #\_))
                    (subseq name 0 (min 64 (length name))))))
    (when (member safe '("." "..") :test #'string=)
      (setf safe "root"))
    (format nil "~A-~A" safe (subseq (content-hash (string-octets root)) 0 16))))

(defun workspace-state-directory (state-home real-root)
  (join-path state-home (workspace-id real-root)))

(defun lock-file-path (state-dir) (join-path state-dir "lock"))
(defun commit-directory (state-dir) (join-path state-dir "commit"))
(defun intent-file-path (state-dir op-id)
  (join-path (commit-directory state-dir) (concatenate 'string op-id ".json")))
(defun journal-directory (state-dir) (join-path state-dir "journal"))
(defun journal-file-path (state-dir) (join-path (journal-directory state-dir) "ops.jsonl"))
(defun blobs-directory (state-dir) (join-path state-dir "blobs"))
(defun blob-file-path (state-dir hash)
  (unless (valid-blob-hash-p hash)
    (%format-error "blob name is not a content hash"))
  (join-path (blobs-directory state-dir) hash))
(defun tx-root-directory (state-dir) (join-path state-dir "tx"))
(defun tx-directory (state-dir tx-id)
  (unless (valid-tx-id-p tx-id)
    (%format-error "tx id has the wrong shape"))
  (join-path (tx-root-directory state-dir) tx-id))
(defun tmp-directory (state-dir) (join-path state-dir "tmp"))

(defun temp-file-name (op-id n)
  "The write protocol's temp file name `.aitools-<op_id>-<n>.tmp`; the scan excludes this pattern
from every scan."
  (format nil ".aitools-~A-~D.tmp" op-id n))

(defun temp-file-name-p (name)
  (let ((length (length name)))
    (and (> length 13)
         (string= ".aitools-" name :end2 9)
         (string= ".tmp" name :start2 (- length 4)))))

;;; Identifiers: `op-YYYYMMDDTHHMMSSZ-xxxxxxxx`, sortable by creation time.

(defun %format-id (prefix universal-time suffix)
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal-time 0)
    (format nil "~A-~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0DZ-~A"
            prefix year month day hour minute second suffix)))

(defun format-op-id (universal-time suffix) (%format-id "op" universal-time suffix))
(defun format-tx-id (universal-time suffix) (%format-id "tx" universal-time suffix))

(defun %valid-id-p (string prefix)
  "An op or tx id arrives as a command argument and is joined into a state
directory path, so its shape is checked exactly: the prefix, 8+6 digits
around `T`, `Z`, and 8 lowercase hex digits. Nothing else can be a path
component that escapes its directory."
  (flet ((digits-p (start end)
           (loop for i from start below end always (char<= #\0 (char string i) #\9))))
    (and (stringp string)
         (= (length string) 28)
         (string= prefix string :end2 3)
         (digits-p 3 11)
         (char= (char string 11) #\T)
         (digits-p 12 18)
         (string= "Z-" string :start2 18 :end2 20)
         (loop for i from 20 below 28
               always (let ((char (char string i))) (or (char<= #\0 char #\9) (char<= #\a char #\f)))))))

(defun valid-op-id-p (string) (%valid-id-p string "op-"))
(defun valid-tx-id-p (string) (%valid-id-p string "tx-"))

;;; Workspace-relative paths.

(defun valid-relative-path-p (string)
  "The only path shape the store writes: `/`-separated, non-empty, no
leading `/`, no empty, `.` or `..` component, and no NUL. Boundary policy
is the workspace context's job; this check only keeps a malformed
path from ever being joined onto the root."
  (and (stringp string)
       (plusp (length string))
       (not (find (code-char 0) string))
       (not (char= (char string 0) #\/))
       (loop with start = 0
             for slash = (position #\/ string :start start)
             for component = (subseq string start (or slash (length string)))
             always (not (member component '("" "." "..") :test #'string=))
             while slash
             do (setf start (1+ slash)))))

(defun parent-relative-path (path)
  "`a/b/c` -> `a/b`; `c` -> `` (the root)."
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path 0 slash) "")))

(defun path-ancestors (path)
  "Proper ancestors of PATH from the outermost: `a/b/c` -> (\"a\" \"a/b\")."
  (loop for slash = (position #\/ path) then (position #\/ path :start (1+ slash))
        while slash
        collect (subseq path 0 slash)))

(defun path-under-p (ancestor path)
  "True when PATH is ANCESTOR itself or inside it. ANCESTOR `` is the root."
  (or (zerop (length ancestor))
      (string= ancestor path)
      (and (> (length path) (length ancestor))
           (string= ancestor path :end2 (length ancestor))
           (char= (char path (length ancestor)) #\/))))

(defun git-metadata-path-p (path)
  "True when a component of the workspace-relative PATH names `.git`, compared
case-insensitively. The same rule as the workspace context's
GIT-METADATA-NAME-P, repeated here because the store may not depend on the
workspace context; recovery applies it to intent records it did not write."
  (loop with start = 0
        for slash = (position #\/ path :start start)
        thereis (string-equal ".git" path :start2 start :end2 (or slash (length path)))
        while slash
        do (setf start (1+ slash))))

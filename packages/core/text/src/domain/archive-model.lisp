;;;; packages/core/text/src/domain/archive-model.lisp
;;;;
;;;; The format-neutral archive vocabulary: ARCHIVE-ENTRY is what the zip
;;;; and tar readers report (`archive list`'s `items`), ARCHIVE-MEMBER is what the
;;;; writers consume. Also the zip-slip checks `archive extract` applies before
;;;; extracting anything.
(in-package #:aitools.text.domain)

(defstruct (archive-entry (:copier nil))
  "FORMAT is :ZIP or :TAR. KIND is :FILE, :DIRECTORY, :SYMLINK, :HARDLINK,
or :OTHER. MODE is the permission bits or NIL when the archive has none;
MTIME is Unix seconds. The remaining slots locate the member's data for
ARCHIVE-ENTRY-DATA and are not part of the public contract."
  (format :zip :type (member :zip :tar) :read-only t)
  (name "" :type string :read-only t)
  (kind :file :type (member :file :directory :symlink :hardlink :other) :read-only t)
  (size 0 :type (integer 0) :read-only t)
  (mode nil :type (or null (integer 0 #o7777)) :read-only t)
  (mtime 0 :type integer :read-only t)
  (link-target nil :type (or null string) :read-only t)
  (data-offset 0 :type (integer 0) :read-only t)
  (compressed-size 0 :type (integer 0) :read-only t)
  (method 0 :type (integer 0) :read-only t)
  (flags 0 :type (integer 0) :read-only t)
  (crc 0 :type (unsigned-byte 32) :read-only t))

(defstruct (archive-member (:copier nil))
  "An entry to write. NAME uses `/` separators with no trailing `/` (the
writers add it for directories). DATA is the file's bytes; LINK-TARGET the
symlink target."
  (name "" :type string :read-only t)
  (kind :file :type (member :file :directory :symlink) :read-only t)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type octets :read-only t)
  (mode #o644 :type (integer 0 #o7777) :read-only t)
  (mtime 0 :type (integer 0) :read-only t)
  (link-target nil :type (or null string) :read-only t))

(defun archive-entry-path-problem (name)
  "NIL when NAME is safe to extract below a directory, else the reason:
:EMPTY, :ABSOLUTE (leading `/` or a drive letter), :BACKSLASH (a Windows
separator that could hide traversal), :NUL, or :PARENT-TRAVERSAL (a `..`
component)."
  (cond ((zerop (length (string-right-trim "/" name))) :empty)
        ((find (code-char 0) name) :nul)
        ((or (char= (char name 0) #\/)
             (and (>= (length name) 2) (char= (char name 1) #\:) (alpha-char-p (char name 0))))
         :absolute)
        ((find #\\ name) :backslash)
        ((loop with start = 0
               for slash = (position #\/ name :start start)
               for component = (subseq name start (or slash (length name)))
               thereis (string= component "..")
               while slash
               do (setf start (1+ slash)))
         :parent-traversal)
        (t nil)))

(defun %path-components (path)
  "PATH's `/`-separated components without empty and `.` ones."
  (loop with start = 0
        for slash = (position #\/ path :start start)
        for component = (subseq path start (or slash (length path)))
        unless (or (string= component "") (string= component "."))
          collect component
        while slash
        do (setf start (1+ slash))))

(defun archive-link-target-problem (name target &key symlink-names)
  "NIL when a symlink named NAME pointing at TARGET stays below the
extraction directory, else :ABSOLUTE or :ESCAPES. TARGET is resolved
lexically against NAME's directory. SYMLINK-NAMES lists the names of every
symlink in the same archive: lexical resolution is wrong for a `..` taken
from a directory that is one of them (with x/d -> .., the target x/d/..
is the parent of the root, not x), so such a target is reported as
:ESCAPES. A `..`-free path through checked symlinks cannot leave the root.
A tar hard link's target is relative to the archive root instead; check it
with ARCHIVE-ENTRY-PATH-PROBLEM."
  (if (or (zerop (length target)) (char= (char target 0) #\/))
      :absolute
      (let ((links (mapcar (lambda (link) (format nil "~{~A~^/~}" (%path-components link))) symlink-names))
            (directory (butlast (%path-components name))))
        (dolist (component (%path-components target) nil)
          (if (string= component "..")
              (cond ((null directory) (return :escapes))
                    ((member (format nil "~{~A~^/~}" directory) links :test #'string=) (return :escapes))
                    (t (setf directory (butlast directory))))
              (setf directory (append directory (list component))))))))

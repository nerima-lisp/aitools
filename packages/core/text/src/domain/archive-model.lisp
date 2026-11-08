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

(defun archive-path-components (path &key keep-dots)
  "Components of PATH without empty components and, unless KEEP-DOTS, dot components."
  (loop with start = 0
        for slash = (position #\/ path :start start)
        for component = (subseq path start (or slash (length path)))
        unless (or (string= component "")
                   (and (not keep-dots) (string= component ".")))
          collect component
        while slash
        do (setf start (1+ slash))))

(defun archive-path-key (path)
  "A case-folded, canonically normalized archive path key."
  (format nil "~{~A~^/~}"
          (mapcar (lambda (component)
                    (normalize-text
                     (sb-unicode:casefold (normalize-text component :nfd))
                     :nfd))
                  (archive-path-components path))))

(defun archive-link-target-problem (name target &key symlink-names)
  "Rejects absolute targets, lexical escapes, and traversal through archive symlinks.
SYMLINK-NAMES lists all symlink entry names in the archive. Link targets are
relative to the directory of NAME; tar hard-link targets are archive-root-relative
and use ARCHIVE-ENTRY-PATH-PROBLEM."
  (if (or (zerop (length target)) (char= (char target 0) #\/))
      :absolute
      (let ((links (mapcar (function archive-path-key) symlink-names))
            (directory (butlast (archive-path-components name))))
        (dolist (component (archive-path-components target :keep-dots t) nil)
          (when (and directory
                     (member (archive-path-key (format nil "~{~A~^/~}" directory))
                             links :test #'string=))
            (return :escapes))
          (cond ((string= component "."))
                ((string= component "..")
                 (if directory
                     (setf directory (butlast directory))
                     (return :escapes)))
                (t (setf directory (append directory (list component)))))))))

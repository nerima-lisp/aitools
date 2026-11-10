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

(defconstant +archive-max-path-bytes+ 4095)
(defconstant +archive-max-component-bytes+ 255)
(defconstant +archive-max-depth+ 2048)
(defconstant +archive-max-link-target-bytes+ 4095)
(defconstant +archive-max-tar-records+ 200000)
(defconstant +archive-max-zip-records+ 65534)
(defconstant +archive-max-path-metadata-bytes+ (* 16 1024 1024))
(defconstant +archive-max-input-bytes+ (* 64 1024 1024))
(defconstant +archive-max-gzip-members+ 1024)
(defconstant +archive-max-gzip-member-output-bytes+ (* 256 1024 1024))
(defconstant +archive-max-gzip-output-bytes+ (* 1024 1024 1024))
(defun %archive-path-metadata-total (current additional)
  (let ((total (+ current additional)))
    (when (> total +archive-max-path-metadata-bytes+)
      (error 'archive-limit-exceeded
             :limit +archive-max-path-metadata-bytes+
             :reason "archive path metadata"))
    total))
(defun %archive-utf8-byte-length (string)
  (loop for character across string
        for code = (char-code character)
        sum (cond ((<= code #x7F) 1)
                  ((<= code #x7FF) 2)
                  ((<= code #xFFFF) 3)
                  (t 4))))
(defun %archive-path-octets-problem (octets start end)
  (declare (type octets octets) (type fixnum start end))
  (when (> (- end start) +archive-max-path-bytes+)
    (return-from %archive-path-octets-problem :path-too-long))
  (loop with component-start = start
        with depth = 0
        for index from start below end
        when (= (aref octets index) #x2f)
        do (let ((component-length (- index component-start)))
             (when (> component-length +archive-max-component-bytes+)
               (return-from %archive-path-octets-problem :component-too-long))
             (unless (or (zerop component-length)
                         (and (= component-length 1)
                              (= (aref octets component-start) #x2e)))
               (incf depth)
               (when (> depth +archive-max-depth+)
                 (return-from %archive-path-octets-problem :too-deep)))
             (setf component-start (1+ index)))
        finally (let ((component-length (- end component-start)))
                  (when (> component-length +archive-max-component-bytes+)
                    (return-from %archive-path-octets-problem :component-too-long))
                  (unless (or (zerop component-length)
                              (and (= component-length 1)
                                   (= (aref octets component-start) #x2e)))
                    (when (>= depth +archive-max-depth+)
                      (return-from %archive-path-octets-problem :too-deep))))))

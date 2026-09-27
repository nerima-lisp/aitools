;;;; packages/core/text/src/domain/archive-format.lisp
;;;;
;;;; The one archive format detector `archive list`/`read` and `archive
;;;; extract` share, so both commands accept the same files. Loaded after
;;;; the codecs because it sniffs through the tar checksum and the inflater.
(in-package #:aitools.text.domain)

(defun %octets-prefix-p (octets &rest bytes)
  (and (>= (length octets) (length bytes))
       (loop for byte in bytes for index from 0 always (= byte (aref octets index)))))

(defun %tar-header-p (octets)
  "True when OCTETS starts with a tar header: the ustar magic, or a
pre-POSIX v7 header (no magic) with a name and a valid checksum."
  (and (>= (length octets) +tar-block+)
       (or (%octets-prefix-p (subseq octets 257 262) 117 115 116 97 114)
           (and (plusp (aref octets 0))
                (handler-case (%tar-checksum-ok-p octets 0)
                  (archive-error () nil))))))

(defun %gzip-content-prefix (octets)
  "The first tar block of the decompressed gzip stream OCTETS (fewer bytes
when it is shorter), or NIL when it does not decode. Inflating stops there,
so sniffing a decompression bomb costs one block."
  (handler-case
      (multiple-value-bind (name mtime data-start) (gzip-member-header octets)
        (declare (ignore name mtime))
        (values (inflate octets :start data-start :truncate-at +tar-block+)))
    (archive-error () nil)))

(defun %name-ends-with-p (name suffix)
  (and name
       (>= (length name) (length suffix))
       (string-equal suffix name :start2 (- (length name) (length suffix)))))

(defun detect-archive-format (octets name)
  "The format of the archive OCTETS: :ZIP, :TAR (ustar, or v7 by its header
checksum), :TAR-GZ (gzip whose content starts with a tar header, or any gzip
NAME calls .tgz or .tar.gz), :GZ (any other gzip), or NIL. NAME, the file
name or NIL, also makes a .tar file :TAR so its reader reports what is wrong
with it."
  (cond ((or (%octets-prefix-p octets #x50 #x4B 3 4) (%octets-prefix-p octets #x50 #x4B 5 6)) :zip)
        ((%octets-prefix-p octets #x1F #x8B)
         (let ((prefix (%gzip-content-prefix octets)))
           (if (or (and prefix (%tar-header-p prefix))
                   (%name-ends-with-p name ".tgz")
                   (%name-ends-with-p name ".tar.gz"))
               :tar-gz
               :gz)))
        ((or (%tar-header-p octets) (%name-ends-with-p name ".tar")) :tar)
        (t nil)))

(defun archive-format-name (format)
  (ecase format (:zip "zip") (:tar "tar") (:tar-gz "tar.gz") (:gz "gz")))

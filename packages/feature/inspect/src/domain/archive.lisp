;;;;
;;;; The read side of `archive list` and `archive read` over the text context's codecs. An archive is
;;;; untrusted input: every malformed byte ends in ARCHIVE-ERROR (the codecs
;;;; guarantee it), and decompression is bounded while it runs by
;;;; :MAX-OUTPUT, so a small `.gz` cannot expand past the cap before the cap
;;;; is consulted.
(in-package #:aitools.inspect.domain)

(defconstant +archive-max-decompressed+
  (* 256 1024 1024)
  "Largest decompressed stream `archive list`/`read` will hold for a
`.gz` or `.tar.gz`.")

(defconstant +archive-max-entry+
  (* 64 1024 1024)
  "Largest member content `archive read` will decode.")

(defstruct
    (archive-item (:constructor make-archive-item (name kind size mode mtime entry))
     (:copier nil) (:predicate nil))
  "One listed member. ENTRY is the codec's ARCHIVE-ENTRY, or NIL for the
single member of a plain `.gz`."
  (name "" :type string :read-only t)
  (kind :file :read-only t)
  (size 0 :type (integer 0) :read-only t)
  (mode nil :read-only t)
  (mtime 0 :type integer :read-only t)
  (entry nil :read-only t))

(defstruct
    (archive (:constructor make-archive (format items data)) (:copier nil) (:predicate nil))
  "FORMAT is :ZIP, :TAR, :TAR.GZ, or :GZ; DATA the bytes the entries index
(the decompressed stream for the gzip formats)."
  (format :zip :read-only t)
  (items '() :type list :read-only t)
  (data nil :read-only t))

(defun archive-format-name (format)
  (ecase format
    (:zip "zip")
    (:tar "tar")
    (:tar.gz "tar.gz")
    (:gz "gz")))

(defun %tar-items (entries)
  (mapcar
   (lambda (entry)
     (make-archive-item (archive-entry-name entry)
                        (archive-entry-kind entry)
                        (archive-entry-size entry)
                        (archive-entry-mode entry)
                        (archive-entry-mtime entry)
                        entry))
   entries))

(defun %open-gzip (octets path tar-gz max-output)
  "The decompressed gzip OCTETS as an archive: a `.tar.gz` when TAR-GZ, else
a single-member `.gz`."
  (let ((data (gzip-decompress octets :max-output max-output)))
    (if tar-gz
        (make-archive :tar.gz (%tar-items (read-tar-entries data)) data)
        (make-archive :gz
                      (list
                       (make-archive-item
                        (aitools.text.domain:gzip-member-name octets path)
                        :file
                        (length data)
                        nil
                        (nth-value 1 (gzip-member-header octets))
                        nil))
                      data))))

(aitools.kernel.domain:define-flow
 (open-archive/k
  octets
  path
  &key
  on-archive
  on-unsupported
  on-malformed
  on-too-large
  (max-gzip-output +archive-max-decompressed+))
 "Detect OCTETS using the shared detector
(AITOOLS.TEXT.DOMAIN:DETECT-ARCHIVE-FORMAT; magic bytes first, a v7 tar by
its header checksum, with .tar, .tgz, and .tar.gz extensions) and call exactly
one continuation: ON-ARCHIVE (archive), ON-UNSUPPORTED (), ON-MALFORMED
(reason), or ON-TOO-LARGE (limit) when decompression exceeds
MAX-GZIP-OUTPUT. MAX-GZIP-OUTPUT sets the decompressed byte limit for :GZ
and :TAR-GZ inputs and defaults to
+ARCHIVE-MAX-DECOMPRESSED+."
 (declare (type function on-archive on-unsupported on-malformed on-too-large))
 (multiple-value-bind (archive outcome value)
     (handler-case (values
                    (ecase (aitools.text.domain:detect-archive-format octets
                                                                      path)
                      (:zip
                       (make-archive :zip
                                     (%tar-items (read-zip-entries octets))
                                     octets))
                      (:tar
                       (make-archive :tar
                                     (%tar-items (read-tar-entries octets))
                                     octets))
                      (:tar-gz (%open-gzip octets path t max-gzip-output))
                      (:gz (%open-gzip octets path nil max-gzip-output))
                      ((nil) nil))
                    :opened
                    nil)
       (archive-limit-exceeded (condition)
         (values nil :too-large (archive-limit-exceeded-limit condition)))
       (archive-error (condition)
         (values nil :malformed (archive-error-reason condition))))
   (case outcome
     (:too-large (aitools.kernel.domain:flow-continue :on-too-large value))
     (:malformed (aitools.kernel.domain:flow-continue :on-malformed value))
     (t
      (if archive
          (aitools.kernel.domain:flow-continue :on-archive archive)
          (aitools.kernel.domain:flow-continue :on-unsupported))))))

(aitools.kernel.domain:define-flow
 (archive-item-content/k archive item &key on-content on-too-large on-malformed)
 "ITEM's bytes, bounded by +ARCHIVE-MAX-ENTRY+ while decoding: calls
ON-CONTENT (octets), ON-TOO-LARGE (limit), or ON-MALFORMED (reason)."
 (declare (type function on-content on-too-large on-malformed))
 (multiple-value-bind (content outcome value)
     (handler-case (if (archive-item-entry item)
                       (values
                        (archive-entry-data (archive-data archive)
                                            (archive-item-entry item)
                                            :max-output
                                            +archive-max-entry+)
                        :content
                        nil)
                       (let ((data (archive-data archive)))
                         (if (> (length data) +archive-max-entry+)
                             (values nil :too-large +archive-max-entry+)
                             (values data :content nil))))
       (archive-limit-exceeded (condition)
         (values nil :too-large (archive-limit-exceeded-limit condition)))
       (archive-error (condition)
         (values nil :malformed (archive-error-reason condition))))
   (case outcome
     (:too-large (aitools.kernel.domain:flow-continue :on-too-large value))
     (:malformed (aitools.kernel.domain:flow-continue :on-malformed value))
     (t (aitools.kernel.domain:flow-continue :on-content content)))))

(defun archive-item-json (item)
  (json-object "path"
               (archive-item-name item)
               "kind"
               (string-downcase (symbol-name (archive-item-kind item)))
               "size"
               (archive-item-size item)
               "mode"
               (json-or-null
                (and (archive-item-mode item)
                     (format-file-mode (archive-item-mode item))))
               "mtime"
               (iso8601-from-unix (archive-item-mtime item))))

(defun find-archive-item (archive name)
  "The item named NAME (a trailing `/` ignored), or NIL."
  (let ((name (string-right-trim "/" name)))
    (find name (archive-items archive) :key #'archive-item-name :test #'string=)))

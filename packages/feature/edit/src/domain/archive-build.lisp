(in-package #:aitools.edit.domain)

(defun build-archive (format members &key name)
  "The bytes of an archive of FORMAT holding MEMBERS (text-domain
ARCHIVE-MEMBERs); :GZ takes exactly one file member and stores NAME."
  (ecase format
    (:zip (aitools.text.domain:write-zip members))
    (:tar (aitools.text.domain:write-tar members))
    (:tar-gz (aitools.text.domain:gzip-compress (aitools.text.domain:write-tar members)))
    (:gz (aitools.text.domain:gzip-compress (aitools.text.domain:archive-member-data (first members)) :name name))))

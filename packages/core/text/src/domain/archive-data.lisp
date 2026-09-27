;;;; packages/core/text/src/domain/archive-data.lisp
;;;;
;;;; Member content for `archive read` and `archive
;;;; extract`), dispatched on the entry's format.
(in-package #:aitools.text.domain)

(defun archive-entry-data (octets entry &key max-output)
  "The content bytes of ENTRY (from READ-ZIP-ENTRIES or READ-TAR-ENTRIES
over the same OCTETS). A zip entry is inflated as needed and checked
against its CRC-32. Signals ARCHIVE-LIMIT-EXCEEDED when the content exceeds
MAX-OUTPUT bytes, before producing it."
  (declare (type octets octets))
  (ecase (archive-entry-format entry)
    (:zip (%zip-entry-data octets entry max-output))
    (:tar
     (let ((start (archive-entry-data-offset entry))
           (size (archive-entry-size entry)))
       (when (and max-output (> size max-output))
         (error 'archive-limit-exceeded :limit max-output :reason "entry size"))
       (when (> (+ start size) (length octets)) (%archive-fail "tar entry data is truncated"))
       (subseq octets start (+ start size))))))

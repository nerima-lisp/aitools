;;;; packages/core/text/src/application/package.lisp
;;;;
;;;; The text context's public flow boundary: the TEXT-SOURCE port (bytes
;;;; of one file, read whole, by prefix, by range, or in chunks) and the
;;;; binary-before-read flow. Other contexts' application layers use this
;;;; package; the pure codecs stay in AITOOLS.TEXT.DOMAIN.
(in-package #:cl-user)

(defpackage #:aitools.text.application
  (:use #:cl)
  (:import-from #:aitools.text.domain
                #:+binary-sniff-length+
                #:binary-octets-p
                #:detect-text-layout)
  (:export
   ;; source.lisp
   #:text-source
   #:text-source-p
   #:make-text-source
   #:source-file-size
   #:source-read-prefix
   #:source-read-octets
   #:source-call-with-chunks
   #:source-read-sniffed
   #:source-read-range
   #:call-with-sniffed-octets/k
   #:call-with-text-file/k))

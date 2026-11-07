;;;;
;;;; The text context's public flows accept a boundary context directly and
;;;; classify text or binary bytes before reading the whole file. Other
;;;; contexts use the same boundary operations directly; pure codecs stay in
;;;; AITOOLS.TEXT.DOMAIN.
(in-package #:cl-user)

(defpackage #:aitools.text.application
  (:use #:cl)
  (:import-from #:aitools.text.domain
                #:+binary-sniff-length+
                #:binary-octets-p
                #:detect-text-layout)
  (:export #:call-with-sniffed-octets/k
           #:call-with-text-file/k))

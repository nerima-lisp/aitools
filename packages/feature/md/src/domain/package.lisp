(in-package #:cl-user)

(defpackage #:aitools.md.domain
            (:use #:cl)
            (:export #:make-md-heading
                     #:md-heading
                     #:md-heading-section
                     #:md-heading-heading
                     #:md-heading-level
                     #:md-heading-line
                     #:md-heading-start-char
                     #:md-heading-end-char
                     #:make-md-document
                     #:md-document
                     #:md-document-source
                     #:md-document-octets
                     #:md-document-headings
                     #:md-document-heading))

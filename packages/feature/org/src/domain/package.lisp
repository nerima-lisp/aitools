(in-package #:cl-user)

(defpackage #:aitools.org.domain
  (:use #:cl)
  (:export
   #:make-org-document
   #:org-document
   #:org-document-engine-value
   #:make-org-failure
   #:org-failure
   #:org-failure-p
   #:org-failure-kind
   #:org-failure-message
   #:org-failure-line
   #:org-failure-column
   #:org-failure-offset
   #:make-org-outline-entry
   #:org-outline-entry
   #:org-outline-entry-kind
   #:org-outline-entry-name
   #:org-outline-entry-level
   #:org-outline-entry-line
   #:make-org-heading-path-entry
   #:org-heading-path-entry-title
   #:org-heading-path-entry-index
   #:make-org-query-match
   #:org-query-match-line
   #:org-query-match-level
   #:org-query-match-title
   #:org-query-match-todo
   #:org-query-match-tags
   #:org-query-match-heading-path))

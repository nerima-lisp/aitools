;;;; Fixture: a domain file that only uses what its layer allows -- another
;;;; core context's domain (aitools.kernel.domain:sha256-hex), a pure kit
;;;; (json-kit:parse), its table data (aitools.data:*fixture*) and one
;;;; listed SBCL symbol (sb-ext:string-to-octets) -- plus prose that MENTIONS
;;;; other packages (host-kit:read-file, aitools.search.application:foo)
;;;; only inside a comment, a block comment, a string literal and after
;;;; character literals that would otherwise open a string or a comment, to
;;;; prove %STRIP-PROSE keeps those from being counted as real references.
(in-package #:aitools.fixture.domain)

(defun quote-char () #\")

(defparameter *doc*
  "host-kit:read-file and aitools.search.application:foo appear only in this string")

(defun semicolon-char () (list #\; (aitools.kernel.domain:sha256-hex #())))

#| host-kit:read-file in a block comment
   #| nested: uiop:getenv |#
   still a comment: process-kit:run |#

(defun good-fn ()
  (list (json-kit:parse "{}")
        aitools.data:*fixture*
        (sb-ext:string-to-octets "x" :external-format :utf-8)))

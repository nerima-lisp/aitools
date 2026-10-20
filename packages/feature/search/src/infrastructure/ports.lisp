;;;; packages/feature/search/src/infrastructure/ports.lisp
;;;;
;;;; The production SEARCH-PORTS. The workspace host, text source, and store
;;;; opener belong to other contexts' infrastructure, which this layer may
;;;; not name (docs/src/reference/architecture.md), so the composition root builds them and
;;;; passes them in; only standard input and the clock are adapted here.
(in-package #:aitools.search.infrastructure)

(defun %unix-now ()
  (aitools.kernel.domain:universal-time-to-unix-seconds (get-universal-time)))

(defun %read-bounded (stream limit)
  "Read STREAM to its end, at most LIMIT octets: one extra octet proves the
input is larger, so LIMIT bounds the read itself. Return (:OCTETS octets) or
(:TOO-LARGE)."
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (chunk (make-array 4096 :element-type '(unsigned-byte 8)))
        (eof-p nil)
        (too-large-p nil))
    (loop until (or eof-p too-large-p)
          do (let ((count (read-sequence chunk stream :end (min (length chunk) (- (1+ limit) (fill-pointer buffer))))))
               (if (zerop count)
                   (setf eof-p t)
                   (loop for index below count do (vector-push-extend (aref chunk index) buffer)))
               (when (> (fill-pointer buffer) limit)
                 (setf too-large-p t))))
    (if too-large-p
        (list :too-large)
        (list :octets (coerce buffer '(simple-array (unsigned-byte 8) (*)))))))

(defun read-stdin-octets (limit)
  "Read standard input as raw octets up to LIMIT.
Return (:OCTETS octets), (:TOO-LARGE), or (:FAILURE message)."
  (handler-case
      (let ((stream (sb-sys:make-fd-stream 0 :input t :element-type (quote (unsigned-byte 8))
                                           :buffering :full)))
        (%read-bounded stream limit))
    (error (condition) (list :failure (princ-to-string condition)))))

(defun make-production-search-ports (&key workspace-host text-source open-store &allow-other-keys)
  "The production SEARCH-PORTS from the composition root's WORKSPACE-HOST,
TEXT-SOURCE, and OPEN-STORE (real-root -> STORE). Performs no I/O."
  (aitools.search.application:make-search-ports
   :workspace-host workspace-host
   :text-source text-source
   :open-store open-store
   :unix-now #'%unix-now
   :read-stdin-octets #'read-stdin-octets))

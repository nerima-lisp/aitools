;;;; packages/feature/search/src/infrastructure/ports.lisp
;;;;
;;;; The production SEARCH-PORTS. The workspace host, text source, and store
;;;; opener belong to other contexts' infrastructure, which this layer may
;;;; not name (docs/src/reference/architecture.md), so the composition root builds them and
;;;; passes them in; only standard input and the clock are adapted here.
(in-package #:aitools.search.infrastructure)

(defun %unix-now ()
  (aitools.kernel.domain:universal-time-to-unix-seconds (get-universal-time)))

(defun %read-bounded (stream limit on-octets on-too-large)
  "Read STREAM to its end, at most LIMIT octets: one extra octet proves the
input is larger, so LIMIT bounds the read itself."
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (chunk (make-array 4096 :element-type '(unsigned-byte 8))))
    (loop for count = (read-sequence chunk stream :end (min (length chunk) (- (1+ limit) (fill-pointer buffer))))
          until (zerop count)
          do (loop for index below count do (vector-push-extend (aref chunk index) buffer))
             (when (> (fill-pointer buffer) limit)
               (return-from %read-bounded (funcall on-too-large))))
    (funcall on-octets (coerce buffer '(simple-array (unsigned-byte 8) (*))))))

(defun read-stdin-octets (limit &key on-octets on-too-large on-failure)
  "Standard input (file descriptor 0) as raw octets, at most LIMIT of them.
The continuation runs after the handler has returned, so its own errors are
not reported as read failures."
  (declare (type function on-octets on-too-large on-failure))
  (let ((outcome (handler-case
                     (let ((stream (sb-sys:make-fd-stream 0 :input t :element-type '(unsigned-byte 8)
                                                            :buffering :full)))
                       (%read-bounded stream limit
                                      (lambda (octets) (list :octets octets))
                                      (lambda () (list :too-large))))
                   (error (condition) (list :failure (princ-to-string condition))))))
    (ecase (first outcome)
      (:octets (funcall on-octets (second outcome)))
      (:too-large (funcall on-too-large))
      (:failure (funcall on-failure (second outcome))))))

(defun make-production-search-ports (&key workspace-host text-source open-store &allow-other-keys)
  "The production SEARCH-PORTS from the composition root's WORKSPACE-HOST,
TEXT-SOURCE, and OPEN-STORE (real-root -> STORE). Performs no I/O."
  (aitools.search.application:make-search-ports
   :workspace-host workspace-host
   :text-source text-source
   :open-store open-store
   :unix-now #'%unix-now
   :read-stdin-octets #'read-stdin-octets))

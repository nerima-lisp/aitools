;;;; packages/feature/util/src/infrastructure/ports.lisp
;;;;
;;;; Adapters from cl-boundary-kit ports (random-source, uuid-source, clock)
;;;; and the host file system to AITOOLS.UTIL.APPLICATION:UTIL-PORTS. The
;;;; composition root calls MAKE-PRODUCTION-UTIL-PORTS; unit tests call
;;;; MAKE-UTIL-PORTS-FROM-BOUNDARIES with cl-boundary-kit fakes, so both go
;;;; through the same adaptation code.
(in-package #:aitools.util.infrastructure)

(defun %unix-ms ()
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ (* seconds 1000) (floor microseconds 1000))))

(defun %read-bounded (stream limit on-octets on-too-large)
  "Read STREAM to its end, at most LIMIT octets: one extra octet proves the
input is larger, so the limit bounds the read itself rather than a result
already in memory."
  (let ((buffer (make-array (min (1+ limit) 65536) :element-type '(unsigned-byte 8)
                                                    :adjustable t :fill-pointer 0))
        (chunk (make-array 65536 :element-type '(unsigned-byte 8))))
    (loop for count = (read-sequence chunk stream :end (min (length chunk) (- (1+ limit) (fill-pointer buffer))))
          until (zerop count)
          do (loop for index below count do (vector-push-extend (aref chunk index) buffer))
             (when (> (fill-pointer buffer) limit)
               (return-from %read-bounded (funcall on-too-large))))
    (funcall on-octets (coerce buffer '(simple-array (unsigned-byte 8) (*))))))

(defun read-file-octets (path limit &key on-octets on-missing on-too-large on-failure)
  "The production READ-FILE-OCTETS port: PATH is taken literally (no
wildcard parsing), relative to the process's current directory."
  (declare (type function on-octets on-missing on-too-large on-failure))
  (let ((stream (handler-case (open (uiop:parse-native-namestring path) :element-type '(unsigned-byte 8)
                                                                       :if-does-not-exist nil)
                  (error (condition) (return-from read-file-octets (funcall on-failure (princ-to-string condition)))))))
    (if (null stream)
        (funcall on-missing)
        (let ((outcome (handler-case
                           (unwind-protect
                                (%read-bounded stream limit
                                               (lambda (octets) (list :octets octets))
                                               (lambda () (list :too-large)))
                             (close stream))
                         (error (condition) (list :failure (princ-to-string condition))))))
          (ecase (first outcome)
            (:octets (funcall on-octets (second outcome)))
            (:too-large (funcall on-too-large))
            (:failure (funcall on-failure (second outcome))))))))

(defun read-stdin-octets (limit &key on-octets on-too-large on-failure)
  "The production READ-STDIN-OCTETS port: reads file descriptor 0 as raw
octets, independent of the Lisp character external format."
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

(defun make-util-ports-from-boundaries (&key random-source uuid-source clock
                                          (read-file-octets #'read-file-octets)
                                          (read-stdin-octets #'read-stdin-octets)
                                          workspace-host open-store)
  "Adapt cl-boundary-kit RANDOM-SOURCE, UUID-SOURCE, and CLOCK (whose
CLOCK-NOW must return Unix milliseconds) plus the two input readers into a
UTIL-PORTS value. WORKSPACE-HOST and OPEN-STORE pass through unchanged for
`util decode --to`."
  (aitools.util.application:make-util-ports
   :random-octets (lambda (count) (cl-boundary-kit:random-source-bytes random-source count))
   :uuid-v4 (lambda () (cl-boundary-kit:uuid-generate uuid-source))
   :unix-ms (lambda () (cl-boundary-kit:clock-now clock))
   :read-file-octets read-file-octets
   :read-stdin-octets read-stdin-octets
   :workspace-host workspace-host
   :open-store open-store))

(defun make-production-util-ports (&key workspace-host open-store &allow-other-keys)
  "The production UTIL-PORTS: the OS random source, a cl-boundary-kit UUID
source whose version-4 generator draws from it, a millisecond wall clock,
and the composition root's WORKSPACE-HOST and OPEN-STORE. Performs no I/O
until a port is called."
  (let ((random-source (make-os-random-source)))
    (make-util-ports-from-boundaries
     :random-source random-source
     :uuid-source (cl-boundary-kit:make-uuid-source
                   :generate-fn (lambda ()
                                  (aitools.util.domain:uuid-v4-from-octets
                                   (cl-boundary-kit:random-source-bytes random-source 16))))
     :clock (cl-boundary-kit:make-clock :now-fn #'%unix-ms)
     :workspace-host workspace-host
     :open-store open-store)))

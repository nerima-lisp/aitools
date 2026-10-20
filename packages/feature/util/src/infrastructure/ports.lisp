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

(defun %read-bounded (stream limit)
  "Read at most LIMIT plus one octet to distinguish a complete input from an oversized one."
  (let ((buffer (make-array (min (1+ limit) 65536) :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0))
        (chunk (make-array 65536 :element-type '(unsigned-byte 8))))
    (loop for count = (read-sequence chunk stream
                                     :end (min (length chunk)
                                               (- (1+ limit) (fill-pointer buffer))))
          until (zerop count)
          do (loop for index below count
                   do (vector-push-extend (aref chunk index) buffer)))
    (if (> (fill-pointer buffer) limit)
        (values :too-large nil)
        (values :octets (coerce buffer '(simple-array (unsigned-byte 8) (*)))))))

(defun read-file-octets (path limit)
  "Read PATH literally, relative to the process's current directory."
  (let ((outcome
          (handler-case
              (let ((stream (open (uiop:parse-native-namestring path)
                                  :element-type '(unsigned-byte 8)
                                  :if-does-not-exist nil)))
                (if stream
                    (unwind-protect
                         (multiple-value-list (%read-bounded stream limit))
                      (close stream))
                    (list :missing nil)))
            (error (condition)
              (list :failure (princ-to-string condition))))))
    (values-list outcome)))

(defun read-stdin-octets (limit)
  "Read file descriptor 0 as raw octets, independent of the character external format."
  (let ((outcome
          (handler-case
              (multiple-value-list
               (%read-bounded
                (sb-sys:make-fd-stream 0 :input t :element-type '(unsigned-byte 8)
                                       :buffering :full)
                limit))
            (error (condition)
              (list :failure (princ-to-string condition))))))
    (values-list outcome)))

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

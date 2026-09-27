;;;; packages/feature/util/src/infrastructure/os-random.lisp
;;;;
;;;; The cryptographic random source `util` requires (random values come from
;;;; the OS cryptographic random source). cl-boundary-kit's own MAKE-RANDOM-SOURCE is
;;;; backed by CL:RANDOM (Mersenne Twister) and documents itself as not
;;;; cryptographic, so this class joins the same port by specializing the
;;;; exported CL-BOUNDARY-KIT:RANDOM-SOURCE-RANDOM generic on octets read
;;;; from /dev/urandom (present on both Linux and Darwin, never blocks after
;;;; boot). Everything derived from that generic -- RANDOM-SOURCE-BYTES,
;;;; RANDOM-SOURCE-ELEMENT -- then draws from the OS source too.
;;;;
;;;; The device is opened on first use, not at construction, so building the
;;;; production ports at startup performs no I/O.
(in-package #:aitools.util.infrastructure)

(defparameter +urandom-path+ "/dev/urandom")

(defclass os-random-source ()
  ((stream :initform nil :accessor %os-random-stream)
   (buffer :initform (make-array 256 :element-type '(unsigned-byte 8)) :reader %os-random-buffer)
   (position :initform 256 :accessor %os-random-position)))

(defun make-os-random-source ()
  (make-instance 'os-random-source))

(defun %next-octet (source)
  (let ((buffer (%os-random-buffer source)))
    (when (= (%os-random-position source) (length buffer))
      (let ((stream (or (%os-random-stream source)
                        (setf (%os-random-stream source)
                              (open +urandom-path+ :element-type '(unsigned-byte 8))))))
        (unless (= (read-sequence buffer stream) (length buffer))
          (error "short read from ~A" +urandom-path+))
        (setf (%os-random-position source) 0)))
    (prog1 (aref buffer (%os-random-position source))
      (incf (%os-random-position source)))))

(defmethod cl-boundary-kit:random-source-random ((source os-random-source) limit)
  "A uniform integer in [0, LIMIT) by rejection sampling: draw just enough
octets to cover LIMIT-1, mask to its bit length, retry when out of range."
  (check-type limit (integer 1))
  (let* ((bits (integer-length (1- limit)))
         (octet-count (ceiling bits 8)))
    (loop for value = (let ((value 0))
                        (dotimes (index octet-count (ldb (byte bits 0) value))
                          (setf value (logior (ash value 8) (%next-octet source)))))
          when (< value limit) return value)))

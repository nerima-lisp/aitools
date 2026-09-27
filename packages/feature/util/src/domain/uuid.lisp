;;;; packages/feature/util/src/domain/uuid.lisp
;;;;
;;;; `util uuid` layouts (RFC 9562). Randomness and time are arguments, so
;;;; the bit layout and the v7 ordering rule are testable without a clock or
;;;; an entropy source.
;;;;
;;;; v7 monotonicity follows RFC 9562 6.2 method 1: the 12-bit `rand_a`
;;;; field is a counter. The first UUID in a millisecond seeds it with 11
;;;; random bits (the top bit clear leaves room to count), later UUIDs in the
;;;; same millisecond increment it, and a counter overflow borrows the next
;;;; millisecond. A wall clock that steps backwards reuses the last
;;;; timestamp and keeps counting, so values from one state never decrease.
(in-package #:aitools.util.domain)

(defconstant +uuid-v7-random-octets+ 10
  "Octets NEXT-UUID-V7 consumes per call: 2 seed the counter, 8 fill rand_b.")

(defun uuid-octets->string (octets)
  "The 8-4-4-4-12 lowercase hex form of the 16 OCTETS."
  (let ((hex (octets->hex octets)))
    (format nil "~A-~A-~A-~A-~A"
            (subseq hex 0 8) (subseq hex 8 12) (subseq hex 12 16) (subseq hex 16 20) (subseq hex 20 32))))

(defun %set-version-and-variant (octets version)
  (setf (aref octets 6) (logior (ash version 4) (logand (aref octets 6) #x0f))
        (aref octets 8) (logior #x80 (logand (aref octets 8) #x3f)))
  octets)

(defun uuid-v4-from-octets (random-octets)
  "A version-4 UUID string from 16 RANDOM-OCTETS (copied, not modified)."
  (assert (= (length random-octets) 16))
  (uuid-octets->string
   (%set-version-and-variant (replace (make-array 16 :element-type '(unsigned-byte 8)) random-octets) 4)))

(defstruct (uuid-v7-state (:constructor make-uuid-v7-state ()) (:copier nil))
  (last-ms -1 :type integer)
  (counter 0 :type (integer 0 #xfff)))

(defun next-uuid-v7 (state unix-ms random-octets)
  "Return the next version-7 UUID string for STATE, updating STATE. UNIX-MS
is the current Unix time in milliseconds; RANDOM-OCTETS holds
+UUID-V7-RANDOM-OCTETS+ fresh random octets."
  (assert (= (length random-octets) +uuid-v7-random-octets+))
  (let ((seed (logand (logior (ash (aref random-octets 0) 8) (aref random-octets 1)) #x7ff))
        (last-ms (uuid-v7-state-last-ms state)))
    (multiple-value-bind (ms counter)
        (cond ((> unix-ms last-ms) (values unix-ms seed))
              ((< (uuid-v7-state-counter state) #xfff)
               (values last-ms (1+ (uuid-v7-state-counter state))))
              (t (values (1+ last-ms) seed)))
      (setf (uuid-v7-state-last-ms state) ms
            (uuid-v7-state-counter state) counter)
      (let ((octets (make-array 16 :element-type '(unsigned-byte 8))))
        (loop for index from 0 below 6
              do (setf (aref octets index) (ldb (byte 8 (* 8 (- 5 index))) ms)))
        (setf (aref octets 6) (ash counter -8)
              (aref octets 7) (logand counter #xff))
        (replace octets random-octets :start1 8 :start2 2)
        (uuid-octets->string (%set-version-and-variant octets 7))))))

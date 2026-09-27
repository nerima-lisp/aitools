;;;; packages/core/kernel/src/domain/digest.lisp
;;;;
;;;; SHA-256 and SHA-1 (FIPS 180-4) implemented here because no kit in the org
;;;; provides them.
;;;; MD5 delegates to SB-MD5, confirmed present and requirable at SBCL 2.6.0.
;;;;
;;;; CONTENT-HASH is the single hash used everywhere else in the system for
;;;; change detection (`hash_before`/`hash_after`, journal
;;;; entries, `--expect-hash`): it is SHA-256, so it doubles as `info --digest
;;;; sha256` with no second algorithm to maintain. SHA-1 and MD5 exist only to
;;;; answer `info --digest sha1|md5` against external tools' output.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-md5))

(in-package #:aitools.kernel.domain)

(deftype octet-vector () '(simple-array (unsigned-byte 8) (*)))

(declaim (inline rotr32))
(defun rotr32 (x n)
  (declare (type (unsigned-byte 32) x) (type (integer 0 31) n))
  (logand #xFFFFFFFF (logior (ash x (- n)) (ash x (- 32 n)))))

(declaim (inline rotl32))
(defun rotl32 (x n)
  (declare (type (unsigned-byte 32) x) (type (integer 0 31) n))
  (logand #xFFFFFFFF (logior (ash x n) (ash x (- n 32)))))

(defmacro u32+ (&rest values)
  "Modular 32-bit addition of two or more forms. Expands to nested
two-argument masked adds so each step is (unsigned-byte 32) arithmetic that
SBCL lowers to a machine add, with no &rest list allocated per call."
  (reduce (lambda (a b) `(logand #xFFFFFFFF (+ ,a ,b))) values))

(defun %pad-tail (bytes)
  "FIPS 180-4 padding shared by SHA-256 and SHA-1, applied only to the trailing
region. Returns two values: a fresh OCTET-VECTOR of 64 or 128 bytes holding the
final partial block plus the appended 1 bit, zero fill up to 448 mod 512, and
the original bit length as a big-endian 64-bit integer; and TAIL-START, the
offset in BYTES (a multiple of 64) where that region begins. Every 64-byte
block below TAIL-START is hashed directly from BYTES, so the whole message is
never copied."
  (declare (type octet-vector bytes))
  (let* ((len (length bytes))
         (bit-length (* 8 len))
         (rem (mod len 64))
         (tail-start (- len rem))
         (tail-length (if (< rem 56) 64 128))
         (tail (make-array tail-length :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace tail bytes :start2 tail-start)
    (setf (aref tail rem) #x80)
    (loop for i from 0 below 8
          do (setf (aref tail (- tail-length 1 i))
                   (ldb (byte 8 (* 8 i)) bit-length)))
    (values tail tail-start)))

(defun %hex-string (words word-bytes)
  "Render WORDS (a vector of unsigned integers, each WORD-BYTES wide) as a
lowercase hex digest string, most significant byte first."
  (with-output-to-string (out)
    (loop for word across words
          do (loop for i from (1- word-bytes) downto 0
                   do (format out "~(~2,'0X~)" (ldb (byte 8 (* 8 i)) word))))))

(defmacro %hash-padded-message (process bytes)
  "Feed BYTES to PROCESS one 64-byte block at a time -- every whole block of
the message, then the FIPS 180-4 padding %PAD-TAIL appends -- the driver
SHA-256 and SHA-1 share. PROCESS names a local function of (SOURCE OFFSET)
that hashes the 64-byte block of SOURCE beginning at OFFSET."
  (let ((tail (gensym "TAIL"))
        (tail-start (gensym "TAIL-START"))
        (chunk (gensym "CHUNK")))
    `(multiple-value-bind (,tail ,tail-start) (%pad-tail ,bytes)
       (declare (type octet-vector ,tail) (type fixnum ,tail-start))
       (loop for ,chunk of-type fixnum from 0 below ,tail-start by 64
             do (,process ,bytes ,chunk))
       (loop for ,chunk of-type fixnum from 0 below (length ,tail) by 64
             do (,process ,tail ,chunk)))))

;;; ------------------------------------------------------------------ SHA-256

(declaim (type (simple-array (unsigned-byte 32) (64)) *sha256-round-constants*))
(defparameter *sha256-round-constants*
  (make-array 64 :element-type '(unsigned-byte 32) :initial-contents
    '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
      #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
      #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
      #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
      #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
      #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
      #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
      #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)))

(declaim (type (simple-array (unsigned-byte 32) (8)) *sha256-initial-state*))
(defparameter *sha256-initial-state*
  (make-array 8 :element-type '(unsigned-byte 32) :initial-contents
    '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))

(defun sha256-hex (bytes)
  "Return the lowercase hex SHA-256 digest of BYTES (an OCTET-VECTOR)."
  (declare (type octet-vector bytes)
           (optimize (speed 3) (safety 1) (debug 0)))
  (let ((h (make-array 8 :element-type '(unsigned-byte 32)))
        (w (make-array 64 :element-type '(unsigned-byte 32))))
    (declare (type (simple-array (unsigned-byte 32) (8)) h)
             (type (simple-array (unsigned-byte 32) (64)) w))
    (replace h *sha256-initial-state*)
    (labels ((process (src off)
               (declare (type octet-vector src) (type fixnum off))
               (loop for i of-type fixnum from 0 below 16
                     do (setf (aref w i)
                              (logior (ash (aref src (+ off (* i 4))) 24)
                                      (ash (aref src (+ off (* i 4) 1)) 16)
                                      (ash (aref src (+ off (* i 4) 2)) 8)
                                      (aref src (+ off (* i 4) 3)))))
               (loop for i of-type fixnum from 16 below 64
                     do (let* ((w15 (aref w (- i 15))) (w2 (aref w (- i 2)))
                               (s0 (logxor (rotr32 w15 7) (rotr32 w15 18) (ash w15 -3)))
                               (s1 (logxor (rotr32 w2 17) (rotr32 w2 19) (ash w2 -10))))
                          (declare (type (unsigned-byte 32) w15 w2 s0 s1))
                          (setf (aref w i) (u32+ (aref w (- i 16)) s0 (aref w (- i 7)) s1))))
               (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
                     (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
                 (declare (type (unsigned-byte 32) a b c d e f g hh))
                 (loop for i of-type fixnum from 0 below 64
                       do (let* ((s1 (logxor (rotr32 e 6) (rotr32 e 11) (rotr32 e 25)))
                                 (ch (logxor (logand e f) (logand (logxor e #xFFFFFFFF) g)))
                                 (temp1 (u32+ hh s1 ch (aref *sha256-round-constants* i) (aref w i)))
                                 (s0 (logxor (rotr32 a 2) (rotr32 a 13) (rotr32 a 22)))
                                 (maj (logxor (logand a b) (logand a c) (logand b c)))
                                 (temp2 (u32+ s0 maj)))
                            (declare (type (unsigned-byte 32) s1 ch temp1 s0 maj temp2))
                            (setf hh g g f f e e (u32+ d temp1) d c c b b a a (u32+ temp1 temp2))))
                 (setf (aref h 0) (u32+ (aref h 0) a) (aref h 1) (u32+ (aref h 1) b)
                       (aref h 2) (u32+ (aref h 2) c) (aref h 3) (u32+ (aref h 3) d)
                       (aref h 4) (u32+ (aref h 4) e) (aref h 5) (u32+ (aref h 5) f)
                       (aref h 6) (u32+ (aref h 6) g) (aref h 7) (u32+ (aref h 7) hh)))))
      (%hash-padded-message process bytes))
    (%hex-string h 4)))

;;; -------------------------------------------------------------------- SHA-1

(defun sha1-hex (bytes)
  "Return the lowercase hex SHA-1 digest of BYTES (an OCTET-VECTOR)."
  (declare (type octet-vector bytes)
           (optimize (speed 3) (safety 1) (debug 0)))
  (let ((h (make-array 5 :element-type '(unsigned-byte 32) :initial-contents
             '(#x67452301 #xEFCDAB89 #x98BADCFE #x10325476 #xC3D2E1F0)))
        (w (make-array 80 :element-type '(unsigned-byte 32))))
    (declare (type (simple-array (unsigned-byte 32) (5)) h)
             (type (simple-array (unsigned-byte 32) (80)) w))
    (labels ((process (src off)
               (declare (type octet-vector src) (type fixnum off))
               (loop for i of-type fixnum from 0 below 16
                     do (setf (aref w i)
                              (logior (ash (aref src (+ off (* i 4))) 24)
                                      (ash (aref src (+ off (* i 4) 1)) 16)
                                      (ash (aref src (+ off (* i 4) 2)) 8)
                                      (aref src (+ off (* i 4) 3)))))
               (loop for i of-type fixnum from 16 below 80
                     do (setf (aref w i)
                              (rotl32 (logxor (aref w (- i 3)) (aref w (- i 8))
                                              (aref w (- i 14)) (aref w (- i 16)))
                                      1)))
               (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3)) (e (aref h 4)))
                 (declare (type (unsigned-byte 32) a b c d e))
                 (loop for i of-type fixnum from 0 below 80
                       do (multiple-value-bind (f k)
                              (cond ((< i 20) (values (logior (logand b c)
                                                              (logand (logxor b #xFFFFFFFF) d))
                                                      #x5A827999))
                                    ((< i 40) (values (logxor b c d) #x6ED9EBA1))
                                    ((< i 60) (values (logior (logand b c) (logand b d) (logand c d))
                                                      #x8F1BBCDC))
                                    (t (values (logxor b c d) #xCA62C1D6)))
                            (declare (type (unsigned-byte 32) f k))
                            (let ((temp (u32+ (rotl32 a 5) f e k (aref w i))))
                              (declare (type (unsigned-byte 32) temp))
                              (setf e d d c c (rotl32 b 30) b a a temp))))
                 (setf (aref h 0) (u32+ (aref h 0) a) (aref h 1) (u32+ (aref h 1) b)
                       (aref h 2) (u32+ (aref h 2) c) (aref h 3) (u32+ (aref h 3) d)
                       (aref h 4) (u32+ (aref h 4) e)))))
      (%hash-padded-message process bytes))
    (%hex-string h 4)))

;;; ---------------------------------------------------------------------- MD5

(defun md5-hex (bytes)
  "Return the lowercase hex MD5 digest of BYTES via SB-MD5, confirmed
requirable and exporting MD5SUM-SEQUENCE at SBCL 2.6.0."
  (declare (type octet-vector bytes))
  (string-downcase
   (with-output-to-string (out)
     (loop for byte across (sb-md5:md5sum-sequence bytes)
           do (format out "~2,'0X" byte)))))

(defun content-hash (bytes)
  "The canonical change-detection hash used throughout aitools: SHA-256 hex.
See the file header for why this is the one hash rather than a separate fast
digest."
  (sha256-hex bytes))

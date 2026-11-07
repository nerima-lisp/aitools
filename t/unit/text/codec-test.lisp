;;;;
;;;; CRC-32 and raw DEFLATE checks: the CRC catalogue vector, RFC 1951 stored
;;;; and fixed blocks, and DEFLATE output from zlib 1.2.12.
(in-package #:aitools.text.test)
;;;; CRC-32 and raw DEFLATE. Known vectors: the CRC-32 check value from
;;;; the CRC catalogue ("123456789" -> CBF43926), RFC 1951 stored block, and
;;;; streams produced by zlib 1.2.12 (Python's zlib.compressobj(9, DEFLATED,
;;;; -15). The dynamic-block vector's input is generated
;;;; below by the same formula used to produce it.
(describe "aitools.text.domain CRC-32"
  (it "matches the catalogue check value and continues incrementally"
    (expect (crc32 (string-bytes "123456789")) :to-be #xCBF43926)
    (expect (crc32 (octets)) :to-be 0)
    (expect (crc32 (string-bytes "6789") :crc (crc32 (string-bytes "12345"))) :to-be #xCBF43926)))

(describe "aitools.text.domain inflate"
  (it "decodes a stored block, a fixed block, and a dynamic block"
    (expect (inflate (hex-octets "010500faff68656c6c6f")) :to-equalp (string-bytes "hello"))
    (expect (inflate (hex-octets "cb48cdc9c90700")) :to-equalp (string-bytes "hello"))
    (expect (inflate (hex-octets *dynamic-vector*)) :to-equalp (dynamic-vector-input)))

  (it "returns the index after the final block"
    (expect (nth-value 1 (inflate (join-octets (hex-octets "cb48cdc9c90700") (octets 1 2 3)))) :to-be 7))

  (it "rejects malformed streams with ARCHIVE-ERROR"
    (signals archive-error (inflate (octets #x07)))
    (signals archive-error (inflate (hex-octets "010500faf068656c6c6f")))
    (signals archive-error (inflate (hex-octets "cb48cdc9")))
    ;; literal 'a' then a match at distance 5; zlib 1.2.12 reports "invalid distance too far back"
    (signals archive-error (inflate (hex-octets "4b041200"))))

  (it "stops at MAX-OUTPUT instead of expanding a bomb"
    (let ((bomb (deflate (make-array 100000 :element-type '(unsigned-byte 8) :initial-element 0))))
      (expect (< (length bomb) 1000) :to-be-truthy)
      (signals archive-limit-exceeded (inflate bomb :max-output 4096))
      (expect (length (inflate bomb :max-output 100000)) :to-be 100000)))

  (it "returns a tar-block prefix and no consumed index when TRUNCATE-AT is set"
    (let* ((source (coerce (loop for index below 100000 collect (mod index 251))
                           '(vector (unsigned-byte 8))))
           (bomb (deflate source)))
      (multiple-value-bind (prefix consumed-index)
          (inflate bomb :truncate-at aitools.text.domain::+tar-block+)
        (expect (equalp prefix (subseq source 0 aitools.text.domain::+tar-block+)) :to-be-truthy)
        (expect consumed-index :to-be-falsy))))

  (it "rejects every truncation and checks bit-flipped streams"
    (let ((valid (hex-octets *dynamic-vector*)))
      (expect (loop for end from 0 below (length valid)
                    always
                      (eq (handler-case
                              (progn (inflate (subseq valid 0 end)) :returned)
                            (archive-error () :archive-error)
                            (error () :other))
                          :archive-error))
              :to-be-truthy)
      (expect (loop for i from 0 below (length valid)
                    always (let ((copy (copy-seq valid)))
                             (setf (aref copy i) (logxor (aref copy i) (ash 1 (mod i 8))))
                             (only-archive-errors-p (lambda (bytes) (inflate bytes :max-output 10000)) copy)))
              :to-be-truthy))))

(describe "aitools.text.domain deflate"
  (it "round-trips through inflate for varied inputs"
    (dolist (input (list (octets)
                         (octets 7)
                         (string-bytes "abcabcabcabcabcabc")
                         (dynamic-vector-input)
                         (pseudo-random-octets 70000 42)
                         (let ((all (make-array 1024 :element-type '(unsigned-byte 8))))
                           (dotimes (i 1024 all) (setf (aref all i) (mod i 256))))))
      (expect (inflate (deflate input)) :to-equalp input)))
  (it "compresses and decodes nonzero input and stream slices"
    (let* ((input (octets #xAA 1 2 3 #xBB))
           (compressed (deflate input :start 1 :end 4))
           (prefix (octets #xCC #xDD))
           (stream (join-octets prefix compressed (octets #xEE)))
           (end (+ (length prefix) (length compressed))))
      (expect (inflate stream :start (length prefix) :end end)
              :to-equalp (octets 1 2 3))
      (expect (nth-value 1 (inflate stream :start (length prefix) :end end))
              :to-be end)
      (signals archive-error
        (inflate stream :start (length prefix) :end (1- end)))))
  (it-property "round-trips generated octet vectors through raw DEFLATE"
    ((input (gen-map (lambda (bytes)
                       (map '(simple-array (unsigned-byte 8) (*)) #'identity bytes))
                     (gen-vector (gen-integer :min 0 :max 255)
                                 :min-length 0
                                 :max-length 4096))))
    (expect (equalp (inflate (deflate input)) input) :to-be t))

  (it "compresses repetitive input"
    (expect (< (length (deflate (dynamic-vector-input))) 600) :to-be-truthy))
  (it-each (("empty input" 0)
            ("one byte" 1)
            ("32 KiB minus one" 32767)
            ("32 KiB" 32768)
            ("32 KiB plus one" 32769)
            ("65,535 bytes" 65535)
            ("65,536 bytes" 65536)
            ("1 MiB" 1048576))
    "round-trips ~A bytes through raw DEFLATE"
    (description size)
    (declare (ignore description))
    (let ((input (pseudo-random-octets size 42)))
      (expect (equalp (inflate (deflate input)) input) :to-be t))))


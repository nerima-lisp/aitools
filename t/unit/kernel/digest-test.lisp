;;;; t/unit/kernel/digest-test.lisp
;;;;
;;;; Standard test vectors from FIPS 180-4 (SHA-256/SHA-1) and RFC 1321 (MD5).
(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain digest"
  (it "computes SHA-256 of \"abc\""
    (expect (sha256-hex (string-bytes "abc"))
            :to-equal "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))

  (it "computes SHA-256 of the empty string"
    (expect (sha256-hex (string-bytes ""))
            :to-equal "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))

  (it "computes SHA-1 of \"abc\""
    (expect (sha1-hex (string-bytes "abc")) :to-equal "a9993e364706816aba3e25717850c26c9cd0d89d"))

  (it "computes SHA-1 of the empty string"
    (expect (sha1-hex (string-bytes "")) :to-equal "da39a3ee5e6b4b0d3255bfef95601890afd80709"))

  (it "computes MD5 of \"abc\""
    (expect (md5-hex (string-bytes "abc")) :to-equal "900150983cd24fb0d6963f7d28e17f72"))

  (it "computes MD5 of the empty string"
    (expect (md5-hex (string-bytes ""))
            :to-equal "d41d8cd98f00b204e9800998ecf8427e"))

  (it "uses SHA-256 as the canonical content-hash"
    (let ((bytes (string-bytes "hello")))
      (expect (content-hash bytes)
              :to-equal "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
      (expect (content-hash bytes) :to-equal (sha256-hex bytes))))

  (it "gives identical content the same hash"
    (let ((bytes (string-bytes "same")))
      (expect (content-hash bytes)
              :to-equal "0967115f2813a3541eaef77de9d9d5773f1c0c04314b0bbfe4ff3b3b1c55b5d5")
      (expect (content-hash bytes)
              :to-equal (content-hash (string-bytes "same")))))

  (it "gives different content different hashes"
    (let ((a (string-bytes "a"))
          (b (string-bytes "b")))
      (expect (content-hash a)
              :to-equal "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb")
      (expect (content-hash b)
              :to-equal "3e23e8160039594a33894f6564e1b1348bbd7a0088d42c4acb73eeaed59c009d"))))

;; Lengths either side of the 55/56-byte padding boundary and the 64-byte
;; block boundary, where a padding bug would show. Expected digests are from
;; `shasum -a 256`, `shasum -a 1`, and `md5` over N ASCII `a` bytes.
(describe "aitools.kernel.domain digest block boundaries"
  (it-each ((55 "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318"
                "c1c8bbdc22796e28c0e15163d20899b65621d65a" "ef1772b6dff9a122358552954ad0df65")
            (56 "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a"
                "c2db330f6083854c99d4b5bfb6e8f29f201be699" "3b0c8ac703f828b04c6c197006d17218")
            (63 "7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34"
                "03f09f5b158a7a8cdad920bddc29b81c18a551f5" "b06521f39153d618550606be297466d5")
            (64 "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"
                "0098ba824b5c16427bd7a1122a5a442a25ec644d" "014842d480b571495a4a0363793f7367")
            (65 "635361c48bb9eab14198e76ea8ab7f1a41685d6ad62aa9146d301d4f17eb0ae0"
                "11655326c708d70319be2610e8a57d9a5b959d3b" "c743a45e0d2e6a95cb859adae0248435")
            (119 "31eba51c313a5c08226adf18d4a359cfdfd8d2e816b13f4af952f7ea6584dcfb"
                 "ee971065aaa017e0632a8ca6c77bb3bf8b1dfc56" "8a7bd0732ed6a28ce75f6dabc90e1613")
            (120 "2f3d335432c70b580af0e8e1b3674a7c020d683aa5f73aaaedfdc55af904c21c"
                 "f34c1488385346a55709ba056ddd08280dd4c6d6" "5f61c0ccad4cac44c75ff505e1f1e537"))
      "hashes ~D bytes"
      (length sha256 sha1 md5)
    (let ((bytes (string-bytes (make-string length :initial-element #\a))))
      (expect (sha256-hex bytes) :to-equal sha256)
      (expect (sha1-hex bytes) :to-equal sha1)
      (expect (md5-hex bytes) :to-equal md5))))
(describe "aitools.kernel.domain digest large input"
  (it "matches fixed hashes for 1 MiB of repeating bytes"
    (let ((bytes (make-array 1048576 :element-type (quote (unsigned-byte 8)))))
      (loop for index below (length bytes)
            do (setf (aref bytes index) (mod index 256)))
      (expect (md5-hex bytes)
              :to-equal "c35cc7d8d91728a0cb052831bc4ef372")
      (expect (sha1-hex bytes)
              :to-equal "ecfc8e86fdd83811f9cc9bf500993b63069923be")
      (expect (sha256-hex bytes)
              :to-equal "fbbab289f7f94b25736c58be46a994c441fd02552cc6022352e3d86d2fab7c83")
      (expect (content-hash bytes)
              :to-equal "fbbab289f7f94b25736c58be46a994c441fd02552cc6022352e3d86d2fab7c83"))))

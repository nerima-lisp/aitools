(in-package #:aitools.kernel.domain)

(deftype octet-vector ()
  '(simple-array (unsigned-byte 8) (*)))

(defun sha256-hex (bytes)
  "Return the lowercase hex SHA-256 digest of BYTES (an OCTET-VECTOR)."
  (declare (type octet-vector bytes))
  (crypto-kit:digest-hex :sha256 bytes))

;;; -------------------------------------------------------------------- SHA-1
(defun sha1-hex (bytes)
  "Return the lowercase hex SHA-1 digest of BYTES for `info --digest sha1` compatibility."
  (declare (type octet-vector bytes))
  (crypto-kit:digest-hex :sha1 bytes))

;;; ---------------------------------------------------------------------- MD5
(defun md5-hex (bytes)
  "Return the lowercase hex MD5 digest of BYTES for `info --digest md5` compatibility."
  (declare (type octet-vector bytes))
  (crypto-kit:digest-hex :md5 bytes))

(defun content-hash (bytes)
  "Canonical SHA-256 change-detection hash for hash_before, hash_after, journal entries, and --expect-hash."
  (declare (type octet-vector bytes))
  (sha256-hex bytes))

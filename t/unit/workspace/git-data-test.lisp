;;;; t/unit/workspace/git-data-test.lisp
;;;;
;;;; git config syntax (git-config(1) "CONFIGURATION FILE") and index
;;;; format (gitformat-index(5)) readers. Index bytes are built here from the
;;;; format description; the parity integration test reads real git indexes.
(in-package #:aitools.workspace.test)

(defun index-varint (value)
  "git's offset varint encoding of VALUE (varint.c encode_varint)."
  (let ((octets (list (logand value 127))))
    (loop while (plusp (setf value (ash value -7)))
          do (decf value)
             (push (logior 128 (logand value 127)) octets))
    (coerce octets 'vector)))

(defun index-octets (paths &key (version 2) extended (hash-size 20))
  "A git index holding PATHS as regular-file entries, with HASH-SIZE-octet
object ids. EXTENDED (version 3) sets each entry's extended flag and writes
the extra flag word."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (previous ""))
    (flet ((u32 (value) (loop for shift from 24 downto 0 by 8 do (vector-push-extend (ldb (byte 8 shift) value) out)))
           (u16 (value) (vector-push-extend (ldb (byte 8 8) value) out) (vector-push-extend (ldb (byte 8 0) value) out))
           (bytes (octets) (loop for byte across octets do (vector-push-extend byte out))))
      (bytes (string-bytes "DIRC"))
      (u32 version)
      (u32 (length paths))
      (dolist (path paths)
        (let ((start (fill-pointer out))
              (name (string-bytes path)))
          (loop repeat 24 do (vector-push-extend 0 out))
          (u32 #o100644)
          (loop repeat 12 do (vector-push-extend 0 out))
          (loop repeat hash-size do (vector-push-extend 0 out))
          (u16 (logior (min #xFFF (length name)) (if extended #x4000 0)))
          (when extended (u16 0))
          (if (= version 4)
              (let ((common (or (mismatch previous path) (length previous))))
                (bytes (index-varint (- (length previous) common)))
                (bytes (string-bytes (subseq path common)))
                (vector-push-extend 0 out))
              (progn
                (bytes name)
                (loop do (vector-push-extend 0 out)
                      until (zerop (mod (- (fill-pointer out) start) 8)))))
          (setf previous path)))
      (coerce out '(simple-array (unsigned-byte 8) (*))))))

(describe "aitools.workspace.domain git config"
  (it "reads sections, subsections, and folds names"
    (let ((entries (parse-git-config (format nil "[Core]~%  ExcludesFile = ~~/ignore~%[remote \"Origin\"]~%url = x~%[a.B]~%k = v~%"))))
      (expect (git-config-value entries "core.excludesfile") :to-equal "~/ignore")
      (expect (git-config-value entries "remote.Origin.url") :to-equal "x")
      (expect (git-config-value entries "a.b.k") :to-equal "v")))

  (it "handles quotes, escapes, comments, continuations, and bare keys"
    (let ((entries (parse-git-config
                    (format nil "[core]~%a = \"x ; y\" # c~%b = one\\~%two~%c = q\\\"t\\\\~%bare~%d = spaced   value   ; x~%"))))
      (expect (git-config-value entries "core.a") :to-equal "x ; y")
      (expect (git-config-value entries "core.b") :to-equal "onetwo")
      (expect (git-config-value entries "core.c") :to-equal "q\"t\\")
      (expect (git-config-value entries "core.bare") :to-be t)
      (expect (git-config-value entries "core.d") :to-equal "spaced   value")))

  (it "lets the last value win and reads booleans"
    (let ((entries (parse-git-config (format nil "[core]~%ignorecase = false~%[core]~%ignoreCase = yes~%"))))
      (expect (git-config-boolean (git-config-value entries "core.ignorecase")) :to-be-truthy)
      (expect (git-config-boolean "0") :to-be-falsy)
      (expect (git-config-boolean t) :to-be-truthy)))

  (it "expands ~/ against HOME"
    (expect (expand-config-path "~/x" "/home/u") :to-equal "/home/u/x")
    (expect (expand-config-path "/abs" "/home/u") :to-equal "/abs")))

(describe "aitools.workspace.domain git index"
  (it "reads version 2 entry names, sorted and deduplicated"
    (expect (coerce (parse-git-index-paths (index-octets '("b.txt" "a/x.lisp" "a/x.lisp"))) 'list)
            :to-equal '("a/x.lisp" "b.txt")))

  (it "reads version 4 prefix-compressed names"
    (expect (coerce (parse-git-index-paths (index-octets '("dir/a" "dir/b" "dir/sub/c") :version 4)) 'list)
            :to-equal '("dir/a" "dir/b" "dir/sub/c")))

  (it "answers membership and prefix queries"
    (let ((paths (parse-git-index-paths (index-octets '("a/b/c" "d")))))
      (expect (sorted-paths-contains-p paths "d") :to-be-truthy)
      (expect (sorted-paths-contains-p paths "a/b") :to-be-falsy)
      (expect (sorted-paths-have-prefix-p paths "a/") :to-be-truthy)
      (expect (sorted-paths-have-prefix-p paths "a/c") :to-be-falsy)))

  (it "signals GIT-INDEX-ERROR on malformed input instead of misreading it"
    (signals git-index-error (parse-git-index-paths (string-bytes "NOPE00000000")))
    (let ((valid (index-octets '("abc"))))
      (signals git-index-error (parse-git-index-paths (subseq valid 0 50))))))

(defun index-prefix (paths &key (version 4))
  "The first 74 octets of an index of PATHS: header and one entry's fixed
fields, up to where the first entry's name begins."
  (subseq (index-octets paths :version version) 0 74))

(defun index-with (prefix &rest tails)
  (coerce (concatenate 'vector prefix (apply #'concatenate 'vector tails)) '(simple-array (unsigned-byte 8) (*))))

(defun index-failure (octets)
  (handler-case (progn (parse-git-index-paths octets) :parsed)
    (git-index-error (condition) (princ-to-string condition))))

(describe "aitools.workspace.domain git index edge cases"
  (it "reads a version 4 name whose prefix strip takes a multi-byte varint"
    (let ((long (make-string 300 :initial-element #\a)))
      (expect (coerce (index-varint 300) 'list) :to-equal '(129 44))
      (expect (coerce (parse-git-index-paths (index-octets (list long "b") :version 4)) 'list)
              :to-equal (list long "b"))))

  (it "skips the extra flag word of a version 3 extended entry"
    (expect (coerce (parse-git-index-paths (index-octets '("x/one" "y") :version 3 :extended t)) 'list)
            :to-equal '("x/one" "y")))

  (it-each (("a header shorter than 12 octets" :short "malformed git index: missing DIRC signature")
            ("version 5" :version-5 "malformed git index: unsupported version 5")
            ("an entry cut inside its flags" :cut-flags "malformed git index: truncated")
            ("a version 2 name without its NUL" :v2-unterminated "malformed git index: unterminated entry name")
            ("a varint at the end of the file" :varint-at-end "malformed git index: truncated varint")
            ("a varint cut after a continuation byte" :varint-cut "malformed git index: truncated varint")
            ("a varint past the fixnum range" :varint-overflow "malformed git index: varint overflow")
            ("a strip longer than the previous name" :strip-too-long
             "malformed git index: prefix strip exceeds previous name"))
      "refuses ~A"
      (label case reason)
    (declare (ignore label))
    (let ((octets (ecase case
                    (:short (string-bytes "DIRC"))
                    (:version-5 (index-octets '("a") :version 5))
                    (:cut-flags (subseq (index-octets '("abc")) 0 50))
                    (:v2-unterminated (subseq (index-octets '("abc") :version 2) 0 77))
                    (:varint-at-end (index-prefix '("a")))
                    (:varint-cut (index-with (index-prefix '("a")) #(128)))
                    (:varint-overflow (index-with (index-prefix '("a")) (make-array 12 :initial-element 255)))
                    (:strip-too-long (index-with (index-prefix '("a")) #(1 97 0))))))
      (expect (index-failure octets) :to-equal reason))))

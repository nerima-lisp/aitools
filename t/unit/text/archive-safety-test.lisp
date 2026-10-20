;;;; t/unit/text/archive-safety-test.lisp
(in-package #:aitools.text.test)

(describe "aitools.text.domain archive format detection on odd input"
  (it "names nothing for a block whose checksum is not a number or a too-short prefix"
    (let ((block (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
      (setf (aref block 0) 97)
      (fill block (char-code #\9) :start 148 :end 156)
      (expect (detect-archive-format block "x") :to-be-falsy))
    (expect (detect-archive-format (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0) "x")
            :to-be-falsy)
    (expect (detect-archive-format (octets #x50) "a") :to-be-falsy))

  (it "calls a gzip whose content does not inflate, or with a short name, plain gzip"
    (expect (detect-archive-format (octets #x1f #x8b 8 0 0 0 0 0 0 3 7) "x.gz") :to-be :gz)
    (expect (detect-archive-format (gzip-compress (string-bytes "x")) "a") :to-be :gz)))

(describe "aitools.text.domain archive format detection"
  (it "detects zip, tar, gzip, and tar inside gzip by content"
    (let ((tar (write-tar (list (make-archive-member :name "a.txt" :data (string-bytes "abc"))))))
      (expect (detect-archive-format (write-zip (sample-members)) "x.zip") :to-be :zip)
      (expect (detect-archive-format tar "x.tar") :to-be :tar)
      (expect (detect-archive-format tar "renamed.bin") :to-be :tar)
      (expect (detect-archive-format (gzip-compress tar) "bundle.gz") :to-be :tar-gz)
      (expect (detect-archive-format (gzip-compress (string-bytes "plain text")) "notes.gz") :to-be :gz)
      (expect (detect-archive-format (gzip-compress (string-bytes "plain text")) "notes.tgz") :to-be :tar-gz)
      (expect (detect-archive-format (string-bytes "plain text") "notes.txt") :to-be-falsy)))

  (it "accepts a v7 tar without the ustar magic when its header checksum is valid"
    (let ((old (v7-tar (make-archive-member :name "a.txt" :data (string-bytes "abc")))))
      (expect (map 'string #'code-char (subseq old 257 262)) :not :to-equal "ustar")
      (expect (archive-entry-name (first (read-tar-entries old))) :to-equal "a.txt")
      (expect (detect-archive-format old "old.tar") :to-be :tar)
      (expect (detect-archive-format old "old") :to-be :tar)
      (expect (detect-archive-format (gzip-compress old) "old.gz") :to-be :tar-gz)))

  (it "names each format"
    (expect (mapcar #'archive-format-name '(:zip :tar :tar-gz :gz)) :to-equal (list "zip" "tar" "tar.gz" "gz"))))

(describe "aitools.text.domain archive paths (zip slip)"
  (it "flags absolute, traversing, backslashed, NUL, and empty names"
    (expect (archive-entry-path-problem "a/b.txt") :to-be-falsy)
    (expect (archive-entry-path-problem "a/..b/c") :to-be-falsy)
    (expect (archive-entry-path-problem "/etc/passwd") :to-be :absolute)
    (expect (archive-entry-path-problem "C:/x") :to-be :absolute)
    (expect (archive-entry-path-problem "1:/x") :to-be-falsy)
    (expect (archive-entry-path-problem "a/../../b") :to-be :parent-traversal)
    (expect (archive-entry-path-problem "..") :to-be :parent-traversal)
    (expect (archive-entry-path-problem "a\\..\\b") :to-be :backslash)
    (expect (archive-entry-path-problem (format nil "a~Cb" (code-char 0))) :to-be :nul)
    (expect (archive-entry-path-problem "/") :to-be :empty))

  (it "flags symlink targets that leave the extraction directory"
    (expect (archive-link-target-problem "a/b/link" "../c") :to-be-falsy)
    (expect (archive-link-target-problem "a/b/link" "../../c") :to-be-falsy)
    (expect (archive-link-target-problem "a/b/link" "../../../c") :to-be :escapes)
    (expect (archive-link-target-problem "link" "x/../../y") :to-be :escapes)
    (expect (archive-link-target-problem "link" "/etc") :to-be :absolute)
    (expect (archive-link-target-problem "link" "") :to-be :absolute))

  (it "flags a target whose `..` climbs out of another symlink of the same archive"
    ;; x/d -> .. is harmless alone, but s -> x/d/.. then resolves to the
    ;; parent of the extraction root, not to x.
    (let ((symlinks (list "x/d" "s")))
      (expect (archive-link-target-problem "x/d" ".." :symlink-names symlinks) :to-be-falsy)
      (expect (archive-link-target-problem "s" "x/d/.." :symlink-names symlinks) :to-be :escapes)
      (expect (archive-link-target-problem "s" "x/./d/.." :symlink-names symlinks) :to-be :escapes)
      (expect (archive-link-target-problem "s" "x//d/.." :symlink-names symlinks) :to-be :escapes)
      (expect (archive-link-target-problem "x/d/t" "../y" :symlink-names symlinks) :to-be :escapes)
      (expect (archive-link-target-problem "s2" "x/d/y" :symlink-names symlinks) :to-be-falsy)
      (expect (archive-link-target-problem "x/e" "../x/f" :symlink-names symlinks) :to-be-falsy))))

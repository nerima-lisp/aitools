;;;; t/integration/text-archive-interop-test.lisp
;;;;
;;;; Interoperability of the text context's zip, tar, and gzip codecs with
;;;; real archivers. Archives made by Info-ZIP, GNU tar, and bsdtar are read
;;;; from checked-in fixtures (text-archive-fixtures.lisp) on every run;
;;;; when zip/unzip/tar/gzip are on PATH, archives are also produced by those
;;;; tools at test time and ours are extracted by them. A missing tool skips
;;;; only its own test.
(in-package #:cl-user)

(defpackage #:aitools.text.integration-test
  (:use #:cl #:aitools.workspace.integration-support #:aitools.text.domain)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect #:fail #:skip))

(in-package #:aitools.text.integration-test)

(defparameter *deep-name*
  (format nil "dir/~{~A~^/~}/file.txt" (loop for i from 0 below 12 collect (format nil "segment~2,'0D" i))))

(defun noise ()
  (let ((bytes (make-array 300 :element-type '(unsigned-byte 8))))
    (dotimes (i 300 bytes) (setf (aref bytes i) (mod (* i 7919) 251)))))

(defun text (octets) (sb-ext:octets-to-string octets :external-format :utf-8))

(defun check-fixture-tree (archive entries)
  "The fixture tree's names, kinds, link, and contents, as read by us."
  (flet ((entry (name) (or (find name entries :key #'archive-entry-name :test #'string=)
                           (fail (format nil "no entry ~A" name)))))
    (expect (archive-entry-kind (entry "dir")) :to-be :directory)
    (expect (text (archive-entry-data archive (entry "dir/hello.txt"))) :to-equal (format nil "hello~%"))
    (expect (archive-entry-data archive (entry "dir/noise.bin")) :to-equalp (noise))
    (expect (text (archive-entry-data archive (entry "dir/日本.txt"))) :to-equal (format nil "unicode~%"))
    (expect (archive-entry-kind (entry "dir/link")) :to-be :symlink)
    (expect (archive-entry-link-target (entry "dir/link")) :to-equal "hello.txt")
    (expect (text (archive-entry-data archive (entry *deep-name*))) :to-equal (format nil "deep~%"))))

(defun sample-members ()
  (list (make-archive-member :name "out" :kind :directory :mode #o755 :mtime 1700000000)
        (make-archive-member :name "out/hello.txt" :data (sb-ext:string-to-octets (format nil "hello~%")) :mtime 1700000000)
        (make-archive-member :name "out/noise.bin" :data (noise) :mtime 1700000000)
        (make-archive-member :name "out/日本.txt" :data (sb-ext:string-to-octets "unicode" :external-format :utf-8)
                             :mtime 1700000000)
        (make-archive-member :name "out/link" :kind :symlink :link-target "hello.txt" :mode #o777 :mtime 1700000000)
        (make-archive-member :name (concatenate 'string "out" (subseq *deep-name* 3))
                             :data (sb-ext:string-to-octets "deep") :mtime 1700000000)))

(defun check-extracted (directory)
  "The files a real extractor produced from SAMPLE-MEMBERS below DIRECTORY."
  (dolist (member (sample-members))
    (let ((path (concatenate 'string directory "/" (archive-member-name member))))
      (case (archive-member-kind member)
        (:file (expect (read-file path) :to-equalp (archive-member-data member)))
        (:symlink (expect (sb-posix:readlink path) :to-equal (archive-member-link-target member)))
        (:directory (expect (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:stat path))) :to-be-truthy))))))

(defun make-fixture-tree (directory)
  (write-file (concatenate 'string directory "/dir/hello.txt") (format nil "hello~%"))
  (write-file (concatenate 'string directory "/dir/noise.bin") (noise))
  (write-file (concatenate 'string directory "/dir/日本.txt") (format nil "unicode~%"))
  (make-symlink "hello.txt" (concatenate 'string directory "/dir/link"))
  (write-file (concatenate 'string directory "/" *deep-name*) (format nil "deep~%")))

(defun run-ok (directory program &rest arguments)
  (multiple-value-bind (code stdout stderr) (run-command directory program arguments)
    (unless (zerop code) (fail (format nil "~A ~{~A~^ ~} failed: ~A" program arguments stderr)))
    stdout))

(describe "aitools text codecs read archives made by real archivers (checked-in fixtures)"
  (it "reads an Info-ZIP zip (stored and deflated entries, symlink, long and UTF-8 names)"
    (let ((archive *infozip-zip*))
      (check-fixture-tree archive (read-zip-entries archive))))

  (it "reads GNU tar's gnu format (././@LongLink names) through gzip"
    (let ((archive (gzip-decompress *gnutar-gnu-tar-gz*)))
      (check-fixture-tree archive (read-tar-entries archive))))

  (it "reads GNU tar's pax format (x headers) through gzip"
    (let ((archive (gzip-decompress *gnutar-pax-tar-gz*)))
      (check-fixture-tree archive (read-tar-entries archive))))

  (it "reads bsdtar's default format (ustar prefix) through gzip"
    (let ((archive (gzip-decompress *bsdtar-tar-gz*)))
      (check-fixture-tree archive (read-tar-entries archive)))))

(describe "aitools text codecs interoperate with the archivers on PATH"
  (it "reads a zip made by zip, and unzip extracts and tests our zip"
    (unless (and (program-path "zip") (program-path "unzip")) (skip "zip/unzip are not on PATH"))
    (with-scratch-directory (scratch)
      (make-fixture-tree scratch)
      (run-ok scratch "zip" "-q" "-r" "-X" "-y" "made.zip" "dir")
      (let ((archive (read-file (concatenate 'string scratch "/made.zip"))))
        (check-fixture-tree archive (read-zip-entries archive)))
      (write-file (concatenate 'string scratch "/ours.zip") (write-zip (sample-members)))
      (run-ok scratch "unzip" "-tq" "ours.zip")
      (run-ok scratch "unzip" "-q" "ours.zip" "-d" "x")
      (check-extracted (concatenate 'string scratch "/x"))))

  (it "reads a tar made by tar, and tar extracts our tar"
    (unless (program-path "tar") (skip "tar is not on PATH"))
    (with-scratch-directory (scratch)
      (make-fixture-tree scratch)
      (run-ok scratch "tar" "-cf" "made.tar" "dir")
      (let ((archive (read-file (concatenate 'string scratch "/made.tar"))))
        (check-fixture-tree archive (read-tar-entries archive)))
      (write-file (concatenate 'string scratch "/ours.tar") (write-tar (sample-members)))
      (make-directories (concatenate 'string scratch "/x"))
      (run-ok scratch "tar" "-xf" "ours.tar" "-C" "x")
      (check-extracted (concatenate 'string scratch "/x"))))

  (it "round-trips tar.gz and gz with tar -z and gzip"
    (unless (and (program-path "tar") (program-path "gzip")) (skip "tar/gzip are not on PATH"))
    (with-scratch-directory (scratch)
      (make-fixture-tree scratch)
      (run-ok scratch "tar" "-czf" "made.tar.gz" "dir")
      (let ((archive (gzip-decompress (read-file (concatenate 'string scratch "/made.tar.gz")))))
        (check-fixture-tree archive (read-tar-entries archive)))
      (write-file (concatenate 'string scratch "/ours.tar.gz") (gzip-compress (write-tar (sample-members))))
      (make-directories (concatenate 'string scratch "/x"))
      (run-ok scratch "tar" "-xzf" "ours.tar.gz" "-C" "x")
      (check-extracted (concatenate 'string scratch "/x"))
      (write-file (concatenate 'string scratch "/ours.gz") (gzip-compress (noise) :name "noise.bin"))
      (expect (run-ok scratch "gzip" "-dc" "ours.gz") :to-equalp (noise))
      (run-ok scratch "gzip" "-k" "dir/noise.bin")
      (expect (gzip-decompress (read-file (concatenate 'string scratch "/dir/noise.bin.gz"))) :to-equalp (noise)))))

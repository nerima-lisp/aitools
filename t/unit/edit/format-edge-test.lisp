;;;;
;;;; Archive extraction edge cases and remaining branches of the value models.
(in-package #:aitools.edit.test)

(defun retype-tar-entry (octets header type &key link)
  "OCTETS (a tar) with the header at byte HEADER given typeflag TYPE and
LINK as its link name, checksum recomputed: the hard links and device
entries WRITE-TAR does not produce."
  (let ((data (copy-seq octets)))
    (setf (aref data (+ header 156)) (char-code type))
    (when link
      (replace data (bytes link) :start1 (+ header 157)))
    (fill data 32 :start (+ header 148) :end (+ header 156))
    (let ((digits (format nil "~6,'0O" (loop for i from header below (+ header 512) sum (aref data i)))))
      (dotimes (i 6) (setf (aref data (+ header 148 i)) (char-code (char digits i))))
      (setf (aref data (+ header 154)) 0 (aref data (+ header 155)) 32))
    data))

(describe "aitools.edit.domain archive extraction plans: formats and entry kinds"
  (it "extracts a gz member under its stored name, else the archive's name without .gz"
    (let ((named (aitools.text.domain:gzip-compress (bytes "hello") :name "inner.txt"))
          (anonymous (aitools.text.domain:gzip-compress (bytes "hello"))))
      (flet ((plan (octets archive-path)
               (plan-archive-extraction octets :gz "out" :archive-path archive-path :max-bytes 100 :max-entries 10
                                                         :lookup-kind (constantly :absent))))
        (let ((step (first (plan named "x/a.gz"))))
          (expect (extract-step-path step) :to-equal "out/inner.txt")
          (expect (extract-step-data step) :to-equalp (bytes "hello"))
          (expect (extract-step-mode step) :to-be #o644))
        (expect (extract-step-path (first (plan anonymous "x/log.GZ"))) :to-equal "out/log")
        (expect (extract-step-path (first (plan anonymous "blob"))) :to-equal "out/blob.out")
        (expect (extract-step-path (first (plan anonymous ".gz"))) :to-equal "out/.gz.out"))))

  (it "refuses a gz member that exists or inflates past --max-bytes"
    (let ((octets (aitools.text.domain:gzip-compress (make-array 5000 :element-type '(unsigned-byte 8) :initial-element 7)
                                                     :name "big")))
      (expect (refusal-code (lambda () (plan-archive-extraction octets :gz "" :max-bytes 100 :max-entries 1
                                                                          :lookup-kind (constantly :absent))))
              :to-equal "refusal.too-large")
      (expect (refusal-code (lambda () (plan-archive-extraction octets :gz "" :max-bytes 10000 :max-entries 1
                                                                          :lookup-kind (constantly :file))))
              :to-equal "refusal.exists")))

  (it "plans a tar.gz, refusing one that inflates past --max-bytes"
    (let ((octets (aitools.text.domain:gzip-compress
                   (aitools.text.domain:write-tar (list (member-file "a.txt" "A"))))))
      (expect (mapcar #'extract-step-path (extract-plan octets :tar-gz)) :to-equal '("out/a.txt"))
      (expect (refusal-code (lambda () (extract-plan octets :tar-gz :max-bytes 600))) :to-equal "refusal.too-large")))

  (it "extracts only the --entry names, ./-prefixed or not, and refuses one the archive lacks"
    (let ((octets (aitools.text.domain:write-tar
                   (list (member-file "./a.txt" "A") (member-file "b.txt" "B")
                         (aitools.text.domain:make-archive-member :name "." :kind :directory)))))
      (expect (mapcar #'extract-step-path (extract-plan octets :tar)) :to-equal '("out/a.txt" "out/b.txt"))
      (expect (mapcar #'extract-step-path (extract-plan octets :tar :selected '("a.txt"))) :to-equal '("out/a.txt"))
      (expect (refusal-code (lambda () (extract-plan octets :tar :selected '("c.txt")))) :to-equal "input.not-found")))

  (it "plans directories, merging into an existing one, and symlinks inside the destination"
    (let* ((octets (aitools.text.domain:write-tar
                    (list (aitools.text.domain:make-archive-member :name "d" :kind :directory :mode #o755)
                          (aitools.text.domain:make-archive-member :name "d/l" :kind :symlink :link-target "../a"))))
           (steps (extract-plan octets :tar :lookup (lambda (path) (if (string= path "out/d") :directory :absent)))))
      (expect (mapcar (lambda (step) (list (extract-step-kind step) (extract-step-path step) (extract-step-target step)))
                      steps)
              :to-equal '((:directory "out/d" nil) (:symlink "out/d/l" "../a")))
      (expect (refusal-code (lambda () (extract-plan octets :tar :lookup (lambda (path) (if (string= path "out/d") :file :absent)))))
              :to-equal "refusal.exists")))

  (it "resolves a tar hard link to the file it names and refuses one naming no file"
    (let* ((tar (aitools.text.domain:write-tar (list (member-file "a.txt" "shared" :mode #o600) (member-file "h" ""))))
           (linked (retype-tar-entry tar 1024 #\1 :link "a.txt"))
           (dangling (retype-tar-entry tar 1024 #\1 :link "zz"))
           (escaping (retype-tar-entry tar 1024 #\1 :link "../a.txt"))
           (steps (extract-plan linked :tar)))
      (expect (mapcar #'extract-step-path steps) :to-equal '("out/a.txt" "out/h"))
      (expect (extract-step-data (second steps)) :to-equalp (bytes "shared"))
      (expect (extract-step-mode (second steps)) :to-be #o600)
      (expect (refusal-code (lambda () (extract-plan dangling :tar))) :to-equal "refusal.outside-workspace")
      (expect (refusal-code (lambda () (extract-plan escaping :tar))) :to-equal "refusal.outside-workspace")))
  (it "matches hard-link targets by normalized case-sensitive names"
  (let* ((case-mismatch
           (aitools.text.domain:write-tar
            (list (member-file "f" "payload") (member-file "h" ""))))
         (normalized-match
           (aitools.text.domain:write-tar
            (list (member-file "d/f" "payload") (member-file "h" "")))))
    (expect (refusal-code
             (lambda ()
               (extract-plan (retype-tar-entry case-mismatch 1024 #\1 :link "F") :tar)))
            :to-equal "refusal.outside-workspace")
    (expect (mapcar #'extract-step-path
                    (extract-plan (retype-tar-entry normalized-match 1024 #\1 :link "./d//f") :tar))
            :to-equal '("out/d/f" "out/h"))))

  (it "refuses a hard link to a directory and a device or special entry"
    (let ((tar (aitools.text.domain:write-tar
                (list (aitools.text.domain:make-archive-member :name "d" :kind :directory) (member-file "h" "")))))
      (expect (refusal-code (lambda () (extract-plan (retype-tar-entry tar 512 #\1 :link "d") :tar)))
              :to-equal "refusal.outside-workspace")
      (expect (refusal-code (lambda () (extract-plan (retype-tar-entry tar 512 #\3) :tar)))
              :to-equal "input.unsupported-format")))

  (it "counts a hard link's bytes against --max-bytes"
    (let* ((tar (aitools.text.domain:write-tar (list (member-file "a" "0123456789") (member-file "h" ""))))
           (linked (retype-tar-entry tar 1024 #\1 :link "a")))
      (expect (length (extract-plan linked :tar :max-bytes 20)) :to-be 2)
      (expect (refusal-code (lambda () (extract-plan linked :tar :max-bytes 15))) :to-equal "refusal.too-large")))

  (it-each (("x.zip" :zip) ("x.TGZ" :tar-gz) ("x.tar.gz" :tar-gz) ("x.tar" :tar) ("x.gz" :gz) ("x.txt" nil) ("" nil))
      "infers the create format of ~S"
      (path format)
    (expect (archive-format-for-path path) :to-be format))

  (it "builds a gz archive holding one member under the given name"
    (let ((octets (build-archive :gz (list (member-file "a.txt" "payload")) :name "a.txt")))
      (expect (aitools.text.domain:gzip-decompress octets) :to-equalp (bytes "payload"))
      (expect (aitools.text.domain:gzip-member-header octets) :to-equal "a.txt"))))

(describe "aitools.edit.domain remaining branches of the value models"
  (it "refuses pointers through numbers and strings, and a patch that is a string"
    (let ((value (j "{\"n\": 1, \"s\": \"t\"}")))
      (expect (refusal-code (lambda () (json-add value (parse-json-pointer "/n/b/c") (j "1")))) :to-equal "input.not-found")
      (expect (refusal-code (lambda () (json-remove value (parse-json-pointer "/s/x")))) :to-equal "input.not-found")
      (expect (refusal-code (lambda () (json-apply-patch value "ops"))) :to-equal "argument.invalid")))

  (it-each (("a quoted field ending in an escaped quote" "a~%\"x\"\"\"~%" 1 "a" "z" "a~%z~%" "x\"")
            ("a last record ending in an empty field" "a,b~%1," 1 "b" "z" "a,b~%1,z" "")
            ("a quoted field closing at the end of the text" "a~%\"x\"" 1 "a" "z" "a~%z" "x"))
      "sets ~A"
      (name text row column value expected previous)
    (declare (ignore name))
    (multiple-value-bind (result old) (table-set-cell (format nil text) #\, row column value)
      (expect result :to-equal (format nil expected))
      (expect old :to-equal previous)))

  (it "names a gz member after the archive when its stored name is a path"
    (let ((octets (aitools.text.domain:gzip-compress (bytes "x") :name "dir/evil")))
      (expect (extract-step-path (first (plan-archive-extraction octets :gz "out" :archive-path "safe.gz" :max-bytes 10
                                                                          :max-entries 1 :lookup-kind (constantly :absent))))
              :to-equal "out/safe"))))

(in-package #:aitools.edit.test)

(defun archive-symlink (name target)
  (aitools.text.domain:make-archive-member :name name :kind :symlink :link-target target))

(defun archive-symlink-refusal (members &optional (lookup (constantly :absent)))
  (refusal-code (lambda ()
                  (extract-plan (aitools.text.domain:write-tar members) :tar :lookup lookup))))

(describe "aitools.edit.domain archive symlink extraction plans"
  (it "rejects archive symlinks reached before a trailing parent component"
    (dolist (links
             (list (list (list "D" ".") (list "s" "D/.."))
                   (list (list "X/D" "..") (list "s" "X/D/.."))
                   (list (list "é" ".") (list "s" "é/.."))
                   (list (list "ß" ".") (list "s" "ß/.."))
                   (list (list "İ" ".") (list "s" "İ/.."))
                   (list (list "A" "B") (list "B" ".") (list "s" "A/.."))
                   (list (list "a/B" "..") (list "s" "a/B/.."))))
      (expect (archive-symlink-refusal
               (mapcar (lambda (link) (archive-symlink (first link) (second link))) links))
              :to-equal "refusal.outside-workspace"))
    (expect (archive-symlink-refusal
             (list (archive-symlink "s" "../escape")))
            :to-equal "refusal.outside-workspace"))

  (it "matches folded and normalized names when checking archive symlink traversal"
  (let ((decomposed (format nil "e~C" (code-char #x301)))
        (dotted-i (format nil "i~C" (code-char #x307))))
    (dolist (case (list (list "X/D" ".." "x/d/..")
                        (list "é/d" ".." (format nil "~A/d/.." decomposed))
                        (list "ß" "." "SS/..")
                        (list "İ" "." (format nil "~A/.." dotted-i))))
      (expect (archive-symlink-refusal
               (list (archive-symlink (first case) (second case))
                     (archive-symlink "s" (third case))))
              :to-equal "refusal.outside-workspace")))
  (expect (archive-symlink-refusal
           (list (archive-symlink "X/D" "..")
                 (member-file "x/d/f" "payload")))
          :to-equal "refusal.outside-workspace"))

  (it "rejects repeated archive symlink aliases before later parent traversal"
    (expect (archive-symlink-refusal
             (list (archive-symlink "x/d" "..")
                   (archive-symlink "s" "x/d/x/d/../../escaped-target")))
            :to-equal "refusal.outside-workspace")
    (expect (archive-symlink-refusal
             (list (archive-symlink "x" ".")
                   (archive-symlink "s" "x/x/x/x/../../../escaped")))
            :to-equal "refusal.outside-workspace"))

  (it "rejects archive entry parents that pass through an archive symlink"
    (expect (archive-symlink-refusal
             (list (archive-symlink "x/d" "..")
                   (member-file "x/d/f" "payload")))
            :to-equal "refusal.outside-workspace"))

  (it "rejects canonically equivalent and case-folded archive symlink path aliases"
    (let ((decomposed (format nil "e~C/d" (code-char #x301))))
      (expect (archive-symlink-refusal
               (list (archive-symlink "é/d" "..")
                     (archive-symlink "s" (format nil "~A/../../escaped" decomposed))))
              :to-equal "refusal.outside-workspace"))
    (expect (archive-symlink-refusal
             (list (archive-symlink "ς/d" "..")
                   (archive-symlink "s" "σ/d/../../escaped")))
            :to-equal "refusal.outside-workspace"))

  (it "rejects links that reach existing destination symlinks through archive aliases"
    (expect (archive-symlink-refusal
             (list (archive-symlink "x/d" "..")
                   (archive-symlink "y" "x/d/e/etc"))
             (lambda (path) (if (string= path "out/e") :symlink :absent)))
            :to-equal "refusal.outside-workspace"))

  (it "rejects an existing destination symlink as the target's final component"
    (expect (archive-symlink-refusal
             (list (archive-symlink "y" "e"))
             (lambda (path) (if (string= path "out/e") :symlink :absent)))
            :to-equal "refusal.outside-workspace"))

  (it "rejects duplicate names after removing empty, dot, and leading dot components"
    (expect (archive-symlink-refusal
             (list (archive-symlink "x/d" "../target")
                   (member-file "x//d" "payload")))
            :to-equal "refusal.exists")
    (expect (archive-symlink-refusal
             (list (member-file "./x/./d" "first")
                   (member-file "x/d" "second")))
            :to-equal "refusal.exists"))

  (it "preserves case and Unicode spelling for entry names and selection"
    (let* ((octets (aitools.text.domain:write-tar
                    (list (member-file "README" "upper")
                          (member-file "readme" "lower"))))
           (steps (extract-plan octets :tar)))
      (expect (mapcar #'extract-step-path steps) :to-equal '("out/README" "out/readme"))
      (expect (mapcar #'extract-step-path (extract-plan octets :tar :selected '("readme")))
              :to-equal '("out/readme")))
    (let* ((decomposed (format nil "e~C" (code-char #x301)))
           (steps (extract-plan
                   (aitools.text.domain:write-tar
                    (list (member-file "é" "composed")
                          (member-file decomposed "decomposed")))
                   :tar)))
      (expect (mapcar #'extract-step-path steps) :to-equal (list "out/é" (format nil "out/~A" decomposed)))))

  (it-each (("lib" "lib64" "lib64/tool")
            ("a/b" "../c" "c")
            ("bin/tool" "../lib/tool" "lib/tool")
            ("libfoo.so" "libfoo.so.1" "libfoo.so.1"))
      "allows valid link ~A -> ~A with target ~A"
      (link-name link-target target-name)
    (let* ((steps (extract-plan
                   (aitools.text.domain:write-tar
                    (list (archive-symlink link-name link-target)
                          (member-file target-name "payload")))
                   :tar))
           (link-step (find (format nil "out/~A" link-name) steps
                            :key #'extract-step-path :test #'string=)))
      (expect (extract-step-kind link-step) :to-be :symlink)
      (expect (extract-step-target link-step) :to-equal link-target))))

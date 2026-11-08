(in-package #:aitools.edit.test)

(defun archive-symlink (name target)
  (aitools.text.domain:make-archive-member :name name :kind :symlink :link-target target))

(defun archive-symlink-refusal (members &optional (lookup (constantly :absent)))
  (refusal-code (lambda ()
                  (extract-plan (aitools.text.domain:write-tar members) :tar :lookup lookup))))

(describe "aitools.edit.domain archive symlink extraction plans"
  (it "rejects archive link chain a during planning"
    (expect (archive-symlink-refusal (list (archive-symlink "x/d" "..")
                                           (archive-symlink "s" "x/d/..")))
            :to-equal "refusal.outside-workspace"))

  (it "rejects archive link chain b during planning"
    (expect (archive-symlink-refusal
             (list (archive-symlink "a/b/c" "../..")
                   (archive-symlink "s" "a/b/c/../../../escaped-target")))
            :to-equal "refusal.outside-workspace"))

  (it "rejects archive link chain c before writing a file through it"
    (expect (archive-symlink-refusal
             (list (archive-symlink "a/b/c" "../..")
                   (archive-symlink "s" "a/b/c/../../../escaped-target")
                   (member-file "s/PWNED" "payload")))
            :to-equal "refusal.outside-workspace"))

  (it "rejects archive link chain d before writing beneath it"
    (expect (archive-symlink-refusal
             (list (archive-symlink "x/d" "..")
                   (member-file "x/d/f" "payload")))
            :to-equal "refusal.outside-workspace"))

  (it "rejects repeated archive symlink aliases before a later parent traversal"
    (expect (archive-symlink-refusal
             (list (archive-symlink "x/d" "..")
                   (archive-symlink "s" "x/d/x/d/../../escaped-target")))
            :to-equal "refusal.outside-workspace"))

  (it "rejects repeated self symlink aliases before a later parent traversal"
    (expect (archive-symlink-refusal
             (list (archive-symlink "x" ".")
                   (archive-symlink "s" "x/x/x/x/../../../escaped")))
            :to-equal "refusal.outside-workspace"))

  (it "matches case-folded archive symlink prefixes in link targets"
    (expect (archive-symlink-refusal
             (list (archive-symlink "X/D" "..")
                   (archive-symlink "s" "x/d/../../escaped")))
            :to-equal "refusal.outside-workspace")
    (expect (archive-symlink-refusal
             (list (archive-symlink "ς/d" "..")
                   (archive-symlink "s" "σ/d/../../escaped")))
            :to-equal "refusal.outside-workspace"))

  (it "matches case-folded archive symlink prefixes in entry parents"
    (expect (archive-symlink-refusal
             (list (archive-symlink "X/D" "..")
                   (member-file "x/d/f" "payload")))
            :to-equal "refusal.outside-workspace"))

  (it "matches canonically equivalent archive symlink prefixes"
    (let ((decomposed (format nil "e~C/d" (code-char #x301))))
      (expect (archive-symlink-refusal
               (list (archive-symlink "é/d" "..")
                     (archive-symlink "s" (format nil "~A/../../escaped" decomposed))))
              :to-equal "refusal.outside-workspace")))

  (it "rejects archive names that collide after Unicode normalization"
    (let ((decomposed (format nil "e~C/d" (code-char #x301))))
      (expect (archive-symlink-refusal
               (list (archive-symlink "é/d" "../target")
                     (member-file decomposed "payload")))
              :to-equal "refusal.exists")))

  (it "rejects a link through an archive alias to an existing destination symlink"
    (expect (archive-symlink-refusal
             (list (archive-symlink "x/d" "..")
                   (archive-symlink "y" "x/d/e/etc"))
             (lambda (path) (if (string= path "out/e") :symlink :absent)))
            :to-equal "refusal.outside-workspace"))

  (it "rejects archive names that collide after slash normalization"
    (expect (archive-symlink-refusal
             (list (archive-symlink "x/d" "..")
                   (member-file "x//d" "payload")))
            :to-equal "refusal.exists"))
  (it "rejects archive names that collide after case folding"
    (expect (archive-symlink-refusal
             (list (archive-symlink "X/D" ".")
                   (member-file "x/d" "payload")))
            :to-equal "refusal.exists")
    (expect (archive-symlink-refusal
             (list (archive-symlink "ς/d" ".")
                   (member-file "σ/d" "payload")))
            :to-equal "refusal.exists"))

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

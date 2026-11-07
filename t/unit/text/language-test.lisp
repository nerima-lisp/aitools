(in-package #:aitools.text.test)

(describe "aitools.text.domain language lookup"
  (it "returns a unique list of names that all resolve"
    (let ((names (language-names)))
      (expect names :to-be-truthy)
      (expect (length (remove-duplicates (copy-list names) :test #'string=))
              :to-be (length names))
      (expect (every (lambda (name)
                       (let ((language (find-language name)))
                         (and language (string= name (language-name language)))))
                     names)
              :to-be t)))

  (it-each (("canonical spelling" "common-lisp" "common-lisp")
            ("case-insensitive spelling" "CoMmOn-LiSp" "common-lisp")
            ("another language" "RUST" "rust")
            ("unknown language" "cobol" nil)
            ("empty name" "" nil))
    "looks up ~A"
    (description name expected)
    (declare (ignore description))
    (let ((language (find-language name)))
      (if expected
          (expect (language-name language) :to-equal expected)
          (expect language :to-be-falsy))))

  (it-each (("an extension in a nested path" "src/a.lisp" "common-lisp")
            ("an uppercase extension" "src/a.RS" "rust")
            ("a multi-part suffix uses its final extension" "bundle.tar.gz" nil)
            ("an exact dotfile name" "home/.emacs" "emacs-lisp")
            ("another exact dotfile name" "home/.bashrc" "shell")
            ("a file without a known extension" "Makefile" nil)
            ("a hidden file without a language name" ".hidden" nil)
            ("an empty path" "" nil))
    "maps ~A"
    (description path expected)
    (declare (ignore description))
    (let ((language (language-for-path path)))
      (expect (and language (language-name language)) :to-equal expected)))

  (it-each (("case-insensitive name and extension" "Python" "src/a.PY" "src/a.lisp" t) ("Common Lisp extension" "common-lisp" "src/a.cl" "src/a.py" t) ("exact filename" "Emacs-Lisp" "home/.emacs" "home/.bashrc" t) ("unknown language" "cobol" "src/a.py" "src/a.py" nil)) "builds a predicate for ~A" (description name matching-path other-path known-p) (declare (ignore description)) (let ((predicate (language-path-predicate name))) (if known-p (progn (expect predicate :to-be-truthy) (expect (funcall predicate matching-path) :to-be-truthy) (expect (funcall predicate other-path) :to-be-falsy)) (expect predicate :to-be-falsy)))))

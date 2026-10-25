(in-package #:aitools.fixture.roots.api)

(defun dynamic-find-symbol-test (name package-name)
  (cl:find-symbol name package-name))
(defun quoted-dynamic-find-symbol ()
  (multiple-value-prog1
    '(cl:find-symbol name package-name)
    `(cl:find-symbol
      "INACTIVE-BACKQUOTE-TARGET"
      "AITOOLS.FIXTURE.ROOTS.API"
      ,(cl:find-symbol "ACTIVE-UNQUOTE-TARGET"
                       "AITOOLS.FIXTURE.ROOTS.API"))))
(defun unqualified-dynamic-find-symbol-test (name package-name)
  (progn
    (cl:find-symbol "OMITTED-PACKAGE-TARGET")
    (cl:find-symbol "EXPLICIT-NIL-PACKAGE-TARGET" nil)
    (find-symbol name package-name)))
(defun lexical-find-symbol-shadow-test (name package-name)
  (flet ((find-symbol (local-name local-package)
           (values local-name local-package)))
    (find-symbol name package-name)))
(defun inactive-backquote-target () nil)
(defun active-unquote-target () nil)
(defun omitted-package-target () nil)
(defun explicit-nil-package-target () nil)

(in-package #:aitools.fixture.roots.api)

(defun dynamic-find-symbol-test (name package-name)
  (cl:find-symbol name package-name))
(defun quoted-dynamic-find-symbol ()
  '(cl:find-symbol name package-name))
(defun unqualified-dynamic-find-symbol-test (name package-name)
  (find-symbol
   name
   package-name))
(defun lexical-find-symbol-shadow-test (name package-name)
  (flet ((find-symbol (local-name local-package)
           (values local-name local-package)))
    (find-symbol name package-name)))

;;;;
;;;; Data is kept separate from logic: every `data/**/*-data.lisp` file
;;;; interns its constants into this one shared package rather than each
;;;; context's own domain package, because a :DATA component is loaded
;;;; before its context's :LIBRARY (docs/src/project/development.md, "The
;;;; component lists") -- before that context's own
;;;; DOMAIN/PACKAGE.LISP has run, so there is no context-specific package yet
;;;; to intern into.
;;;;
;;;; Each data file EXPORTs its own symbols at its own end (see e.g.
;;;; error-catalog-data.lisp) instead of listing them in a shared
;;;; DEFPACKAGE :EXPORT clause here, so adding a data file does not require
;;;; updating a shared export list.
;;;; Validate the stable package shape when ASDF recompiles this component
;;;; after those data-file exports have been added.
(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((package (find-package "AITOOLS.DATA"))
        (common-lisp (find-package "COMMON-LISP")))
    (if package
        (unless
            (and (string= (package-name package) "AITOOLS.DATA")
                 (null (package-nicknames package))
                 (equal (package-use-list package) (list common-lisp))
                 (null (package-shadowing-symbols package))
                 (loop for symbol being the symbols of package
                       always (or (eq (symbol-package symbol) package)
                                  (eq (symbol-package symbol) common-lisp))))
          (error "Unexpected package definition for AITOOLS.DATA"))
        (defpackage #:aitools.data (:use #:cl)))))

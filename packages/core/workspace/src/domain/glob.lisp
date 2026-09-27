;;;; packages/core/workspace/src/domain/glob.lisp
;;;;
;;;; The `--glob <pattern>` filter. Globs use the same wildmatch
;;;; semantics as .gitignore: a glob without `/` matches the base name at any
;;;; depth; a glob with `/` matches the whole workspace-relative path (a
;;;; leading `/` is dropped). A glob starting with `!` excludes.
(in-package #:aitools.workspace.domain)

(defstruct (glob-filter (:constructor %make-glob-filter (include exclude casefold)) (:copier nil))
  (include '() :type list :read-only t)
  (exclude '() :type list :read-only t)
  (casefold nil :type boolean :read-only t))

(defun %compile-glob (text)
  (let ((pathname (position #\/ text)))
    (cons (if (and pathname (char= (char text 0) #\/)) (subseq text 1) text)
          (and pathname t))))

(defun make-glob-filter (globs &key casefold)
  "A GLOB-FILTER for GLOBS (a list of strings), or NIL when GLOBS is empty."
  (when globs
    (let ((include '()) (exclude '()))
      (dolist (glob globs)
        (if (and (plusp (length glob)) (char= (char glob 0) #\!))
            (push (%compile-glob (subseq glob 1)) exclude)
            (push (%compile-glob glob) include)))
      (%make-glob-filter (nreverse include) (nreverse exclude) casefold))))

(defun %glob-matches-p (glob path casefold)
  (destructuring-bind (text . pathname) glob
    (if pathname
        (wildmatch text path :pathname t :casefold casefold)
        (wildmatch text (path-basename path) :casefold casefold))))

(defun glob-filter-accepts-p (filter path)
  "True when PATH (workspace-relative) passes FILTER: it matches some include
glob (or there are none) and no exclude glob. A NIL FILTER accepts all."
  (or (null filter)
      (let ((casefold (glob-filter-casefold filter)))
        (and (or (null (glob-filter-include filter))
                 (some (lambda (glob) (%glob-matches-p glob path casefold)) (glob-filter-include filter)))
             (notany (lambda (glob) (%glob-matches-p glob path casefold)) (glob-filter-exclude filter))))))

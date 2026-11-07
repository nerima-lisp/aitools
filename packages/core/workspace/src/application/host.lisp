;;;;
;;;; Workspace flows receive a cl-boundary-kit context directly. The context
;;;; carries the kit's environment and working-directory objects plus the
;;;; filesystem operations whose metadata and byte semantics the kit does not
;;;; expose.
;;;;
;;;; Every path argument is an absolute, normalized POSIX string.
(in-package #:aitools.workspace.application)

(defmacro define-workspace-operation (name key arguments documentation)
  `(defun ,name (boundaries ,@arguments)
     ,documentation
     (funcall
      (cl-boundary-kit:boundary-context-require boundaries ,key)
      ,@arguments)))

(defun make-workspace-boundaries
    (&key list-directory stat read-link read-octets environment
          working-directory home-directory
          (call-with-ordered-mapper
            (lambda (thunk)
              (funcall thunk (lambda (function items) (mapcar function items))))))
  "Build the context with workspace-specific lstat and byte-reading operations."
  (flet ((need (value name)
           (unless (functionp value)
             (error "make-workspace-boundaries: ~A must be a function, got ~S" name value))
           value))
    (unless environment
      (error "make-workspace-boundaries: ENVIRONMENT is required"))
    (unless working-directory
      (error "make-workspace-boundaries: WORKING-DIRECTORY is required"))
    (let ((context (cl-boundary-kit:make-boundary-context)))
      (dolist (capability
               (list (cons :workspace-list-directory (need list-directory "LIST-DIRECTORY"))
                     (cons :workspace-stat (need stat "STAT"))
                     (cons :workspace-read-link (need read-link "READ-LINK"))
                     (cons :workspace-read-octets (need read-octets "READ-OCTETS"))
                     (cons :workspace-home-directory (need home-directory "HOME-DIRECTORY"))
                     (cons :workspace-call-with-ordered-mapper
                           (need call-with-ordered-mapper "CALL-WITH-ORDERED-MAPPER"))
                     (cons :environment environment)
                     (cons :working-directory working-directory)))
        (setf context
              (cl-boundary-kit:boundary-context-with
               context (car capability) (cdr capability))))
      context)))

(define-workspace-operation
 host-list-directory
 :workspace-list-directory
 (path)
 "Return (VALUES entries readable-p): PATH children as WORKSPACE-ENTRY
values using lstat semantics, in any order, and omit the . and .. entries.
READABLE-P is NIL when PATH is missing, not a directory, or unreadable.")

(define-workspace-operation
 host-stat
 :workspace-stat
 (path)
 "Return the lstat WORKSPACE-ENTRY for PATH, named by its base name, or NIL.")

(define-workspace-operation
 host-read-link
 :workspace-read-link
 (path)
 "Return the raw symlink target string for PATH, or NIL when PATH is not a
readable symbolic link.")

(define-workspace-operation
 host-read-octets
 :workspace-read-octets
 (path)
 "Return PATH bytes as an (unsigned-byte 8) vector, or NIL when PATH is
missing or unreadable.")

(defun host-getenv (boundaries name)
  "Return a non-empty environment value for NAME, or NIL."
  (let ((value
          (cl-boundary-kit:environment-get
           (cl-boundary-kit:boundary-context-require boundaries :environment)
           name)))
    (and value (plusp (length value)) value)))

(define-workspace-operation
 host-home-directory
 :workspace-home-directory
 ()
 "Return the absolute home-directory string without a trailing slash.")

(defun host-current-directory (boundaries)
  "Return the absolute current-directory string without a trailing slash."
  (normalize-path
   (cl-boundary-kit:working-directory-get
    (cl-boundary-kit:boundary-context-require boundaries :working-directory))))

(define-workspace-operation
 host-call-with-ordered-mapper
 :workspace-call-with-ordered-mapper
 (thunk)
 "Call THUNK with a mapper (FUNCTION ITEMS) that applies FUNCTION to ITEMS
and returns results in input order. The default maps sequentially; production
uses a CPU-sized pool.")

;;;;
;;;; Workspace flows receive a cl-boundary-kit context directly. The context
;;;; carries the kit's environment and working-directory objects plus the
;;;; filesystem operations whose metadata and byte semantics the kit does not
;;;; expose.
;;;;
;;;; Every path argument is an absolute, normalized POSIX string.
(in-package #:aitools.workspace.application)

(defmacro define-workspace-operation (name key arguments)
  `(defun ,name (boundaries ,@arguments)
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

(define-workspace-operation host-list-directory :workspace-list-directory (path))

(define-workspace-operation host-stat :workspace-stat (path))

(define-workspace-operation host-read-link :workspace-read-link (path))

(define-workspace-operation host-read-octets :workspace-read-octets (path))

(defun host-getenv (boundaries name)
  (let ((value
          (cl-boundary-kit:environment-get
           (cl-boundary-kit:boundary-context-require boundaries :environment)
           name)))
    (and value (plusp (length value)) value)))

(define-workspace-operation host-home-directory :workspace-home-directory ())

(defun host-current-directory (boundaries) (let ((directory (cl-boundary-kit:working-directory-get (cl-boundary-kit:boundary-context-require boundaries :working-directory)))) (normalize-path (if (stringp directory) directory (sb-ext:native-namestring directory)))))

(define-workspace-operation host-call-with-ordered-mapper :workspace-call-with-ordered-mapper (thunk))

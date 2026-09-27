;;;; packages/core/workspace/src/application/host.lisp
;;;;
;;;; The WORKSPACE-HOST port: every effect the workspace flows need, as one
;;;; record of closures (ports are defined in application,
;;;; implemented in infrastructure, and replaced by fakes in unit tests). The
;;;; granularity is one call per directory or per file, never per line or
;;;; per pattern. The infrastructure layer wraps the production
;;;; instance as a cl-boundary-kit boundary for the composition root.
;;;;
;;;; Every path argument is an absolute, normalized POSIX string.
(in-package #:aitools.workspace.application)

(defstruct (workspace-host (:constructor %make-workspace-host) (:copier nil))
  (list-directory nil :type function :read-only t)
  (stat nil :type function :read-only t)
  (read-link nil :type function :read-only t)
  (read-octets nil :type function :read-only t)
  (getenv nil :type function :read-only t)
  (home-directory nil :type function :read-only t)
  (current-directory nil :type function :read-only t)
  (call-with-ordered-mapper nil :type function :read-only t))

(defun make-workspace-host (&key list-directory stat read-link read-octets getenv
                                 home-directory current-directory
                                 (call-with-ordered-mapper
                                  (lambda (thunk) (funcall thunk (lambda (function items) (mapcar function items))))))
  "Build a WORKSPACE-HOST. Each argument is a function:

LIST-DIRECTORY (path) -> (VALUES entries readable-p): the WORKSPACE-ENTRY
  list of PATH's children (lstat semantics, any order, no `.`/`..`);
  READABLE-P is NIL when PATH is missing, not a directory, or unreadable.
STAT (path) -> WORKSPACE-ENTRY or NIL: lstat of PATH, named by its base name.
READ-LINK (path) -> the raw symlink target string, or NIL.
READ-OCTETS (path) -> the file's bytes as an (unsigned-byte 8) vector, or NIL
  when it is missing or unreadable.
GETENV (name) -> string or NIL. HOME-DIRECTORY () and CURRENT-DIRECTORY () ->
  absolute directory strings without a trailing `/`.
CALL-WITH-ORDERED-MAPPER (thunk) calls THUNK with a mapper (function items)
  that returns FUNCTION applied to each of ITEMS, results in ITEMS order.
  The default maps sequentially; production runs a CPU-sized pool."
  (flet ((need (value name)
           (unless (functionp value)
             (error "make-workspace-host: ~A must be a function, got ~S" name value))
           value))
    (%make-workspace-host
     :list-directory (need list-directory "LIST-DIRECTORY")
     :stat (need stat "STAT")
     :read-link (need read-link "READ-LINK")
     :read-octets (need read-octets "READ-OCTETS")
     :getenv (need getenv "GETENV")
     :home-directory (need home-directory "HOME-DIRECTORY")
     :current-directory (need current-directory "CURRENT-DIRECTORY")
     :call-with-ordered-mapper (need call-with-ordered-mapper "CALL-WITH-ORDERED-MAPPER"))))

(defun host-list-directory (host path)
  (funcall (workspace-host-list-directory host) path))

(defun host-stat (host path)
  (funcall (workspace-host-stat host) path))

(defun host-read-link (host path)
  (funcall (workspace-host-read-link host) path))

(defun host-read-octets (host path)
  (funcall (workspace-host-read-octets host) path))

(defun host-getenv (host name)
  (let ((value (funcall (workspace-host-getenv host) name)))
    (and value (plusp (length value)) value)))

(defun host-home-directory (host)
  (funcall (workspace-host-home-directory host)))

(defun host-current-directory (host)
  (funcall (workspace-host-current-directory host)))

(defun host-call-with-ordered-mapper (host thunk)
  (funcall (workspace-host-call-with-ordered-mapper host) thunk))

;;;; packages/feature/edit/src/infrastructure/ports.lisp
;;;;
;;;; MAKE-PRODUCTION-EDIT-PORTS, which the composition root calls with the
;;;; shared host, store constructor and text source. Construction does no
;;;; I/O, so building the ports costs nothing at process start.
(in-package #:aitools.edit.infrastructure)

(defun %read-bounded (stream limit)
  "(values octets too-large-p): STREAM to its end, reading at most LIMIT+1
bytes so the limit bounds the read itself."
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (chunk (make-array 65536 :element-type '(unsigned-byte 8))))
    (loop for count = (read-sequence chunk stream :end (min (length chunk) (- (1+ limit) (fill-pointer buffer))))
          until (zerop count)
          do (loop for index below count do (vector-push-extend (aref chunk index) buffer))
             (when (> (fill-pointer buffer) limit)
               (return-from %read-bounded (values nil t))))
    (values (coerce buffer '(simple-array (unsigned-byte 8) (*))) nil)))

(defun read-stdin-octets (limit &key on-octets on-too-large on-failure)
  "Standard input (file descriptor 0) as raw bytes, independent of the Lisp
external format; read only when a command was given --stdin."
  (declare (type function on-octets on-too-large on-failure))
  (multiple-value-bind (octets too-large)
      (handler-case (%read-bounded (sb-sys:make-fd-stream 0 :input t :element-type '(unsigned-byte 8) :buffering :full)
                                   limit)
        (error (condition) (return-from read-stdin-octets (funcall on-failure (princ-to-string condition)))))
    (if too-large (funcall on-too-large) (funcall on-octets octets))))

(defun unix-now ()
  (aitools.kernel.domain:universal-time-to-unix-seconds (get-universal-time)))

(defun make-production-edit-ports (&key workspace-host open-store text-source &allow-other-keys)
  (aitools.edit.application:make-edit-ports
   :workspace-host workspace-host
   :open-store open-store
   :text-source text-source
   :read-stdin-octets #'read-stdin-octets
   :unix-now #'unix-now))

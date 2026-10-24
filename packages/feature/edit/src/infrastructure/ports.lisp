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
    (loop
      for
      count
      =
      (read-sequence chunk stream :end (min (length chunk) (- (1+ limit) (fill-pointer buffer))))
      until
      (zerop count)
      do
      (loop
        for
        index
        below
        count
        do
        (vector-push-extend (aref chunk index) buffer))
      (when (> (fill-pointer buffer) limit)
        (return-from %read-bounded
          (values nil t))))
    (values (coerce buffer '(simple-array (unsigned-byte 8) (*))) nil)))

(defun read-stdin-octets (limit)
  "Read file descriptor 0 as raw octets, independent of the character external format."
  (handler-case
      (multiple-value-bind (octets too-large)
          (%read-bounded
           (sb-sys:make-fd-stream 0 :input t :element-type (quote (unsigned-byte 8))
                                  :buffering :full)
           limit)
        (if too-large
            (values :too-large nil)
            (values :octets octets)))
    (error (condition)
      (values :failure (princ-to-string condition)))))

(defun unix-now ()
  (aitools.kernel.domain:universal-time-to-unix-seconds (get-universal-time)))

(defun make-production-edit-ports (&key workspace-host open-store text-source &allow-other-keys)
  (aitools.edit.application:make-edit-ports
   :workspace-host workspace-host
   :open-store open-store
   :text-source text-source
   :read-stdin-octets (function read-stdin-octets)
   :unix-now (function unix-now)
   :serialize-document (function serialize-document)))

(defun %yaml-native-value (value)
  (case (aitools.edit.domain:json-value-kind value)
    (:object
     (yaml-kit:make-yaml-mapping
      (mapcar (lambda (member)
                (cons (car member)
                      (%yaml-native-value (cdr member))))
              (aitools.edit.domain:json-obj-members value))))
    (:array
     (map (quote simple-vector) (function %yaml-native-value) value))
    (:number (aitools.edit.domain:edit-json-number value))
    (:null yaml-kit:+yaml-null+)
    (:false yaml-kit:+yaml-false+)
    (otherwise value)))

(defun %toml-native-value (value)
  (case (aitools.edit.domain:json-value-kind value)
    (:object
     (let ((table (make-hash-table :test (function equal))))
       (dolist (member (aitools.edit.domain:json-obj-members value) table)
         (setf (gethash (car member) table)
               (%toml-native-value (cdr member))))))
    (:array
     (map (quote simple-vector) (function %toml-native-value) value))
    (:number (aitools.edit.domain:edit-json-number value))
    (:false cl-toml-kit:+toml-false+)
    (:null (error "validated TOML value contains null"))
    (otherwise value)))

(defun serialize-document (format value)
  (cond
    ((string= format "yaml")
     (yaml-kit:emit (%yaml-native-value value)))
    ((string= format "toml")
     (cl-toml-kit:encode (%toml-native-value value)))
    (t (error "unsupported YAML/TOML format: ~S" format))))

;;;; t/unit/env/support.lisp
;;;;
;;;; Deterministic ENV-PORTS for the flow tests: cl-boundary-kit fakes for the
;;;; clock, environment, filesystem, and host info, wired through the same
;;;; MAKE-ENV-PORTS-FROM-BOUNDARIES the production ports use, plus recorded
;;;; fakes for the adapters cl-boundary-kit has no boundary for.
(in-package #:aitools.env.test)

(defun octets-from-hex (hex)
  (let ((octets (make-array (floor (length hex) 2) :element-type '(unsigned-byte 8))))
    (dotimes (index (length octets) octets)
      (setf (aref octets index) (parse-integer hex :start (* index 2) :end (+ (* index 2) 2) :radix 16)))))

(defun latin-1-from-hex (hex)
  "The file content as the latin-1 string READ-FILE returns for it."
  (map 'string #'code-char (octets-from-hex hex)))

(defparameter *zoneinfo-files*
  (list (cons "/usr/share/zoneinfo/America/New_York" (latin-1-from-hex *new-york-tzif-hex*))
        (cons "/usr/share/zoneinfo/Asia/Tokyo" (latin-1-from-hex *tokyo-tzif-hex*))))

(defparameter *fake-epoch-milliseconds* 1772951400250
  "2026-03-08T06:30:00.250Z, i.e. 01:30:00.250 EST in New York, half an
hour before that day's spring-forward.")

(defstruct (fake-host (:copier nil))
  (runs nil)
  (programs nil))

(defun make-fake-ports (&key (now *fake-epoch-milliseconds*) (environment '(("TZ" . "UTC")))
                          (files *zoneinfo-files*) (directories '("/usr/share/zoneinfo"))
                          (links nil) (executables nil)
                          (identity '("Darwin" "25.6.0" "arm64"))
                          (space '(1000 400)) (host (make-fake-host)))
  "ENV-PORTS over fakes. FILES is an alist path -> content; DIRECTORIES is an
alist path -> entry names, or a bare path for an existing directory whose
entries do not matter; LINKS maps path -> target; EXECUTABLES lists
executable paths. HOST's PROGRAMS maps a program path to a function of
(ARGUMENTS TIMEOUT) returning the RUN-PROGRAM values; every call is
appended to HOST's RUNS as (PROGRAM ARGUMENTS)."
  (aitools.env.infrastructure:make-env-ports-from-boundaries
   :clock (cl-boundary-kit:make-fake-clock :start now)
   :environment (cl-boundary-kit:make-test-environment :initial-values environment)
   :filesystem (cl-boundary-kit:make-test-filesystem :initial-files files)
   :host-info (cl-boundary-kit:make-test-host-info :hostname "fake-host" :username "tester")
   :run-program (lambda (program arguments timeout)
                  (setf (fake-host-runs host) (append (fake-host-runs host) (list (list program arguments))))
                  (let ((behaviour (cdr (assoc program (fake-host-programs host) :test #'string=))))
                    (if behaviour
                        (funcall behaviour arguments timeout)
                        (values :not-started nil nil nil))))
   :executable-p (lambda (path) (and (member path executables :test #'string=) t))
   :list-directory (lambda (path)
                     (loop for entry in directories
                           when (and (stringp entry) (string= entry path)) return (list "entry")
                           when (and (consp entry) (string= (car entry) path)) return (cdr entry)))
   :read-link (lambda (path) (cdr (assoc path links :test #'string=)))
   :file-system-space (lambda (path) (declare (ignore path)) (values-list space))
   :system-identity (lambda () (values-list identity))
   :user-id (lambda () 1000)
   :workspace-root (lambda () "/work")))

(defun exits-with (stdout &key (exit-code 0) (stderr ""))
  (lambda (arguments timeout)
    (declare (ignore arguments timeout))
    (values :exited exit-code stdout stderr)))

(defun run-flow (function &rest arguments)
  "Call FUNCTION (a /k flow) with ARGUMENTS and return (VALUES KIND FIELDS):
KIND :OK, :PARTIAL, or :ERROR; FIELDS the alist, or for :ERROR the list
(CODE MESSAGE . KEYWORDS)."
  (block done
    (apply function (append arguments
                            (list :on-ok (lambda (fields) (return-from done (values :ok fields)))
                                  :on-error (lambda (code message &rest keys)
                                              (return-from done (values :error (list* code message keys)))))
                            (when (member function (list #'sys-procs/k))
                              (list :on-partial (lambda (fields) (return-from done (values :partial fields)))))))
    (error "flow ~S returned without calling a continuation" function)))

(defun field (fields name)
  (cdr (assoc name fields :test #'string=)))

(defun object-field (object name)
  (cdr (assoc name (aitools.protocol.domain:json-object-members object) :test #'string=)))

(defun json-null-p (value)
  (eq value (json-null)))

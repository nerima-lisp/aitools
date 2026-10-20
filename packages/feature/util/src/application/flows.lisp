;;;; packages/feature/util/src/application/flows.lisp
;;;;
;;;; Shared constants and flow helpers for the util command flows.
(in-package #:aitools.util.application)

(defparameter +util-codec-schemes+ aitools.util.domain:+codec-schemes+)

(defparameter +util-random-alphabets+ (mapcar #'car aitools.util.domain:+random-alphabets+)
  "Alphabet names; the first is the `--alphabet` default.")

(defparameter +util-uuid-kinds+ '("v4" "v7"))

(defconstant +util-default-decimals+ 10)

(defconstant +util-max-count+ 1000
  "Largest `--count` for `util uuid` and `util random`.")

(defconstant +util-max-random-length+ 4096
  "Largest `--length` for `util random`.")

(defun %schema-repair (command)
  (schema-repair (format nil "aitools schema ~A" command)))

(defun %input-text (octets text)
  (or text (aitools.util.domain:octets->lenient-text octets)))

(aitools.kernel.domain:define-flow
    (%choice-error option value choices command &key on-error)
  (aitools.kernel.domain:flow-continue
   :on-error "argument.invalid"
   (format nil "~A must be one of ~{~A~^, ~}, not ~S" option choices value)
   :repairs (list (%schema-repair command))))

(aitools.kernel.domain:define-flow
    (%write-decoded ports scheme input decoded to
     &key root lock-timeout dry-run tx display-argv on-ok on-error)
  "Write DECODED to TO as a new file; reject existing targets with the `refusal.exists` error."
  (let ((outcome nil))
    (aitools.edit.application:run-write-command/k
     (aitools.edit.application:make-write-edit-ports
      :workspace-host (util-ports-workspace-host ports)
      :open-store (util-ports-open-store ports))
     (aitools.edit.application:make-write-plan
      :command "util.decode"
      :targets (list (aitools.edit.application:make-write-target to))
      :inputs (list input decoded)
      :record-options (lambda (paths) (list :to (first paths)))
      :record-positionals (lambda (paths)
                            (declare (ignore paths))
                            (list scheme))
      :plan (lambda (context &key on-committed on-rejected)
              (let ((path (aitools.edit.application:context-path context)))
                (if (aitools.store.domain:entry-state-absent-p
                     (aitools.store.application:view-path-state
                      (aitools.edit.application:write-context-view context)
                      path))
                    (funcall on-committed
                             (list
                              (aitools.store.domain:write-file-request path decoded))
                             (list (cons "bytes" (length decoded))))
                    (funcall on-rejected
                             "refusal.exists"
                             (format nil
                                     "~A already exists; util decode --to writes only a new file"
                                     path))))))
     :root root :lock-timeout lock-timeout :dry-run dry-run :tx tx
     :display-argv display-argv
     :on-ok (lambda (fields)
              (setf outcome (list :ok fields)))
     :on-error (lambda (code message
                        &key candidates diagnostics conflicts repairs)
                (setf outcome
                      (list :error code message candidates diagnostics conflicts repairs))))
    (ecase (first outcome)
      (:ok
       (aitools.kernel.domain:flow-continue :on-ok (second outcome)))
      (:error
       (aitools.kernel.domain:flow-continue
        :on-error (second outcome) (third outcome)
        :candidates (fourth outcome)
        :diagnostics (fifth outcome)
        :conflicts (sixth outcome)
        :repairs (seventh outcome))))))

(aitools.kernel.domain:define-flow
    (%count-error command &key on-error)
  (aitools.kernel.domain:flow-continue
   :on-error "argument.invalid"
   (format nil "--count must be between 1 and ~D" +util-max-count+)
   :repairs
   (list
    (repair
     "use-default" "Generate one value."
     (format nil "aitools ~A --count 1" command)))))

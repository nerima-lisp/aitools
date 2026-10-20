(in-package #:aitools.env.presentation)

;;; --------------------------------------------------------------------- time

(defun %time-now-command (ports)
  (make-command
   :name "now"
   :description "Show the current time."
   :options (list (%tz-option))
   :handler
   (lambda (invocation)
     (%call-flow
      (lambda (on-ok on-partial on-error)
        (declare (ignore on-partial))
        (aitools.kernel.domain:flow-call
            (aitools.env.application:time-now/k
             ports :tz (option-value invocation :tz))
          (:on-ok (fields)
            (funcall on-ok fields))
          (:on-error (code message &key repairs)
            (funcall on-error code message :repairs repairs))))))))

(defun %time-convert-command (ports)
  (make-command
   :name "convert"
   :description "Convert a time between formats and zones."
   :positionals
   (list (make-positional :name "value" :key :value :required-p t))
   :options
   (list
    (make-option
     :name "to" :kind :value :default "iso8601"
     :choices '("iso8601" "epoch_ms" "epoch_s")
     :description "Output format.")
    (make-option
     :name "add" :kind :value :multiple-p t :value-name "DURATION"
     :description "Add a duration (repeatable).")
    (make-option
     :name "sub" :kind :value :multiple-p t :value-name "DURATION"
     :description "Subtract a duration (repeatable).")
    (%tz-option))
   :handler
   (lambda (invocation)
     (%call-flow
      (lambda (on-ok on-partial on-error)
        (declare (ignore on-partial))
        (aitools.kernel.domain:flow-call
            (aitools.env.application:time-convert/k
             ports
             (positional-value invocation :value)
             :to (option-value invocation :to)
             :add (option-value invocation :add)
             :sub (option-value invocation :sub)
             :tz (option-value invocation :tz))
          (:on-ok (fields)
            (funcall on-ok fields))
          (:on-error (code message &key repairs)
            (funcall on-error code message :repairs repairs))))))))

(defun %time-diff-command (ports)
  (make-command
   :name "diff"
   :description "Difference between two times."
   :positionals
   (list
    (make-positional :name "a" :key :a :required-p t)
    (make-positional :name "b" :key :b :required-p t))
   :handler
   (lambda (invocation)
     (%call-flow
      (lambda (on-ok on-partial on-error)
        (declare (ignore on-partial))
        (aitools.kernel.domain:flow-call
            (aitools.env.application:time-diff/k
             ports
             (positional-value invocation :a)
             (positional-value invocation :b))
          (:on-ok (fields)
            (funcall on-ok fields))
          (:on-error (code message &key repairs)
            (funcall on-error code message :repairs repairs))))))))

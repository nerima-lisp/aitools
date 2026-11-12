(in-package #:aitools.env.application)

(defvar *snapshot-deadline* nil)

(define-condition snapshot-deadline-exceeded (error) ())

(defun %snapshot-monotonic-time (boundaries)
  (cl-boundary-kit:clock-monotonic
   (cl-boundary-kit:boundary-context-require boundaries :env-clock)))

(defun %snapshot-remaining-seconds (boundaries)
  (when *snapshot-deadline*
    (/ (- *snapshot-deadline* (%snapshot-monotonic-time boundaries))
       internal-time-units-per-second)))

(defun %check-snapshot-deadline (boundaries)
  (when (and *snapshot-deadline*
             (not (plusp (%snapshot-remaining-seconds boundaries))))
    (error 'snapshot-deadline-exceeded)))
(defun %call-with-snapshot-deadline (boundaries thunk)
  (if (null *snapshot-deadline*)
      (funcall thunk)
      (progn
        (%check-snapshot-deadline boundaries)
        (unwind-protect
             (funcall thunk)
          (%check-snapshot-deadline boundaries)))))

(defun %call-with-env-operation-deadline (boundaries key operation arguments)
  (%call-with-snapshot-deadline
   boundaries
   (lambda ()
     (cond
       ((eq key :env-run-program)
        (let* ((requested (getf (cddr arguments) :timeout))
               (remaining (%snapshot-remaining-seconds boundaries)))
          (unless (and (realp requested) (plusp requested))
            (error "An explicit positive process timeout is required."))
          (funcall operation
                   (first arguments)
                   (second arguments)
                   :timeout (if remaining
                                (progn
                                  (unless (plusp remaining)
                                    (error 'snapshot-deadline-exceeded))
                                  (min requested remaining))
                                requested))))
       ((eq key :env-sleep-seconds)
        (let ((remaining (%snapshot-remaining-seconds boundaries)))
          (funcall operation
                   (if remaining
                       (progn
                         (unless (plusp remaining)
                           (error 'snapshot-deadline-exceeded))
                         (min (first arguments) remaining))
                       (first arguments)))))
       ((eq key :env-list-directory)
        (funcall operation
                 (first arguments)
                 :check-deadline (lambda () (%check-snapshot-deadline boundaries))))
       ((eq key :env-workspace-root)
        (funcall operation
                 :check-deadline (lambda () (%check-snapshot-deadline boundaries))))
       (t (apply operation arguments))))))

(defun %snapshot-timeout-seconds (text)
  (when (stringp text)
    (handler-case
        (let* ((milliseconds
                 (aitools.kernel.domain:duration-milliseconds
                  (aitools.kernel.domain:parse-duration text)))
               (seconds (/ milliseconds 1000)))
          (when (and (plusp milliseconds)
                     (< seconds most-positive-double-float))
            seconds))
      (aitools.kernel.domain:invalid-duration-error () nil))))

(in-package #:aitools.integration.structure-test)

(defvar *liveness-index-diagnostics* nil)

(defun %liveness-index-static-builder-p (builder allow-lambda-p)
  (and (consp builder)
       (symbolp (first builder))
       (member (%liveness-symbol-name (first builder))
               (if allow-lambda-p '("FUNCTION" "QUOTE" "LAMBDA")
                   '("FUNCTION" "QUOTE"))
               :test #'string=)))

(defun %liveness-index-form-position (record operator cursor fallback)
  (or (loop for candidate in
              (%liveness-form-positions (getf record :code) operator)
            when (>= candidate cursor)
              return candidate)
      fallback))

(defun %liveness-index-scan-builder (record position object context depth)
  (if (not (consp object))
      position
      (let* ((head (first object))
             (operator (and (symbolp head) (%liveness-symbol-name head)))
             (next-position position))
        (cond
          ((eq head :u05-quasiquote)
           (case context
             (:active
              (%liveness-index-scan-builder record position (second object)
                                            :template 1))
             (:template
              (%liveness-index-scan-builder record position (second object)
                                            :template (1+ depth)))
             (otherwise
              (%liveness-index-scan-builder record position (second object)
                                            :inactive 0))))
          ((member head '(:u05-unquote :u05-unquote-splicing))
           (case context
             (:template
              (if (= depth 1)
                  (%liveness-index-scan-builder record position (second object)
                                                :active 0)
                  (%liveness-index-scan-builder record position (second object)
                                                :template (1- depth))))
             (otherwise
              (%liveness-index-scan-builder record position (second object)
                                            :inactive 0))))
          (t
           (when (and operator
                      (member operator '("DEFINE-COMMAND"
                                         "DEFINE-INSPECT-COMMAND")
                              :test #'string=))
             (let ((at (%liveness-index-form-position record operator position
                                                      position)))
               (setf next-position (1+ at))
               (when (eq context :active)
                 (cond
                   ((string= operator "DEFINE-COMMAND")
                    (let* ((options (third object))
                           (builder
                             (loop for tail = options then (cdr tail)
                                   while (consp tail)
                                   when (eq (first tail) :command-builder)
                                     return (second tail))))
                      (unless (%liveness-index-static-builder-p builder t)
                        (%liveness-index-lookup-diagnostic
                         record at "unresolved command builder"))))
                   (t
                    (unless (%liveness-index-static-builder-p
                             (fourth object) nil)
                      (%liveness-index-lookup-diagnostic
                       record at "unresolved inspect command builder")))))))
           (if (and operator (string= operator "QUOTE"))
               (setf next-position
                     (%liveness-index-scan-builder
                      record next-position (second object)
                      (if (eq context :active) :inactive context) depth))
               (loop for tail = object then (cdr tail)
                     while (consp tail)
                     do (setf next-position
                              (%liveness-index-scan-builder
                               record next-position (car tail) context depth))))
           next-position)))))

(defun %liveness-index-lookup-diagnostic (record position message-or-form)
  (when (eq (getf record :kind) :production)
    (if (stringp message-or-form)
        (push (format nil "~A:~D: ~A"
                      (getf record :relative)
                      (%line-at (getf record :text) position)
                      message-or-form)
              *liveness-index-diagnostics*)
        (%liveness-index-scan-builder record position message-or-form
                                      :active 0))))

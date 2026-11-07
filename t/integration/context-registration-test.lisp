(in-package #:aitools/test)

(defun %feature-presentation-context (system-name)
  (when (stringp system-name)
    (let* ((prefix "aitools/feature/")
           (suffix "/presentation")
           (suffix-start (- (length system-name) (length suffix))))
      (when (and (>= suffix-start (length prefix))
                 (string= prefix system-name :end2 (length prefix))
                 (string= suffix system-name :start2 suffix-start))
        (intern (string-upcase (subseq system-name (length prefix) suffix-start))
                :keyword)))))

(defun %declared-feature-presentation-contexts ()
  (loop for system-name in (asdf:system-depends-on
                            (asdf:find-system "aitools/cli"))
        for context = (%feature-presentation-context system-name)
        when context
          collect context))

(describe "Feature command registration"
  (it "registers every feature presentation in CLI dependency order"
    (let ((expected (%declared-feature-presentation-contexts))
          (actual
           (mapcar (function first)
                   (aitools/cli::context-command-registrations))))
      (expect expected :to-be-truthy)
      (expect actual :to-equal expected)))
  (it "registers commands from each context dependency constructor"
    (let* ((registry (aitools.protocol.application:make-command-registry))
           (schemas
             (progn
               (aitools/cli:register-all-context-commands registry)
               (aitools.protocol.application:all-command-schemas registry))))
      (expect schemas :to-be-truthy)
      (let ((aitools/cli::*context-command-registrations* nil))
        (eval '(aitools/cli::define-context-command-registration
                :coverage-fixture #'identity #'identity))
        (expect (mapcar #'first (aitools/cli::context-command-registrations))
                :to-equal '(:coverage-fixture))
        (eval '(aitools/cli::define-context-command-registration
                :coverage-fixture #'car #'cdr))
        (let ((registration
                (first (aitools/cli::context-command-registrations))))
          (expect (first registration) :to-equal :coverage-fixture)
          (expect (eq (second registration) #'car) :to-be t)
          (expect (eq (third registration) #'cdr) :to-be t)))
      (let* ((with-span
               (getf (aitools/cli::%context-dependency-arguments) :with-span))
             (arguments (list 0 :payload)))
        (let ((aitools/cli::*trace-session* nil))
          (expect (multiple-value-list
                   (funcall with-span :io "coverage" #'values arguments))
                  :to-equal '(0 :payload)))
        (let ((aitools/cli::*trace-session* (aitools/cli::%make-trace-session)))
          (expect (multiple-value-list
                   (funcall with-span :io "coverage" #'values arguments))
                  :to-equal '(0 :payload)))))))

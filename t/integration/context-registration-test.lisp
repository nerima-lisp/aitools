(in-package #:aitools/test)

(defvar *context-registration-provider-calls* 0)

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
    (let ((expected (list :search :inspect :edit :journal :process :org :env :util))
          (declared (%declared-feature-presentation-contexts))
          (actual
           (mapcar (function first)
                   (aitools/cli::context-command-registrations))))
      (expect declared :to-equal expected)
      (expect actual :to-equal expected)))
  (it "registers commands from each context dependency constructor"
    (let* ((registry (aitools.protocol.application:make-command-registry))
           (schemas
             (progn
               (aitools/cli:register-all-context-commands registry)
               (aitools.protocol.application:all-command-schemas registry))))
      (expect schemas :to-be-truthy)
      (let ((aitools/cli::*context-command-registrations* nil)
            (*context-registration-provider-calls* 0))
        (eval '(aitools/cli::define-context-command-registration
                :coverage-fixture
                (progn (incf *context-registration-provider-calls*) #'identity)
                (progn (incf *context-registration-provider-calls*) #'identity)))
        (expect *context-registration-provider-calls* :to-equal 0)
        (eval '(aitools/cli::define-context-command-registration
                :coverage-fixture
                (progn (incf *context-registration-provider-calls*) #'car)
                (progn (incf *context-registration-provider-calls*) #'cdr)))
        (expect *context-registration-provider-calls* :to-equal 0)
        (let ((registrations (aitools/cli::context-command-registrations)))
          (expect *context-registration-provider-calls* :to-equal 2)
          (expect (mapcar #'first registrations) :to-equal '(:coverage-fixture))
          (let ((registration (first registrations)))
            (expect (first registration) :to-equal :coverage-fixture)
            (expect (eq (second registration) #'car) :to-be t)
            (expect (eq (third registration) #'cdr) :to-be t))))
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

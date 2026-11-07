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
  (it "reloads each composition registration without changing context order"
    (let ((expected (%declared-feature-presentation-contexts)))
      (aitools.test.support:call-with-temporary-directory
       (lambda (directory)
         (dolist (context expected)
           (let* ((context-name (string-downcase (symbol-name context)))
                  (system-name
                    (format nil "aitools/feature/~A/presentation" context-name))
                  (source
                    (asdf:component-pathname
                     (asdf:find-component
                      system-name (list "composition" "context-registration"))))
                  (output-file
                    (merge-pathnames
                     (make-pathname :name context-name :type "fasl")
                     directory)))
             (multiple-value-bind (compiled-file warnings-p failure-p)
                 (compile-file source :output-file output-file)
               (declare (ignore warnings-p))
               (when failure-p
                 (error "Failed to compile feature composition registration for ~A."
                        context))
               (load compiled-file))))))
      (expect (mapcar (function first)
                      (aitools/cli::context-command-registrations))
              :to-equal expected)))
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

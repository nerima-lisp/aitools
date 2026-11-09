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

(describe "Context test source attribution"
  (it "matches edit and environment tests loaded from a different root"
    (let* ((source-files (%context-test-source-files))
           (paths '("t/unit/edit/domain-test.lisp"
                    "t/integration/env-sysctl-test.lisp")))
      (aitools.test.support:call-with-temporary-directory
       (lambda (directory)
         (dolist (path paths)
           (let* ((context (if (search "edit/" path) :edit :env))
                  (source-file
                    (cdr (find-if (lambda (entry)
                                    (and (eq (car entry) context)
                                         (search path (cdr entry) :from-end t)))
                                  source-files))))
             (expect source-file :to-be-truthy)
             (when source-file
               (let* ((marker (search "/t/" source-file :from-end t))
                      (relative (subseq source-file (+ marker 3)))
                      (alternate (merge-pathnames
                                  (format nil "nix-store/source/t/~A" relative)
                                  directory)))
                 (aitools.test.support:write-text (namestring alternate) "")
                 (expect (%test-context-from-source-file
                          (namestring alternate) source-files)
                         :to-equal context))))))
       :prefix "aitools-context-source-alias"))))

(describe "Feature command registration"
  (it "registers every feature presentation in CLI dependency order"
    (let ((expected (list :search :inspect :edit :journal :process :org :env :md :util :net))
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

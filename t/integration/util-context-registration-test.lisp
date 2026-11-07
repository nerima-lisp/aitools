(in-package #:cl-user)

(defpackage #:aitools.integration.util-context-registration-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect))

(in-package #:aitools.integration.util-context-registration-test)

(describe "aitools util context registration"
  (it "registers util commands from the composition dependencies"
    (let ((aitools/cli::*context-command-registrations* nil))
      (expect (find-package "AITOOLS.UTIL.COMPOSITION") :to-be-truthy)
      (progn
        (expect (delete-package "AITOOLS.UTIL.COMPOSITION") :to-be-truthy)
        (expect (not (find-package "AITOOLS.UTIL.COMPOSITION")) :to-be-truthy))
      (aitools.test.support:call-with-temporary-directory
       (lambda (directory)
         (dolist (component-name (list "package" "context-registration"))
           (let* ((source
                   (asdf:component-pathname
                    (asdf:find-component
                     "aitools/feature/util/presentation"
                     (list "composition" component-name))))
                  (output-file
                   (merge-pathnames
                    (make-pathname :name component-name :type "fasl")
                    directory)))
             (multiple-value-bind (compiled-file warnings-p failure-p)
                 (compile-file source :output-file output-file)
               (declare (ignore warnings-p))
               (when failure-p
                 (error "Failed to compile util composition component ~A."
                        component-name))
               (load compiled-file))))))
      (let* ((registration
               (find :util (aitools/cli::context-command-registrations)
                     :key #'first))
             (registry (aitools.protocol.application:make-command-registry)))
        (expect (find-package "AITOOLS.UTIL.COMPOSITION") :to-be-truthy)
        (expect registration :to-be-truthy)
        (funcall (second registration)
                 registry
                 (apply (third registration)
                        (aitools/cli::%context-dependency-arguments)))
        (expect (mapcar #'aitools.protocol.domain:command-schema-name
                        (aitools.protocol.application:all-command-schemas registry))
                :to-equal '("util.encode" "util.decode" "util.redact" "util.tokens"
                            "util.calc" "util.uuid" "util.random"))))))

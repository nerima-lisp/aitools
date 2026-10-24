(in-package #:aitools.process.integration-test)

(describe "aitools detached-process trace port (integration)"
  (it "preserves unavailable launcher results and assigns the fixed span outcome"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (let* ((observed nil)
             (ports
               (aitools.process.infrastructure:make-production-process-ports
                :with-span
                (lambda (kind name operation arguments &key outcome-function)
                  (let* ((values (multiple-value-list (apply operation arguments)))
                         (outcome (funcall outcome-function values)))
                    (push (list kind name outcome) observed)
                    (values-list values)))))
             (sleep-path (program-on-path "sleep"))
             (log (merge-pathnames "bg-1.log" directory))
             (exit (merge-pathnames "bg-1.exit" directory)))
        (call-with-launcher-environment
         directory
         (lambda ()
           (multiple-value-bind (status message program)
               (call-port ports 'aitools.process.application::process-ports-launch-detached
                          (list sleep-path "30") log exit)
             (expect status :to-be :unavailable)
             (expect (starts-with-p "bg start needs the cl-process-kit-spawn helper" message) :to-be t)
             (expect program :to-equal "cl-process-kit-spawn")))
         :override "" :path "")
        (expect observed :to-equal
                '((:child_process "process.launch-detached" :unavailable)))))
    :prefix "aitools-process")))

(in-package #:aitools.edit.test)

(defun make-edit-test-boundaries (&rest capabilities)
  (let ((boundaries (cl-boundary-kit:make-boundary-context)))
    (loop for (key value) on capabilities by (function cddr)
          do (setf boundaries
                   (cl-boundary-kit:boundary-context-with boundaries key value)))
    boundaries))

(defun call-with-edit-workspace (function)
  (aitools.test.support:call-with-workspace
   (lambda (root home)
     (let ((*root* root)
           (*home* home)
           (*stdin* nil)
           (*stdin-outcome* nil)
           (*boundaries*
            (cl-weave:with-mocked-functions
                (((symbol-function (quote aitools.edit.infrastructure:read-stdin-octets))
                  (lambda (limit)
                    (expect limit :to-be aitools.edit.application:+max-input-bytes+)
                    (case *stdin-outcome*
                      (:too-large (values :too-large nil))
                      (:failure (values :failure "read failed"))
                      (otherwise
                       (if *stdin*
                           (values :octets *stdin*)
                           (values :failure "the test did not expect stdin to be read")))))))
              (make-edit-test-boundaries
               :edit-workspace-host (aitools.workspace.infrastructure:make-host-workspace-boundaries)
               :edit-open-store (function open-store)
               :edit-text-source (aitools.text.infrastructure:make-host-text-source)
               :edit-read-stdin-octets
               (symbol-function (quote aitools.edit.infrastructure:read-stdin-octets))
               :edit-unix-now (function aitools.edit.infrastructure:unix-now)
               :edit-serialize-document (function aitools.edit.infrastructure::serialize-document)))))
       (funcall function)))
   :chdir t))

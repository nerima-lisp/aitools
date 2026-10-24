(in-package #:aitools.org.application)
(defun %org-write-inputs (options)
  (loop for (key value) on options by #'cddr
        when (stringp value) collect value))
(defun %org-reject-write-failure (failure command path)
  (multiple-value-bind (code message diagnostics repairs)
      (%failure-response failure command)
    (list :rejected code message diagnostics repairs path)))
(defun %org-apply-write-edit (ports path octets operation options command)
  (multiple-value-bind (document failure)
      (funcall (org-ports-parse-org ports) octets :source-name path)
    (if failure
        (%org-reject-write-failure failure command path)
        (multiple-value-bind (updated fields edit-failure)
            (funcall (org-ports-edit-org ports)
                     (aitools.org.domain:org-document-engine-value document)
                     operation options)
          (if edit-failure
              (%org-reject-write-failure edit-failure command path)
              (multiple-value-bind (output write-failure)
                  (funcall (org-ports-write-org ports) updated)
                (if write-failure
                    (%org-reject-write-failure write-failure command path)
                    (let ((changed (not (equalp octets output))))
                      (list :committed
                            (if changed
                                (list (aitools.store.domain:write-file-request
                                       path output))
                                '())
                            (append fields
                                    (list (cons "changed"
                                                (aitools.protocol.domain:json-boolean
                                                 changed)))))))))))))
(defun %org-plan-write (ports context operation options command)
  (let* ((view (aitools.edit.application:write-context-view context))
         (path (aitools.edit.application:context-path context))
         (state (aitools.store.application:view-path-state view path)))
    (cond
      ((aitools.store.domain:entry-state-absent-p state)
       (list :rejected "input.not-found" "Org file does not exist"
             nil nil path))
      ((not (eq (aitools.store.domain:entry-state-kind state) :file))
       (list :rejected "refusal.not-a-file" "Org target is not a regular file"
             nil nil path))
      (t
       (let ((octets (aitools.store.application:view-read-file view path)))
         (if octets
             (if (find 0 octets)
                 (%org-reject-write-failure
                  (aitools.org.domain:make-org-failure
                   :kind :unsupported
                   :message "Org input contains a NUL byte")
                  command path)
                 (%org-apply-write-edit ports path octets operation options command))
             (list :rejected "input.not-found" "Org file does not exist"
                   nil nil path)))))))
(defun %org-write-plan (ports path command operation options)
  (aitools.edit.application:make-write-plan
   :command command
   :targets (list (aitools.edit.application:make-write-target path))
   :inputs (%org-write-inputs options)
   :replayable nil
   :record-options (lambda (paths)
                     (declare (ignore paths))
                     options)
   :record-positionals (lambda (paths) (list (first paths)))
   :plan (lambda (context &key on-committed on-rejected)
           (let ((result (%org-plan-write ports context operation options command)))
             (ecase (first result)
               (:committed
                (funcall on-committed (second result) (third result)))
               (:rejected
                (destructuring-bind (tag code message diagnostics repairs path)
                    result
                  (declare (ignore tag))
                  (funcall on-rejected code message
                           :diagnostics diagnostics
                           :repairs repairs
                           :path path))))))))
(aitools.kernel.domain:define-flow
    (%org-write-result ports path command operation options &key root lock-timeout dry-run tx display-argv expect-count expect-hash
       on-ok on-error on-partial)
  (declare (ignore expect-count expect-hash on-partial))
  (cond
    ((or (not (stringp path)) (zerop (length path)))
     (aitools.kernel.domain:flow-continue
      :on-error "argument.invalid" "an Org file path is required"
      :repairs (%schema-repair command "Pass an Org file path.")))
    ((or (null (org-ports-open-store ports))
         (null (org-ports-edit-org ports))
         (null (org-ports-write-org ports)))
     (aitools.kernel.domain:flow-continue
      :on-error "internal.unexpected" "the Org write ports are unavailable"
      :repairs (%schema-repair command "Show this command's requirements.")))
    (t
     (let ((outcome nil))
       (aitools.edit.application:run-write-command/k
        (aitools.edit.application:make-write-edit-ports
         :workspace-host (org-ports-workspace-host ports)
         :open-store (org-ports-open-store ports))
        (%org-write-plan ports path command operation options)
        :root root :lock-timeout lock-timeout :dry-run dry-run :tx tx
        :display-argv display-argv
        :on-ok (lambda (fields) (setf outcome (list :ok fields)))
        :on-error (lambda (code message &key candidates diagnostics conflicts repairs)
                    (setf outcome
                          (list :error code message candidates diagnostics
                                conflicts repairs))))
       (ecase (first outcome)
         (:ok
          (aitools.kernel.domain:flow-continue :on-ok (second outcome)))
         (:error
          (aitools.kernel.domain:flow-continue
           :on-error (second outcome) (third outcome)
           :candidates (fourth outcome) :diagnostics (fifth outcome)
           :conflicts (sixth outcome) :repairs (seventh outcome))))))))
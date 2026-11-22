(in-package #:aitools.org.application)

(defun %archive-path-segments (path)
  (when (plusp (length path))
    (loop for start = 0 then (1+ end)
          for end = (position #\/ path :start start)
          collect (subseq path start end)
          while end)))

(defun %archive-relative-path (source destination)
  (let ((directory (butlast (%archive-path-segments destination)))
        (parts (%archive-path-segments source)))
    (loop while (and directory parts (string= (first directory) (first parts)))
          do (pop directory) (pop parts))
    (format nil "~{~A~^/~}" (append (make-list (length directory) :initial-element "..") parts))))

(defun %archive-plan (boundaries context options)
  (let* ((view (aitools.edit.application:write-context-view context))
         (source (aitools.edit.application:context-path context 0))
         (target (aitools.edit.application:context-path context 1))
         (same (string= source target))
         (source-state (aitools.store.application:view-path-state view source))
         (target-state (aitools.store.application:view-path-state view target)))
    (cond
      ((aitools.store.domain:entry-state-absent-p source-state)
       (list :rejected "input.not-found" "Org source does not exist" nil nil source))
      ((not (eq (aitools.store.domain:entry-state-kind source-state) :file))
       (list :rejected "refusal.not-a-file" "Org source is not a regular file" nil nil source))
      ((and (not (aitools.store.domain:entry-state-absent-p target-state))
            (not (eq (aitools.store.domain:entry-state-kind target-state) :file)))
       (list :rejected "refusal.not-a-file" "Org destination is not a regular file" nil nil target))
      (t
       (let* ((source-bytes (aitools.store.application:view-read-file view source))
              (target-bytes (if same source-bytes
                                (if (aitools.store.domain:entry-state-absent-p target-state)
                                    (make-array 0 :element-type '(unsigned-byte 8))
                                    (aitools.store.application:view-read-file view target))))
              (parse (cl-boundary-kit:boundary-context-require boundaries :org-parse))
              (write (cl-boundary-kit:boundary-context-require boundaries :org-write)))
         (when (or (find 0 source-bytes) (find 0 target-bytes))
           (return-from %archive-plan (%org-reject-write-failure (aitools.org.domain:make-org-failure :kind :unsupported :message "Org input contains a NUL byte") "org.archive" source)))
         (multiple-value-bind (source-doc failure) (funcall parse source-bytes :source-name (%archive-relative-path source target))
           (when failure (return-from %archive-plan (%org-reject-write-failure failure "org.archive" source)))
           (multiple-value-bind (target-doc failure) (if same (values source-doc nil) (funcall parse target-bytes :source-name target))
             (when failure (return-from %archive-plan (%org-reject-write-failure failure "org.archive" target)))
             (multiple-value-bind (source-result target-result failure)
                 (funcall (cl-boundary-kit:boundary-context-require boundaries :org-archive)
                          (aitools.org.domain:org-document-engine-value source-doc)
                          (aitools.org.domain:org-document-engine-value target-doc) options same)
               (when failure (return-from %archive-plan (%org-reject-write-failure failure "org.archive" source)))
               (multiple-value-bind (source-output failure) (funcall write source-result)
                 (when failure (return-from %archive-plan (%org-reject-write-failure failure "org.archive" source)))
                 (multiple-value-bind (target-output failure) (if same (values source-output nil) (funcall write target-result))
                   (when failure (return-from %archive-plan (%org-reject-write-failure failure "org.archive" target)))
                   (list :committed
                         (if same
                             (list (aitools.store.domain:write-file-request source source-output))
                             (list (aitools.store.domain:write-file-request source source-output)
                                   (aitools.store.domain:write-file-request target target-output)))
                         (list (cons "changed" (aitools.protocol.domain:json-boolean t))))))))))))))

(aitools.kernel.domain:define-flow
 (org-archive/k boundaries &key path to target-heading match heading-path at tz dst-policy root lock-timeout dry-run tx display-argv expect-count expect-hash on-ok on-error on-partial)
 (declare (ignore expect-count expect-hash on-partial))
 (if (or (zerop (length target-heading))
         (find-if (lambda (character) (find character '(#\Newline #\Return))) target-heading))
     (aitools.kernel.domain:flow-continue
      :on-error "argument.invalid" "--target-heading must be a nonempty single-line title"
      :repairs (%schema-repair "org archive" "Pass a nonempty single-line heading title."))
     (let* ((options (list :to to :target-heading target-heading :match match :heading-path heading-path :at at :tz tz :dst-policy dst-policy))
            (plan (aitools.edit.application:make-write-plan
                   :command "org.archive"
                   :targets (list (aitools.edit.application:make-write-target path) (aitools.edit.application:make-write-target to))
                   :inputs (%org-write-inputs options) :replayable nil
                   :record-options (lambda (paths) (let ((copy (copy-list options))) (setf (getf copy :to) (second paths)) copy))
                   :record-positionals (lambda (paths) (list (first paths)))
                   :plan (lambda (context &key on-committed on-rejected)
                           (let ((result (%archive-plan boundaries context options)))
                             (ecase (first result)
                               (:committed (funcall on-committed (second result) (third result)))
                               (:rejected (destructuring-bind (tag code message diagnostics repairs rejected-path) result
                                            (declare (ignore tag))
                                            (funcall on-rejected code message :diagnostics diagnostics :repairs repairs :path rejected-path))))))))
            (outcome nil))
       (aitools.edit.application:run-write-command/k
        (aitools.edit.application:make-edit-write-boundaries (cl-boundary-kit:boundary-context-require boundaries :org-workspace-host) (cl-boundary-kit:boundary-context-require boundaries :org-open-store))
        plan :root root :lock-timeout lock-timeout :dry-run dry-run :tx tx :display-argv display-argv
        :on-ok (lambda (fields) (setf outcome (list :ok fields)))
        :on-error (lambda (code message &key candidates diagnostics conflicts repairs) (setf outcome (list :error code message candidates diagnostics conflicts repairs))))
       (ecase (first outcome)
         (:ok (aitools.kernel.domain:flow-continue :on-ok (second outcome)))
         (:error (aitools.kernel.domain:flow-continue :on-error (second outcome) (third outcome) :candidates (fourth outcome) :diagnostics (fifth outcome) :conflicts (sixth outcome) :repairs (seventh outcome)))))))

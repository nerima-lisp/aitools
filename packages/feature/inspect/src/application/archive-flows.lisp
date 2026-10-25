;;;;
;;;; `archive list` returns archive members; `archive read` returns the text
;;;; view of one selected member with line selectors.
(in-package #:aitools.inspect.application)

(defun %archive-list-command (context path)
  (command-line context (list "archive" "list" path)))

(aitools.kernel.domain:define-flow
 (call-with-archive/k
  context
  target
  &key
  on-archive
  on-error
  (max-gzip-output +archive-max-decompressed+))
 "Read TARGET's bytes and open them as an archive: ON-ARCHIVE (archive), or
the matching error through ON-ERROR."
 (declare (type function on-archive on-error))
 (let ((path (file-target-argument target))
       (absolute (file-target-absolute target)))
   (multiple-value-bind (octets problem) (read-target-octets context target)
     (if (null octets)
         (if (eq problem :unreadable)
             (aitools.kernel.domain:flow-continue :on-error
                                                  "environment.io"
                                                  (format nil
                                                          "file ~A cannot be read: permission denied"
                                                          path)
                                                  :repairs
                                                  (list
                                                   (repair "describe"
                                                           "Show the file's mode and owner to fix its permissions."
                                                           (command-line context
                                                                         (list
                                                                          "info"
                                                                          path)
                                                                         :tx
                                                                         :none))))
           (aitools.kernel.domain:flow-continue :on-error
                                                "input.not-found"
                                                (format nil
                                                        "file ~A does not exist"
                                                        path)
                                                :candidates
                                                (missing-path-candidates
                                                 context
                                                 absolute)
                                                :repairs
                                                (list
                                                 (repair "find"
                                                         "List nearby files."
                                                         (command-line context
                                                                       (list
                                                                        "find"
                                                                        (path-basename
                                                                         absolute))
                                                                       :tx
                                                                       :none)))))
       (aitools.kernel.domain:flow-call
        (open-archive/k octets absolute :max-gzip-output max-gzip-output)
        (:on-archive (archive)
                     (aitools.kernel.domain:flow-continue
                      :on-archive
                      archive))
        (:on-unsupported ()
                         (aitools.kernel.domain:flow-continue
                          :on-error
                          "input.unsupported-format"
                          (format nil
                                  "~A is not a zip, tar, tar.gz, or gz archive"
                                  path)
                          :repairs
                          (list
                           (repair "describe"
                                   "Check the file's type."
                                   (command-line
                                    context
                                    (list
                                     "info"
                                     path))))))
        (:on-malformed (reason)
                       (aitools.kernel.domain:flow-continue
                        :on-error
                        "input.unsupported-format"
                        (format nil
                                "~A cannot be read as an archive: ~A"
                                path
                                reason)
                        :repairs
                        (list
                         (repair "describe"
                                 "Check the file's type."
                                 (command-line
                                  context
                                  (list "info"
                                        path))))))
        (:on-too-large (limit)
                       (aitools.kernel.domain:flow-continue
                        :on-error
                        "refusal.too-large"
                        (format nil
                                "~A decompresses past the ~D-byte limit"
                                path
                                limit)
                        :repairs
                        (list
                         (repair "list"
                                 "List the members instead."
                                 (%archive-list-command
                                  context
                                  path))))))))))

(aitools.kernel.domain:define-flow
 (archive-list-flow ports
                    path
                    &key
                    root
                    tx
                    lock-timeout
                    (limit 200)
                    on-ok
                    on-partial
                    on-error)
 "Archive list command. Calls ON-OK, ON-PARTIAL (fields), or ON-ERROR."
 (declare (type function on-ok on-partial on-error))
 (aitools.kernel.domain:flow-call
  (call-with-inspect-file/k ports path
                            :root root
                            :tx tx
                            :lock-timeout lock-timeout)
  (:on-file (context target)
    (aitools.kernel.domain:flow-call
     (call-with-archive/k context target)
     (:on-archive (archive)
       (multiple-value-bind (fields truncated)
           (%archive-list-result path archive limit)
         (if truncated
             (aitools.kernel.domain:flow-continue :on-partial fields)
             (aitools.kernel.domain:flow-continue :on-ok fields))))
     (:on-error (code message &key candidates diagnostics conflicts repairs)
       (aitools.kernel.domain:flow-continue
        :on-error code message
        :candidates candidates
        :diagnostics diagnostics
        :conflicts conflicts
        :repairs repairs))))
  (:on-error (code message &key candidates diagnostics conflicts repairs)
    (aitools.kernel.domain:flow-continue
     :on-error code message
     :candidates candidates
     :diagnostics diagnostics
     :conflicts conflicts
     :repairs repairs))))

(defun %archive-list-result (path archive limit)
  (let* ((items (archive-items archive))
         (truncated (> (length items) limit))
         (shown
          (if truncated
              (subseq items 0 limit)
              items)))
    (values
     (append
      (list (cons "path" path)
            (cons "format" (archive-format-name (archive-format archive)))
            (cons "items" (mapcar #'archive-item-json shown))
            (cons "total" (length items))
            (cons "truncated" (json-bool truncated)))
      (when truncated
        (list
         (cons "next_commands"
               (list
                (format nil
                        "aitools archive list ~A --limit ~D"
                        (shell-quote path)
                        (length items)))))))
     truncated)))

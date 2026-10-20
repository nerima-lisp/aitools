(in-package #:aitools.edit.application)

(defun %archive-member (context host root-path entry)
  "The ARCHIVE-MEMBER for scan ENTRY, read through the write's view."
  (let* ((path (aitools.workspace.application:scan-entry-path entry))
         (kind (aitools.workspace.application:scan-entry-kind entry))
         (view (write-context-view context))
         (mtime (max 0 (or (aitools.workspace.application:scan-entry-mtime entry) 0)))
         (mode (logand (or (aitools.workspace.application:scan-entry-mode entry) #o644) #o7777)))
    (ecase kind
      (:directory (aitools.text.domain:make-archive-member :name path :kind :directory :mode mode :mtime mtime))
      (:file (aitools.text.domain:make-archive-member
              :name path :kind :file :mode mode :mtime mtime
              :data (coerce (or (aitools.store.application:view-read-file view path)
                                (aitools.workspace.application:host-read-octets
                                 host (aitools.workspace.domain:join-path root-path path)))
                            'octets)))
      (:symlink (aitools.text.domain:make-archive-member
                 :name path :kind :symlink :mode mode :mtime mtime
                 :link-target (aitools.store.domain:entry-state-target
                               (aitools.store.application:view-path-state view path)))))))

(define-write-command "archive.create"
                      (ports env positionals options on-plan fail)
                      (let* ((path (first positionals))
                             (sources (rest positionals))
                             (format
                              (if (getf options :format)
                              (parse-archive-format
                               (getf options :format))
                              (and path
                                   (archive-format-for-path path)))))
                      (cond
                      ((or (null path) (null sources))
                       (funcall fail
                                "argument.invalid"
                                "archive create takes PATH and at least one SRC"))
                      ((null format)
                       (funcall fail
                                "argument.invalid"
                                (format nil
                                        "cannot tell the format of ~A; pass --format zip|tar|tar.gz|gz"
                                        path)))
                      (t
                       (scan-files/k env
                                     (mapcar
                                      (lambda (source)
                                      (aitools.workspace.application:user-path-absolute
                                       (command-env-host env)
                                       source))
                                      sources)
                                     options
                                     fail
                                     (lambda (entries)
                                     (cond
                                     ((null entries)
                                      (funcall fail
                                               "selection.no-match"
                                               "no files to archive (ignored files need --no-ignore)"))
                                     ((and (eq format :gz)
                                           (or (rest entries)
                                               (not
                                                (eq
                                                 (aitools.workspace.application:scan-entry-kind
                                                  (first entries))
                                                 :file))))
                                      (funcall fail
                                               "argument.invalid"
                                               "a .gz archive holds exactly one file; use tar.gz for several"))
                                     (t
                                      (funcall on-plan
                                               (make-write-plan
                                                :command
                                                "archive.create"
                                                :targets
                                                (list
                                                 (make-write-target
                                                  path))
                                                :plan
                                                (lambda
                                                (context commit
                                                         reject)
                                                (let ((target
                                                       (context-path
                                                        context)))
                                                (if (not
                                                     (aitools.store.domain:entry-state-absent-p
                                                      (aitools.store.application:view-path-state
                                                       (write-context-view
                                                        context)
                                                       target)))
                                                (funcall reject
                                                         "refusal.exists"
                                                         (format nil
                                                                 "~A already exists"
                                                                 target))
                                                (let* ((host
                                                        (command-env-host
                                                         env))
                                                       (root-path
                                                        (aitools.workspace.application:workspace-root-real
                                                         (command-env-root
                                                          env)))
                                                       (members
                                                        (loop for entry in entries
                                                              unless (string=
                                                                      (aitools.workspace.application:scan-entry-path
                                                                       entry)
                                                                      target)
                                                              collect (%archive-member
                                                                       context
                                                                       host
                                                                       root-path
                                                                       entry))))
                                                (funcall commit
                                                         (list
                                                          (aitools.store.domain:write-file-request
                                                           target
                                                           (build-archive
                                                            format
                                                            members
                                                            :name
                                                            (and
                                                             (eq
                                                              format
                                                              :gz)
                                                             (aitools.workspace.domain:path-basename
                                                              (concatenate
                                                               'string
                                                               "/"
                                                               (aitools.text.domain:archive-member-name
                                                                (first
                                                                 members))))))))
                                                         (list
                                                          (cons
                                                           "format"
                                                           (aitools.text.domain:archive-format-name
                                                            format))
                                                          (cons
                                                           "entries"
                                                           (length
                                                            members))))))))
                                                :record-options
                                                options
                                                :record-positionals
                                                (lambda (paths)
                                                (list* (first paths)
                                                       sources)))))))
                                     :kinds
                                     '(:file :directory :symlink))))))


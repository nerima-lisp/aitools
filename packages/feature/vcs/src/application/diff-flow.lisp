(in-package #:aitools.vcs.application)

(defun %diff-command (root path staged ref max-lines)
  "The hunks-mode `git diff` command line; stat mode never truncates or reruns
the patch, so no suggested command carries `--output stat`."
  (%git-command root "diff" path (when staged "--staged") (when ref "--ref") ref
                "--max-lines" (princ-to-string max-lines)))

(aitools.kernel.domain:define-flow
    (git-diff/k port &key root path staged ref (output :hunks) (max-lines 400)
                  on-ok on-partial on-error)
  "Changed files with line counts, and with OUTPUT :HUNKS their hunks
until MAX-LINES rendered lines; later files come back as mode \"summary\"
and the result is PARTIAL."
  (if (and ref (%option-like-p ref))
      (aitools.kernel.domain:flow-continue
       :on-error
       "argument.invalid"
       (format nil "--ref must name a revision, not an option: ~A" ref)
       :repairs
       (list (repair "diff-range" "Compare two revisions."
                     (%git-command root "diff" "--ref" "HEAD~1..HEAD"))))
      (let ((port (port-at-root port root))
            (repairs
              (list (repair "list-commits" "Find a revision to compare against."
                            (%git-command root "log"))))
            (mode (if (eq output :stat) "stat" "hunks")))
        (aitools.kernel.domain:flow-call
            (%call-in-repository port root path)
          (:on-repository (port repository-path top directory)
            (let ((arguments
                    (append *diff-arguments*
                            (when staged '("--cached"))
                            (when ref (list ref))
                            (%pathspec repository-path))))
              (labels ((response-fields (files characters next-commands)
                         (append
                          (when repository-path
                            (list (cons "path" repository-path)))
                          (list (cons "mode" mode)
                                (cons "files" files)
                                (cons "truncated"
                                      (aitools.protocol.domain:json-boolean next-commands))
                                (cons "approx_tokens"
                                      (aitools.kernel.domain:approx-token-count characters)))))
                       (complete (files characters)
                         (aitools.kernel.domain:flow-continue
                          :on-ok
                          (%finish-fields
                           (response-fields files characters nil))))
                       (truncated (files characters summary-path summary-lines)
                         (let ((next-command
                                 (%diff-command
                                  root
                                  (aitools.vcs.domain:path-from-directory
                                   summary-path top directory)
                                  staged ref (max max-lines summary-lines))))
                           (aitools.kernel.domain:flow-continue
                            :on-partial
                            (%finish-fields
                             (response-fields
                              files characters (list next-command))
                             (list next-command)))))
                       (shape (records patches)
                         (aitools.kernel.domain:flow-call
                             (aitools.vcs.domain:diff-files/k
                              records patches :output output :max-lines max-lines)
                           (:on-complete (files characters)
                             (complete files characters))
                           (:on-truncated
                               (files characters summary-path summary-lines)
                             (truncated
                              files characters summary-path summary-lines)))))
                (funcall (git-port-numstat port)
                         arguments
                         :on-failure
                         (lambda (kind message)
                           (aitools.kernel.domain:flow-call
                               (%fail kind message :not-found-repairs repairs)
                             (:on-error (code text &key repairs)
                               (aitools.kernel.domain:flow-continue
                                :on-error code text :repairs repairs))))
                         :on-success
                         (lambda (records)
                           (if (eq output :stat)
                               (shape records nil)
                               (aitools.kernel.domain:flow-call
                                   (%run-git
                                    port "diff"
                                    (append
                                     '("--src-prefix=a/" "--dst-prefix=b/" "-U3")
                                     arguments)
                                    :not-found-repairs repairs)
                                 (:on-success (text)
                                   (let ((patches
                                           (aitools.vcs.domain:split-patch-by-file
                                            text)))
                                     ;; Two git runs can observe different work-tree states.
                                     (if (= (length patches) (length records))
                                         (shape records patches)
                                         (aitools.kernel.domain:flow-continue
                                          :on-error
                                          "environment.io"
                                          "the diff changed between git runs; run the command again"
                                          :repairs
                                          (list
                                           (repair
                                            "retry"
                                            "Run the same diff again."
                                            (%diff-command
                                             root path staged ref max-lines)))))))
                                 (:on-error (code message &key repairs)
                                   (aitools.kernel.domain:flow-continue
                                    :on-error code message :repairs repairs)))))))))
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message :repairs repairs))))))

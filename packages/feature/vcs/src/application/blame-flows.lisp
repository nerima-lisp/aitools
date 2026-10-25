;;;;
;;;; `git blame` flow using the inspect context's public selector API.
;;;; Shared flow helpers are defined in flows.lisp.
(in-package #:aitools.vcs.application)

(aitools.kernel.domain:define-flow
    (%call-with-selector command subcommand selector-options &key on-selector on-error)
  "Parse selector options for COMMAND and return the selector, or NIL."
  (apply #'aitools.inspect.application:parse-selector-options/k command
         :on-selector (lambda (selector)
                        (aitools.kernel.domain:flow-continue :on-selector selector))
         :on-none (lambda ()
                    (aitools.kernel.domain:flow-continue :on-selector nil))
         :on-invalid (lambda (message repairs)
                       (declare (ignore repairs))
                       (aitools.kernel.domain:flow-continue
                        :on-error "argument.invalid" message
                        :repairs
                        (list
                         (repair
                          "use-one-selector"
                          "Check which selectors this command takes."
                          (format nil "aitools schema git.~A" subcommand)))))
         selector-options))

(aitools.kernel.domain:define-flow
    (%resolve-selection lines selector path root subcommand target &key on-selected on-error)
  "Resolve SELECTOR against LINES and report any failed selection with a repair."
  (flet ((range-repair (action detail range)
           (list (repair action detail (%git-command root subcommand target "--range" range)))))
    (aitools.inspect.application:resolve-selector/k
     lines selector
     :path path
     :on-selected (lambda (ranges)
                    (aitools.kernel.domain:flow-continue :on-selected ranges))
     :on-no-match
     (lambda (candidates)
       (aitools.kernel.domain:flow-continue
        :on-error "selection.no-match"
        (format nil "the selector matched nothing in ~A" path)
        :candidates candidates
        :repairs
        (range-repair
         "read-lines" "Select lines that exist."
         (format nil "1:~D"
                 (max 1 (min (length lines) *blame-default-lines*))))))
     :on-ambiguous
     (lambda (candidates)
       (aitools.kernel.domain:flow-continue
        :on-error "selection.ambiguous"
        (format nil "the selector matched ~D places in ~A; it must match one"
                (length candidates) path)
        :candidates candidates
        :repairs
        (range-repair
         "use-range" "Select one match by its line range."
         (let ((first (first candidates)))
           (format nil "~D:~D"
                   (%candidate-field first "line")
                   (%candidate-field first "end_line"))))))
     :on-invalid
     (lambda (code message)
       (aitools.kernel.domain:flow-continue
        :on-error code message
        :repairs
        (range-repair "use-range" "Select lines by number instead." "1:80"))))))

(defun %candidate-field (candidate name)
  (cdr (assoc name (aitools.protocol.domain:json-object-members candidate) :test #'string=)))

(aitools.kernel.domain:define-flow
    (git-blame/k port path &rest selector-options
                  &key root range symbol kind between exclusive match invert
                       on-ok on-partial on-error)
  "Per-line blame of the work-tree PATH, limited by one selector
(all but --old). Without a selector, the first
*BLAME-DEFAULT-LINES* lines, PARTIAL when the file is longer."
  (declare (ignore range symbol kind between exclusive match invert))
  (let ((port (port-at-root port root)))
    (aitools.kernel.domain:flow-call
        (%call-with-selector
         :git-blame "blame" (%selector-options selector-options))
      (:on-selector (selector)
        (aitools.kernel.domain:flow-call
            (%call-in-repository port root path)
          (:on-repository (port repository-path top directory)
            (declare (ignore top directory))
            (aitools.kernel.domain:flow-call
                (%run-git
                 port "blame" (list "--porcelain" "--" repository-path)
                 :not-found-repairs
                 (list
                  (repair
                   "check-status" "Blame needs a path git tracks; list changes."
                   (%git-command root "status"))))
              (:on-success (text)
                (let ((entries nil))
                  (flet ((collect (n sha author date line-text)
                           (push (list n sha author date line-text) entries)))
                    (declare (dynamic-extent #'collect))
                    (aitools.vcs.domain:map-blame-lines text #'collect))
                  (let* ((entries
                           (coerce (sort entries #'< :key #'first) 'simple-vector))
                         (total (length entries)))
                    (labels ((line-object (number)
                               (apply #'aitools.vcs.domain:blame-line
                                      (svref entries (1- number))))
                             (respond (numbers start truncated next-range)
                               (let* ((fields
                                        (list (cons "path" repository-path)
                                              (cons "start_line" start)
                                              (cons "lines" (mapcar #'line-object numbers))
                                              (cons "total_lines" total)
                                              (cons "truncated"
                                                    (aitools.protocol.domain:json-boolean
                                                     truncated))))
                                      (next-commands
                                        (when truncated
                                          (list
                                           (%git-command root "blame" path
                                                         "--range" next-range)))))
                                 (if truncated
                                     (aitools.kernel.domain:flow-continue
                                      :on-partial
                                      (%finish-fields fields next-commands))
                                     (aitools.kernel.domain:flow-continue
                                      :on-ok (%finish-fields fields))))))
                      (if (null selector)
                          (aitools.kernel.domain:flow-call
                              (aitools.vcs.domain:line-window/k
                               total :max-lines *blame-default-lines*)
                            (:on-window (first last truncated next-range)
                              (respond
                               (loop for n from first to last collect n)
                               first truncated next-range)))
                          (aitools.kernel.domain:flow-call
                              (%resolve-selection
                               (map 'simple-vector #'fifth entries)
                               selector repository-path root "blame" path)
                            (:on-selected (ranges)
                              (let ((numbers
                                      (loop for (start . end) in ranges
                                            append
                                            (loop for n from start to end collect n))))
                                (respond numbers (car (first ranges)) nil nil)))
                            (:on-error (code message &key candidates repairs)
                              (aitools.kernel.domain:flow-continue
                               :on-error code message :candidates candidates
                               :repairs repairs))))))))
              (:on-error (code message &key repairs)
                (aitools.kernel.domain:flow-continue
                 :on-error code message :repairs repairs))))
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message :repairs repairs))))
      (:on-error (code message &key repairs)
        (aitools.kernel.domain:flow-continue
         :on-error code message :repairs repairs)))))

(defun %selector-options (options)
  "The selector keys of OPTIONS, a flow's keyword arguments."
  (loop for (key value) on options by #'cddr
        when (member key '(:range :symbol :kind :between :exclusive :match :invert))
          append (list key value)))



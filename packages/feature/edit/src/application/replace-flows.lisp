;;;; packages/feature/edit/src/application/replace-flows.lisp
;;;;
;;;; `replace`: the multi-file write that validates every file
;;;; before the store writes any. Its file scan is in scan.lisp, and `apply`
;;;; in apply-flows.lisp.
(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
  (compile-pattern/k pattern &key fixed word ignore-case on-error on-regex)
  "PATTERN compiled, or ON-ERROR input.syntax-error."
  (declare (ignore on-error))
  (aitools.kernel.domain:flow-call
    (compile-search-pattern/k pattern :fixed fixed :word word :ignore-case ignore-case)
    (:on-regex (regex)
               (aitools.kernel.domain:flow-continue
                 :on-regex
                 regex))
    (:on-invalid (message)
                 (aitools.kernel.domain:flow-continue
                   :on-error
                   "input.syntax-error"
                   message))))

(aitools.kernel.domain:define-flow
  (%replacement-function/k replacement regex literal &key on-error on-function)
  "The regex replacement function for REPLACEMENT: verbatim when LITERAL,
else the parsed replacement template. `\\N` naming an existing group is refused
(:on-error via ON-ERROR with the repair rewritten to ${N})."
  (declare (type function on-error on-function))
  (if literal
      (aitools.kernel.domain:flow-continue
        :on-function
        (literal-replacement replacement))
      (let ((perl (perl-backreferences replacement (regex-group-count regex))))
        (if perl
            (aitools.kernel.domain:flow-continue
              :on-error
              "argument.invalid"
              (format nil
                      (concatenate 'string
                                   "the replacement writes \\~D, which cl-regex-kit copies literally; "
                                   "write ${~D} (or pass --literal-replacement for a literal backslash)")
                      (first perl)
                      (first perl))
              :rewrite
              (rewrite-perl-backreferences replacement))
            (multiple-value-bind (parts condition)
                (handler-case (values (parse-replacement-template replacement) nil)
                  (edit-refusal (condition)
                    (values nil condition)))
              (if condition
                  (aitools.kernel.domain:flow-continue
                    :on-error
                    (edit-refusal-code condition)
                    (edit-refusal-detail condition))
                  (aitools.kernel.domain:flow-continue
                    :on-function
                    (template-regex-replacement parts))))))))

(aitools.kernel.domain:define-flow
  (%replace-with-targets env paths options replacer selector pattern replacement record-options &key on-error on-plan)
  "PATHS are user paths or recorded workspace-relative paths during tx rebase."
  (flet ((plan (files explicit skipped)
           (aitools.kernel.domain:flow-continue
             :on-plan
             (make-write-plan :command
                              "replace"
                              :targets
                              (mapcar
                               (function
                                (lambda (file)
                                  (make-write-target file :base :root)))
                               files)
                              :inputs
                              (list pattern replacement)
                              :guard-requirements
                              (list* (list :expect-count) (selector-guards selector (first files)))
                              :expect-hashes
                              (getf options :expect-hash)
                              :expect-count
                              (getf options :expect-count)
                              :replayable
                              (and (content-selector-p selector) (null (getf options :expect-hash)))
                              :plan
                              (%replace-plan replacer (getf options :multiline) selector explicit skipped)
                              :record-options
                              record-options
                              :record-positionals
                              (if (or (getf options :stdin)
                                      (getf options :stdin-data)
                                      (getf record-options :stdin-data))
                                  (lambda (resolved)
                                    resolved)
                                  (lambda (resolved)
                                    (list* pattern replacement resolved)))))))
    (if (null env)
        (plan paths paths nil)
        (let* ((host (command-env-host env))
               (absolute
                (mapcar
                 (function
                  (lambda (path)
                    (aitools.workspace.application:user-path-absolute host path)))
                 paths))
               (relative
                (mapcar
                 (lambda (path)
                   (aitools.workspace.application:resolve-user-path/k host
                                                                      (command-env-root env)
                                                                      path
                                                                      :on-inside
                                                                      (lambda (absolute relative)
                                                                        (declare (ignore absolute))
                                                                        relative)
                                                                      :on-outside
                                                                      (function identity)))
                 paths))
               (explicit
                (loop
                  for
                  path
                  in
                  absolute
                  for
                  path-relative
                  in
                  relative
                  for
                  entry
                  =
                  (aitools.workspace.application:host-stat host path)
                  when
                  (and entry (eq (aitools.workspace.application:workspace-entry-kind entry) :file))
                  collect
                  path-relative)))
          (if (and paths (= (length explicit) (length paths)))
              (plan relative explicit nil)
              (let ((skipped nil))
                (aitools.kernel.domain:flow-call
                  (scan-files/k env
                                absolute
                                options
                                :skip-handler
                                (lambda (entry reason)
                                  (when (eq reason :too-large)
                                    (push
                                     (aitools.protocol.domain:json-object-from-alist
                                      (list (cons "path" (aitools.workspace.application:scan-entry-path entry))
                                            (cons "reason" "too-large")))
                                     skipped))))
                  (:on-error (code message &key candidates diagnostics conflicts repairs path)
                             (flow-continue :on-error
                                            code
                                            message
                                            :candidates
                                            candidates
                                            :diagnostics
                                            diagnostics
                                            :conflicts
                                            conflicts
                                            :repairs
                                            repairs
                                            :path
                                            path))
                  (:on-files (entries)
                             (let ((files (mapcar (function aitools.workspace.application:scan-entry-path) entries)))
                               (cond
                                 ((null files)
                                  (aitools.kernel.domain:flow-continue
                                    :on-error
                                    "selection.no-match"
                                    "no files to search"
                                    :path
                                    (first paths)))
                                 ((and selector (/= (length files) 1))
                                  (aitools.kernel.domain:flow-continue
                                    :on-error
                                    "argument.invalid"
                                    "a selector needs exactly one file path"))
                                 (t (plan files (intersection files explicit :test (function equal)) skipped))))))))))))

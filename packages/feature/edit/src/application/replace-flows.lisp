;;;; packages/feature/edit/src/application/replace-flows.lisp
;;;;
;;;; `replace`: the multi-file write that validates every file
;;;; before the store writes any. Its file scan is in scan.lisp, and `apply`
;;;; in apply-flows.lisp.
(in-package #:aitools.edit.application)

;;; ---------------------------------------------------------------- replace

(defun compile-pattern/k (pattern fail on-regex &key fixed word ignore-case)
  "PATTERN compiled, or FAIL input.syntax-error."
  (declare (type function fail on-regex))
  (compile-search-pattern/k pattern :fixed fixed :word word :ignore-case ignore-case
                                     :on-regex on-regex
                                     :on-invalid (lambda (message) (funcall fail "input.syntax-error" message))))

(defun %replace-plan (replacer multiline selector explicit skipped)
  "EXPLICIT: paths the user named as files, whose unreadable or non-text
state is an error; scanned files in that state are skipped."
  (lambda (context commit reject)
    (block plan
      (let ((requests '()) (per-change '()) (total 0)
            ;; A byte-level required-literal check, so a file lacking
            ;; the pattern's literal is skipped without decoding. Sound only
            ;; where a miss means zero replacements and no error, so it is
            ;; applied only to scanned (non-explicit) files and dropped when a
            ;; selector restricts the lines. A file that clears it is still
            ;; matched exactly as before.
            (literal (and (null selector) (replacer-required-literal replacer))))
        (dolist (path (write-context-paths context))
          (block one
            (flet ((skip-or-reject (&rest rejection)
                     (if (member path explicit :test #'equal)
                         (return-from plan (apply reject rejection))
                         (return-from one))))
              (read-document/k
               context path #'skip-or-reject
               (lambda (document)
                 (flet ((replace-in (ranges)
                          (reset-replacer replacer)
                          (multiple-value-bind (result count) (replace-document document replacer ranges multiline)
                            (when (plusp count)
                              (incf total count)
                              (push (write-document-request path result) requests)
                              (push (cons path (list (cons "count" count))) per-change)))))
                   (if selector
                       (resolve-lines/k document selector path #'skip-or-reject #'replace-in)
                       (replace-in nil))))
               :prefilter (and literal (not (member path explicit :test #'equal))
                               (lambda (octets) (octets-search literal octets)))
               :on-filtered (lambda () (return-from one))))))
        (check-expect-count/k context total reject
                              (lambda ()
                                (funcall commit (nreverse requests)
                                         (append (list (cons :per-change per-change))
                                                 (when skipped
                                                   (list (cons "skipped" (nreverse skipped))))))))))))

(defun %replacement-function/k (replacement regex literal fail on-function)
  "The regex replacement function for REPLACEMENT: verbatim when LITERAL,
else the parsed replacement template. `\\N` naming an existing group is refused
(ON-FAILURE via FAIL with the repair rewritten to ${N})."
  (declare (type function fail on-function))
  (if literal
      (funcall on-function (literal-replacement replacement))
      (let ((perl (perl-backreferences replacement (regex-group-count regex))))
        (if perl
            (funcall fail "argument.invalid"
                     (format nil "the replacement writes \\~D, which cl-regex-kit copies literally; write ${~D} (or pass --literal-replacement for a literal backslash)"
                             (first perl) (first perl))
                     :rewrite (rewrite-perl-backreferences replacement))
            (let ((parts (handler-case (parse-replacement-template replacement)
                           (edit-refusal (condition)
                             (return-from %replacement-function/k
                               (funcall fail (edit-refusal-code condition) (edit-refusal-detail condition)))))))
              (funcall on-function (template-regex-replacement parts)))))))

(define-write-command "replace" (ports env positionals options on-plan fail)
  ;; A refusal without its own :PATH is about the first file searched, not the
  ;; first positional (the pattern), so its default repair names a file.
  (setf fail (let ((first-path (if (or (getf options :stdin) (getf options :stdin-data))
                                  (first positionals)
                                  (third positionals)))
                   (outer fail))
               (lambda (code message &rest keys)
                 (apply outer code message (append keys (list :path first-path))))))
  (flet ((prepare (pattern replacement paths record-options)
           (let ((nth-text (getf options :nth)))
             (when (and nth-text (not (and (parse-count nth-text) (plusp (parse-count nth-text)))))
               (return-from prepare (funcall fail "argument.invalid" (format nil "--nth ~S must be a positive integer" nth-text))))
             (compile-pattern/k
              pattern fail
              (lambda (regex)
                (%replacement-function/k
                 replacement regex (getf options :literal-replacement)
                 (lambda (code message &key rewrite)
                   (funcall fail code message
                            :repairs (and rewrite
                                          (list (repair "use-template" "Use ${N} for a group reference."
                                                        (command-line (options-argv "replace" (list* pattern rewrite paths)
                                                                                    record-options)))))))
                 (lambda (expand)
                   (let ((replacer (make-replacer regex expand :nth (and nth-text (parse-count nth-text)))))
                     (parse-selector/k
                      :replace options :on-error fail
                      :on-selector (lambda (selector)
                                     (if (/= (length paths) 1)
                                         (funcall fail "argument.invalid" "a selector needs exactly one file path")
                                         (%replace-with-targets env paths options fail on-plan replacer selector
                                                                pattern replacement record-options)))
                      :on-none (lambda ()
                                 (%replace-with-targets env paths options fail on-plan replacer nil
                                                        pattern replacement record-options)))))))
              :fixed (getf options :fixed) :word (getf options :word) :ignore-case (getf options :ignore-case)))))
    (if (or (getf options :stdin) (getf options :stdin-data))
        (read-stdin-json/k ports options
                           :on-error fail
                           :on-json (lambda (value text)
                                      (let ((pattern (cdr (%json-member value "pattern")))
                                            (replacement (cdr (%json-member value "replacement"))))
                                        (if (and (stringp pattern) (stringp replacement))
                                            (prepare pattern replacement positionals (inline-stdin-options options text))
                                            (funcall fail "argument.invalid"
                                                     "replace --stdin reads {\"pattern\": string, \"replacement\": string}")))))
        (if (< (length positionals) 2)
            (funcall fail "argument.invalid" "replace needs PATTERN and REPLACEMENT (or --stdin)")
            (prepare (first positionals) (second positionals) (cddr positionals) options)))))

(defun %replace-with-targets (env paths options fail on-plan replacer selector pattern replacement record-options)
  "PATHS are as the user typed them (relative to the working directory);
during `tx rebase` (no ENV) they are the recorded workspace-relative paths."
  (flet ((plan (files explicit skipped)
           (funcall on-plan
                    (make-write-plan
                     :command "replace"
                     :targets (mapcar (lambda (file) (make-write-target file :base :root)) files)
                     :inputs (list pattern replacement)
                     :guard-requirements (list* (list :expect-count) (selector-guards selector (first files)))
                     :expect-hashes (getf options :expect-hash)
                     :expect-count (getf options :expect-count)
                     :replayable (and (content-selector-p selector) (null (getf options :expect-hash)))
                     :plan (%replace-plan replacer (getf options :multiline) selector explicit skipped)
                     :record-options record-options
                     :record-positionals (if (or (getf options :stdin) (getf options :stdin-data)
                                                 (getf record-options :stdin-data))
                                             (lambda (resolved) resolved)
                                             (lambda (resolved) (list* pattern replacement resolved)))))))
    (if (null env)
        (plan paths paths nil)
        (let* ((host (command-env-host env))
               (absolute (mapcar (lambda (path) (aitools.workspace.application:user-path-absolute host path)) paths))
               ;; Workspace-relative where inside (the form the plan's context
               ;; paths take), else absolute for the boundary check to refuse.
               (relative (mapcar (lambda (path)
                                   (aitools.workspace.application:resolve-user-path/k
                                    host (command-env-root env) path
                                    :on-inside (lambda (absolute relative) (declare (ignore absolute)) relative)
                                    :on-outside #'identity))
                                 paths))
               (explicit (loop for path in absolute
                               for path-relative in relative
                               for entry = (aitools.workspace.application:host-stat host path)
                               when (and entry (eq (aitools.workspace.application:workspace-entry-kind entry) :file))
                                 collect path-relative)))
          (if (and paths (= (length explicit) (length paths)))
              (plan relative explicit nil)
              (let ((skipped '()))
                (scan-files/k env absolute options fail
                              (lambda (entries)
                                (let ((files (mapcar #'aitools.workspace.application:scan-entry-path entries)))
                                  (if (null files)
                                      (funcall fail "selection.no-match" "no files to search" :path (first paths))
                                      (plan files (intersection files explicit :test #'equal) skipped))))
                              :on-skip (lambda (entry reason)
                                         (when (eq reason :too-large)
                                           (push (aitools.protocol.domain:json-object-from-alist
                                                  (list (cons "path" (aitools.workspace.application:scan-entry-path entry))
                                                        (cons "reason" "too-large")))
                                                 skipped))))))))))

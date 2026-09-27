;;;; packages/feature/vcs/src/application/blame-show-flows.lisp
;;;;
;;;; `git blame` and `git show`: the per-file flows that take
;;;; one of `read`'s selectors, parsed and resolved through the inspect
;;;; context's public selector API. The shared flow helpers are in flows.lisp.
(in-package #:aitools.vcs.application)

;;; ------------------------------------------------------------- selectors

(defun %call-with-selector (command subcommand selector-options on-error continuation)
  "Parse SELECTOR-OPTIONS (the plist of --range --symbol --kind --between
--exclusive --match --invert values) under COMMAND's selector rules and call
CONTINUATION with the kernel SELECTOR, or NIL when none was given. A bad
combination points at `git SUBCOMMAND`'s schema."
  (apply #'aitools.inspect.application:parse-selector-options/k command
         :on-selector continuation
         :on-none (lambda () (funcall continuation nil))
         :on-invalid (lambda (message repairs)
                       (declare (ignore repairs))
                       (funcall on-error "argument.invalid" message
                                :repairs (list (%repair "use-one-selector" "Check which selectors this command takes."
                                                        (format nil "aitools schema git.~A" subcommand)))))
         selector-options))

(defun %resolve-selection (lines selector path on-error root subcommand target on-selected)
  "Resolve SELECTOR against LINES (a simple-vector) and call ON-SELECTED
with the ranges, or report the failed selection with repairs that name
SUBCOMMAND on TARGET."
  (flet ((range-repair (action detail range)
           (list (%repair action detail (%git-command root subcommand target "--range" range)))))
    (aitools.inspect.application:resolve-selector/k
     lines selector
     :path path
     :on-selected on-selected
     :on-no-match (lambda (candidates)
                    (funcall on-error "selection.no-match" (format nil "the selector matched nothing in ~A" path)
                             :candidates candidates
                             :repairs (range-repair "read-lines" "Select lines that exist."
                                                    (format nil "1:~D" (max 1 (min (length lines) *blame-default-lines*))))))
     :on-ambiguous (lambda (candidates)
                     (funcall on-error "selection.ambiguous"
                              (format nil "the selector matched ~D places in ~A; it must match one"
                                      (length candidates) path)
                              :candidates candidates
                              :repairs (range-repair "use-range" "Select one match by its line range."
                                                     (let ((first (first candidates)))
                                                       (format nil "~D:~D" (%candidate-field first "line")
                                                               (%candidate-field first "end_line"))))))
     :on-invalid (lambda (code message)
                   (funcall on-error code message
                            :repairs (range-repair "use-range" "Select lines by number instead." "1:80"))))))

(defun %candidate-field (candidate name)
  (cdr (assoc name (aitools.protocol.domain:json-object-members candidate) :test #'string=)))

;;; ------------------------------------------------------------- git blame

(defun git-blame/k (port path &rest selector-options
                    &key root range symbol kind between exclusive match invert on-ok on-partial on-error)
  "Per-line blame of the work-tree PATH, limited by one selector
(all but --old). Without a selector, the first
*BLAME-DEFAULT-LINES* lines, PARTIAL when the file is longer."
  (declare (ignore range symbol kind between exclusive match invert))
  (let ((port (port-at-root port root)))
    (%call-with-selector
     :git-blame "blame" (%selector-options selector-options) on-error
     (lambda (selector)
       (%call-in-repository
        port on-error root path
        (lambda (port repository-path top directory)
          (declare (ignore top directory))
          (%run-git
           port "blame" (list "--porcelain" "--" repository-path) on-error
           (lambda (text)
             (let ((entries nil))
               (flet ((collect (n sha author date line-text)
                        (push (list n sha author date line-text) entries)))
                 (declare (dynamic-extent #'collect))
                 (aitools.vcs.domain:map-blame-lines text #'collect))
               (let* ((entries (coerce (sort entries #'< :key #'first) 'simple-vector))
                      (total (length entries)))
                 (labels ((line-object (number)
                            (apply #'aitools.vcs.domain:blame-line (svref entries (1- number))))
                          (respond (continuation numbers start truncated next-range)
                            (%finish continuation
                                     (list (cons "path" repository-path)
                                           (cons "start_line" start)
                                           (cons "lines" (mapcar #'line-object numbers))
                                           (cons "total_lines" total)
                                           (cons "truncated" (aitools.protocol.domain:json-boolean truncated)))
                                     (when truncated
                                       (list (%git-command root "blame" path "--range" next-range))))))
                   (if (null selector)
                       (aitools.vcs.domain:line-window/k
                        total :max-lines *blame-default-lines*
                        :on-window (lambda (first last truncated next-range)
                                     (respond (if truncated on-partial on-ok)
                                              (loop for n from first to last collect n)
                                              first truncated next-range)))
                       (%resolve-selection
                        (map 'simple-vector #'fifth entries) selector repository-path on-error root "blame" path
                        (lambda (ranges)
                          (let ((numbers (loop for (start . end) in ranges
                                               append (loop for n from start to end collect n))))
                            (respond on-ok numbers (car (first ranges)) nil nil)))))))))
           :not-found-repairs (list (%repair "check-status" "Blame needs a path git tracks; list changes."
                                             (%git-command root "status"))))))))))

(defun %selector-options (options)
  "The selector keys of OPTIONS, a flow's keyword arguments."
  (loop for (key value) on options by #'cddr
        when (member key '(:range :symbol :kind :between :exclusive :match :invert))
          append (list key value)))

;;; -------------------------------------------------------------- git show

(defun %show-invalid-spec (on-error root spec)
  (funcall on-error "argument.invalid" (format nil "expected <rev>:<path>, got ~A" spec)
           :repairs (list (%repair "show-head" "Show the file as committed at HEAD."
                                   (%git-command root "show"
                                                 (concatenate 'string "HEAD:"
                                                              (if (%option-like-p spec) "<path>" spec)))))))

(defun %text-fields (first shown truncated total hash errors &key line-numbers)
  "`read`'s text-mode fields for SHOWN (line strings) starting at FIRST."
  (append (list (cons "mode" "text")
                (cons "start_line" first)
                (cons "lines" shown))
          (when line-numbers (list (cons "line_numbers" line-numbers)))
          (list (cons "total_lines" total)
                (cons "hash" hash)
                (cons "truncated" (aitools.protocol.domain:json-boolean truncated))
                (cons "encoding_errors" errors)
                (cons "approx_tokens"
                      (aitools.kernel.domain:approx-token-count
                       (+ (length shown) (reduce #'+ shown :key #'length)))))))

(defun git-show/k (port spec &rest selector-options
                   &key root range symbol kind between exclusive match invert (max-lines 80)
                     on-ok on-partial on-error)
  "The blob named by SPEC (`<rev>:<path>`) in `read`'s shape: one
selector (all but --old), and never more than MAX-LINES
lines."
  (declare (ignore range symbol kind between exclusive match invert))
  (multiple-value-bind (rev path) (aitools.vcs.domain:split-object-spec spec)
    (if (or (null rev) (%option-like-p spec))
        (%show-invalid-spec on-error root spec)
        (let ((port (port-at-root port root)))
          (%call-with-selector
           :git-show "show" (%selector-options selector-options) on-error
           (lambda (selector)
             (%call-in-repository
              port on-error root (when (plusp (length path)) path)
              (lambda (port repository-path top directory)
                (declare (ignore top directory))
                (%run-git
                 port "cat-file"
                 (list "blob" (if repository-path (concatenate 'string rev ":" repository-path) spec))
                 on-error
                 (lambda (bytes)
                   (%show-blob bytes rev (or repository-path path) spec selector max-lines root
                               on-ok on-partial on-error))
                 :octets t
                 :not-found-repairs (list (%repair "find-revision" "List commits that touched the path."
                                                   (%git-command root "log"
                                                                 (when (plusp (length path)) path)))))))))))))

(defun %show-blob (bytes rev path spec selector max-lines root on-ok on-partial on-error)
  (let ((head (list (cons "rev" rev) (cons "path" path)))
        (hash (aitools.kernel.domain:content-hash bytes)))
    (if (aitools.text.domain:binary-octets-p bytes)
        (%finish on-ok (append head (list (cons "mode" "text") (cons "binary" t)
                                          (cons "size" (length bytes))
                                          (cons "mime" (aitools.text.domain:guess-mime bytes :path path))
                                          (cons "hash" hash))))
        (multiple-value-bind (lines layout errors) (aitools.text.domain:decode-text-lines bytes)
          (declare (ignore layout))
          (let ((total (length lines)))
            (labels ((emit (first shown truncated next-command &key line-numbers)
                       (%finish (if truncated on-partial on-ok)
                                (append head (%text-fields first shown truncated total hash errors
                                                           :line-numbers line-numbers))
                                (when next-command (list next-command))))
                     (window (start end)
                       (aitools.vcs.domain:line-window/k
                        total :start start :end end :max-lines max-lines
                        :on-window (lambda (first last truncated next-range)
                                     (emit first (coerce (subseq lines (1- first) last) 'list) truncated
                                           (when truncated
                                             (%git-command root "show" spec "--range" next-range)))))))
              (if (null selector)
                  (window nil nil)
                  (%resolve-selection
                   lines selector path on-error root "show" spec
                   (lambda (ranges)
                     (if (eq (aitools.kernel.domain:selector-kind selector) :match)
                         (let* ((numbers (mapcar #'car ranges))
                                (shown (subseq numbers 0 (min max-lines (length numbers))))
                                (truncated (< (length shown) (length numbers))))
                           (emit (if shown (first shown) 1)
                                 (mapcar (lambda (number) (svref lines (1- number))) shown)
                                 truncated
                                 (when truncated
                                   (%git-command root "show" spec "--match"
                                                 (aitools.kernel.domain:selector-match-pattern selector)
                                                 (when (aitools.kernel.domain:selector-invert selector) "--invert")
                                                 "--max-lines" (princ-to-string (length numbers))))
                                 :line-numbers shown))
                         (destructuring-bind ((start . end)) ranges
                           (if (> start end)
                               (emit start nil nil nil)
                               (window start end)))))))))))))

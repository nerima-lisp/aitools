(in-package #:aitools.vcs.application)

(aitools.kernel.domain:define-flow
    (%show-invalid-spec root spec &key on-error)
  (aitools.kernel.domain:flow-continue
   :on-error "argument.invalid"
   (format nil "expected <rev>:<path>, got ~A" spec)
   :repairs
   (list
    (repair
     "show-head" "Show the file as committed at HEAD."
     (%git-command
      root "show"
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

(aitools.kernel.domain:define-flow
    (%select-show-lines lines ranges selector root spec max-lines &key on-window)
  (if (eq (aitools.kernel.domain:selector-kind selector) :match)
      (let* ((numbers (mapcar #'car ranges))
             (shown (subseq numbers 0 (min max-lines (length numbers))))
             (truncated (< (length shown) (length numbers))))
        (aitools.kernel.domain:flow-continue
         :on-window
         (if shown (first shown) 1)
         (mapcar (lambda (number) (svref lines (1- number))) shown)
         truncated
         (when truncated
           (%git-command
            root "show" spec "--match"
            (aitools.kernel.domain:selector-match-pattern selector)
            (when (aitools.kernel.domain:selector-invert selector) "--invert")
            "--max-lines" (princ-to-string (length numbers))))
         shown))
      (destructuring-bind ((start . end)) ranges
        (if (> start end)
            (aitools.kernel.domain:flow-continue :on-window start nil nil nil nil)
            (aitools.kernel.domain:flow-call
                (aitools.vcs.domain:line-window/k
                 (length lines) :start start :end end :max-lines max-lines)
              (:on-window (first last truncated next-range)
                (aitools.kernel.domain:flow-continue
                 :on-window first
                 (coerce (subseq lines (1- first) last) 'list)
                 truncated
                 (when truncated
                   (%git-command root "show" spec "--range" next-range))
                 nil)))))))

(aitools.kernel.domain:define-flow
    (%show-text-lines lines hash rev path spec selector max-lines root errors
                      &key on-ok on-partial on-error)
  (let ((head (list (cons "rev" rev) (cons "path" path)))
        (total (length lines)))
    (labels ((emit (first shown truncated next-command &key line-numbers)
               (let* ((fields
                        (append
                         head
                         (%text-fields first shown truncated total hash errors
                                       :line-numbers line-numbers)))
                      (next-commands (when next-command (list next-command))))
                 (if truncated
                     (aitools.kernel.domain:flow-continue
                      :on-partial (%finish-fields fields next-commands))
                     (aitools.kernel.domain:flow-continue
                      :on-ok (%finish-fields fields next-commands)))))
             (window (start end)
               (aitools.kernel.domain:flow-call
                   (aitools.vcs.domain:line-window/k
                    total :start start :end end :max-lines max-lines)
                 (:on-window (first last truncated next-range)
                   (emit
                    first (coerce (subseq lines (1- first) last) 'list)
                    truncated
                    (when truncated
                      (%git-command root "show" spec "--range" next-range)))))))
      (if (null selector)
          (window nil nil)
          (aitools.kernel.domain:flow-call
              (%resolve-selection lines selector path root "show" spec)
            (:on-selected (ranges)
              (aitools.kernel.domain:flow-call
                  (%select-show-lines lines ranges selector root spec max-lines)
                (:on-window (first shown truncated next-command line-numbers)
                  (emit first shown truncated next-command
                        :line-numbers line-numbers))))
            (:on-error (code message &key candidates repairs)
              (aitools.kernel.domain:flow-continue
               :on-error code message :candidates candidates
               :repairs repairs)))))))

(aitools.kernel.domain:define-flow
    (%show-blob bytes rev path spec selector max-lines root
                &key on-ok on-partial on-error)
  (let ((hash (aitools.kernel.domain:content-hash bytes)))
    (if (aitools.text.domain:binary-octets-p bytes)
        (aitools.kernel.domain:flow-continue
         :on-ok
         (%finish-fields
          (append
           (list (cons "rev" rev) (cons "path" path))
           (list (cons "mode" "text")
                 (cons "binary" t)
                 (cons "size" (length bytes))
                 (cons "mime" (aitools.text.domain:guess-mime bytes :path path))
                 (cons "hash" hash)))))
        (multiple-value-bind (lines layout errors)
            (aitools.text.domain:decode-text-lines bytes)
          (declare (ignore layout))
          (aitools.kernel.domain:flow-call
              (%show-text-lines lines hash rev path spec selector max-lines root errors)
            (:on-ok (fields)
              (aitools.kernel.domain:flow-continue :on-ok fields))
            (:on-partial (fields)
              (aitools.kernel.domain:flow-continue :on-partial fields))
            (:on-error (code message &key candidates repairs)
              (aitools.kernel.domain:flow-continue
               :on-error code message :candidates candidates
               :repairs repairs)))))))

(aitools.kernel.domain:define-flow
    (git-show/k port spec &rest selector-options
                 &key root range symbol kind between exclusive match invert
                   (max-lines 80) on-ok on-partial on-error)
  "The blob named by SPEC (`<rev>:<path>`) in `read`'s shape: one
selector (all but --old), and never more than MAX-LINES
lines."
  (declare (ignore range symbol kind between exclusive match invert))
  (multiple-value-bind (rev path)
      (aitools.vcs.domain:split-object-spec spec)
    (if (or (null rev) (%option-like-p spec))
        (aitools.kernel.domain:flow-call
            (%show-invalid-spec root spec)
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message :repairs repairs)))
        (let ((port (port-at-root port root)))
          (aitools.kernel.domain:flow-call
              (%call-with-selector
               :git-show "show" (%selector-options selector-options))
            (:on-selector (selector)
              (aitools.kernel.domain:flow-call
                  (%call-in-repository
                   port root (when (plusp (length path)) path))
                (:on-repository (port repository-path top directory)
                  (declare (ignore top directory))
                  (aitools.kernel.domain:flow-call
                      (%run-git
                       port "cat-file"
                       (list
                        "blob"
                        (if repository-path
                            (concatenate 'string rev ":" repository-path)
                            spec))
                       :octets t
                       :not-found-repairs
                       (list
                        (repair
                         "find-revision" "List commits that touched the path."
                         (%git-command root "log"
                                       (when (plusp (length path)) path)))))
                    (:on-success (bytes)
                      (aitools.kernel.domain:flow-call
                          (%show-blob
                           bytes rev (or repository-path path) spec selector
                           max-lines root)
                        (:on-ok (fields)
                          (aitools.kernel.domain:flow-continue :on-ok fields))
                        (:on-partial (fields)
                          (aitools.kernel.domain:flow-continue :on-partial fields))
                        (:on-error (code message &key candidates repairs)
                          (aitools.kernel.domain:flow-continue
                           :on-error code message :candidates candidates
                           :repairs repairs))))
                    (:on-error (code message &key repairs)
                      (aitools.kernel.domain:flow-continue
                       :on-error code message :repairs repairs))))
                (:on-error (code message &key repairs)
                  (aitools.kernel.domain:flow-continue
                   :on-error code message :repairs repairs))))
            (:on-error (code message &key repairs)
              (aitools.kernel.domain:flow-continue
               :on-error code message :repairs repairs)))))))

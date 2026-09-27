;;;; packages/feature/inspect/src/application/archive-flows.lisp
;;;;
;;;; `archive list` and `archive read` (the `tar -tf`,
;;;; `unzip -l`, `zcat`, `unzip -p` replacements). `archive read` answers in
;;;; `read`'s shape through the same text view and selectors.
(in-package #:aitools.inspect.application)

(defun %archive-list-command (context path)
  (command-line context (list "archive" "list" path)))

(defun call-with-archive/k (context target &key on-archive on-error)
  "Read TARGET's bytes and open them as an archive: ON-ARCHIVE (archive), or
the matching error through ON-ERROR."
  (declare (type function on-archive on-error))
  (multiple-value-bind (octets problem) (read-target-octets context target)
    (let ((path (file-target-argument target)))
    (if (null octets)
        (fail-target-read context target on-error problem)
        (open-archive/k
         octets (file-target-absolute target)
         :on-archive on-archive
         :on-unsupported (lambda ()
                           (fail on-error "input.unsupported-format"
                                 (format nil "~A is not a zip, tar, tar.gz, or gz archive" path)
                                 :repairs (list (repair "describe" "Check the file's type."
                                                        (command-line context (list "info" path))))))
         :on-malformed (lambda (reason)
                         (fail on-error "input.unsupported-format"
                               (format nil "~A cannot be read as an archive: ~A" path reason)
                               :repairs (list (repair "describe" "Check the file's type."
                                                      (command-line context (list "info" path))))))
         :on-too-large (lambda (limit)
                         (fail on-error "refusal.too-large"
                               (format nil "~A decompresses past the ~D-byte limit" path limit)
                               :repairs (list (repair "list" "List the members instead."
                                                      (%archive-list-command context path))))))))))

(defun archive-list-flow (ports path &key root tx lock-timeout (limit 200) on-ok on-partial on-error)
  "`archive list`. Calls ON-OK or ON-PARTIAL (fields), or ON-ERROR."
  (declare (type function on-ok on-partial on-error))
  (call-with-inspect-file/k
   ports path :root root :tx tx :lock-timeout lock-timeout :on-error on-error
   :on-file (lambda (context target)
              (call-with-archive/k
               context target
               :on-error on-error
               :on-archive (lambda (archive)
                             (%report-archive-list path archive limit on-ok on-partial))))))

(defun %report-archive-list (path archive limit on-ok on-partial)
  (let* ((items (archive-items archive))
         (truncated (> (length items) limit))
         (shown (if truncated (subseq items 0 limit) items)))
    (funcall (if truncated on-partial on-ok)
             (append (list (cons "path" path)
                           (cons "format" (archive-format-name (archive-format archive)))
                           (cons "items" (mapcar #'archive-item-json shown))
                           (cons "total" (length items))
                           (cons "truncated" (json-bool truncated)))
                     (when truncated
                       (list (cons "next_commands"
                                   (list (format nil "aitools archive list ~A --limit ~D"
                                                 (shell-quote path) (length items))))))))))

;;; ------------------------------------------------------------ archive read

(defun %entry-missing (context path entry archive on-error)
  (fail on-error "input.not-found"
        (format nil "~A has no entry ~A" path entry)
        :candidates (mapcar (lambda (item) (json-object "path" (archive-item-name item)))
                            (rank-similar entry (archive-items archive) :key #'archive-item-name :count 5))
        :repairs (list (repair "list" "List the archive's entries." (%archive-list-command context path)))))

(defun %choose-item/k (context path entry archive on-item on-error)
  "The member ENTRY names (a `.gz` has one member and needs no ENTRY)."
  (let ((gz (eq (archive-format archive) :gz)))
    (cond
      ((and gz (null entry)) (funcall on-item (first (archive-items archive))))
      ((null entry)
       (fail on-error "argument.invalid" (format nil "archive read ~A needs an entry name" path)
             :repairs (list (repair "list" "List the archive's entries." (%archive-list-command context path)))))
      (t
       (let ((item (if gz
                       (and (string= (string-right-trim "/" entry) (archive-item-name (first (archive-items archive))))
                            (first (archive-items archive)))
                       (find-archive-item archive entry))))
         (cond ((null item) (%entry-missing context path entry archive on-error))
               ((not (eq (archive-item-kind item) :file))
                (fail on-error "refusal.not-a-file"
                      (format nil "entry ~A of ~A is a ~(~A~), not a file" entry path (archive-item-kind item))
                      :repairs (list (repair "list" "List the archive's entries."
                                             (%archive-list-command context path)))))
               (t (funcall on-item item))))))))

(defun %archive-hex (context path entry content max-lines on-ok on-partial)
  (let* ((end (min (length content) (* 16 max-lines)))
         (truncated (< end (length content))))
    (funcall (if truncated on-partial on-ok)
             (append (list (cons "mode" "hex") (cons "path" path) (cons "entry" entry)
                           (cons "size" (length content)) (cons "start" 0) (cons "end" end)
                           (cons "rows" (hex-rows content 0 end))
                           (cons "truncated" (json-bool truncated))
                           (cons "approx_tokens" (approx-token-count (* 3 end))))
                     (when truncated
                       (list (cons "next_commands"
                                   (list (command-line context
                                                       (remove nil (list "archive" "read" path entry "--as" "hex"
                                                                         "--max-lines"
                                                                         (princ-to-string (ceiling (length content) 16)))))))))))))

(defun %archive-text (context path entry content selector max-lines on-ok on-partial on-error)
  (if (binary-octets-p content)
      (funcall on-ok (list (cons "mode" "text") (cons "path" path) (cons "entry" entry)
                           (cons "binary" t) (cons "size" (length content))
                           (cons "mime" (guess-mime (subseq content 0 (min (length content) +binary-sniff-length+))
                                                    :path (or entry path)))))
      (flet ((with-head (continuation)
               (lambda (fields)
                 (funcall continuation (list* (cons "mode" "text") (cons "path" path) (cons "entry" entry) fields)))))
        (text-view/k context content
                     (make-text-view-options :selector selector :max-lines max-lines
                                             :path (or entry path)
                                             :command-words (remove nil (list "archive" "read" path entry))
                                             ;; `archive read --as hex` has no --bytes: dump through the cut.
                                             :hex-words (lambda (start end)
                                                          (declare (ignore start))
                                                          (remove nil (list "archive" "read" path entry "--as" "hex"
                                                                            "--max-lines" (princ-to-string (ceiling end 16))))))
                     :on-ok (with-head on-ok) :on-partial (with-head on-partial) :on-error on-error))))

(defun %archive-read-item (context path entry archive item selector mode max-lines on-ok on-partial on-error)
  (archive-item-content/k
   archive item
   :on-content (lambda (content)
                 (if (eq mode :hex)
                     (%archive-hex context path entry content max-lines on-ok on-partial)
                     (%archive-text context path entry content selector max-lines on-ok on-partial on-error)))
   :on-too-large (lambda (limit)
                   (fail on-error "refusal.too-large"
                         (format nil "entry ~A of ~A is larger than ~D bytes" (archive-item-name item) path limit)
                         :repairs (list (repair "list" "List the archive's entries."
                                                (%archive-list-command context path)))))
   :on-malformed (lambda (reason)
                   (fail on-error "input.unsupported-format"
                         (format nil "entry ~A of ~A cannot be decoded: ~A" (archive-item-name item) path reason)
                         :repairs (list (repair "list" "List the archive's entries."
                                                (%archive-list-command context path)))))))

(defun %archive-read-target (context target path entry selector mode max-lines on-ok on-partial on-error)
  (call-with-archive/k
   context target
   :on-error on-error
   :on-archive (lambda (archive)
                 (%choose-item/k context path entry archive
                                 (lambda (item)
                                   (%archive-read-item context path entry archive item selector mode
                                                       max-lines on-ok on-partial on-error))
                                 on-error))))

(defun archive-read-flow (ports path entry &key root tx lock-timeout range symbol kind between exclusive match invert
                                                (as "text") (max-lines 80) on-ok on-partial on-error)
  "`archive read`. Calls exactly one of ON-OK, ON-PARTIAL (fields) or ON-ERROR."
  (declare (type function on-ok on-partial on-error))
  (let ((mode (if (string= as "hex") :hex :text)))
    (flet ((continue-with (selector)
             (call-with-inspect-file/k
              ports path :root root :tx tx :lock-timeout lock-timeout :record t :on-error on-error
              :on-file (lambda (context target)
                         (%archive-read-target context target path entry selector mode max-lines
                                               on-ok on-partial on-error)))))
      (parse-selector-options/k
       :archive-read :range range :symbol symbol :kind kind :between between :exclusive exclusive
                     :match match :invert invert
                     :extra-exclusive (list (cons "--as hex" (eq mode :hex)))
                     :on-invalid (lambda (message repairs) (fail on-error "argument.invalid" message :repairs repairs))
                     :on-none (lambda () (continue-with nil))
                     :on-selector #'continue-with))))

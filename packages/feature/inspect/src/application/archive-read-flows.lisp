(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (%choose-item/k context path entry archive &key on-item on-error)
 "The member ENTRY names (a `.gz` has one member and needs no ENTRY)."
 (declare (type function on-item on-error))
 (let ((gz (eq (archive-format archive) :gz)))
   (cond
     ((and gz (null entry))
      (aitools.kernel.domain:flow-continue :on-item
                                           (first (archive-items archive))))
     ((null entry)
      (aitools.kernel.domain:flow-continue :on-error
                                           "argument.invalid"
                                           (format nil
                                                   "archive read ~A needs an entry name"
                                                   path)
                                           :repairs
                                           (list
                                            (repair "list"
                                                    "List the archive's entries."
                                                    (%archive-list-command
                                                     context
                                                     path)))))
     (t
      (let ((item
             (if gz
                 (and
                  (string= (string-right-trim "/" entry)
                           (archive-item-name (first (archive-items archive))))
                  (first (archive-items archive)))
                 (find-archive-item archive entry))))
        (cond
          ((null item)
           (aitools.kernel.domain:flow-continue :on-error
                                                "input.not-found"
                                                (format nil
                                                        "~A has no entry ~A"
                                                        path
                                                        entry)
                                                :candidates
                                                (mapcar
                                                 (lambda (candidate)
                                                   (json-object "path"
                                                                (archive-item-name
                                                                 candidate)))
                                                 (rank-similar entry
                                                               (archive-items
                                                                archive)
                                                               :key
                                                               #'archive-item-name
                                                               :count
                                                               5))
                                                :repairs
                                                (list
                                                 (repair "list"
                                                         "List the archive's entries."
                                                         (%archive-list-command
                                                          context
                                                          path)))))
          ((not (eq (archive-item-kind item) :file))
           (aitools.kernel.domain:flow-continue :on-error
                                                "refusal.not-a-file"
                                                (format nil
                                                        "entry ~A of ~A is a ~(~A~), not a file"
                                                        entry
                                                        path
                                                        (archive-item-kind item))
                                                :repairs
                                                (list
                                                 (repair "list"
                                                         "List the archive's entries."
                                                         (%archive-list-command
                                                          context
                                                          path)))))
          (t (aitools.kernel.domain:flow-continue :on-item item))))))))

(aitools.kernel.domain:define-flow
 (%archive-hex context path entry content max-lines &key on-ok on-partial)
 (declare (type function on-ok on-partial))
 (let* ((end (min (length content) (* 16 max-lines)))
        (truncated (< end (length content))))
   (if truncated
       (aitools.kernel.domain:flow-continue :on-partial
                                            (append
                                             (list (cons "mode" "hex")
                                                   (cons "path" path)
                                                   (cons "entry" entry)
                                                   (cons "size"
                                                         (length content))
                                                   (cons "start" 0)
                                                   (cons "end" end)
                                                   (cons "rows"
                                                         (hex-rows content
                                                                   0
                                                                   end))
                                                   (cons "truncated"
                                                         (json-bool truncated))
                                                   (cons "approx_tokens"
                                                         (approx-token-count
                                                          (* 3 end))))
                                             (list
                                              (cons "next_commands"
                                                    (list
                                                     (command-line context
                                                                   (remove nil
                                                                           (list
                                                                            "archive"
                                                                            "read"
                                                                            path
                                                                            entry
                                                                            "--as"
                                                                            "hex"
                                                                            "--max-lines"
                                                                            (princ-to-string
                                                                             (ceiling
                                                                              (length
                                                                               content)
                                                                              16))))))))))
       (aitools.kernel.domain:flow-continue :on-ok
                                            (list (cons "mode" "hex")
                                                  (cons "path" path)
                                                  (cons "entry" entry)
                                                  (cons "size" (length content))
                                                  (cons "start" 0)
                                                  (cons "end" end)
                                                  (cons "rows"
                                                        (hex-rows content 0 end))
                                                  (cons "truncated"
                                                        (json-bool truncated))
                                                  (cons "approx_tokens"
                                                        (approx-token-count
                                                         (* 3 end))))))))

(aitools.kernel.domain:define-flow
 (%archive-text context path entry content selector max-lines &key on-ok on-partial on-error)
 (declare (type function on-ok on-partial on-error))
 (if (binary-octets-p content)
     (aitools.kernel.domain:flow-continue
      :on-ok
      (list (cons "mode" "text") (cons "path" path) (cons "entry" entry)
            (cons "binary" t) (cons "size" (length content))
            (cons "mime" (guess-mime (subseq content 0 (min (length content) +binary-sniff-length+))
                                     :path (or entry path)))))
   (aitools.kernel.domain:flow-call
    (text-view/k
         context content
         (make-text-view-options :selector selector :max-lines max-lines
                                 :path (or entry path)
                                 :command-words (remove nil (list "archive" "read" path entry))
                                 ;; `archive read --as hex` has no --bytes: dump through the cut.
                                 :hex-words (lambda (start end)
                                              (declare (ignore start))
                                              (remove nil (list "archive" "read" path entry "--as" "hex"
                                                                "--max-lines"
                                                                (princ-to-string (ceiling end 16)))))))
    (:on-ok (fields)
      (aitools.kernel.domain:flow-continue :on-ok
                                           (list* (cons "mode" "text")
                                                  (cons "path" path)
                                                  (cons "entry" entry)
                                                  fields)))
    (:on-partial (fields)
      (aitools.kernel.domain:flow-continue :on-partial
                                           (list* (cons "mode" "text")
                                                  (cons "path" path)
                                                  (cons "entry" entry)
                                                  fields)))
    (:on-error (code message &key candidates diagnostics conflicts repairs)
      (aitools.kernel.domain:flow-continue
       :on-error code message
       :candidates candidates
       :diagnostics diagnostics
       :conflicts conflicts
       :repairs repairs))))
 )

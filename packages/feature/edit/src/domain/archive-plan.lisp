;;;;
;;;; `archive extract` and `archive create`. Extraction is
;;;; planned in full before anything is written (rejecting
;;;; zip slip and decompression bombs after a partial extraction would leave
;;;; the partial files behind). The archive is untrusted input: every name
;;;; and link target is checked, the declared sizes are used only for early
;;;; rejection, and each member's data is inflated against the bytes still
;;;; left in the budget so the real output, not the header's claim, is what
;;;; the limit bounds.
(in-package #:aitools.edit.domain)

;;; Extract and create share format detection in AITOOLS.TEXT.DOMAIN, including
;;; v7 tar support.
(defun %plan-selected-archive-records (chosen body
                                              destination
                                              checker
                                              lookup-kind
                                              entries-by-name
                                              take-entry
                                              check-link-target)
  (let ((seen (make-hash-table :test 'equal)))
    (loop for record in chosen
          collect (let* ((entry (first record))
                         (name (second record))
                         (problem
                          (aitools.text.domain:archive-entry-path-problem
                           (aitools.text.domain:archive-entry-name entry)))
                         (kind (aitools.text.domain:archive-entry-kind entry))
                         (path
                          (aitools.workspace.domain:join-path destination name)))
                    (when problem
                      (refuse "refusal.outside-workspace"
                              "archive entry ~S is unsafe (~(~A~))"
                              name
                              problem))
                    (when (gethash name seen)
                      (refuse "refusal.exists"
                              "archive entry ~S occurs twice"
                              name))
                    (setf (gethash name seen) t)
                    (check-entry-parents name checker)
                    (let ((existing (funcall lookup-kind path)))
                      (unless
                          (or (eq existing :absent)
                              (and (eq kind :directory)
                                   (eq existing :directory)))
                        (refuse "refusal.exists" "~A already exists" path)))
                    (ecase kind
                      (:directory (make-extract-step :directory path))
                      (:file
                       (make-extract-step :file
                                          path
                                          :data
                                          (funcall take-entry body entry)
                                          :mode
                                          (%archive-entry-mode entry)))
                      (:symlink
                       (let ((target
                              (or
                               (aitools.text.domain:archive-entry-link-target
                                entry)
                               "")))
                         (funcall check-link-target name target checker)
                         (make-extract-step :symlink path :target target)))
                      (:hardlink
                       (let* ((target (%archive-entry-link-target entry))
                              (target-name (normalized-name target))
                              (source-record
                               (and
                                (null
                                 (aitools.text.domain:archive-entry-path-problem
                                  target))
                                (gethash target-name entries-by-name)))
                              (source (and source-record (first source-record))))
                         (unless
                             (and source
                                  (eq
                                   (aitools.text.domain:archive-entry-kind
                                    source)
                                   :file))
                           (refuse "refusal.outside-workspace"
                                   "archive hard link ~S has no file ~S in the archive"
                                   name
                                   target))
                         (make-extract-step :file
                                            path
                                            :data
                                            (funcall take-entry body source)
                                            :mode
                                            (%archive-entry-mode source))))
                      (:other
                       (refuse "input.unsupported-format"
                               "archive entry ~S is a device or special file"
                               name)))))))

(defun plan-archive-extraction (octets format
                                       destination
                                       &key
                                       archive-path
                                       selected
                                       max-bytes
                                       max-entries
                                       lookup-kind
                                       make-prefix-checker)
  "The EXTRACT-STEPs extracting OCTETS below DESTINATION. LOOKUP-KIND
returns the current kind of a workspace-relative path. MAKE-PREFIX-CHECKER
builds the store application checker from the planned archive symlink paths.

Selection, duplicate, and hard-link names preserve case and Unicode spelling
after removing empty and `.` components. The checker handles symlink prefixes."
  (declare (type function lookup-kind make-prefix-checker))
  (let ((budget max-bytes))
    (labels ((check-link-target (name target checker)
               (%check-link-target-syntax name target)
               (%check-link-target-traversal name target checker))
             (take (data)
               (decf budget (length data))
               data)
             (take-entry (body entry)
               (take
                (handler-case (aitools.text.domain:archive-entry-data body
                                                                      entry
                                                                      :max-output
                                                                      budget)
                  (aitools.text.domain:archive-limit-exceeded ()
                    (refuse "refusal.too-large"
                            "the extracted size exceeds --max-bytes ~D"
                            max-bytes))))))
      (if (eq format :gz)
          (let ((name
                 (handler-case (aitools.text.domain:gzip-member-name octets
                                                                     (or
                                                                      archive-path
                                                                      ""))
                   (aitools.text.domain:archive-limit-exceeded (condition)
                     (refuse "refusal.too-large"
                             "archive exceeds reader limit ~A"
                             (aitools.text.domain:archive-limit-exceeded-limit
                              condition))))))
            (check-path-limit name
                              aitools.text.domain:+archive-max-path-bytes+
                              "gzip member name")
            (let* ((data
                    (handler-case (aitools.text.domain:gzip-decompress octets
                                                                       :max-output
                                                                       max-bytes)
                      (aitools.text.domain:archive-limit-exceeded ()
                        (refuse "refusal.too-large"
                                "the extracted size exceeds --max-bytes ~D"
                                max-bytes))))
                   (path (aitools.workspace.domain:join-path destination name)))
              (unless (eq (funcall lookup-kind path) :absent)
                (refuse "refusal.exists" "~A already exists" path))
              (list
               (make-extract-step :file path :data (take data) :mode #o644))))
          (multiple-value-bind (raw-entries body)
              (handler-case (if (eq format :tar-gz)
                                (let ((tar
                                       (handler-case (aitools.text.domain:gzip-decompress
                                                      octets
                                                      :max-output
                                                      max-bytes)
                                         (aitools.text.domain:archive-limit-exceeded ()
                                           (refuse "refusal.too-large"
                                                   "the extracted size exceeds --max-bytes ~D"
                                                   max-bytes)))))
                                  (%archive-entries tar :tar))
                                (%archive-entries octets format))
                (aitools.text.domain:archive-limit-exceeded (condition)
                  (refuse "refusal.too-large"
                          "archive exceeds reader limit ~A"
                          (aitools.text.domain:archive-limit-exceeded-limit
                           condition))))
            (dolist (entry raw-entries)
              (check-path-limit (aitools.text.domain:archive-entry-name entry)
                                aitools.text.domain:+archive-max-path-bytes+
                                "archive entry name")
              (when
                  (member (aitools.text.domain:archive-entry-kind entry)
                          '(:symlink :hardlink))
                (check-path-limit
                 (or (aitools.text.domain:archive-entry-link-target entry) "")
                 aitools.text.domain:+archive-max-link-target-bytes+
                 "archive link target")))
            (dolist (name selected)
              (check-path-limit name
                                aitools.text.domain:+archive-max-path-bytes+
                                "selected archive entry name"))
            (let* ((records
                    (loop for entry in raw-entries
                          for name = (normalized-name
                                      (%archive-entry-name entry))
                          unless (string= name "")
                            collect (list entry name)))
                   (entries-by-name (make-hash-table :test 'equal))
                   (archive-symlink-paths
                    (loop for record in records
                          for entry = (first record)
                          when (eq
                                (aitools.text.domain:archive-entry-kind entry)
                                :symlink)
                            collect (second record)))
                   (checker (funcall make-prefix-checker archive-symlink-paths))
                   (chosen nil)
                   (steps '()))
              (unwind-protect
                  (progn
                    (dolist (record records)
                      (let ((entry (first record))
                            (name (second record)))
                        (unless (nth-value 1 (gethash name entries-by-name))
                          (setf (gethash name entries-by-name) record))))
                    (setf chosen (if selected
                                     (loop for name in selected
                                           for record = (gethash
                                                         (normalized-name name)
                                                         entries-by-name)
                                           collect (or record
                                                       (refuse "input.not-found"
                                                               "no entry ~S in the archive"
                                                               name)))
                                     records))
                    (when (> (length chosen) max-entries)
                      (refuse "refusal.too-large"
                              "the archive has ~D entries, over --max-entries ~D"
                              (length chosen)
                              max-entries))
                    (let ((declared
                           (reduce #'+
                                   chosen
                                   :key
                                   (lambda (record)
                                     (aitools.text.domain:archive-entry-size
                                      (first record))))))
                      (when (> declared max-bytes)
                        (refuse "refusal.too-large"
                                "the entries declare ~D bytes, over --max-bytes ~D"
                                declared
                                max-bytes)))
                    (setf steps (%plan-selected-archive-records chosen
                                                                body
                                                                destination
                                                                checker
                                                                lookup-kind
                                                                entries-by-name
                                                                #'take-entry
                                                                #'check-link-target))
                    steps)
                (funcall checker :finish))))))))

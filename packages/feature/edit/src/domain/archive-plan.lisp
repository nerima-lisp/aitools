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

;;; Archive-format detection lives in AITOOLS.TEXT.DOMAIN: edit's extract
;;; and create call DETECT-ARCHIVE-FORMAT / ARCHIVE-FORMAT-NAME there and use
;;; its keywords (:ZIP :TAR :TAR-GZ :GZ), so v7 tar is accepted the same way
;;; on both the read and the write side.
(defun archive-format-for-path (path)
  "The `archive create` format implied by PATH's extension, or NIL."
  (let ((lower (string-downcase path)))
    (flet ((ends (suffix)
             (let ((start (- (length lower) (length suffix))))
               (and (>= start 0) (string= suffix lower :start2 start)))))
      (cond
        ((ends ".zip") :zip)
        ((or (ends ".tar.gz") (ends ".tgz")) :tar-gz)
        ((ends ".tar") :tar)
        ((ends ".gz") :gz)))))

(defun parse-archive-format (name)
  (cdr (assoc name '(("zip" . :zip) ("tar" . :tar) ("tar.gz" . :tar-gz) ("gz" . :gz)) :test #'string=)))

(defun %normalize-entry-name (name)
  "NAME without leading `./` components."
  (loop
    while
    (and (>= (length name) 2) (string= name "./" :end1 2))
    do
    (setf name (subseq name 2)))
  name)

(defstruct (extract-step (:constructor make-extract-step (kind path &key data mode target)) (:copier nil) (:predicate nil))
  "One write of a validated extraction: KIND is :DIRECTORY, :FILE (DATA,
MODE) or :SYMLINK (TARGET); PATH is workspace-relative."
  (kind :file :type (member :directory :file :symlink) :read-only t)
  (path "" :type string :read-only t)
  (data nil :read-only t)
  (mode nil :read-only t)
  (target nil :read-only t))

(defun %archive-entries (octets format)
  (ecase format
    (:zip (values (aitools.text.domain:read-zip-entries octets) octets))
    (:tar (values (aitools.text.domain:read-tar-entries octets) octets))))

(defun %archive-entry-name (entry)
  (%normalize-entry-name (aitools.text.domain:archive-entry-name entry)))

(defun %archive-entry-mode (entry)
  (or (aitools.text.domain:archive-entry-mode entry) #o644))

(defun %archive-entry-link-target (entry)
  (%normalize-entry-name (or (aitools.text.domain:archive-entry-link-target entry) "")))

(defun plan-archive-extraction (octets format destination &key archive-path selected max-bytes max-entries
                                                               lookup-kind)
  "The EXTRACT-STEPs extracting OCTETS (of FORMAT) below DESTINATION (a
workspace-relative directory, \"\" for the root). SELECTED limits the
entries by name (NIL: all). LOOKUP-KIND (relative-path) returns the path's
current kind (:absent :file :directory :symlink).

Signals EDIT-REFUSAL: refusal.outside-workspace (absolute names, parent
traversal, links leaving DESTINATION, or paths crossing symlinks),
refusal.too-large (MAX-BYTES of output or MAX-ENTRIES entries exceeded),
refusal.exists (a collision with an existing path or a duplicate normalized
name), input.not-found (a SELECTED name absent), input.unsupported-format
(device or other special entries). Selection, duplicate, and hard-link names
preserve case and Unicode spelling after removing empty and `.` components.
Case-folded NFD keys are used only to detect traversal through archive symlinks."
  (let ((budget max-bytes))
    (labels ((path-components (path &key keep-dots)
               (let ((components (aitools.workspace.domain:path-components path)))
                 (if keep-dots
                     components
                     (remove "." components :test (function string=)))))
             (path-name (components)
               (format nil "~{~A~^/~}" components))
             (normalized-name (path)
               (path-name (path-components path)))
             (path-key (path)
               (format nil "~{~A~^/~}"
                       (mapcar (lambda (component)
                                 (aitools.text.domain:normalize-text
                                  (sb-unicode:casefold
                                   (aitools.text.domain:normalize-text component :nfd))
                                  :nfd))
                               (path-components path))))
             (parent-paths (path)
               (let ((parents (butlast (path-components path))))
                 (loop for end from 1 to (length parents)
                       collect (path-name (subseq parents 0 end)))))
             (check-entry-parents (name symlink-names)
               (dolist (parent (parent-paths name))
                 (when (gethash (path-key parent) symlink-names)
                   (refuse "refusal.outside-workspace"
                           "archive entry ~S has symlink parent ~S" name parent))
                 (when (eq (funcall lookup-kind
                                    (aitools.workspace.domain:join-path destination parent))
                           :symlink)
                   (refuse "refusal.outside-workspace"
                           "archive entry ~S has existing symlink parent ~S" name parent))))
             (check-link-prefix (name directory symlink-names)
               (when directory
                 (let ((prefix (path-name directory)))
                   (when (gethash (path-key prefix) symlink-names)
                     (refuse "refusal.outside-workspace"
                             "archive symlink ~S traverses archive symlink ~S" name prefix))
                   (when (eq (funcall lookup-kind
                                      (aitools.workspace.domain:join-path destination prefix))
                             :symlink)
                     (refuse "refusal.outside-workspace"
                             "archive symlink ~S traverses existing symlink ~S" name prefix)))))
             (check-link-target (name target symlink-names)
               (when (or (zerop (length target))
                         (char= (char target 0) #\/)
                         (and (>= (length target) 2)
                              (char= (char target 1) #\:)
                              (alpha-char-p (char target 0)))
                         (find #\\ target)
                         (find (code-char 0) target))
                 (refuse "refusal.outside-workspace"
                         "archive symlink ~S has an absolute or invalid target ~S" name target))
               (let ((directory (butlast (path-components name)))
                     (components (path-components target :keep-dots t)))
                 (dolist (component components)
                   (check-link-prefix name directory symlink-names)
                   (cond ((string= component "..")
                          (if (null directory)
                              (refuse "refusal.outside-workspace"
                                      "archive symlink ~S points outside the extraction (~A)"
                                      name
                                      (aitools.text.domain:archive-link-target-problem name target))
                            (setf directory (butlast directory))))
                         ((string= component "."))
                         (t (setf directory (append directory (list component)))))
                   (check-link-prefix name directory symlink-names))))
             (take (data)
               ;; DATA was inflated with the budget left as its :MAX-OUTPUT, so
               ;; it never overdraws it: the inflater signals first.
               (decf budget (length data))
               data)
             (take-entry (body entry)
               ;; ENTRY's data inflated against the budget left: a file, or the
               ;; file a hard link names, whose bytes count again.
               (take (handler-case (aitools.text.domain:archive-entry-data body entry :max-output budget)
                       (aitools.text.domain:archive-limit-exceeded ()
                         (refuse "refusal.too-large" "the extracted size exceeds --max-bytes ~D" max-bytes))))))
      (if (eq format :gz)
          (let* ((name (aitools.text.domain:gzip-member-name octets (or archive-path "")))
                 (data (handler-case (aitools.text.domain:gzip-decompress octets :max-output max-bytes)
                         (aitools.text.domain:archive-limit-exceeded ()
                           (refuse "refusal.too-large" "the extracted size exceeds --max-bytes ~D" max-bytes))))
                 (path (aitools.workspace.domain:join-path destination name)))
            (unless (eq (funcall lookup-kind path) :absent)
              (refuse "refusal.exists" "~A already exists" path))
            (list (make-extract-step :file path :data (take data) :mode #o644)))
          (multiple-value-bind (raw-entries body)
              (if (eq format :tar-gz)
                  (let ((tar (handler-case (aitools.text.domain:gzip-decompress octets :max-output max-bytes)
                               (aitools.text.domain:archive-limit-exceeded ()
                                 (refuse "refusal.too-large" "the extracted size exceeds --max-bytes ~D"
                                         max-bytes)))))
                    (%archive-entries tar :tar))
                  (%archive-entries octets format))
            (let* ((records
                     (loop for entry in raw-entries
                           for name = (normalized-name (%archive-entry-name entry))
                           unless (string= name "")
                             collect (list entry name (path-key name))))
                   (entries-by-name (make-hash-table :test 'equal))
                   (symlink-names (make-hash-table :test 'equal))
                   (chosen nil)
                   (seen (make-hash-table :test 'equal))
                   (steps '()))
              (dolist (record records)
                (let ((entry (first record))
                      (name (second record)))
                  (unless (nth-value 1 (gethash name entries-by-name))
                    (setf (gethash name entries-by-name) record))
                  (when (eq (aitools.text.domain:archive-entry-kind entry) :symlink)
                    (setf (gethash (third record) symlink-names) t))))
              (setf chosen
                    (if selected
                        (loop for name in selected
                              for record = (gethash (normalized-name name) entries-by-name)
                              collect (or record
                                          (refuse "input.not-found"
                                                  "no entry ~S in the archive" name)))
                        records))
              (when (> (length chosen) max-entries)
                (refuse "refusal.too-large" "the archive has ~D entries, over --max-entries ~D"
                        (length chosen) max-entries))
              (let ((declared (reduce #'+ chosen
                                      :key (lambda (record)
                                             (aitools.text.domain:archive-entry-size
                                              (first record))))))
                (when (> declared max-bytes)
                  (refuse "refusal.too-large" "the entries declare ~D bytes, over --max-bytes ~D"
                          declared max-bytes)))
              (dolist (record chosen)
                (let* ((entry (first record))
                       (name (second record))
                       (problem (aitools.text.domain:archive-entry-path-problem
                                (aitools.text.domain:archive-entry-name entry)))
                       (kind (aitools.text.domain:archive-entry-kind entry))
                       (path (aitools.workspace.domain:join-path destination name)))
                  (when problem
                    (refuse "refusal.outside-workspace" "archive entry ~S is unsafe (~(~A~))" name problem))
                  (when (gethash name seen)
                    (refuse "refusal.exists" "archive entry ~S occurs twice" name))
                  (setf (gethash name seen) t)
                  (check-entry-parents name symlink-names)
                  (let ((existing (funcall lookup-kind path)))
                    (unless (or (eq existing :absent)
                                (and (eq kind :directory) (eq existing :directory)))
                      (refuse "refusal.exists" "~A already exists" path)))
                  (ecase kind
                    (:directory (push (make-extract-step :directory path) steps))
                    (:file
                     (push (make-extract-step :file path :data (take-entry body entry)
                                              :mode (%archive-entry-mode entry))
                           steps))
                    (:symlink
                     (let ((target (or (aitools.text.domain:archive-entry-link-target entry) "")))
                       (check-link-target name target symlink-names)
                       (push (make-extract-step :symlink path :target target) steps)))
                    (:hardlink
                     (let* ((target (%archive-entry-link-target entry))
                            (target-name (normalized-name target))
                            (source-record
                              (and (null (aitools.text.domain:archive-entry-path-problem target))
                                   (gethash target-name entries-by-name)))
                            (source (and source-record (first source-record))))
                       (unless (and source (eq (aitools.text.domain:archive-entry-kind source) :file))
                         (refuse "refusal.outside-workspace"
                                 "archive hard link ~S has no file ~S in the archive" name target))
                       (push (make-extract-step :file path :data (take-entry body source)
                                                :mode (%archive-entry-mode source))
                             steps)))
                    (:other
                     (refuse "input.unsupported-format" "archive entry ~S is a device or special file" name)))))
              (nreverse steps)))))))

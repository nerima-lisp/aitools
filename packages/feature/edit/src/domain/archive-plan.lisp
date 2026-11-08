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
  (let ((start 0)
        (length (length name)))
    (loop while (and (<= (+ start 2) length)
                     (char= (char name start) #\.)
                     (char= (char name (1+ start)) #\/))
          do (incf start 2))
    (if (zerop start) name (subseq name start))))

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

(defun plan-archive-extraction
    (octets format destination
     &key archive-path selected max-bytes max-entries lookup-kind
       make-prefix-checker)
  "The EXTRACT-STEPs extracting OCTETS below DESTINATION. LOOKUP-KIND
returns the current kind of a workspace-relative path. MAKE-PREFIX-CHECKER
builds the store application checker from the planned archive symlink paths.

Selection, duplicate, and hard-link names preserve case and Unicode spelling
after removing empty and `.` components. The checker handles symlink prefixes."
  (declare (type function lookup-kind make-prefix-checker))
  (let ((budget max-bytes))
    (labels ((path-problem (path max-bytes)
               (let ((bytes 0)
                     (component-bytes 0)
                     (component-start 0)
                     (depth 0))
                 (loop for index from 0 below (length path)
                       for char = (char path index)
                       for code = (char-code char)
                       for size = (cond ((<= code #x7f) 1)
                                        ((<= code #x7ff) 2)
                                        ((<= code #xffff) 3)
                                        (t 4))
                       do (incf bytes size)
                          (when (> bytes max-bytes)
                            (return-from path-problem :path-too-long))
                          (if (char= char #\/)
                              (progn
                                (when (> component-bytes
                                         aitools.text.domain:+archive-max-component-bytes+)
                                  (return-from path-problem :component-too-long))
                                (unless (or (= index component-start)
                                            (and (= component-bytes 1)
                                                 (char= (char path component-start) #\.)))
                                  (incf depth)
                                  (when (> depth aitools.text.domain:+archive-max-depth+)
                                    (return-from path-problem :too-deep)))
                                (setf component-start (1+ index)
                                      component-bytes 0))
                              (progn
                                (incf component-bytes size)
                                (when (> component-bytes
                                         aitools.text.domain:+archive-max-component-bytes+)
                                  (return-from path-problem :component-too-long))))
                       finally
                         (progn
                           (when (> component-bytes
                                    aitools.text.domain:+archive-max-component-bytes+)
                             (return-from path-problem :component-too-long))
                           (unless (or (= component-start (length path))
                                       (and (= component-bytes 1)
                                            (char= (char path component-start) #\.)))
                             (when (>= depth aitools.text.domain:+archive-max-depth+)
                               (return-from path-problem :too-deep)))))))
             (check-path-limit (path max-bytes label)
               (let ((problem (path-problem path max-bytes)))
                 (when problem
                   (refuse "refusal.too-large"
                           "~A exceeds the archive path limits (~(~A~))"
                           label problem))))
             (path-components (path)
               (remove "." (aitools.workspace.domain:path-components path)
                       :test (function string=)))
             (path-name (components)
               (format nil "~{~A~^/~}" components))
             (normalized-name (path)
               (path-name (path-components path)))
             (check-entry-parents (name checker)
                                  (let ((cursor (funcall checker :start)))
                                    (unwind-protect
                                        (loop for components on (path-components name)
                                              while (cdr components)
                                              do (multiple-value-bind (next result)
                                                     (funcall checker :enter cursor (car components))
                                                   (setf cursor next)
                                                   (when (eq result :symlink)
                                                     (refuse "refusal.outside-workspace"
                                                             "archive entry ~S crosses a symlink parent"
                                                             name))))
                                      (funcall checker :finish cursor)))) (check-link-target (name target checker)
  (when (or (zerop (length target))
            (char= (char target 0) #\/)
            (and (>= (length target) 2)
                 (char= (char target 1) #\:)
                 (alpha-char-p (char target 0)))
            (find #\\ target)
            (find (code-char 0) target))
    (refuse "refusal.outside-workspace"
            "archive symlink ~S has an absolute or invalid target ~S"
            name target))
  (let ((cursor (funcall checker :start)))
    (unwind-protect
         (progn
           (loop for components on (path-components name)
                 while (cdr components)
                 do (multiple-value-bind (next result)
                        (funcall checker :enter cursor (car components))
                      (setf cursor next)
                      (when (eq result :symlink)
                        (refuse "refusal.outside-workspace"
                                "archive symlink ~S has a symlink parent"
                                name))))
           (loop with start = 0
                 for slash = (position #\/ target :start start)
                 for end = (or slash (length target))
                 for component = (subseq target start end)
                 do (cond
                      ((or (string= component "")
                           (string= component "."))
                       nil)
                      ((string= component "..")
                       (multiple-value-bind (next result)
                           (funcall checker :parent cursor)
                         (setf cursor next)
                         (when (eq result :outside)
                           (refuse "refusal.outside-workspace"
                                   "archive symlink ~S points outside the extraction"
                                   name))))
                      (t
                       (multiple-value-bind (next result)
                           (funcall checker :enter cursor component)
                         (setf cursor next)
                         (when (eq result :symlink)
                           (refuse "refusal.outside-workspace"
                                   "archive symlink ~S traverses a symlink prefix"
                                   name)))))
                    (if slash
                        (setf start (1+ slash))
                        (return))))
      (funcall checker :finish cursor)))) (take (data)
               (decf budget (length data))
               data) (take-entry (body entry)
               (take
                (handler-case
                    (aitools.text.domain:archive-entry-data
                     body entry :max-output budget)
                  (aitools.text.domain:archive-limit-exceeded ()
                    (refuse "refusal.too-large"
                            "the extracted size exceeds --max-bytes ~D"
                            max-bytes)))))) (if (eq format :gz)
          (let ((name (handler-case (aitools.text.domain:gzip-member-name octets (or archive-path "")) (aitools.text.domain:archive-limit-exceeded (condition) (refuse "refusal.too-large" "archive exceeds reader limit ~A" (aitools.text.domain:archive-limit-exceeded-limit condition))))))
            (check-path-limit
             name aitools.text.domain:+archive-max-path-bytes+ "gzip member name")
            (let* ((data
                     (handler-case
                         (aitools.text.domain:gzip-decompress
                          octets :max-output max-bytes)
                       (aitools.text.domain:archive-limit-exceeded ()
                         (refuse "refusal.too-large"
                                 "the extracted size exceeds --max-bytes ~D"
                                 max-bytes))))
                   (path (aitools.workspace.domain:join-path destination name)))
              (unless (eq (funcall lookup-kind path) :absent)
                (refuse "refusal.exists" "~A already exists" path))
              (list (make-extract-step :file path :data (take data) :mode #o644))))
          (multiple-value-bind (raw-entries body)
              (handler-case
                  (if (eq format :tar-gz)
                      (let ((tar
                              (handler-case
                                  (aitools.text.domain:gzip-decompress
                                   octets :max-output max-bytes)
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
              (check-path-limit
               (aitools.text.domain:archive-entry-name entry)
               aitools.text.domain:+archive-max-path-bytes+
               "archive entry name")
              (when (member (aitools.text.domain:archive-entry-kind entry)
                            '(:symlink :hardlink))
                (check-path-limit
                 (or (aitools.text.domain:archive-entry-link-target entry) "")
                 aitools.text.domain:+archive-max-link-target-bytes+
                 "archive link target")))
            (dolist (name selected)
              (check-path-limit name aitools.text.domain:+archive-max-path-bytes+
                                "selected archive entry name"))
            (let* ((records
                     (loop for entry in raw-entries
                           for name = (normalized-name (%archive-entry-name entry))
                           unless (string= name "")
                             collect (list entry name)))
                   (entries-by-name (make-hash-table :test 'equal))
                   (archive-symlink-paths
                     (loop for record in records
                           for entry = (first record)
                           when (eq (aitools.text.domain:archive-entry-kind entry)
                                    :symlink)
                             collect (second record)))
                   (checker (funcall make-prefix-checker archive-symlink-paths))
                   (chosen nil)
                   (seen (make-hash-table :test 'equal))
                   (steps '()))
              (unwind-protect
  (progn
  (dolist (record records)
    (let ((entry (first record))
          (name (second record)))
      (unless (nth-value 1 (gethash name entries-by-name))
        (setf (gethash name entries-by-name) record)))) (setf chosen
                    (if selected
                        (loop for name in selected
                              for record = (gethash (normalized-name name)
                                                    entries-by-name)
                              collect (or record
                                          (refuse "input.not-found"
                                                  "no entry ~S in the archive"
                                                  name)))
                        records)) (when (> (length chosen) max-entries)
                (refuse "refusal.too-large"
                        "the archive has ~D entries, over --max-entries ~D"
                        (length chosen) max-entries)) (let ((declared
                      (reduce #'+ chosen
                              :key (lambda (record)
                                     (aitools.text.domain:archive-entry-size
                                      (first record))))))
                (when (> declared max-bytes)
                  (refuse "refusal.too-large"
                          "the entries declare ~D bytes, over --max-bytes ~D"
                          declared max-bytes))) (dolist (record chosen)
                (let* ((entry (first record))
                       (name (second record))
                       (problem
                         (aitools.text.domain:archive-entry-path-problem
                          (aitools.text.domain:archive-entry-name entry)))
                       (kind (aitools.text.domain:archive-entry-kind entry))
                       (path (aitools.workspace.domain:join-path destination name)))
                  (when problem
                    (refuse "refusal.outside-workspace"
                            "archive entry ~S is unsafe (~(~A~))"
                            name problem))
                  (when (gethash name seen)
                    (refuse "refusal.exists"
                            "archive entry ~S occurs twice" name))
                  (setf (gethash name seen) t)
                  (check-entry-parents name checker)
                  (let ((existing (funcall lookup-kind path)))
                    (unless (or (eq existing :absent)
                                (and (eq kind :directory)
                                     (eq existing :directory)))
                      (refuse "refusal.exists" "~A already exists" path)))
                  (ecase kind
                    (:directory
                     (push (make-extract-step :directory path) steps))
                    (:file
                     (push (make-extract-step
                            :file path :data (take-entry body entry)
                            :mode (%archive-entry-mode entry))
                           steps))
                    (:symlink
                     (let ((target
                             (or (aitools.text.domain:archive-entry-link-target
                                  entry)
                                 "")))
                       (check-link-target name target checker)
                       (push (make-extract-step :symlink path :target target)
                             steps)))
                    (:hardlink
                     (let* ((target (%archive-entry-link-target entry))
                            (target-name (normalized-name target))
                            (source-record
                              (and (null
                                    (aitools.text.domain:archive-entry-path-problem
                                     target))
                                   (gethash target-name entries-by-name)))
                            (source (and source-record (first source-record))))
                       (unless (and source
                                    (eq (aitools.text.domain:archive-entry-kind
                                         source)
                                        :file))
                         (refuse "refusal.outside-workspace"
                                 "archive hard link ~S has no file ~S in the archive"
                                 name target))
                       (push (make-extract-step
                              :file path :data (take-entry body source)
                              :mode (%archive-entry-mode source))
                             steps)))
                    (:other
                     (refuse "input.unsupported-format"
                             "archive entry ~S is a device or special file"
                             name))))) (nreverse steps))
  (funcall checker :finish))))))))

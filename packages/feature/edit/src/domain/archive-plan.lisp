;;;; packages/feature/edit/src/domain/archive-plan.lisp
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
    (flet ((ends (suffix) (let ((start (- (length lower) (length suffix))))
                            (and (>= start 0) (string= suffix lower :start2 start)))))
      (cond ((ends ".zip") :zip)
            ((or (ends ".tar.gz") (ends ".tgz")) :tar-gz)
            ((ends ".tar") :tar)
            ((ends ".gz") :gz)))))

(defun parse-archive-format (name)
  (cdr (assoc name '(("zip" . :zip) ("tar" . :tar) ("tar.gz" . :tar-gz) ("gz" . :gz)) :test #'string=)))

(defun %normalize-entry-name (name)
  "NAME without leading `./` components."
  (loop while (and (>= (length name) 2) (string= name "./" :end1 2))
        do (setf name (subseq name 2)))
  name)

(defstruct (extract-step (:constructor make-extract-step (kind path &key data mode target)) (:copier nil))
  "One write of a validated extraction: KIND is :DIRECTORY, :FILE (DATA,
MODE) or :SYMLINK (TARGET); PATH is workspace-relative."
  (kind :file :type (member :directory :file :symlink) :read-only t)
  (path "" :type string :read-only t)
  (data nil :read-only t)
  (mode nil :read-only t)
  (target nil :read-only t))

(defun gz-member-name (octets archive-path)
  "The name a `.gz` member extracts to: its FNAME header when present, else
the archive's base name without `.gz`."
  (let ((name (ignore-errors (nth-value 0 (aitools.text.domain:gzip-member-header octets))))
        (base (subseq archive-path (1+ (or (position #\/ archive-path :from-end t) -1)))))
    (if (and name (plusp (length name)) (null (aitools.text.domain:archive-entry-path-problem name)) (not (find #\/ name)))
        name
        (let ((dot (search ".gz" base :from-end t :test #'char-equal)))
          (if (and dot (plusp dot)) (subseq base 0 dot) (concatenate 'string base ".out"))))))

(defun %archive-entries (octets format)
  (ecase format
    (:zip (values (aitools.text.domain:read-zip-entries octets) octets))
    (:tar (values (aitools.text.domain:read-tar-entries octets) octets))))

(defun plan-archive-extraction (octets format destination &key archive-path selected max-bytes max-entries
                                                               lookup-kind)
  "The EXTRACT-STEPs extracting OCTETS (of FORMAT) below DESTINATION (a
workspace-relative directory, \"\" for the root). SELECTED limits the
entries by name (NIL: all). LOOKUP-KIND (relative-path) returns the path's
current kind (:absent :file :directory :symlink).

Signals EDIT-REFUSAL: refusal.outside-workspace (absolute names, `..`,
backslashes, links leaving DESTINATION), refusal.too-large (MAX-BYTES of
output or MAX-ENTRIES entries exceeded), refusal.exists (a collision with
an existing path or a duplicate name), input.not-found (a SELECTED name
absent), input.unsupported-format (device or other special entries)."
  (let ((budget max-bytes))
    (labels ((take (data)
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
          (let* ((name (gz-member-name octets (or archive-path "")))
                 (data (handler-case (aitools.text.domain:gzip-decompress octets :max-output max-bytes)
                         (aitools.text.domain:archive-limit-exceeded ()
                           (refuse "refusal.too-large" "the extracted size exceeds --max-bytes ~D" max-bytes))))
                 (path (aitools.workspace.domain:join-path destination name)))
            (unless (eq (funcall lookup-kind path) :absent)
              (refuse "refusal.exists" "~A already exists" path))
            (list (make-extract-step :file path :data (take data) :mode #o644)))
          (multiple-value-bind (entries body)
              (if (eq format :tar-gz)
                  (let ((tar (handler-case (aitools.text.domain:gzip-decompress octets :max-output max-bytes)
                               (aitools.text.domain:archive-limit-exceeded ()
                                 (refuse "refusal.too-large" "the extracted size exceeds --max-bytes ~D"
                                          max-bytes)))))
                    (%archive-entries tar :tar))
                  (%archive-entries octets format))
            (let* ((entries (remove-if (lambda (entry)
                                         (member (%normalize-entry-name (aitools.text.domain:archive-entry-name entry)) '("" ".")
                                                 :test #'string=))
                                       entries))
                   (chosen (if selected
                               (loop for name in selected
                                     collect (or (find name entries
                                                       :key (lambda (entry) (%normalize-entry-name (aitools.text.domain:archive-entry-name entry)))
                                                       :test #'string=)
                                                 (refuse "input.not-found" "no entry ~S in the archive" name)))
                               entries))
                   (seen (make-hash-table :test 'equal))
                   (steps '()))
              (when (> (length chosen) max-entries)
                (refuse "refusal.too-large" "the archive has ~D entries, over --max-entries ~D"
                         (length chosen) max-entries))
              (let ((declared (reduce #'+ chosen :key #'aitools.text.domain:archive-entry-size)))
                (when (> declared max-bytes)
                  (refuse "refusal.too-large" "the entries declare ~D bytes, over --max-bytes ~D" declared max-bytes)))
              (dolist (entry chosen)
                (let* ((name (%normalize-entry-name (aitools.text.domain:archive-entry-name entry)))
                       (problem (aitools.text.domain:archive-entry-path-problem name))
                       (kind (aitools.text.domain:archive-entry-kind entry))
                       (path (aitools.workspace.domain:join-path destination name)))
                  (when problem
                    (refuse "refusal.outside-workspace" "archive entry ~S is unsafe (~(~A~))" name problem))
                  (when (gethash name seen)
                    (refuse "refusal.exists" "archive entry ~S occurs twice" name))
                  (setf (gethash name seen) t)
                  (let ((existing (funcall lookup-kind path)))
                    (unless (or (eq existing :absent) (and (eq kind :directory) (eq existing :directory)))
                      (refuse "refusal.exists" "~A already exists" path)))
                  (ecase kind
                    (:directory (push (make-extract-step :directory path) steps))
                    (:file
                     (push (make-extract-step :file path :data (take-entry body entry)
                                                         :mode (or (aitools.text.domain:archive-entry-mode entry) #o644))
                           steps))
                    (:symlink
                     (let ((target (or (aitools.text.domain:archive-entry-link-target entry) "")))
                       (when (aitools.text.domain:archive-link-target-problem name target)
                         (refuse "refusal.outside-workspace" "archive symlink ~S points outside the extraction (~A)"
                                  name target))
                       (push (make-extract-step :symlink path :target target) steps)))
                    (:hardlink
                     (let* ((target (%normalize-entry-name (or (aitools.text.domain:archive-entry-link-target entry) "")))
                            (source (and (null (aitools.text.domain:archive-entry-path-problem target))
                                         (find target entries :key (lambda (e) (%normalize-entry-name (aitools.text.domain:archive-entry-name e)))
                                                              :test #'string=))))
                       (unless (and source (eq (aitools.text.domain:archive-entry-kind source) :file))
                         (refuse "refusal.outside-workspace" "archive hard link ~S has no file ~S in the archive"
                                  name target))
                       (push (make-extract-step :file path :data (take-entry body source)
                                                           :mode (or (aitools.text.domain:archive-entry-mode source) #o644))
                             steps)))
                    (:other
                     (refuse "input.unsupported-format" "archive entry ~S is a device or special file" name)))))
              (nreverse steps)))))))

(defun build-archive (format members &key name)
  "The bytes of an archive of FORMAT holding MEMBERS (text-domain
ARCHIVE-MEMBERs); :GZ takes exactly one file member and stores NAME."
  (ecase format
    (:zip (aitools.text.domain:write-zip members))
    (:tar (aitools.text.domain:write-tar members))
    (:tar-gz (aitools.text.domain:gzip-compress (aitools.text.domain:write-tar members)))
    (:gz (aitools.text.domain:gzip-compress (aitools.text.domain:archive-member-data (first members)) :name name))))

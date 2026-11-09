(in-package #:aitools.edit.domain)

(defun path-problem (path max-bytes)
  (let ((bytes 0)
        (component-bytes 0)
        (component-start 0)
        (depth 0))
    (loop for index from 0 below (length path)
          for char = (char path index)
          for code = (char-code char)
          for size = (cond
                       ((<= code #x7f) 1)
                       ((<= code #x7ff) 2)
                       ((<= code #xffff) 3)
                       (t 4))
          do (incf bytes size) (when (> bytes max-bytes)
                                 (return-from path-problem
                                   :path-too-long)) (if (char= char #\/)
                                                        (progn
                                                          (when
                                                              (> component-bytes
                                                                 aitools.text.domain:+archive-max-component-bytes+)
                                                            (return-from
                                                                path-problem
                                                              :component-too-long))
                                                          (unless
                                                              (or
                                                               (= index
                                                                  component-start)
                                                               (and
                                                                (=
                                                                 component-bytes
                                                                 1)
                                                                (char=
                                                                 (char path
                                                                       component-start)
                                                                 #\.)))
                                                            (incf depth)
                                                            (when
                                                                (> depth
                                                                   aitools.text.domain:+archive-max-depth+)
                                                              (return-from
                                                                  path-problem
                                                                :too-deep)))
                                                          (setf component-start (1+
                                                                                 index)
                                                                component-bytes 0))
                                                        (progn
                                                          (incf component-bytes
                                                                size)
                                                          (when
                                                              (> component-bytes
                                                                 aitools.text.domain:+archive-max-component-bytes+)
                                                            (return-from
                                                                path-problem
                                                              :component-too-long))))
          finally (progn
                    (when
                        (> component-bytes
                           aitools.text.domain:+archive-max-component-bytes+)
                      (return-from path-problem
                        :component-too-long))
                    (unless
                        (or (= component-start (length path))
                            (and (= component-bytes 1)
                                 (char= (char path component-start) #\.)))
                      (when (>= depth aitools.text.domain:+archive-max-depth+)
                        (return-from path-problem
                          :too-deep)))))))

(defun check-path-limit (path max-bytes label)
  (let ((problem (path-problem path max-bytes)))
    (when problem
      (refuse "refusal.too-large"
              "~A exceeds the archive path limits (~(~A~))"
              label
              problem))))

(defun path-components (path)
  (remove "."
          (aitools.workspace.domain:path-components path)
          :test
          (function string=)))

(defun path-name (components)
  (format nil "~{~A~^/~}" components))

(defun normalized-name (path)
  (path-name (path-components path)))

(defun check-entry-parents (name checker)
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
      (funcall checker :finish cursor))))

(defun %check-link-target-syntax (name target)
  (when
      (or (zerop (length target))
          (char= (char target 0) #\/)
          (and (>= (length target) 2)
               (char= (char target 1) #\:)
               (alpha-char-p (char target 0)))
          (find #\\ target)
          (find (code-char 0) target))
    (refuse "refusal.outside-workspace"
            "archive symlink ~S has an absolute or invalid target ~S"
            name
            target)))

(defun %check-link-target-traversal (name target checker)
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
                for
                end = (or slash (length target))
                for component = (subseq target start end)
                do (cond
                     ((or (string= component "") (string= component ".")) nil)
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
                                  name))))) (if slash
                                                (setf start (1+ slash))
                                                (return))))
      (funcall checker :finish cursor))))

(defun %normalize-entry-name (name)
  "NAME without leading `./` components."
  (let ((start 0)
        (length (length name)))
    (loop while (and (<= (+ start 2) length)
                     (char= (char name start) #\.)
                     (char= (char name (1+ start)) #\/))
          do (incf start 2))
    (if (zerop start)
        name
        (subseq name start))))

(defun %archive-entry-name (entry)
  (%normalize-entry-name (aitools.text.domain:archive-entry-name entry)))

(defun %archive-entry-mode (entry)
  (or (aitools.text.domain:archive-entry-mode entry) #o644))

(defun %archive-entry-link-target (entry)
  (%normalize-entry-name
   (or (aitools.text.domain:archive-entry-link-target entry) "")))

(defun %archive-entries (octets format)
  (ecase format
    (:zip (values (aitools.text.domain:read-zip-entries octets) octets))
    (:tar (values (aitools.text.domain:read-tar-entries octets) octets))))

(defstruct
    (extract-step
     (:constructor make-extract-step (kind path &key data mode target))
     (:copier nil)
     (:predicate nil))
  "One write of a validated extraction: KIND is :DIRECTORY, :FILE (DATA,
MODE) or :SYMLINK (TARGET); PATH is workspace-relative."
  (kind :file :type (member :directory :file :symlink) :read-only t)
  (path "" :type string :read-only t)
  (data nil :read-only t)
  (mode nil :read-only t)
  (target nil :read-only t))

(defun parse-archive-format (name)
  (cdr
   (assoc name
          '(("zip" . :zip) ("tar" . :tar) ("tar.gz" . :tar-gz) ("gz" . :gz))
          :test
          #'string=)))

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

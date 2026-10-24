(in-package #:aitools.edit.application)

(defun path-candidates (view path)
  "Up to 3 entries of PATH's directory with names closest to PATH's, as
{path} JSON objects, for input.not-found."
  (let* ((slash (position #\/ path :from-end t))
         (directory
          (if slash
              (subseq path 0 slash)
              ""))
         (name
          (subseq path
                  (if slash
                      (1+ slash)
                      0)))
         (entries
          (handler-case (aitools.store.application:view-directory-entries view directory)
            (error ()
              '()))))
    (loop for (nil . candidate) in (sort
                                    (mapcar
                                     (lambda (entry)
                                       (cons (levenshtein-distance name (car entry))
                                             (if (zerop (length directory))
                                                 (car entry)
                                                 (concatenate 'string directory "/" (car entry)))))
                                     entries)
                                    (lambda (a b)
                                      (or (< (car a) (car b)) (and (= (car a) (car b)) (string< (cdr a) (cdr b))))))
          repeat 3
          collect (json-object "path" candidate))))

(aitools.kernel.domain:define-flow
    (read-file-octets/k context path &key max-bytes on-error on-octets)
  "PATH's bytes in the write's view: ON-OCTETS (octets and mode), or ON-ERROR
with input.not-found (including candidates), refusal.not-a-file, or
refusal.too-large when MAX-BYTES is exceeded."
  (let* ((view (write-context-view context))
         ;; Only the kind and mode decide the branch here; VIEW-READ-FILE
         ;; reads the bytes below, so hashing the file through VIEW-PATH-STATE
         ;; would read it a second time for a value never used.
         (state (aitools.store.application:view-path-kind view path)))
    (case (aitools.store.domain:entry-state-kind state)
      (:file
       (multiple-value-bind (octets too-large-p)
           (handler-case
               (values (aitools.store.application:view-read-file
                        view path :max-bytes max-bytes)
                       nil)
             (aitools.store.application:store-byte-limit-error ()
               (values nil t)))
         (if too-large-p
             (aitools.kernel.domain:flow-continue
              :on-error "refusal.too-large"
              (format nil "~A exceeds the ~D-byte document limit"
                      path max-bytes))
             (aitools.kernel.domain:flow-continue
              :on-octets
              (coerce octets 'octets)
              (aitools.store.domain:entry-state-mode state)))))
      (:absent
       (aitools.kernel.domain:flow-continue
        :on-error "input.not-found" (format nil "~A does not exist" path)
        :candidates (path-candidates view path)))
      (t
       (aitools.kernel.domain:flow-continue
        :on-error "refusal.not-a-file"
        (format nil "~A is a ~(~A~), not a file"
                path (aitools.store.domain:entry-state-kind state)))))))

(aitools.kernel.domain:define-flow
  (read-document/k context path &key prefilter max-bytes on-error on-document on-filtered)
  "PATH decoded for a text edit: ON-DOCUMENT (document), or ON-ERROR for
refusal.not-a-file (binary), input.not-utf8, input.not-found, or
refusal.too-large when MAX-BYTES is exceeded.

When PREFILTER returns NIL for PATH's raw octets, ON-FILTERED is called
without decoding them. The prefilter must be sound: NIL must mean that no
possible match exists."
  (aitools.kernel.domain:flow-call
    (read-file-octets/k context path :max-bytes max-bytes)
    (:on-octets (octets mode)
      (declare (ignore mode))
      (if (and prefilter (not (funcall prefilter octets)))
          (aitools.kernel.domain:flow-continue
            :on-filtered)
          (aitools.kernel.domain:flow-call
            (decode-text-document/k octets)
            (:on-decoded (document)
              (aitools.kernel.domain:flow-continue
                :on-document
                document))
            (:on-binary ()
              (aitools.kernel.domain:flow-continue
                :on-error
                "refusal.not-a-file"
                (format
                  nil
                  "~A is binary; text edits refuse it"
                  path)))
            (:on-invalid (offset)
              (aitools.kernel.domain:flow-continue
                :on-error
                "input.not-utf8"
                (format
                  nil
                  "~A is not valid UTF-8 at byte ~D"
                  path
                  offset)
                :diagnostics
                (list (json-object "path" path "offset" offset)))))))
    (:on-error (code message &key candidates diagnostics conflicts repairs path)
      (aitools.kernel.domain:flow-continue
        :on-error
        code
        message
        :candidates
        candidates
        :diagnostics
        diagnostics
        :conflicts
        conflicts
        :repairs
        repairs
        :path
        path))))

(defun %child (directory name)
  "NAME below the workspace-relative DIRECTORY (\"\" for the root)."
  (if (zerop (length directory))
      name
      (concatenate 'string directory "/" name)))

(defun write-document-request (path document)
  (aitools.store.domain:write-file-request path (render-document document)))

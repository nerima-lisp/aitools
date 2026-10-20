(in-package #:aitools.edit.application)

(defun path-candidates (view path)
  "Up to 3 entries of PATH's directory with names closest to PATH's, as
{path} JSON objects, for input.not-found."
  (let* ((slash (position #\/ path :from-end t))
         (directory (if slash (subseq path 0 slash) ""))
         (name (subseq path (if slash (1+ slash) 0)))
         (entries (handler-case (aitools.store.application:view-directory-entries view directory)
                    (error () '()))))
    (loop for (nil . candidate)
            in (sort (mapcar (lambda (entry)
                               (cons (levenshtein-distance name (car entry))
                                     (if (zerop (length directory)) (car entry)
                                         (concatenate 'string directory "/" (car entry)))))
                             entries)
                     (lambda (a b) (or (< (car a) (car b))
                                       (and (= (car a) (car b)) (string< (cdr a) (cdr b))))))
          repeat 3
          collect (json-object "path" candidate))))

(defun read-file-octets/k (context path reject on-octets)
  "PATH's bytes in the write's view: ON-OCTETS (octets mode), or REJECT with
input.not-found (with candidates) or refusal.not-a-file."
  (declare (type function reject on-octets))
  (let* ((view (write-context-view context))
         ;; Only the kind and mode decide the branch here; VIEW-READ-FILE
         ;; reads the bytes below, so hashing the file through VIEW-PATH-STATE
         ;; would read it a second time for a value never used.
         (state (aitools.store.application:view-path-kind view path)))
    (case (aitools.store.domain:entry-state-kind state)
      (:file (funcall on-octets (coerce (aitools.store.application:view-read-file view path) 'octets)
                      (aitools.store.domain:entry-state-mode state)))
      (:absent (funcall reject "input.not-found" (format nil "~A does not exist" path)
                        :candidates (path-candidates view path)))
      (t (funcall reject "refusal.not-a-file" (format nil "~A is a ~(~A~), not a file" path
                                                      (aitools.store.domain:entry-state-kind state)))))))

(defun read-document/k (context path reject on-document &key prefilter on-filtered)
  "PATH decoded for a text edit: ON-DOCUMENT (document), or
REJECT refusal.not-a-file (binary), input.not-utf8, input.not-found.

When PREFILTER (a predicate on the file's raw octets) and ON-FILTERED are
both supplied and PREFILTER returns NIL, PATH cannot match, so ON-FILTERED
is called without decoding the bytes. The prefilter must be sound: a NIL
result must mean no possible match."
  (declare (type function reject on-document))
  (read-file-octets/k context path reject
                      (lambda (octets mode)
                        (declare (ignore mode))
                        (if (and prefilter on-filtered (not (funcall prefilter octets)))
                            (funcall on-filtered)
                          (decode-text-document/k
                           octets
                           :on-decoded on-document
                           :on-binary (lambda ()
                                        (funcall reject "refusal.not-a-file"
                                                 (format nil "~A is binary; text edits refuse it" path)))
                           :on-invalid (lambda (offset)
                                         (funcall reject "input.not-utf8"
                                                  (format nil "~A is not valid UTF-8 at byte ~D" path offset)
                                                  :diagnostics (list (json-object "path" path "offset" offset)))))))))

(defun %child (directory name)
  "NAME below the workspace-relative DIRECTORY (\"\" for the root)."
  (if (zerop (length directory)) name (concatenate 'string directory "/" name)))

(defun write-document-request (path document)
  (aitools.store.domain:write-file-request path (render-document document)))

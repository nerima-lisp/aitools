;;;; packages/feature/inspect/src/application/info-flow.lisp
;;;;
;;;; `info`: the `wc`, `stat`, `file`, `sha256sum`, `test -e`, and
;;;; `realpath` replacement. Path fields describe where the name leads
;;;; (including a symlink out of the workspace); content fields describe the
;;;; bytes, through the tx view when `--tx` is given.
(in-package #:aitools.inspect.application)

(defun %digest-value (algorithm octets)
  (cond ((string= algorithm "sha256") (sha256-hex octets))
        ((string= algorithm "sha1") (sha1-hex octets))
        (t (md5-hex octets))))

(defun %path-fields (context target)
  (let* ((host (context-host context))
         (absolute (file-target-absolute target))
         (root-path (workspace-root-path (inspect-context-root context)))
         (lexical-inside (path-inside-p root-path absolute))
         (lstat (host-stat host absolute))
         (exists (not (eq (file-target-kind target) :absent)))
         (kind (cond (exists (string-downcase (symbol-name (file-target-kind target))))
                     ((and lstat (eq (workspace-entry-kind lstat) :symlink)) "symlink")
                     (t nil))))
    (list (cons "absolute" absolute)
          (cons "real" (json-or-null (file-target-real target)))
          (cons "relative" (json-or-null (and lexical-inside
                                              (let ((relative (path-relative-to root-path absolute)))
                                                (if (string= relative "") "." relative)))))
          (cons "exists" (json-bool exists))
          (cons "kind" (json-or-null kind))
          (cons "inside_workspace" (json-bool (file-target-relative target)))
          (cons "ignored" (json-bool (and (file-target-relative target)
                                          (plusp (length (file-target-relative target)))
                                          (workspace-path-ignored-p host (inspect-context-root context)
                                                                    (file-target-relative target))))))))

(defun %stat-fields (context target)
  "(VALUES mode mtime) of TARGET: for a path the tx changed, the staged mode
and the staged mtime (a tx `touch`; NIL after any other change), else the
disk's."
  (let ((view (inspect-context-view context))
        (relative (file-target-relative target)))
    (if (and view relative (plusp (length relative))
             (let ((staged (view-path-state view relative)))
               (or (entry-state-mtime staged)
                   (not (entry-state-equal staged (workspace-state (inspect-context-store context) relative))))))
        (let ((staged (view-path-state view relative)))
          (values (entry-state-mode staged) (entry-state-mtime staged)))
        (let ((entry (and (file-target-real target) (host-stat (context-host context) (file-target-real target)))))
          (if entry
              (values (workspace-entry-mode entry) (workspace-entry-mtime entry))
              (values nil nil))))))

(defun %content-fields (octets target digest)
  ;; TEXT-CONTENT-COUNTS scans the decoded bytes once instead of
  ;; materializing a per-line string vector, so the byte counts below are
  ;; unchanged while the large intermediate consing of DECODE-TEXT-LINES is
  ;; gone. LINE-COUNT/WORD-COUNT/MAX-CHARS/CHARACTERS are meaningful only for
  ;; text; a binary file reports them as null exactly as before.
  (let* ((binary (binary-octets-p octets))
         (layout (detect-text-layout octets)))
    (multiple-value-bind (line-count word-count max-chars characters)
        (if binary (values nil nil nil nil) (text-content-counts octets))
      (append
       (list (cons "size" (length octets))
             (cons "lines" (json-or-null line-count))
             (cons "words" (json-or-null word-count))
             (cons "max_line_chars" (json-or-null max-chars))
             (cons "binary" (json-bool binary))
           (cons "mime" (guess-mime (subseq octets 0 (min (length octets) +binary-sniff-length+))
                                    :path (file-target-absolute target)))
           (cons "utf8_valid" (json-bool (utf8-valid-p octets)))
           (cons "encoding_guess" (string-downcase (symbol-name (guess-encoding octets))))
           (cons "line_ending" (json-or-null (and (not binary) (line-ending-name (text-layout-line-ending layout)))))
           (cons "trailing_newline" (json-bool (text-layout-final-newline-p layout)))
           (cons "bom" (json-bool (text-layout-bom-p layout))))
       (list (cons "hash" (content-hash octets))
             (cons "approx_tokens" (json-or-null (and characters (approx-token-count characters)))))
       (when digest
         (list (cons "digest" (json-object "algorithm" digest "value" (%digest-value digest octets)))))))))

(defun %link-hash-fields (context target)
  "`hash` for a path that is a symlink leading to no regular file (PATH-HASH's
rule, so the value is the one --expect-hash compares), else nothing."
  (let ((hash (path-hash (context-host context) (context-root-real context) (inspect-context-view context)
                         (file-target-absolute target))))
    (when hash (list (cons "hash" hash)))))

(defun info-flow (ports path &key root tx lock-timeout digest allow-missing on-ok on-partial on-error)
  "`info`. Calls ON-OK (fields) or ON-ERROR."
  (declare (ignore on-partial) (type function on-ok on-error))
  (call-with-inspect-context/k
   ports :root root :tx tx :lock-timeout lock-timeout :on-error on-error
   :on-ready
   (lambda (context)
     (let* ((target (probe-target context path))
            (path-fields (list* (cons "path" (target-display-path target)) (%path-fields context target))))
       (flet ((with-stat (fields)
                (multiple-value-bind (mode mtime) (%stat-fields context target)
                  (append fields
                          (list (cons "mode" (json-or-null (and mode (format-file-mode mode))))
                                (cons "mtime" (json-or-null (and mtime (iso8601-from-unix mtime)))))))))
         (case (file-target-kind target)
           (:absent
            (if allow-missing
                (funcall on-ok (append path-fields (%link-hash-fields context target)))
                (fail-missing context target on-error)))
           (:file
            (record-read/k
             context target
             :on-error on-error
             :on-recorded
             (lambda ()
               (multiple-value-bind (octets problem) (read-target-octets context target)
                 (if octets
                     (let* ((content (%content-fields octets target digest))
                            (hash-tail (member "hash" content :key #'car :test #'string=)))
                       ;; mode and mtime sit between the layout fields and hash.
                       (funcall on-ok (append path-fields
                                              (with-stat (ldiff content hash-tail))
                                              hash-tail)))
                     (fail-target-read context target on-error problem))))))
           (t
            (let ((entry (and (file-target-real target) (host-stat (context-host context) (file-target-real target)))))
              ;; hash follows mode and mtime, as it does for a file.
              (funcall on-ok (append (with-stat (append path-fields
                                                        (list (cons "size" (if entry (workspace-entry-size entry) 0)))))
                                     (%link-hash-fields context target)))))))))))

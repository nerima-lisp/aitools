;;;; packages/feature/edit/src/application/content-flows.lisp
;;;;
;;;; `write`, `split` and `transcode`: writes of
;;;; whole-file bytes, exempt from the UTF-8 text-edit rule where
;;;; docs/src/reference/commands.md says so.
(in-package #:aitools.edit.application)

(defun %concatenate-octets (parts separator)
  (let ((pieces (loop for (part . rest) on parts
                      collect part
                      when (and rest separator) collect separator)))
    (let ((result (make-array (reduce #'+ pieces :key #'length) :element-type '(unsigned-byte 8)))
          (offset 0))
      (dolist (piece pieces result)
        (replace result piece :start1 offset)
        (incf offset (length piece))))))

(defun %write-inputs/k (ports options fail on-octets)
  "`write`'s content: every --content (strings), every --content-file
(bytes), or --stdin, joined by --separator. ON-OCTETS (octets inputs)."
  (declare (type function fail on-octets))
  (let* ((contents (getf options :content))
         (files (getf options :content-file))
         (stdin (or (getf options :stdin) (getf options :stdin-data)))
         (given (count-if #'identity (list contents files stdin)))
         (separator (and (getf options :separator) (aitools.text.domain:encode-utf8 (getf options :separator)))))
    (cond
      ((zerop given)
       (funcall fail "argument.invalid" "no content: pass --content, --content-file, or --stdin (stdin is never read implicitly)"))
      ((> given 1)
       (funcall fail "argument.invalid" "pass --content (repeatable), --content-file (repeatable), or --stdin, not a mix"))
      (contents
       (let ((parts (mapcar #'aitools.text.domain:encode-utf8 contents)))
         (funcall on-octets (%concatenate-octets parts separator) contents)))
      (files
       (let ((parts '()))
         (dolist (file files)
           (read-input-file/k ports file
                              :on-octets (lambda (octets) (push octets parts))
                              :on-error (lambda (code message)
                                          (return-from %write-inputs/k (funcall fail code message)))))
         (let ((parts (nreverse parts)))
           (funcall on-octets (%concatenate-octets parts separator) parts))))
      (t (read-stdin/k ports options
                       :on-octets (lambda (octets) (funcall on-octets octets (list octets)))
                       :on-error fail)))))

(define-write-command "write" (ports env positionals options on-plan fail)
  (let ((path (first positionals)))
    (if (or (null path) (rest positionals))
        (funcall fail "argument.invalid" "write takes exactly one path")
        (%write-inputs/k
         ports options fail
         (lambda (octets inputs)
           (funcall on-plan
                    (make-write-plan
                     :command "write"
                     :targets (list (make-write-target path))
                     :inputs inputs
                     :guard-requirements (and (getf options :overwrite) (list (list :expect-hash path)))
                     :expect-hashes (getf options :expect-hash)
                     :plan (lambda (context commit reject)
                             (let* ((target (context-path context))
                                    (state (aitools.store.application:view-path-state (write-context-view context) target)))
                               (case (aitools.store.domain:entry-state-kind state)
                                 (:absent (funcall commit (list (aitools.store.domain:write-file-request target octets))))
                                 (:file
                                  (if (getf options :overwrite)
                                      (funcall commit (list (aitools.store.domain:write-file-request target octets)))
                                      (funcall reject "refusal.exists"
                                               (format nil "~A exists; pass --overwrite with --expect-hash to replace it" target)
                                               :repairs (list (repair "get-hash" "Read its hash to overwrite it deliberately."
                                                                      (format nil "aitools info ~A" (aitools.protocol.domain:shell-quote target)))))))
                                 (t (funcall reject "refusal.not-a-file"
                                             (format nil "~A is a ~(~A~)" target (aitools.store.domain:entry-state-kind state)))))))
                     :record-options (let ((copy (copy-list options)))
                                       (when (or (getf options :stdin) (getf options :stdin-data))
                                         (remf copy :stdin-data)
                                         (setf (getf copy :stdin) t))
                                       copy)
                     :record-positionals (lambda (paths) (list (first paths))))))))))

;;; ------------------------------------------------------------------ split

(define-write-command "split" (ports env positionals options on-plan fail)
  (let* ((path (first positionals))
         (methods (remove nil (list (and (getf options :lines) :lines) (and (getf options :at-match) :at-match)
                                    (and (getf options :bytes) :bytes))))
         (digits (parse-count (or (getf options :suffix-digits) "3"))))
    (cond
      ((or (null path) (rest positionals)) (funcall fail "argument.invalid" "split takes exactly one path"))
      ((/= (length methods) 1) (funcall fail "argument.invalid" "split needs exactly one of --lines, --at-match, --bytes"))
      ((not (and digits (<= 1 digits 9))) (funcall fail "argument.invalid" "--suffix-digits must be 1 to 9"))
      (t
       (let ((method (first methods))
             (prefix (or (getf options :prefix) (concatenate 'string path "."))))
         (flet ((plan (piece-function)
                  (funcall on-plan
                           (make-write-plan
                            :command "split"
                            :targets (list (make-write-target path)
                                           (make-write-target (split-piece-name prefix 1 digits)))
                            :plan (lambda (context commit reject)
                                    (%split-plan context commit reject piece-function prefix digits))
                            :record-options options
                            :record-positionals (lambda (paths) (list (first paths)))))))
           (ecase method
             ((:lines :bytes)
              (let ((count (parse-count (getf options method))))
                (if (and count (plusp count))
                    (plan (lambda (octets document) (declare (ignore document))
                            (if (eq method :lines) (split-by-lines octets count) (split-by-bytes octets count))))
                    (funcall fail "argument.invalid" (format nil "--~(~A~) must be a positive integer" method)))))
             (:at-match
              (compile-pattern/k (getf options :at-match) fail
                                 (lambda (regex)
                                   (plan (lambda (octets document)
                                           (split-at-matches octets (text-document-lines document) regex)))))))))))))

(defun %split-plan (context commit reject piece-function prefix digits)
  (let ((path (context-path context)))
    (read-file-octets/k
     context path reject
     (lambda (octets mode)
       (flet ((cut (document)
                (let ((pieces (funcall piece-function octets document)))
                  (when (>= (length pieces) (expt 10 digits))
                    (return-from %split-plan
                      (funcall reject "argument.invalid"
                               (format nil "~D pieces need more than --suffix-digits ~D" (length pieces) digits))))
                  (let ((requests '()) (per-change '()))
                    (loop for piece in pieces
                          for index from 1
                          do (resolve-extra-path/k
                              context (split-piece-name prefix index digits) :base :cwd
                              :on-outside (lambda (message)
                                            (return-from %split-plan (funcall reject "refusal.outside-workspace" message)))
                              :on-inside
                              (lambda (target)
                                (unless (aitools.store.domain:entry-state-absent-p
                                         (aitools.store.application:view-path-state (write-context-view context) target))
                                  (return-from %split-plan
                                    (funcall reject "refusal.exists" (format nil "~A already exists; nothing was written" target))))
                                (push (aitools.store.domain:write-file-request
                                       target (subseq octets (split-piece-start piece) (split-piece-end piece)) :mode mode)
                                      requests)
                                (push (cons target (list (cons "start_line" (split-piece-start-line piece))
                                                         (cons "lines" (split-piece-lines piece))))
                                      per-change))))
                    (funcall commit (nreverse requests) (list (cons :per-change per-change)))))))
         (if (eq (aitools.text.domain:line-ending-style octets) :none)
             (cut (make-text-document ""))
             (decode-text-document/k octets
                                     :on-decoded #'cut
                                     :on-binary (lambda () (cut (make-text-document "")))
                                     :on-invalid (lambda (offset)
                                                   (declare (ignore offset))
                                                   (cut (make-text-document ""))))))))))

;;; -------------------------------------------------------------- transcode

(defun %strip-leading-bom (string)
  (if (and (plusp (length string)) (char= (char string 0) (code-char #xFEFF)))
      (values (subseq string 1) t)
      (values string nil)))

(define-write-command "transcode" (ports env positionals options on-plan fail)
  (let* ((path (first positionals))
         (from-name (getf options :from))
         (to-name (or (getf options :to) "utf-8"))
         (from (and from-name (aitools.text.domain:find-encoding from-name)))
         (to (aitools.text.domain:find-encoding to-name))
         (names (mapcar #'aitools.text.domain:encoding-name aitools.text.domain:*supported-encodings*)))
    (cond
      ((or (null path) (rest positionals)) (funcall fail "argument.invalid" "transcode takes exactly one path"))
      ((and from-name (null from))
       (funcall fail "input.unsupported-format" (format nil "unsupported --from ~S; supported: ~{~A~^, ~}" from-name names)))
      ((null to)
       (funcall fail "input.unsupported-format" (format nil "unsupported --to ~S; supported: ~{~A~^, ~}" to-name names)))
      (t
       (funcall on-plan
                (make-write-plan
                 :command "transcode"
                 :targets (list (make-write-target path))
                 :expect-hashes (getf options :expect-hash)
                 :plan (lambda (context commit reject)
                         (%transcode-plan context commit reject from to (getf options :replace-unmappable)))
                 :record-options options
                 :record-positionals (lambda (paths) (list (first paths)))))))))

(defun %transcode-plan (context commit reject from to replace-unmappable)
  (let ((path (context-path context)))
    (read-file-octets/k
     context path reject
     (lambda (octets mode)
       (declare (ignore mode))
       (let ((from (or from (let ((guess (aitools.text.domain:guess-encoding octets)))
                              (if (eq guess :unknown)
                                  (return-from %transcode-plan
                                    (funcall reject "input.unsupported-format"
                                             (format nil "cannot tell ~A's encoding; pass --from" path)))
                                  guess)))))
         (aitools.text.domain:decode-octets/k
          octets from
          :on-invalid (lambda (offset)
                        (funcall reject "input.syntax-error"
                                 (format nil "~A is not valid ~A at byte ~D" path (aitools.text.domain:encoding-name from) offset)
                                 :diagnostics (list (json-object "path" path "offset" offset))))
          :on-decoded
          (lambda (text replacements)
            (declare (ignore replacements))
            (multiple-value-bind (body bom) (%strip-leading-bom text)
              (let ((text (if (and bom (member to '(:utf-8 :utf-16le :utf-16be)))
                              (concatenate 'string (string (code-char #xFEFF)) body)
                              body)))
                (aitools.text.domain:encode-string/k
                 text to
                 :replace-unmappable replace-unmappable
                 :on-unmappable (lambda (index char)
                                  (funcall reject "argument.invalid"
                                           (format nil "~A cannot represent U+~4,'0X (character ~D); pass --replace-unmappable to write ?"
                                                   (aitools.text.domain:encoding-name to) (char-code char) index)
                                           :diagnostics (list (json-object "index" index
                                                                           "codepoint" (format nil "U+~4,'0X" (char-code char))))
                                           :repairs (list (repair "replace-unmappable" "Write ? for unrepresentable characters."
                                                                  (concatenate 'string (write-context-command-line context)
                                                                               " --replace-unmappable")))))
                 :on-encoded (lambda (encoded replaced)
                               (let ((fields (list (cons "from" (aitools.text.domain:encoding-name from))
                                                   (cons "to" (aitools.text.domain:encoding-name to))
                                                   (cons "replaced" replaced))))
                                 (funcall commit (if (equalp encoded octets)
                                                     '()
                                                     (list (aitools.store.domain:write-file-request path encoded)))
                                          fields)))))))))))))

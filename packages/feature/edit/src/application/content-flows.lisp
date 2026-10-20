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


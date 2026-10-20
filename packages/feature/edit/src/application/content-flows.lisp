;;;; packages/feature/edit/src/application/content-flows.lisp
;;;;
;;;; `write` and `split` handle whole-file bytes, which the
;;;; UTF-8 text-edit rule in docs/src/reference/commands.md exempts.
(in-package #:aitools.edit.application)

(defun %concatenate-octets (parts separator)
  (let ((pieces
         (loop for (part . rest) on parts
               collect part
               when (and rest separator)
                 collect separator)))
    (let ((result (make-array (reduce #'+ pieces :key #'length) :element-type '(unsigned-byte 8)))
          (offset 0))
      (dolist (piece pieces result)
        (replace result piece :start1 offset)
        (incf offset (length piece))))))

(aitools.kernel.domain:define-flow
  (%write-inputs/k ports options &key on-error on-octets)
  "`write` content: every --content (strings), every --content-file
(bytes), or --stdin, joined by --separator. ON-OCTETS receives inputs."
  (declare (type function on-error on-octets))
  (let* ((contents (getf options :content))
         (files (getf options :content-file))
         (stdin (or (getf options :stdin) (getf options :stdin-data)))
         (given (count-if (function identity) (list contents files stdin)))
         (separator (and (getf options :separator) (aitools.text.domain:encode-utf8 (getf options :separator)))))
    (cond
      ((zerop given)
       (aitools.kernel.domain:flow-continue
         :on-error
         "argument.invalid"
         "no content: pass --content, --content-file, or --stdin (stdin is never read implicitly)"))
      ((> given 1)
       (aitools.kernel.domain:flow-continue
         :on-error
         "argument.invalid"
         "pass --content (repeatable), --content-file (repeatable), or --stdin, not a mix"))
      (contents
       (let ((parts (mapcar (function aitools.text.domain:encode-utf8) contents)))
         (aitools.kernel.domain:flow-continue
           :on-octets
           (%concatenate-octets parts separator)
           contents)))
      (files
       (let ((parts nil)
             (failure nil))
         (dolist (file files)
           (unless failure
             (read-input-file/k ports
                                file
                                :on-octets (lambda (octets) (push octets parts))
                                :on-error (lambda (code message)
                                            (setf failure (list code message))))))
         (if failure
             (aitools.kernel.domain:flow-continue :on-error (first failure) (second failure))
             (let ((parts (nreverse parts)))
               (aitools.kernel.domain:flow-continue
                 :on-octets
                 (%concatenate-octets parts separator)
                 parts)))))
      (t
       (aitools.kernel.domain:flow-call
         (read-stdin/k ports options)
         (:on-octets (octets)
           (aitools.kernel.domain:flow-continue
             :on-octets
             octets
             (list octets)))
         (:on-error (code message)
           (aitools.kernel.domain:flow-continue
             :on-error
             code
             message)))))))

(define-write-command "write" (ports env positionals options &key on-plan on-error)
  (let ((path (first positionals)))
    (if (or (null path) (rest positionals))
        (flow-continue
          :on-error
          "argument.invalid"
          "write takes exactly one path")
        (aitools.kernel.domain:flow-call
          (%write-inputs/k ports options)
          (:on-octets (octets inputs)
                      (flow-continue
                        :on-plan
                        (make-write-plan
                          :command
                          "write"
                          :targets
                          (list (make-write-target path))
                          :inputs
                          inputs
                          :guard-requirements
                          (and (getf options :overwrite) (list (list :expect-hash path)))
                          :expect-hashes
                          (getf options :expect-hash)
                          :plan
                          (lambda (context &key on-committed on-rejected)
                            (let* ((target (context-path context))
                                   (state
                                    (aitools.store.application:view-path-state (write-context-view context) target)))
                              (case (aitools.store.domain:entry-state-kind state)
                                (:absent
                                 (funcall on-committed (list (aitools.store.domain:write-file-request target octets))))
                                (:file
                                 (if (getf options :overwrite)
                                     (funcall on-committed
                                              (list (aitools.store.domain:write-file-request target octets)))
                                     (funcall on-rejected
                                              "refusal.exists"
                                              (format
                                                nil
                                                (concatenate 'string
                                                             "~A exists; pass --overwrite with "
                                                             "--expect-hash to replace it")
                                                target)
                                              :repairs
                                              (list
                                               (repair "get-hash"
                                                       "Read its hash to overwrite it deliberately."
                                                       (format
                                                         nil
                                                         "aitools info ~A"
                                                         (aitools.protocol.domain:shell-quote target)))))))
                                (t
                                 (funcall on-rejected
                                          "refusal.not-a-file"
                                          (format
                                            nil
                                            "~A is a ~(~A~)"
                                            target
                                            (aitools.store.domain:entry-state-kind state)))))))
                          :record-options
                          (let ((copy (copy-list options)))
                            (when (or (getf options :stdin) (getf options :stdin-data))
                              (remf copy :stdin-data)
                              (setf (getf copy :stdin) t))
                            copy)
                          :record-positionals
                          (lambda (paths)
                            (list (first paths))))))
          (:on-error (code message)
                     (flow-continue
                       :on-error
                       code
                       message))))))



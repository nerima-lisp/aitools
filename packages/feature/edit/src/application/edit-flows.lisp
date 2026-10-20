;;;; packages/feature/edit/src/application/edit-flows.lisp
;;;;
;;;; `edit` and `insert`.
(in-package #:aitools.edit.application)

(defun replace-ranges (document ranges new-lines)
  "DOCUMENT with each 1-based inclusive range of RANGES (ascending) replaced
by NEW-LINES; an empty range (end = start - 1) inserts before START."
  (let ((result document))
    (dolist (range (reverse ranges) result)
      (setf result (document-replace-lines result (1- (car range)) (cdr range) new-lines)))))

(defun %json-member (object key)
  (and (json-obj-p object) (assoc key (json-obj-members object) :test #'string=)))

(defun %edit-plan (edits)
  (lambda (context &key on-committed on-rejected)
    (let ((path (context-path context)))
            (aitools.kernel.domain:flow-call
        (read-document/k context path)
        (:on-error (code message &key candidates diagnostics conflicts repairs ((:path ignored-path)))
          (declare (ignore ignored-path))
          (funcall on-rejected
                   code
                   message
                   :candidates
                   candidates
                   :diagnostics
                   diagnostics
                   :conflicts
                   conflicts
                   :repairs
                   repairs))
        (:on-document (document)
          (let ((strategy nil)
                (failure nil))
            (dolist (edit edits)
              (unless failure
                (aitools.kernel.domain:flow-call
                  (%apply-edit/k context document path edit)
                  (:on-error (code message &key candidates diagnostics conflicts repairs path)
                    (setf failure (list code
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
                                        path)))
                  (:on-applied (edited edit-strategy)
                    (setf document edited
                          strategy edit-strategy)))))
            (if failure
                (destructuring-bind (code message &key candidates diagnostics conflicts repairs path) failure
                  (declare (ignore path))
                  (funcall on-rejected
                           code
                           message
                           :candidates
                           candidates
                           :diagnostics
                           diagnostics
                           :conflicts
                           conflicts
                           :repairs
                           repairs))
                (aitools.kernel.domain:flow-call
                  (commit-document context
                                   path
                                   document
                                   :extra
                                   (and strategy
                                        (null (rest edits))
                                        (list (cons "strategy" (string-downcase (symbol-name strategy))))))
                  (:on-committed (requests &optional extra)
                    (funcall on-committed requests extra))))))
        (:on-filtered ())))))

(define-write-command "edit"
                      (ports env positionals options &key on-plan on-error)
                      (let ((path (first positionals)))
                        (flet ((plan (edits &key selector record-options inputs)
                                 (flow-continue :on-plan
                                                (make-write-plan :command
                                                                 "edit"
                                                                 :targets
                                                                 (list (make-write-target path))
                                                                 :inputs
                                                                 inputs
                                                                 :guard-requirements
                                                                 (selector-guards selector path)
                                                                 :expect-hashes
                                                                 (getf options :expect-hash)
                                                                 :expect-count
                                                                 (getf options :expect-count)
                                                                 :replayable
                                                                 (and (content-selector-p selector)
                                                                      (null (getf options :expect-hash)))
                                                                 :plan
                                                                 (%edit-plan edits)
                                                                 :record-options
                                                                 record-options
                                                                 :record-positionals
                                                                 (lambda (paths)
                                                                   (list (first paths)))))))
                          (cond
                            ((or (null path) (rest positionals))
                             (flow-continue :on-error "argument.invalid" "edit takes exactly one path"))
                            ((or (getf options :stdin) (getf options :stdin-data))
                             (if (or (getf options :old)
                                     (getf options :new)
                                     (some
                                      (lambda (key)
                                        (getf options key))
                                      '(:range :symbol :between :match)))
                                 (flow-continue :on-error
                                                "argument.invalid"
                                                (concatenate 'string
                                                             "--stdin carries old/new or edits[]; do not "
                                                             "combine it with --old, --new or a selector"))
                                                                  (aitools.kernel.domain:flow-call
                                   (read-stdin-json/k ports options)
                                   (:on-json (value text)
                                     (aitools.kernel.domain:flow-call
                                       (%stdin-edits/k value)
                                       (:on-edits (edits)
                                         (plan edits
                                               :record-options
                                               (inline-stdin-options options text)
                                               :inputs
                                               (list text)))
                                       (:on-error (code message)
                                         (flow-continue
                                           :on-error
                                           code
                                           message))))
                                   (:on-error (code message)
                                     (flow-continue
                                       :on-error
                                       code
                                       message)))))
                            (t
                             (aitools.kernel.domain:flow-call
                              (parse-selector/k :edit
                                                options
                                                :extra-exclusive
                                                (list (cons "--old" (getf options :old))))
                              (:on-error (code message &key repairs)
                                         (flow-continue :on-error code message :repairs repairs))
                              (:on-selector (selector)
                                            (let ((new (getf options :new)))
                                              (if (null new)
                                                  (flow-continue :on-error
                                                                 "argument.invalid"
                                                                 "a selector needs --new ('' deletes the selection)")
                                                  (plan (list (list :selector selector :new new))
                                                        :selector
                                                        selector
                                                        :record-options
                                                        options
                                                        :inputs
                                                        (list new)))))
                              (:on-none ()
                                        (let ((old (getf options :old))
                                              (new (getf options :new)))
                                          (cond
                                            ((null old)
                                             (flow-continue :on-error
                                                            "argument.invalid"
                                                            "edit needs --old, a selector, or --stdin"))
                                            ((zerop (length old))
                                             (flow-continue :on-error "argument.invalid" "--old must not be empty"))
                                            ((null new)
                                             (flow-continue :on-error
                                                            "argument.invalid"
                                                            "--old needs --new ('' deletes the match)"))
                                            (t
                                             (plan (list (list :old old :new new))
                                                   :record-options
                                                   options
                                                   :inputs
                                                   (list old new))))))))))))

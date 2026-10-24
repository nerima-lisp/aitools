(in-package #:aitools.integration.cps-contract-test)

(defun %terminal-call-forms (body active &optional exit-target)
  (let ((forms '()))
    (labels ((local-macro-call-p (form visible)
  (and (consp form)
       (symbolp (car form))
       (loop for entry in visible
             thereis
               (and (consp entry)
                    (member (car form) (cdr entry) :test #'eq)
                    (or (eq (car entry) :local-macro)
                        (and (eq (car entry) :local-function)
                             (not (%terminal-form-rule form))))))))
             (macrolet-visible (visible definitions)
               (append visible
                       (list (cons :local-macro
                                   (mapcar #'first definitions)))))
             (symbol-macrolet-visible (visible bindings)
               (%symbol-macrolet-callback-variables visible bindings))
             (immediate-lambda-form (form)
               (let* ((operator (car form))
                      (designator
                        (cond
                          ((%operator-name-p form "FUNCALL") (second form))
                          ((%operator-name-p form "APPLY") (second form))
                          ((and (consp operator)
                                (%operator-name-p operator "LAMBDA"))
                           operator))))
                 (cond
                   ((%operator-name-p designator "LAMBDA") designator)
                   ((and (%operator-name-p designator "FUNCTION")
                         (%operator-name-p (second designator) "LAMBDA"))
                    (second designator)))))
             (walk-immediate-lambda (lambda-form visible loop-terminal-p
                                      shadowed-blocks terminal-block-targets tail-p)
               (let ((lambda-visible
                       (%without-bound-symbols visible (second lambda-form)))
                     (body (cddr lambda-form)))
                 (if tail-p
                     (walk-body body lambda-visible loop-terminal-p
                                shadowed-blocks terminal-block-targets)
                     (dolist (form body)
                       (walk-nonlocal form lambda-visible loop-terminal-p
                                      shadowed-blocks
                                      terminal-block-targets)))))
             (walk-nonlocal (form visible &optional loop-terminal-p
                                  shadowed-blocks terminal-block-targets)
               (when (consp form)
                 (cond
                   ((local-macro-call-p form visible) nil)
                   ((or (%operator-name-p form "QUOTE")
                        (%operator-name-p form "FUNCTION")
                        (%operator-name-p form "LAMBDA"))
                    nil)
                   (t
                    (let ((rule (%terminal-form-rule form))
                          (lambda-form (immediate-lambda-form form)))
                      (cond
                        (lambda-form
                         (walk-immediate-lambda lambda-form visible
                                                loop-terminal-p shadowed-blocks
                                                terminal-block-targets nil))
                        ((and rule (eq (second rule) :return-from))
                         (let ((target (second form)))
                           (when (and (cddr form)
                                      (not (member target shadowed-blocks
                                                   :test #'eq))
                                      (or (eq target exit-target)
                                          (member target terminal-block-targets
                                                  :test #'eq)
                                          (and loop-terminal-p (null target))))
                             (walk-tail (nth (third rule) form) visible
                                        terminal-block-targets))))
                        ((and rule (eq (second rule) :loop-return))
                         (when (and (not (member nil shadowed-blocks :test #'eq))
                                    (or loop-terminal-p
                                        (member nil terminal-block-targets
                                                :test #'eq)))
                           (walk-tail (nth (third rule) form) visible
                                      terminal-block-targets)))
                        ((and rule (eq (second rule) :loop))
                         (walk-nonlocal (cdr form) visible nil
                                        (cons nil shadowed-blocks)
                                        terminal-block-targets))
                        ((and rule (eq (second rule) :macrolet-body))
                         (walk-nonlocal
                          (nthcdr (third rule) form)
                          (macrolet-visible visible (nth (fourth rule) form))
                          loop-terminal-p shadowed-blocks terminal-block-targets))
                        ((and rule (eq (second rule) :symbol-macrolet-body))
                         (walk-nonlocal
                          (nthcdr (third rule) form)
                          (symbol-macrolet-visible
                           visible (nth (fourth rule) form))
                          loop-terminal-p shadowed-blocks terminal-block-targets))
                        ((and rule (eq (second rule) :local-body))
                         (walk-nonlocal
                          (nthcdr (third rule) form)
                          (append visible
                                  (list (cons :local-function
                                              (mapcar #'first (nth (fourth rule) form)))))
                          loop-terminal-p shadowed-blocks
                          terminal-block-targets))
                        ((and rule (eq (second rule) :block))
                         (let ((target (nth (fourth rule) form)))
                           (walk-nonlocal
                            (nthcdr (third rule) form) visible
                            (and loop-terminal-p (not (null target)))
                            (cons target shadowed-blocks)
                            terminal-block-targets)))
                        (t
                         (walk-nonlocal (car form) visible loop-terminal-p
                                        shadowed-blocks terminal-block-targets)
                         (walk-nonlocal (cdr form) visible loop-terminal-p
                                        shadowed-blocks terminal-block-targets))))))))
             (walk-body (body visible &optional loop-terminal-p shadowed-blocks
                                       terminal-block-targets)
               (when body
                 (dolist (form (butlast body))
                   (walk-nonlocal form visible loop-terminal-p shadowed-blocks
                                  terminal-block-targets))
                 (walk-tail (car (last body)) visible terminal-block-targets)))
             (walk-clauses (clauses body-start visible terminal-block-targets)
               (dolist (clause clauses)
                 (when (and (consp clause) (nthcdr body-start clause))
                   (walk-body (nthcdr body-start clause) visible nil nil
                              terminal-block-targets))))
             (walk-handler-clauses (clauses body-start visible
                                            terminal-block-targets)
               (dolist (clause clauses)
                 (when (and (consp clause) (nthcdr body-start clause))
                   (walk-body
                    (nthcdr body-start clause)
                    (%without-bound-symbols visible (second clause))
                    nil nil terminal-block-targets))))
             (walk-tail (form visible &optional terminal-block-targets)
               (when (consp form)
                 (let ((rule (and (not (local-macro-call-p form visible))
                                  (%terminal-form-rule form))))
                   (if (null rule)
                       (unless (local-macro-call-p form visible)
                         (let ((lambda-form (immediate-lambda-form form)))
                           (if lambda-form
                               (walk-immediate-lambda
                                lambda-form visible nil nil terminal-block-targets t)
                               (progn
                                 (push (list form visible) forms)
                                 (walk-nonlocal form visible nil nil
                                                terminal-block-targets)))))
                       (case (second rule)
                         (:arms
                          (dolist (index (cddr rule))
                            (walk-tail (nth index form) visible
                                       terminal-block-targets)))
                         ((:return-from :loop-return)
                          (walk-tail (nth (third rule) form) visible
                                     terminal-block-targets))
                         (:clauses
                          (walk-clauses (nthcdr (third rule) form) (fourth rule) visible terminal-block-targets))
                         (:body
                          (walk-body (nthcdr (third rule) form) visible nil nil
                                     terminal-block-targets))
                         (:binding-body
                          (walk-body
                           (nthcdr (third rule) form)
                           (%let-bound-callback-variables
                            visible
                            (nth (fourth rule) form)
                            (fifth rule)
                            #'%callback-designator-variables
                            (and (sixth rule) (nth (sixth rule) form)))
                           nil nil terminal-block-targets))
                         (:handler-case
                          (walk-tail (nth (third rule) form) visible
                                     terminal-block-targets)
                          (walk-handler-clauses
                           (nthcdr (fourth rule) form) (fifth rule) visible
                           terminal-block-targets))
                         (:local-body
                          (walk-body
                           (nthcdr (third rule) form)
                           (append visible
                                   (list (cons :local-function
                                               (mapcar (function first) (nth (fourth rule) form)))))
                           nil nil terminal-block-targets))
                         (:macrolet-body
                          (walk-body
                           (nthcdr (third rule) form)
                           (macrolet-visible visible (nth (fourth rule) form))
                           nil nil terminal-block-targets))
                         (:symbol-macrolet-body
                          (walk-body
                           (nthcdr (third rule) form)
                           (symbol-macrolet-visible
                            visible (nth (fourth rule) form))
                           nil nil terminal-block-targets))
                         (:block
                          (walk-body
                           (nthcdr (third rule) form) visible nil nil
                           (cons (nth (fourth rule) form)
                                 terminal-block-targets)))
                         (:flow-call
                          (when (consp (second form))
                            (push (list (second form) visible) forms))
                          (dolist (clause (cddr form))
                            (when (and (consp clause)
                                       (keywordp (first clause))
                                       (consp (rest clause))
                                       (listp (second clause)))
                              (walk-body
                               (cddr clause)
                               (%without-bound-symbols
                                visible (second clause))
                               nil nil terminal-block-targets))))
                         (:loop
                           (walk-nonlocal (cdr form) visible t nil terminal-block-targets))))))))
      (walk-body body active))
    (nreverse forms)))

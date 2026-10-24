(in-package #:aitools.integration.cps-contract-test)

(defun %callback-designator-variables (form active)
  (remove-duplicates
   (cond
     ((and (symbolp form) (member form active :test #'eq)) (list form))
     ((not (consp form)) nil)
     ((or (%operator-name-p form "QUOTE")
          (%operator-name-p form "FUNCTION"))
      nil)
     ((%operator-name-p form "IF")
      (append (%callback-designator-variables (third form) active)
              (%callback-designator-variables (fourth form) active)))
     ((%operator-name-p form "COND")
      (loop for clause in (cdr form)
            when (and (consp clause) (cdr clause))
              append (%callback-designator-variables
                      (car (last (cdr clause))) active)))
     ((%operator-name-p form "PROGN")
      (when (cdr form)
        (%callback-designator-variables (car (last (cdr form))) active)))
     (t nil))
   :test #'eq))

(defun %callback-call-shape (arguments)
  (let ((length (%proper-list-length arguments)))
    (if (null length)
        (values :unclassified :unclassified)
        (let ((keyword-position (position-if #'keywordp arguments)))
          (if (null keyword-position)
              (values length nil)
              (let ((tail (nthcdr keyword-position arguments)))
                (if (and (evenp (length tail))
                         (loop for rest on tail by #'cddr
                               always (keywordp (first rest))))
                    (values keyword-position
                            (loop for rest on tail by #'cddr
                                  collect (first rest)))
                    (values :unclassified :unclassified))))))))

(defun %callback-invocation (form variable visible)
  (let* ((apply-p (%operator-name-p form "APPLY"))
         (designator (second form))
         (references (%callback-designator-variables designator visible)))
    (when (member variable references :test #'eq)
      (if apply-p
          '(:form :apply
            :positional-arity :unclassified
            :keywords :unclassified)
          (if (eq designator variable)
              (multiple-value-bind (arity keywords)
                  (%callback-call-shape (cddr form))
                (list :form :funcall
                      :positional-arity arity
                      :keywords keywords))
              '(:form :funcall
                :positional-arity :unclassified
                :keywords :unclassified))))))

(defun %terminal-call-forms (body active &optional exit-target)
  (let ((forms '()))
    (labels ((walk-nonlocal (form visible)
               (when (consp form)
                 (cond
                   ((or (%operator-name-p form "QUOTE")
                        (%operator-name-p form "FUNCTION")
                        (%operator-name-p form "LAMBDA"))
                    nil)
                   ((%operator-name-p form "RETURN-FROM")
                    (when (and exit-target
                               (eq (second form) exit-target)
                               (cddr form))
                      (walk-tail (third form) visible)))
                   (t
                    (progn
                      (walk-nonlocal (car form) visible)
                      (walk-nonlocal (cdr form) visible))))))
             (walk-body (body visible)
               (when body
                 (dolist (form (butlast body))
                   (walk-nonlocal form visible))
                 (walk-tail (car (last body)) visible)))
             (walk-tail (form visible)
               (when (consp form)
                 (cond
                   ((%operator-name-p form "IF")
                    (walk-tail (third form) visible)
                    (when (cdddr form)
                      (walk-tail (fourth form) visible)))
                   ((%operator-name-p form "RETURN-FROM")
                    (when (cddr form)
                      (walk-tail (third form) visible)))
                   ((%operator-name-p form "COND")
                    (dolist (clause (cdr form))
                      (when (and (consp clause) (cdr clause))
                        (walk-body (cdr clause) visible))))
                   ((or (%operator-name-p form "PROGN")
                        (%operator-name-p form "LOCALLY"))
                    (walk-body (cdr form) visible))
                   ((or (%operator-name-p form "LET")
                        (%operator-name-p form "LET*"))
                    (walk-body
                     (cddr form)
                     (%without-bound-symbols visible (second form))))
                   ((%operator-name-p form "MULTIPLE-VALUE-BIND")
                    (walk-body
                     (cdddr form)
                     (%without-bound-symbols visible (second form))))
                   ((%operator-name-p form "HANDLER-CASE")
                    (walk-tail (second form) visible)
                    (dolist (clause (cddr form))
                      (when (consp clause)
                        (walk-body (cddr clause) visible))))
                   ((%operator-name-p form "FLOW-CALL")
                    (when (consp (second form))
                      (push (list (second form) visible) forms))
                    (dolist (clause (cddr form))
                      (when (and (consp clause) (keywordp (first clause))
                                 (consp (rest clause)) (listp (second clause)))
                        (walk-body
                         (cddr clause)
                         (%without-bound-symbols visible (second clause))))))
                   ((%operator-name-p form "LOOP")
                    (walk-nonlocal form visible))
                   ((or (%operator-name-p form "WHEN")
                        (%operator-name-p form "UNLESS"))
                    (walk-body (cddr form) visible))
                   (t (push (list form visible) forms))))))
      (walk-body body active))
    (nreverse forms)))

(defun %positional-continuation-invocations (body variable function-symbol)
  (loop for (form visible) in
          (%terminal-call-forms body (list variable) function-symbol)
        for invocation =
          (and (consp form)
               (member variable visible :test #'eq)
               (or (%operator-name-p form "FUNCALL")
                   (%operator-name-p form "APPLY"))
               (%callback-invocation form variable visible))
        when invocation collect invocation))

(defun %function-designator-symbol (form)
  (when (or (%operator-name-p form "FUNCTION")
            (%operator-name-p form "QUOTE"))
    (let ((designator (second form)))
      (and (symbolp designator) designator))))

(defun %callback-lambda-form (form)
  (cond
    ((%operator-name-p form "LAMBDA") form)
    ((and (%operator-name-p form "FUNCTION")
          (%operator-name-p (second form) "LAMBDA"))
     (second form))))

(defun %terminal-flow-continue-p (body)
  (some (lambda (entry)
          (%operator-name-p (first entry) "FLOW-CONTINUE"))
        (%terminal-call-forms body nil)))

(defun %continuation-actual-p (callsite)
  (let* ((argument (getf callsite :actual-argument))
         (lambda-form (%callback-lambda-form argument)))
    (and lambda-form
         (%terminal-flow-continue-p (cddr lambda-form)))))

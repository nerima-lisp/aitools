(in-package #:aitools.integration.cps-contract-test)

(defun %terminal-form-rule (form)
  (when (consp form)
    (loop for rule in '((("IF") :arms 2 3)
                        (("RETURN-FROM") :return-from 2)
                        (("RETURN") :loop-return 1)
                        (("COND") :clauses 1 1)
                        (("CASE" "ECASE" "TYPECASE" "ETYPECASE" "CCASE" "CTYPECASE") :clauses 2 1)
                        (("PROGN" "LOCALLY") :body 1)
                        (("WHEN" "UNLESS") :body 2)
                        (("LET") :binding-body 2 1 :parallel)
                        (("LET*") :binding-body 2 1 :sequential)
                        (("MULTIPLE-VALUE-BIND") :binding-body 3 1 :multiple-values 2)
                        (("HANDLER-CASE") :handler-case 1 2 2)
                        (("BLOCK") :block 2 1)
                        (("FLET" "LABELS") :local-body 2 1)
                        (("MACROLET") :macrolet-body 2 1)
                        (("SYMBOL-MACROLET") :symbol-macrolet-body 2 1)
                        (("FLOW-CALL") :flow-call)
                        (("LOOP") :loop))
          when (some (lambda (name) (%operator-name-p form name)) (first rule))
            return rule)))

(defun %let-bound-callback-variables (visible bindings mode designator-variable-p &optional values-form)
  (labels ((local-call-shadow-p (kind operator)
             (loop for entry in visible
                   thereis (and (consp entry)
                                (eq (car entry) kind)
                                (member operator (cdr entry) :test #'eq))))
           (multiple-value-bindings (variables form)
             (let ((value-forms
                     (cond
                       ((and (%operator-name-p form "VALUES")
                             (not (or (local-call-shadow-p :local-function (car form))
                                      (local-call-shadow-p :local-macro (car form)))))
                        (cdr form))
                       ((= (length variables) 1) (list form)))))
               (loop for variable in variables
                     for index from 0
                     collect (list variable (nth index value-forms))))))
    (cond
      ((eq mode :multiple-values)
       (%let-bound-callback-variables visible
                                      (multiple-value-bindings bindings values-form)
                                      :parallel designator-variable-p))
      ((eq mode :sequential)
       (loop with active = visible
             for binding in bindings
             for variable = (%parameter-variable binding)
             for initializer = (and (consp binding) (second binding))
             for roots = (and initializer (funcall designator-variable-p initializer active))
             do (setf active (remove variable active :key #'car :test #'eq))
                (dolist (root roots) (push (cons variable root) active))
             finally (return active)))
      (t
       (let* ((bound (mapcar #'%parameter-variable bindings))
              (active (%without-bound-symbols visible bound)))
         (dolist (binding bindings)
           (let ((variable (%parameter-variable binding))
                 (initializer (and (consp binding) (second binding))))
             (dolist (root (and initializer (funcall designator-variable-p initializer visible)))
               (push (cons variable root) active))))
         active)))))

(defun %callback-designator-variables (form active)
  (labels ((local-macro-call-p (form visible)
             (and (consp form)
                  (symbolp (car form))
                  (loop for entry in visible
                        thereis (and (consp entry)
                                     (eq (car entry) :local-macro)
                                     (member (car form) (cdr entry) :test #'eq)))))
           (local-function-call-p (form visible)
             (and (consp form)
                  (symbolp (car form))
                  (not (%terminal-form-rule form))
                  (loop for entry in visible
                        thereis (and (consp entry)
                                     (eq (car entry) :local-function)
                                     (member (car form) (cdr entry) :test #'eq)))))
           (macrolet-visible (visible definitions)
             (append visible (list (cons :local-macro (mapcar #'first definitions)))))
           (local-function-visible (visible definitions)
             (append visible (list (cons :local-function (mapcar #'first definitions)))))
           (symbol-macrolet-visible (visible bindings)
             (%symbol-macrolet-callback-variables visible bindings))
           (walk-body (body visible &optional expansion-stack)
             (when body
               (walk (car (last body)) visible expansion-stack)))
           (walk-clauses (clauses body-start visible expansion-stack)
             (loop for clause in clauses
                   when (and (consp clause) (nthcdr body-start clause))
                     append (walk-body (nthcdr body-start clause) visible expansion-stack)))
           (walk-loop-return-clauses (loop-form visible expansion-stack)
             (loop for tail on (cdr loop-form)
                   when (and (symbolp (first tail))
                             (string= (symbol-name (first tail)) "RETURN")
                             (rest tail))
                     append (walk (second tail) visible expansion-stack)))
           (walk (form visible &optional expansion-stack)
             (cond
               ((symbolp form)
                (let ((binding (find-if (lambda (entry) (and (consp entry) (eq (car entry) form))) visible)))
                  (cond
                    ((and binding (consp (cdr binding)) (eq (second binding) :symbol-macro))
                     (unless (member form expansion-stack :test #'eq)
                       (walk (third binding) visible (cons form expansion-stack))))
                    ((member form visible :test #'eq) (list form))
                    ((and binding (cdr binding)) (list (cdr binding))))))
               ((not (consp form)) nil)
               ((or (local-macro-call-p form visible) (local-function-call-p form visible)) nil)
               ((or (%operator-name-p form "QUOTE") (%operator-name-p form "FUNCTION")) nil)
               (t
                (let ((rule (%terminal-form-rule form)))
                  (when rule
                    (case (second rule)
                      (:arms
                       (loop for index in (cddr rule)
                             append (walk (nth index form) visible expansion-stack)))
                      ((:return-from :loop-return)
                       (walk (nth (third rule) form) visible expansion-stack))
                      (:clauses
                       (walk-clauses (nthcdr (third rule) form)
                                     (fourth rule)
                                     visible
                                     expansion-stack))
                      (:body (walk-body (nthcdr (third rule) form) visible expansion-stack))
                      (:binding-body
                       (walk-body (nthcdr (third rule) form)
                                  (%let-bound-callback-variables
                                   visible
                                   (nth (fourth rule) form)
                                   (fifth rule)
                                   #'walk
                                   (and (sixth rule) (nth (sixth rule) form)))
                                  expansion-stack))
                      (:handler-case
                       (append (walk (nth (third rule) form) visible expansion-stack)
                               (loop for clause in (nthcdr (fourth rule) form)
                                     when (and (consp clause) (nthcdr (fifth rule) clause))
                                       append (walk-body (nthcdr (fifth rule) clause)
                                                         (%without-bound-symbols visible
                                                                                 (second clause))
                                                         expansion-stack))))
                      (:block (walk-body (nthcdr (third rule) form) visible expansion-stack))
                      (:local-body
                       (walk-body (nthcdr (third rule) form)
                                  (local-function-visible visible (nth (fourth rule) form))
                                  expansion-stack))
                      (:macrolet-body
                       (walk-body (nthcdr (third rule) form)
                                  (macrolet-visible visible (nth (fourth rule) form))
                                  expansion-stack))
                      (:symbol-macrolet-body
                       (walk-body (nthcdr (third rule) form)
                                  (symbol-macrolet-visible visible (nth (fourth rule) form))
                                  expansion-stack))
                      (:loop (walk-loop-return-clauses form visible expansion-stack))
                      (otherwise nil))))))))
    (remove-duplicates (walk form active) :test #'eq)))

(defun %symbol-macrolet-callback-variables (visible bindings)
  (append (%without-bound-symbols visible (mapcar #'first bindings))
          (mapcar
           (lambda (binding)
             (list (first binding) :symbol-macro (second binding)))
           bindings)))

(defun %callback-invocation (form variable visible)
  (let* ((apply-p (%operator-name-p form "APPLY"))
         (designator (second form))
         (references (%callback-designator-variables designator visible)))
    (when (member variable references :test #'eq)
      (if apply-p
          '(:form :apply :positional-arity :unclassified :keywords :unclassified)
          (if (eq designator variable)
              (multiple-value-bind (arity keywords) (%callback-call-shape (cddr form))
                (list :form :funcall :positional-arity arity :keywords keywords))
              '(:form :funcall :positional-arity :unclassified :keywords :unclassified))))))

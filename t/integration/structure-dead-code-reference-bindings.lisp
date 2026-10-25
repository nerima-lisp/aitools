(in-package #:aitools.integration.structure-test)

(defun %liveness-walk-destructuring-bind (object values functions)
  (let ((names '()) (initializers '()))
    (labels ((bind-pattern (pattern)
           (cond
             ((symbolp pattern)
              (let ((name (%liveness-reference-name-of pattern)))
                (when
                    (and name
                         (not (keywordp pattern))
                         (not
                          (member name
                                  (quote
                                   ("&WHOLE" "&ENVIRONMENT"
                                             "&OPTIONAL"
                                             "&REST"
                                             "&BODY"
                                             "&KEY"
                                             "&ALLOW-OTHER-KEYS"
                                             "&AUX"))
                                  :test
                                  (function equal))))
                  (pushnew name names :test (function equal)))))
             ((consp pattern) (parse-pattern pattern))))
         (parse-pattern (lambda-list)
           (let ((mode :required))
             (loop while (consp lambda-list)
                   for item = (pop lambda-list)
                   for name = (%liveness-reference-name-of item)
                   do (cond
                        ((member name '("&WHOLE" "&ENVIRONMENT") :test #'equal)
                         (setf mode :whole-next))
                        ((equal name "&OPTIONAL")
                         (setf mode :optional))
                        ((member name '("&REST" "&BODY") :test #'equal)
                         (setf mode :rest-next))
                        ((equal name "&KEY")
                         (setf mode :key))
                        ((equal name "&AUX")
                         (setf mode :aux))
                        ((equal name "&ALLOW-OTHER-KEYS") nil)
                        ((member mode '(:whole-next :rest-next))
                          (bind-pattern item)
                          (setf mode :required))
                        ((member mode '(:optional :key :aux))
                         (let* ((designator (and (consp item) (first item)))
                                (variable
                                 (if (and (eq mode :key) (consp designator))
                                     (second designator)
                                     designator))
                                (initial (and (consp item) (second item)))
                                (supplied (and (consp item) (third item))))
                           (when initial
                             (push (cons initial (copy-list names)) initializers))
                           (bind-pattern (or variable item))
                           (when supplied
                             (bind-pattern supplied))))
                        (t (bind-pattern item))))
             (when lambda-list
               (bind-pattern lambda-list)))))
  (parse-pattern (second object))
  (%liveness-walk-form (third object) values functions)
  (dolist (initializer (nreverse initializers))
    (%liveness-walk-form (car initializer) (append values (cdr initializer)) functions))
  (%liveness-walk-sequence (cdddr object) (append values (nreverse names)) functions))
    t))

(defun %liveness-walk-binding-form (object operator values functions)
  (cond
    ((member operator '("LET" "LET*" "MULTIPLE-VALUE-BIND") :test #'equal)
     (if (equal operator "MULTIPLE-VALUE-BIND")
         (let ((locals (remove nil (mapcar #'%liveness-reference-name-of
                                           (second object)))))
           (%liveness-walk-form (third object) values functions)
           (%liveness-walk-sequence (cdddr object) (append values locals)
                                    functions))
         (let ((locals '()) (sequential (equal operator "LET*")))
           (dolist (binding (second object))
             (let ((variable (if (consp binding) (first binding) binding))
                   (initial (and (consp binding) (second binding))))
               (when initial
                 (%liveness-walk-form initial
                                      (if sequential (append values locals) values)
                                      functions))
               (when (%liveness-reference-name-of variable)
                 (pushnew (%liveness-reference-name-of variable) locals
                          :test #'equal))))
           (%liveness-walk-sequence (cddr object) (append values locals)
                                    functions)))
     t)
    ((equal operator "DESTRUCTURING-BIND")
     (%liveness-walk-destructuring-bind object values functions))
    ((member operator '("DOLIST" "DOTIMES") :test #'equal)
     (let* ((binding (second object))
            (variable (first binding))
            (locals (and (%liveness-reference-name-of variable)
                         (list (%liveness-reference-name-of variable)))))
       (%liveness-walk-form (second binding) values functions)
       (%liveness-walk-sequence (cddr object) (append values locals)
                                functions))
     t)
    ((member operator '("FLET" "LABELS") :test #'equal)
     (let* ((definitions (second object))
            (names (mapcar (lambda (definition)
                             (%liveness-reference-name-of (first definition)))
                           definitions))
            (local-functions (if (equal operator "LABELS")
                                 (append names functions)
                                 functions)))
       (dolist (definition definitions)
         (let ((locals (%liveness-walk-lambda-vars (second definition) values
                                                   local-functions nil)))
           (%liveness-walk-sequence (cddr definition) (append values locals)
                                    local-functions)))
       (%liveness-walk-sequence (cddr object) values
                                (append names functions)))
     t)
    (t nil)))

(defun %liveness-walk-loop-form (object operator values functions)
  (when (equal operator "LOOP")
    (if (and (rest object) (consp (second object)))
        (%liveness-walk-sequence (rest object) values functions)
        (let ((locals '()) (expressions '()))
          (labels ((variable-names (pattern)
                     (cond
                       ((symbolp pattern)
                        (let ((name (%liveness-reference-name-of pattern)))
                          (when (and name
                                     (not (member name '("NIL" "T")
                                                  :test #'equal))
                                     (or (zerop (length name))
                                         (char/= (char name 0) #\&)))
                            (list name))))
                       ((consp pattern)
                        (append (variable-names (car pattern))
                                (variable-names (cdr pattern))))
                       (t nil)))
                   (scan-clauses (clauses)
                     (loop for tail on clauses
                           for marker =
                             (%liveness-reference-name-of (first tail))
                           do (cond
                                ((member marker '("FOR" "AS" "WITH" "INTO")
                                         :test #'equal)
                                 (setf locals
                                       (append
                                        (variable-names (second tail))
                                        locals)))
                                ((member marker
                                         '("IN" "ON" "ACROSS" "=" "THEN"
                                           "FROM" "UPFROM" "DOWNFROM" "TO"
                                           "UPTO" "DOWNTO" "ABOVE" "BELOW"
                                           "BY" "REPEAT" "WHILE" "UNTIL"
                                           "ALWAYS" "NEVER" "THEREIS" "WHEN"
                                           "UNLESS" "IF" "ELSEIF" "ELSE"
                                           "COLLECT" "COLLECTING" "APPEND"
                                           "APPENDING" "NCONC" "NCONCING"
                                           "COUNT" "COUNTING" "SUM" "SUMMING"
                                           "MAXIMIZE" "MAXIMIZING" "MINIMIZE"
                                           "MINIMIZING" "DO" "DOING"
                                           "INITIALLY" "FINALLY" "RETURN"
                                           "RETURNING" "OF")
                                         :test #'equal)
                                 (when (cdr tail)
                                   (push (second tail) expressions)
                                   (when (member marker '("DO" "DOING")
                                                :test #'equal)
                                     (loop for body-tail on (cddr tail)
                                           for body-form = (first body-tail)
                                           while (and (consp body-tail)
                                                      (not
                                                       (member
                                                        (%liveness-reference-name-of
                                                         body-form)
                                                        '("FOR" "AS" "WITH"
                                                          "INTO" "IN" "ON"
                                                          "ACROSS" "=" "THEN"
                                                          "FROM" "UPFROM"
                                                          "DOWNFROM" "TO"
                                                          "UPTO" "DOWNTO"
                                                          "ABOVE" "BELOW" "BY"
                                                          "REPEAT" "WHILE"
                                                          "UNTIL" "ALWAYS" "NEVER"
                                                          "THEREIS" "WHEN"
                                                          "UNLESS" "IF" "ELSEIF"
                                                          "ELSE" "COLLECT"
                                                          "COLLECTING" "APPEND"
                                                          "APPENDING" "NCONC"
                                                          "NCONCING" "COUNT"
                                                          "COUNTING" "SUM" "SUMMING"
                                                          "MAXIMIZE" "MAXIMIZING"
                                                          "MINIMIZE" "MINIMIZING"
                                                          "DO" "DOING" "INITIALLY"
                                                          "FINALLY" "RETURN"
                                                          "RETURNING" "OF" "AND"
                                                          "END")
                                                        :test #'equal)))
                                           do (push body-form expressions)))))))))
            (scan-clauses (rest object))
            (%liveness-walk-sequence (nreverse expressions)
                                     (append values locals) functions))))
    t))

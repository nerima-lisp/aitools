(in-package #:aitools.integration.cps-contract-test)

(defun %positional-helper-definitions (definitions)
  (let ((slots
          (loop for definition in definitions
                when (and (getf definition :function-symbol)
                          (member (getf definition :kind)
                                  (list :defun :define-flow)
                                  :test (function eq)))
                  append
                    (loop for (parameter position) in
                            (%positional-parameters
                             (getf definition :lambda-list))
                          collect
                            (list :definition-path (getf definition :path)
                                  :definition-kind (getf definition :kind)
                                  :symbol (getf definition :symbol)
                                  :parameter-name
                                  (string-upcase (symbol-name parameter))
                                  :parameter-position position
                                  :invocations
                                  (%positional-continuation-invocations
                                   (getf definition :body)
                                   parameter
                                   (getf definition :function-symbol))
                                  :function-symbol
                                  (getf definition :function-symbol)
                                  :callback-symbol parameter
                                  :body (getf definition :body)
                                  :forward-edges nil
                                  :continuation-p nil)))))
    (dolist (slot slots)
      (setf (getf slot :forward-edges)
            (%positional-helper-forward-edges slot slots)))
    (setf slots (%resolve-positional-helper-forwarding slots))
    (%resolve-positional-helper-continuations slots definitions)
    (remove-if-not
     (lambda (slot)
       (and (getf slot :invocations)
            (or (%on-name-p (getf slot :callback-symbol))
                (getf slot :continuation-p))))
     slots)))

(defun %safe-list-nth (sequence index)
  (loop with tail = sequence
        for position from 0
        do (cond
             ((not (consp tail)) (return (values nil nil)))
             ((= position index) (return (values (car tail) t)))
             (t (setf tail (cdr tail))))))

(defun %definition-call-sites (definition function-symbol argument-position)
  (let ((call-sites nil))
    (labels ((record (call-form argument present-p)
               (push (list :path (getf definition :path)
                           :enclosing-kind (getf definition :kind)
                           :enclosing-symbol (getf definition :symbol)
                           :enclosing-function-symbol
                           (getf definition :function-symbol)
                           :call-form call-form
                           :argument-shape
                           (if (or (eq call-form :apply) (not present-p))
                               :unclassified
                               (%function-argument-shape argument))
                           :actual-argument (and present-p argument))
                     call-sites))
             (walk-list (tail)
               (cond
                 ((consp tail)
                  (walk (car tail))
                  (walk-list (cdr tail)))
                 (tail (walk tail))))
             (walk (form)
               (when (consp form)
                 (cond
                   ((%operator-name-p form "QUOTE") nil)
                   ((and (%operator-name-p form "FUNCTION")
                         (symbolp (second form)))
                    nil)
                   (t
                    (when (eq (first form) function-symbol)
                      (multiple-value-bind
                            (argument present-p)
                          (%safe-list-nth (cdr form)
                                          (1- argument-position))
                        (record :direct argument present-p)))
                    (when (or (%operator-name-p form "FUNCALL")
                              (%operator-name-p form "APPLY"))
                      (when (eq (%function-designator-symbol (second form))
                                function-symbol)
                        (if (%operator-name-p form "APPLY")
                            (multiple-value-bind
                                  (argument present-p)
                                (let* ((arguments (cddr form))
                                       (length (%proper-list-length arguments)))
                                  (if (and length
                                           (< (1- argument-position)
                                              (1- length)))
                                      (%safe-list-nth
                                       arguments (1- argument-position))
                                      (values nil nil)))
                              (record :apply argument present-p))
                            (multiple-value-bind
                                  (argument present-p)
                                (%safe-list-nth (cddr form)
                                                (1- argument-position))
                              (record :funcall argument present-p)))))
                    (walk-list form))))))
      (walk-list (getf definition :body)))
    (nreverse call-sites)))

(defun %positional-helper-call-sites (helper definitions)
  (loop for definition in definitions
        append (%definition-call-sites
                definition
                (getf helper :function-symbol)
                (getf helper :parameter-position))))

(defun %public-positional-helper-record (helper definitions)
  (list :definition-path (getf helper :definition-path)
        :definition-kind (getf helper :definition-kind)
        :symbol (getf helper :symbol)
        :parameter-name (getf helper :parameter-name)
        :parameter-position (getf helper :parameter-position)
        :invocations
        (mapcar (function %public-positional-invocation)
                (getf helper :invocations))
        :callsites
        (mapcar (function %public-positional-callsite)
                (%positional-helper-call-sites helper definitions))))

(defun %positional-helper-inventory (definitions)
  (sort (mapcar (lambda (helper)
                  (%public-positional-helper-record helper definitions))
                (%positional-helper-definitions definitions))
        #'string< :key (lambda (record)
                         (format nil "~A|~A|~D"
                                 (getf record :definition-path)
                                 (getf record :symbol)
                                 (getf record :parameter-position)))))

(defun %fixture-positional-continuation-inventory
    (text &optional (relative-path "fixture.lisp"))
  (%positional-helper-inventory (%source-definitions text relative-path)))

(defun %cps-positional-continuation-inventory (root)
  (%positional-helper-inventory
   (loop for pathname in (%source-pathnames root)
         append (%source-definitions
                 (uiop:read-file-string pathname)
                 (enough-namestring pathname root)))))

(defun %positional-helper-forward-edges (source slots)
  (loop for (form visible) in
          (%terminal-call-forms
           (getf source :body)
           (list (getf source :callback-symbol))
           (getf source :function-symbol))
        when (and (consp form) (symbolp (first form)))
          append
            (loop for target in slots
                  when (eq (first form) (getf target :function-symbol))
                    append
                      (multiple-value-bind (argument present-p)
                          (%safe-list-nth
                           (cdr form)
                           (1- (getf target :parameter-position)))
                        (when (and present-p
                                   (member
                                    (getf source :callback-symbol)
                                    (%callback-designator-variables argument visible)
                                    :test #'eq))
                          (list (list :target target)))))))

(defun %forwarded-invocations (source target)
  (let ((source-symbol (getf source :function-symbol))
        (target-symbol (getf target :function-symbol)))
    (unless (eq source-symbol target-symbol)
      (loop for invocation in (getf target :invocations)
            for via = (getf invocation :via)
            unless (member source-symbol via :test #'eq)
              collect
                (let ((forwarded (copy-list invocation)))
                  (setf (getf forwarded :via) (cons target-symbol via))
                  forwarded)))))

(defun %resolve-positional-helper-forwarding (slots)
  (loop with changed = t
        while changed
        do (setf changed nil)
           (dolist (slot slots)
             (let ((invocations
                     (remove-duplicates
                      (append
                       (getf slot :invocations)
                       (loop for edge in (getf slot :forward-edges)
                             append
                               (%forwarded-invocations
                                slot (getf edge :target))))
                      :test #'equal)))
               (unless (equal invocations (getf slot :invocations))
                 (setf (getf slot :invocations) invocations
                       changed t))))
        finally (return slots)))

(defun %public-positional-invocation (invocation)
  (let ((public (copy-list invocation)))
    (when (getf public :via)
      (setf (getf public :via)
            (mapcar #'%function-name-string (getf public :via))))
    public))

(defun %public-positional-callsite (callsite)
  (list :path (getf callsite :path)
        :enclosing-kind (getf callsite :enclosing-kind)
        :enclosing-symbol (getf callsite :enclosing-symbol)
        :call-form (getf callsite :call-form)
        :argument-shape (getf callsite :argument-shape)))

(defun %forwarded-continuation-actual-p (callsite slots)
  (let ((argument (getf callsite :actual-argument))
        (caller (getf callsite :enclosing-function-symbol)))
    (and (symbolp argument)
         (some (lambda (slot)
                 (and (eq caller (getf slot :function-symbol))
                      (eq argument (getf slot :callback-symbol))
                      (getf slot :continuation-p)))
               slots))))

(defun %resolve-positional-helper-continuations (slots definitions)
  (dolist (slot slots)
    (when (and (getf slot :invocations)
               (%on-name-p (getf slot :callback-symbol)))
      (setf (getf slot :continuation-p) t)))
  (loop with changed = t
        while changed
        do (setf changed nil)
           (dolist (slot slots)
             (when (and (getf slot :invocations)
                        (not (getf slot :continuation-p))
                        (some
                         (lambda (callsite)
                           (or (%continuation-actual-p callsite)
                               (%forwarded-continuation-actual-p callsite slots)))
                         (%positional-helper-call-sites slot definitions)))
               (setf (getf slot :continuation-p) t
                     changed t)))
        finally (return slots)))

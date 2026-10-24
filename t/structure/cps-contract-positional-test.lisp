(in-package #:aitools.integration.cps-contract-test)

(describe "aitools positional continuation detector"
  (it "keeps legacy checks and inventories function and lambda callbacks"
    (expect
     (%fixture-violations
      "(defun new/k (value) value)
(defun new-helper (&key on-ok) (funcall on-ok))
(defun (setf new-setter/k) (value object) (setf object value))")
     :to-equal (list (list "fixture.lisp" "NEW/K" :slash-k-defun)
                     (list "fixture.lisp" "NEW-HELPER" :on-key-defun)
                     (list "fixture.lisp" "NEW-SETTER/K" :slash-k-defun)))
    (let ((source
           "(defun fail-with-report-error (target on-error)
  (funcall on-error target :reason \"invalid\"))
(defun fail-with-invalid (target on-invalid)
  (apply on-invalid (list target)))
(defun fail-with-lambda (target callback)
  (funcall callback target))
(define-flow (bad-report-error target &key on-error)
  (fail-with-report-error target #'report-error))
(define-flow (bad-invalid target &key on-error)
  (fail-with-invalid target #'invalid))
(define-flow (bad-lambda target &key on-error)
  (fail-with-lambda target (lambda (value) (flow-continue :on-ok value))))"))
      (expect
       (%fixture-violations source)
       :to-equal
       (list (list "fixture.lisp" "FAIL-WITH-INVALID"
                   :positional-continuation-defun)
             (list "fixture.lisp" "FAIL-WITH-LAMBDA"
                   :positional-continuation-defun)
             (list "fixture.lisp" "FAIL-WITH-REPORT-ERROR"
                   :positional-continuation-defun)))
      (expect
       (%fixture-positional-continuation-inventory source)
       :to-equal
       (list
        (list :definition-path "fixture.lisp"
              :definition-kind :defun
              :symbol "FAIL-WITH-INVALID"
              :parameter-name "ON-INVALID"
              :parameter-position 2
              :invocations
              '((:form :apply
                 :positional-arity :unclassified
                 :keywords :unclassified))
              :callsites
              (list
               (list :path "fixture.lisp"
                     :enclosing-kind :define-flow
                     :enclosing-symbol "BAD-INVALID"
                     :call-form :direct
                     :argument-shape '(:function-designator "INVALID"))))
        (list :definition-path "fixture.lisp"
              :definition-kind :defun
              :symbol "FAIL-WITH-LAMBDA"
              :parameter-name "CALLBACK"
              :parameter-position 2
              :invocations
              '((:form :funcall :positional-arity 1 :keywords nil))
              :callsites
              (list
               (list :path "fixture.lisp"
                     :enclosing-kind :define-flow
                     :enclosing-symbol "BAD-LAMBDA"
                     :call-form :direct
                     :argument-shape '(:lambda))))
        (list :definition-path "fixture.lisp"
              :definition-kind :defun
              :symbol "FAIL-WITH-REPORT-ERROR"
              :parameter-name "ON-ERROR"
              :parameter-position 2
              :invocations
              '((:form :funcall :positional-arity 1 :keywords (:reason)))
              :callsites
              (list
               (list :path "fixture.lisp"
                     :enclosing-kind :define-flow
                     :enclosing-symbol "BAD-REPORT-ERROR"
                     :call-form :direct
                     :argument-shape
                     '(:function-designator "REPORT-ERROR"))))))))

  (it "recognizes computed designators and terminal control forms"
    (let ((inventory
            (%fixture-positional-continuation-inventory
             "(defun finish-target (target truncated on-ok on-partial)
  (funcall (if truncated on-partial on-ok) target))
(defun terminal-cond (on-cond selected)
  (cond (selected (funcall on-cond selected))
        (t (funcall on-cond nil))))
(defun terminal-let (on-let value)
  (let ((copy value))
    (funcall on-let copy)))
(defun terminal-mvb (on-mvb values)
  (multiple-value-bind (value present) (values-list values)
    (funcall on-mvb value present)))
(defun terminal-progn (on-progn value)
  (progn :ready (funcall on-progn value)))
(defun return-on-error (on-error value)
  (return-from return-on-error (funcall on-error value)))
(defun loop-on-invalid (on-invalid values)
  (loop for value in values
        do (when value
             (return-from loop-on-invalid (funcall on-invalid value))))
  nil)
(defun handler-on-invalid (on-invalid value)
  (handler-case nil
    (error (condition)
      (return-from handler-on-invalid
        (funcall on-invalid condition)))))")))
      (expect (equal (mapcar (lambda (record) (getf record :parameter-name)) inventory)
               (list "ON-OK" "ON-PARTIAL" "ON-INVALID" "ON-INVALID" "ON-ERROR" "ON-COND" "ON-LET" "ON-MVB" "ON-PROGN"))
        :to-equal t)
      (expect (equal (mapcar
                         (lambda (record)
                           (getf (first (getf record :invocations)) :positional-arity))
                         inventory)
               (list :unclassified :unclassified 1 1 1 1 1 2 1))
        :to-equal t)))

  (it "keeps terminal forwarding helpers through the fixed point"
    (let* ((source
             "(defun fail-target-read (target on-error problem)
  (if problem
      (fail-unreadable target on-error)
      (fail-missing target on-error)))
(defun fail-unreadable (target callback)
  (fail callback \"environment.io\" target :repairs nil))
(defun fail-missing (target callback)
  (fail callback \"input.not-found\" target))
(defun fail (callback code message &rest keys)
  (apply callback code message keys))")
           (inventory (%fixture-positional-continuation-inventory source))
           (target-read
             (find "FAIL-TARGET-READ" inventory
                   :key (lambda (record) (getf record :symbol))
                   :test #'string=))
           (routes
             (mapcar (lambda (invocation) (getf invocation :via))
                     (getf target-read :invocations))))
      (expect (equal (mapcar (lambda (record) (getf record :symbol)) inventory)
               '("FAIL-MISSING" "FAIL-TARGET-READ" "FAIL-UNREADABLE" "FAIL"))
        :to-equal t)
      (expect (equal (sort routes #'string< :key (lambda (route)
                                            (format nil "~{~A~^-~}" route)))
               '(("FAIL-MISSING" "FAIL") ("FAIL-UNREADABLE" "FAIL")))
        :to-equal t)))

  (it "separates ordinary callbacks from continuations"
    (expect
     (%fixture-positional-continuation-inventory
      "(defun map-with-function (function items) (mapcar function items))
(defun map-with-on-map (on-map items) (mapcar on-map items))
(defun filter-with-predicate (predicate items) (remove-if-not predicate items))
(defun boolean-on-error (on-error) (if on-error :enabled :disabled))
(defun nonterminal-funcall (callback value) (progn (funcall callback value) :computed))
(defun classify-value (classify value)
  (multiple-value-bind (kind payload) (funcall classify value) (cons kind payload)))
(defun wait-with-probe (probe) (loop repeat 1 do (funcall probe)))
(defun store-io-adapter (primitive port) (funcall primitive port))
(defun json-model-update-at-adapter (function value key) (funcall function value key))
(defun ordinary-generic-callback (callback value) (funcall callback value))
(defun ordinary-caller (value)
  (ordinary-generic-callback (lambda (item) (+ item 1)) value))
(defun value-primitive (value) value)
(defun adapter-caller (value key)
  (store-io-adapter #'value-primitive value)
  (json-model-update-at-adapter (lambda (item token) (list item token)) value key))
(defun d (e) (destructuring-bind (a . b) e nil) nil)")
     :to-equal nil)
    (let* ((source
             "(defun invoke-generic-callback (callback value)
  (funcall callback value))
(defun forward-generic-callback (callback value)
  (invoke-generic-callback callback value))
(define-flow (via-funcall target &key on-ok)
  (funcall #'forward-generic-callback
           (lambda (value) (flow-continue :on-ok value)) target))
(define-flow (via-lambda target &key on-ok)
  (forward-generic-callback
   (lambda (value) (flow-continue :on-ok value)) target))
(define-flow (via-apply target &key on-ok)
  (apply #'forward-generic-callback
         (lambda (value) (flow-continue :on-ok value)) (list target)))
(define-flow (flow-target value &key on-ok) (flow-continue :on-ok value))
(defun flow-clause-callback (callback value)
  (flow-call (flow-target value) (:on-ok (result) (funcall callback result))))
(define-flow (flow-clause-caller value &key on-ok)
  (flow-call (flow-target value)
    (:on-ok (result)
      (flow-continue :on-ok (flow-clause-callback
        (lambda (item) (flow-continue :on-ok item)) result)))))")
       (inventory (%fixture-positional-continuation-inventory source))
       (apply-record
         (first
          (%fixture-positional-continuation-inventory
           "(defun apply-generic-callback (callback value)
  (funcall callback value))
(define-flow (apply-callback-lambda value &key on-ok)
  (apply #'apply-generic-callback
         (lambda (item) (flow-continue :on-ok item)) (list value)))"))))
  (expect
   (%fixture-violations source)
   :to-equal
   (list
    (list "fixture.lisp" "FLOW-CLAUSE-CALLBACK"
          :positional-continuation-defun)
    (list "fixture.lisp" "FORWARD-GENERIC-CALLBACK"
          :positional-continuation-defun)
    (list "fixture.lisp" "INVOKE-GENERIC-CALLBACK"
          :positional-continuation-defun)))
  (expect
   (equal
    (mapcar
     (lambda (record)
       (list (getf record :symbol) (getf record :parameter-name)
             (getf record :parameter-position) (getf record :invocations)
             (mapcar
              (lambda (site)
                (list (getf site :call-form) (getf site :enclosing-symbol)
                      (getf site :argument-shape)))
              (getf record :callsites))))
     inventory)
    '(("FLOW-CLAUSE-CALLBACK" "CALLBACK" 1
   ((:form :funcall :positional-arity 1 :keywords nil))
   ((:direct "FLOW-CLAUSE-CALLER" (:lambda))))
  ("FORWARD-GENERIC-CALLBACK" "CALLBACK" 1
   ((:via ("INVOKE-GENERIC-CALLBACK") :form :funcall
     :positional-arity 1 :keywords nil))
   ((:funcall "VIA-FUNCALL" (:lambda))
    (:direct "VIA-LAMBDA" (:lambda))
    (:apply "VIA-APPLY" :unclassified)))
  ("INVOKE-GENERIC-CALLBACK" "CALLBACK" 1
   ((:form :funcall :positional-arity 1 :keywords nil))
   ((:direct "FORWARD-GENERIC-CALLBACK" :dynamic)))))
   :to-equal t)
  (expect
   (equal
    (list (getf apply-record :symbol)
          (getf apply-record :invocations)
          (getf apply-record :callsites))
    '("APPLY-GENERIC-CALLBACK"
      ((:form :funcall :positional-arity 1 :keywords nil))
      ((:path "fixture.lisp" :enclosing-kind :define-flow
        :enclosing-symbol "APPLY-CALLBACK-LAMBDA"
        :call-form :apply :argument-shape :unclassified))))
   :to-equal t))))

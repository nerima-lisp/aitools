(in-package #:aitools.store.test)

(describe "Store macro and inline-function expansion coverage"
  (it "expands temporary directory setup with its callback and arguments"
    (let* ((expansion
             (macroexpand-1
              (quote
               (aitools.store.application::with-temp-dir
                   (path store target :mode #o755)
                 (list :created path)))))
           (definition (first (second expansion)))
           (call (cadddr expansion)))
      (expect (first expansion) :to-be (quote flet))
      (expect (second definition) :to-equal (quote (path)))
      (expect (cddr definition) :to-equal (quote ((list :created path))))
      (expect (first call)
              :to-be (quote aitools.store.application::call-with-temp-dir))
      (expect (rest call)
              :to-equal
              (list (quote store) (quote target)
                    (list (quote function) (first definition))
                    :mode #o755))
      (expect (second (third expansion))
              :to-equal
              (list (quote dynamic-extent)
                    (list (quote function) (first definition))))))
  (it "expands the I/O macro to a boundary call with its original arguments"
    (let ((expansion
            (macroexpand-1
             (quote
              (aitools.store.application::%io
               store create-file target (bytes "x") :mode #o600)))))
      (expect (first expansion) :to-be (quote funcall))
      (expect (second expansion)
              :to-equal
              (list
               (quote cl-boundary-kit:boundary-context-require)
               (list (quote aitools.store.application::store-boundaries)
                     (quote store))
               :create-file))
      (expect (cddr expansion)
              :to-equal
              (list (quote target) (list (quote bytes) "x") :mode #o600))))
  (it "expands prepared intent to its continuation callback"
    (let* ((expansion
             (macroexpand-1
              (quote
               (aitools.store.application::with-prepared-intent
                   (store intent steps)
                 (list :ready store)))))
           (definition (first (second expansion)))
           (call (cadddr expansion)))
      (expect (first expansion) :to-be (quote flet))
      (expect (second definition) :to-equal nil)
      (expect (cddr definition)
              :to-equal (quote ((list :ready store))))
      (expect (first call) :to-be (quote aitools.kernel.domain:flow-call))
      (expect (second call)
              :to-equal
              (list (quote aitools.store.application::call-with-prepared-intent/k)
                    (quote store) (quote intent) (quote steps)))
      (expect (third call)
              :to-equal (list :on-prepared nil (list (first definition))))
      (expect (second (third expansion))
              :to-equal
              (list (quote dynamic-extent)
                    (list (quote function) (first definition))))))
  (it "calls an inline fault point through the function cell"
    (let ((fault-function
            (symbol-function (quote aitools.store.application::fault-point)))
          (observed nil))
      (let ((aitools.store.application::*fault-hook* nil))
        (expect (funcall fault-function :without-hook "detail") :to-be nil))
      (let ((aitools.store.application::*fault-hook*
              (lambda (&rest event)
                (setf observed event)
                :called)))
        (expect (funcall fault-function :after-intent "op-1" 2)
                :to-be :called))
      (expect observed :to-equal (list :after-intent "op-1" 2)))))

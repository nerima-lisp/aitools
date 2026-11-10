(in-package #:aitools.store.test)

(describe "Store boundary primitives"
  (it "normalizes only absolute roots"
    (expect (aitools.store.application::%normalize-root "/workspace/")
            :to-equal "/workspace")
    (expect (aitools.store.application::%normalize-root "/") :to-equal "/")
    (signals error (aitools.store.application::%normalize-root "workspace")))

  (it "requires filesystem capabilities from the boundary context"
    (let* ((boundaries (cl-boundary-kit:make-boundary-context))
           (store (make-store boundaries "/workspace/" "/state"
                              :clock (cl-boundary-kit:make-fake-clock :start 0)
                              :sleeper (cl-boundary-kit:make-sleeper))))
      (expect (store-root store) :to-equal "/workspace")
      (signals error (aitools.store.application::workspace-kind store "missing"))))

  (it "calls fault hooks only when bound"
    (let ((observed nil))
      (let ((aitools.store.application::*fault-hook* nil))
        (expect (aitools.store.application::fault-point :test) :to-be nil))
      (let ((aitools.store.application::*fault-hook*
              (lambda (&rest event) (setf observed event))))
        (aitools.store.application::fault-point :after-intent "op-1"))
      (expect observed :to-equal (list :after-intent "op-1")))))

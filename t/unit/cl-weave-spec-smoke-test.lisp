(in-package #:aitools/test)

;;; Smoke check that cl-weave/spec loads in the Nix checks and in
;;; run-host-tests.lisp.
(cl-weave/spec:defspec smoke-bounded-counter ((limit 3) (cap 3))
  (:variables x)
  (:init (x 0))
  (:action inc
    (:guard (< x limit))
    (:next (x (1+ x))))
  (:invariant bounded (<= x cap))
  ;; The counter stops at its limit; that final state is not a deadlock.
  (:deadlock-check nil))

(describe "cl-weave/spec smoke"
  (cl-weave/spec:it-spec "explores every reachable state of a bounded counter without a violation"
    (cl-weave/spec:explore-spec (smoke-bounded-counter)))
  (it "reports a violation when the invariant is one step too tight"
    (let ((result (cl-weave/spec:explore-spec (smoke-bounded-counter :cap 2))))
      (expect (cl-weave/spec:spec-result-passed-p result) :to-be-falsy)
      (expect (cl-weave/spec:spec-result-states-found result) :to-be 4))))

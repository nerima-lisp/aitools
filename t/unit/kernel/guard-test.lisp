;;;; t/unit/kernel/guard-test.lisp
(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain guard"
  (it "parses the single-target --expect-hash form"
    (let ((entry (parse-expect-hash-argument "abc123")))
      (expect (expect-hash-entry-path entry) :to-be-falsy)
      (expect (expect-hash-entry-hash entry) :to-equal "abc123")))

  (it "parses the path=hash --expect-hash form"
    (let ((entry (parse-expect-hash-argument "src/a.lisp=abc123")))
      (expect (expect-hash-entry-path entry) :to-equal "src/a.lisp")
      (expect (expect-hash-entry-hash entry) :to-equal "abc123")))

  (it "rejects an empty hash"
    (signals error (parse-expect-hash-argument "")))

  (it "rejects an empty path before ="
    (signals error (parse-expect-hash-argument "=abc123")))

  (it "requires --expect-hash for a position-based selector"
    (expect (guard-required-p :expect-hash (make-range-selector "1:2")) :to-be-truthy))

  (it "does not require --expect-hash for a content-based selector"
    (expect (guard-required-p :expect-hash (make-old-selector "x")) :to-be-falsy))

  (it "requires --expect-count for --match"
    (expect (guard-required-p :expect-count (make-match-selector "re")) :to-be-truthy))

  (it "does not require --expect-count for --old"
    (expect (guard-required-p :expect-count (make-old-selector "x")) :to-be-falsy))

  (it "requires neither guard with no selector at all"
    (expect (guard-required-p :expect-hash nil) :to-be-falsy)
    (expect (guard-required-p :expect-count nil) :to-be-falsy)))

(describe "aitools.kernel.domain guard edges"
  (it "splits path=hash on the first ="
    (let ((entry (parse-expect-hash-argument "p=a=b")))
      (expect (expect-hash-entry-path entry) :to-equal "p")
      (expect (expect-hash-entry-hash entry) :to-equal "a=b")))

  (it "rejects a path=hash form with an empty hash"
    (signals simple-error (parse-expect-hash-argument "src/a.lisp=")))

  (it "signals for a guard it does not know"
    (signals error (guard-required-p :expect-size (make-range-selector "1")))))

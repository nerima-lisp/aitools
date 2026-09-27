;;;; t/unit/kernel/path-test.lisp
(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain path"
  (it "treats the root itself as inside"
    (expect (path-inside-p "/ws" "/ws") :to-be-truthy))

  (it "treats a child path as inside"
    (expect (path-inside-p "/ws" "/ws/a/b.lisp") :to-be-truthy))

  (it "rejects a sibling path that merely shares a prefix"
    (expect (path-inside-p "/ws" "/wsx/a") :to-be-falsy))

  (it "rejects a path outside the root"
    (expect (path-inside-p "/ws" "/other/a") :to-be-falsy))

  (it "computes a relative path for a child"
    (expect (path-relative-to "/ws" "/ws/a/b.lisp") :to-equal "a/b.lisp"))

  (it "computes an empty relative path for the root itself"
    (expect (path-relative-to "/ws" "/ws") :to-equal ""))

  (it "signals when the path is not inside the root"
    (signals error (path-relative-to "/ws" "/other/a")))

  (it "ignores a trailing separator on either side"
    (expect (path-inside-p "/ws/" "/ws") :to-be-truthy)
    (expect (path-relative-to "/ws/" "/ws/a/") :to-equal "a"))

  (it "treats every absolute path as inside the filesystem root"
    (expect (path-inside-p "/" "/") :to-be-truthy)
    (expect (path-inside-p "/" "/a/b") :to-be-truthy)
    (expect (path-relative-to "/" "/a/b") :to-equal "a/b")
    (expect (path-relative-to "/" "/") :to-equal "")))

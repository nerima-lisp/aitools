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

  (it-each (("/ws/" "/ws/a/" "/ws" "/ws/a" "a" t)
              ("/ws" "/ws/a" "/ws" "/ws/a" "a" t)
              ("/" "/a/" "/" "/a" "a" t)
              ("/" "/" "/" "/" "" t)
              ("" "" "" "" "" t))
  "normalizes trailing separators before containment and relative-path checks"
  (root candidate stripped-root stripped-candidate relative inside-p)
  (let ((strip (symbol-function
                (quote aitools.kernel.domain::%strip-trailing-separator))))
    (expect (funcall strip root) :to-equal stripped-root)
    (expect (funcall strip candidate) :to-equal stripped-candidate))
  (expect (path-inside-p root candidate) :to-be inside-p)
  (expect (path-relative-to root candidate) :to-equal relative))

  (it "treats every absolute path as inside the filesystem root"
    (expect (path-inside-p "/" "/") :to-be-truthy)
    (expect (path-inside-p "/" "/a/b") :to-be-truthy)
    (expect (path-relative-to "/" "/a/b") :to-equal "a/b")
    (expect (path-relative-to "/" "/") :to-equal ""))

  (it-each (("/real/ws/a" "a")
            ("/real" ""))
    "keeps resolved ~A and relative ~A"
    (real relative)
    (let ((path (make-workspace-path real relative)))
      (expect (workspace-path-real path) :to-equal real)
      (expect (workspace-path-relative path) :to-equal relative)))
  (it-each ((:real)
            (:relative))
           "rejects a non-string ~S workspace path field"
           (field)
           (let ((value (read-from-string "1")))
             (signals type-error
                      (ecase field
                        (:real (make-workspace-path value "relative"))
                        (:relative (make-workspace-path "/real" value)))))))

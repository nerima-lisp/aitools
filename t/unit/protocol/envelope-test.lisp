;;;; t/unit/protocol/envelope-test.lisp
(in-package #:aitools.protocol.test)

(describe "aitools.protocol.domain envelope"
  (it "builds an ok envelope with schema_version, status, and command first"
    (let* ((envelope (make-ok-envelope "read" (list (cons "path" "a.lisp"))))
           (alist (json-alist envelope)))
      (expect (json-alist-value envelope "schema_version") :to-be 1)
      (expect (json-alist-value envelope "status") :to-equal "ok")
      (expect (json-alist-value envelope "command") :to-equal "read")
      (expect (json-alist-value envelope "path") :to-equal "a.lisp")
      (expect (mapcar #'car alist) :to-equal (list "schema_version" "status" "command" "path"))))

  (it "omits next_commands when none are given"
    (let ((envelope (make-ok-envelope "read" nil)))
      (expect (assoc "next_commands" (json-alist envelope) :test #'string=) :to-be-falsy)))

  (it "includes next_commands when given"
    (let ((envelope (make-ok-envelope "read" nil :next-commands (list "aitools read a --range 2:3"))))
      (expect (json-alist-value envelope "next_commands") :to-equal (list "aitools read a --range 2:3"))))

  (it "builds a partial-status envelope"
    (let ((envelope (make-ok-envelope "read" nil :status "partial")))
      (expect (json-alist-value envelope "status") :to-equal "partial")))

  (it "includes recovered entries"
    (let ((envelope (make-ok-envelope "read" nil :recovered (list (list :op-id "op-1" :action "rolled-forward")))))
      (let ((recovered (first (json-alist-value envelope "recovered"))))
        (expect (json-alist-value recovered "op_id") :to-equal "op-1")
        (expect (json-alist-value recovered "action") :to-equal "rolled-forward"))))

  (it "builds an error envelope with the catalog's exit code"
    (let ((envelope (make-error-envelope "edit" "selection.no-match" "no match"
                                        :repairs (list (list :action "a" :detail "d" :command "c")))))
      (let ((error-object (json-alist-value envelope "error")))
        (expect (json-alist-value envelope "status") :to-equal "error")
        (expect (json-alist-value error-object "code") :to-equal "selection.no-match")
        (expect (json-alist-value error-object "exit_code") :to-be 2)
        (expect (json-alist-value error-object "message") :to-equal "no match")
        (let ((repair (first (json-alist-value error-object "repairs"))))
          (expect (json-alist-value repair "command") :to-equal "c")))))

  (it "omits candidates/diagnostics/conflicts when not given"
    (let* ((envelope (make-error-envelope "edit" "argument.invalid" "m"
                                          :repairs (list (list :action "a" :detail "d" :command "c"))))
           (error-object (json-alist-value envelope "error")))
      (expect (assoc "candidates" (json-alist error-object) :test #'string=) :to-be-falsy)
      (expect (assoc "diagnostics" (json-alist error-object) :test #'string=) :to-be-falsy)
      (expect (assoc "conflicts" (json-alist error-object) :test #'string=) :to-be-falsy)))

  (it "signals when no repairs are given"
    (signals error (make-error-envelope "edit" "argument.invalid" "m" :repairs nil))))

(describe "aitools.protocol.domain envelope helpers"
  (it "builds a JSON object from alternating keys and values in argument order"
    (expect (json-alist (aitools.protocol.domain:json-object "b" 1 "a" 2)) :to-equal (list (cons "b" 1) (cons "a" 2)))
    (expect (json-alist (aitools.protocol.domain:json-object)) :to-equal '()))

  (it "spells JSON null and false as json-kit sentinels, never NIL"
    (expect (aitools.protocol.domain:json-null) :to-be json-kit:+json-null+)
    (expect (aitools.protocol.domain:json-boolean nil) :to-be json-kit:+json-false+)
    (expect (aitools.protocol.domain:json-boolean t) :to-be t)
    (expect (aitools.protocol.domain:json-boolean 0) :to-be t))

  (it "places candidates, diagnostics, and conflicts after repairs when given"
    (let* ((envelope (make-error-envelope "edit" "selection.ambiguous" "two matches"
                                          :repairs (list (list :action "a" :detail "d" :command "c"))
                                          :candidates (list 1 2) :diagnostics (list "x") :conflicts (list "y")))
           (error-object (json-alist-value envelope "error")))
      (expect (mapcar #'car (json-alist error-object))
              :to-equal (list "code" "message" "exit_code" "repairs" "candidates" "diagnostics" "conflicts"))
      (expect (json-alist-value error-object "candidates") :to-equal (list 1 2))
      (expect (json-alist-value error-object "exit_code") :to-be 2))))

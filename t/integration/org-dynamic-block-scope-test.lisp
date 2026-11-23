(in-package #:aitools.edit.test)

(describe "Org dynamic block update clocktable scopes"
  (it "still updates the scopes that need no agenda files"
    (call-with-edit-workspace
     (lambda ()
       (dolist (scope '("file" "nil" "subtree" "tree" "tree1"))
         (put "report.org"
              (format nil "* Task~%~A~%#+BEGIN: clocktable :scope ~A~%old~%#+END:~%"
                      +dyn-clock+ scope))
         (%dyn-ok (%dyn-update-arguments "report.org" 1) "org dynamic-block update")
         (expect (list scope (and (search "*Total time*" (text "report.org")) t))
                 :to-equal (list scope t))
         (expect (list scope (search "old" (text "report.org")))
                 :to-equal (list scope nil)))))))

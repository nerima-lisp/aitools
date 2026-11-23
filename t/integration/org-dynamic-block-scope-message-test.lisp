(in-package #:aitools.edit.test)

(describe "Org dynamic block unsupported scope message"
  (it "echoes a file-name scope up to 80 characters and cuts a longer one with an ellipsis"
    (call-with-edit-workspace
     (lambda ()
       (let ((kept (make-string 80 :initial-element #\a)))
         (dolist (case-row (list (list 80 kept)
                                 (list 81 (concatenate 'string kept "..."))
                                 (list 200 (concatenate 'string kept "..."))))
           (put "report.org"
                (format nil "#+BEGIN: clocktable :scope ~S~%old~%#+END:~%"
                        (make-string (first case-row) :initial-element #\a)))
           (let ((envelope (%dyn-error (%dyn-update-arguments "report.org" 1)
                                       "org dynamic-block update" "argument.invalid")))
             (expect (cons (first case-row)
                           (aitools.test.support:envelope-value envelope "error" "message"))
                     :to-equal
                     (cons (first case-row)
                           (format nil "Unsupported clocktable scope ~A; use file, agenda, subtree, tree, treeN (N is 1 or more), or nil (the default)"
                                   (second case-row)))))))))))

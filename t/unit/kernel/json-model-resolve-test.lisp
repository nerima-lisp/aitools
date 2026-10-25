(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain JSON model continuation"
  (it "calls on-found after resolving every pointer token"
    (let* ((model aitools.edit.domain::*json-value-model*)
           (profile (json-object-with (cons "name" "Ada")))
           (document (json-object-with (cons "profile" profile)))
           (result nil))
      (aitools.kernel.domain::json-model-resolve/k
       model document (quote ("profile" "name"))
       :on-found (lambda (value) (setf result value))
       :on-missing (lambda (&rest values) (setf result values)))
      (expect result :to-equal "Ada")))
  (it "passes the prefix, current value, and missing token to on-missing"
    (let* ((model aitools.edit.domain::*json-value-model*)
           (profile (json-object-with (cons "name" "Ada")))
           (document (json-object-with (cons "profile" profile)))
           (result nil))
      (aitools.kernel.domain::json-model-resolve/k
       model document (quote ("profile" "age"))
       :on-found (lambda (value) (setf result (list :found value)))
       :on-missing (lambda (path value token)
                     (setf result (list path value token))))
      (expect (first result) :to-equal (quote ("profile")))
      (expect (second result) :to-be profile)
      (expect (third result) :to-equal "age"))))

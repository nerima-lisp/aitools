(in-package #:aitools.store.test)

(describe "Store lock and content diff coverage"
  (it "leaves an already-private directory unchanged"
    (call-with-temp-store
     (lambda (plain)
       (let* ((path "/state/private")
              (calls nil)
              (boundaries
                (cl-boundary-kit:boundary-context-with
                 (store-boundaries plain)
                 :lstat (lambda (candidate)
                          (push (list :lstat candidate) calls)
                          (values :directory #o700))
                 :chmod (lambda (candidate mode)
                          (push (list :chmod candidate mode) calls))))
              (store
                (aitools.store.application::%make-store
                 boundaries (store-clock plain) (aitools.store.application::store-sleeper plain)
                 (store-root plain) (store-state-directory plain) nil)))
         (aitools.store.application::%tighten-directory store path)
         (expect (nreverse calls) :to-equal `((:lstat ,path)))))
     :io nil))
  (it "reports a changed file as modified in transaction diff"
    (call-with-temp-store
     (lambda (store)
       (put-file store "content.txt" "before")
       (let ((tx (begin store)))
         (stage store tx (list (write-file-request "content.txt" (bytes "after"))))
         (tx-diff/k store tx
                    :on-diff (lambda (results)
                               (expect (mapcar #'change-result-action results)
                                       :to-equal '(:modified)))
                    :on-not-found (lambda () (error "transaction disappeared")))))
     :io nil)))

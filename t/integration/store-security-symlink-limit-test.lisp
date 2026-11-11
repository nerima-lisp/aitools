(in-package #:aitools.store.test)

(describe "aitools.store symlink resolution limits"
  (it "stops after 40 links"
    (call-with-temp-store
     (lambda (store)
       (loop for n from 1 to 41
             for target = (if (= n 41) "target" (format nil "link-~D" (1+ n)))
             do (sb-posix:symlink target (disk-path store (format nil "link-~D" n))))
       (let ((loop-root
               (make-store (store-boundaries store)
                          (disk-path store "link-1")
                          (state-home-of store))))
          (expect (aitools.store.application::%path-violation loop-root "target")
                  :to-equal "cannot be resolved (symlink loop)"))
       (expect (aitools.store.application::%real-path store (disk-path store "link-1")) :to-be nil))
     :io nil)))

(in-package #:aitools.store.test)

(describe "aitools.store case-insensitive replacement paths"
  (it "refuses case-only replacement paths on a case-insensitive filesystem"
    (call-with-temp-store
     (lambda (plain)
       (let* ((base-boundaries (store-boundaries plain))
              (native-lstat
                (cl-boundary-kit:boundary-context-require base-boundaries :lstat))
              (list-directory
                (cl-boundary-kit:boundary-context-require base-boundaries :list-directory))
              (boundaries
                (cl-boundary-kit:boundary-context-with
                 base-boundaries
                 :lstat
                 (lambda (path)
                   (multiple-value-bind (kind mode target mtime links identity)
                       (funcall native-lstat path)
                     (if (eq kind :absent)
                         (let* ((slash (position (code-char 47) path :from-end t))
                                (parent (if slash (subseq path 0 slash) "."))
                                (name (if slash (subseq path (1+ slash)) path))
                                (alias (find name (funcall list-directory parent)
                                             :test (function string-equal))))
                           (if alias
                               (funcall native-lstat (format nil "~A/~A" parent alias))
                               (values kind mode target mtime links identity)))
                         (values kind mode target mtime links identity))))))
              (store (make-store boundaries (store-root plain)
                                 (subseq (store-state-directory plain) 0
                                         (position #\/ (store-state-directory plain) :from-end t)))))
         (sb-posix:mkdir (disk-path store "nested") #o755)
         (dolist (paths (list (list "README" "readme")
                              (list "nested/README" "nested/readme")))
           (expect (nth-value 1
                              (commit store
                                      (mapcar (lambda (path)
                                                (write-file-request path (bytes "content")))
                                              paths)))
                   :to-equal "refusal.exists")
           (dolist (path paths)
             (expect (entry-state-kind (workspace-state store path)) :to-be :absent))
           (expect (temp-files store) :to-equal nil)
           (expect (intent-files store) :to-equal nil))
         (expect (handler-case
                     (progn
                       (aitools.store.application::%refuse-case-collisions
                        store
                        (list (make-intent-step :op :utime :path "ignored")
                              (make-intent-step :op :replace :path "README"
                                                :temp ".aitools-probe.tmp" :kind :file)
                              (make-intent-step :op :replace :path "README"
                                                :temp ".aitools-probe.tmp" :kind :file)
                              (make-intent-step :op :replace :path "readme"
                                                :temp ".aitools-probe.tmp" :kind :file)
                              (make-intent-step :op :replace :path "ignored-after-collision"
                                                :temp ".aitools-probe.tmp" :kind :file)))
                       nil)
                   (aitools.store.application::store-refusal (condition)
                     (aitools.store.application::store-refusal-code condition)))
                 :to-equal "refusal.exists")
         (expect (entry-state-kind (workspace-state store ".aitools-probe.tmp")) :to-be :absent)
         (expect (temp-files store) :to-equal nil)))
     :io nil)))

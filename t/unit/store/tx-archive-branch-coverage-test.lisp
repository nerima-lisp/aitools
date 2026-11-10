(in-package #:aitools.store.test)

(describe "store archive commit and case preparation branch coverage"
  (it "validates a one-character relative symlink target"
    (expect
     (aitools.store.application::%tx-commit-symlink-unsafe-target-p "x")
     :to-be nil))
  (it "refuses a symlink target under an on-disk symlink parent"
    (call-with-temp-store
     (lambda (store)
       (sb-posix:mkdir (disk-path store "real") #o755)
       (sb-posix:symlink "real" (disk-path store "alias"))
       (let ((index (make-tx-index))
             (refusal nil))
         (tx-index-put index
                       "alias/link"
                       (absent-state)
                       (symlink-state "target"))
         (handler-case
             (aitools.store.application::%recheck-tx-symlink-targets store index)
           (aitools.store.application::store-refusal (condition)
             (setf refusal condition)))
         (expect refusal :to-be-truthy)
         (expect
          (aitools.store.application::store-refusal-code refusal)
          :to-equal "refusal.outside-workspace")))
     :io nil))
  (it "covers absent and present case-variant temp probes"
    (call-with-temp-store
     (lambda (store)
       (let* ((io (store-io-port store))
              (native-lstat (store-io-lstat io))
              (variant-path (disk-path store ".AITOOLS-CASE-PROBE.TMP"))
              (variant-kind :absent)
              (probe-io
                (copy-store-io
                 io
                 :lstat
                 (lambda (path)
                   (if (string= path variant-path)
                       (values variant-kind nil nil nil nil nil)
                       (funcall native-lstat path)))))
              (probe-store
                (make-store probe-io
                            (store-root store)
                            (store-state-directory store)))
              (steps
                (list (make-intent-step :op :replace
                                        :path "readme"
                                        :temp ".aitools-first.tmp"
                                        :kind :file)
                      (make-intent-step :op :replace
                                        :path "README"
                                        :temp ".aitools-case-probe.tmp"
                                        :kind :file)))
              (temp-path (disk-path store ".aitools-case-probe.tmp")))
         (aitools.store.application::%refuse-case-collisions probe-store steps)
         (expect (aitools.store.application::%kind-at probe-store temp-path)
                 :to-be :absent)
         (setf variant-kind :file)
         (let ((refusal nil))
           (handler-case
               (aitools.store.application::%refuse-case-collisions probe-store steps)
             (aitools.store.application::store-refusal (condition)
               (setf refusal condition)))
           (expect refusal :to-be-truthy)
           (expect
            (aitools.store.application::store-refusal-code refusal)
            :to-equal "refusal.exists")
           (expect (aitools.store.application::%kind-at probe-store temp-path)
                   :to-be :absent))))
     :io nil)))

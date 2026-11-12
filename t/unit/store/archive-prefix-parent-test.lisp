(in-package #:aitools.store.test)

(describe "archive prefix parent descriptor validation"
  (it "rejects parent results without a matching directory handle"
    (call-with-temp-store
     (lambda (store)
       (sb-posix:mkdir (disk-path store "base") #o755)
       (let* ((io (store-boundaries store))
              (opened 0)
              (closed 0)
              (injected-fault nil)
              (tracked-io
                (cl-boundary-kit:boundary-context-with io
                 :open-search-root
                 (lambda (path)
                   (multiple-value-bind (fd identity)
                       (funcall (store-operation io :open-search-root) path)
                     (when fd (incf opened))
                     (values fd identity)))
                 :open-search-entry
                 (lambda (dirfd name path)
                   (multiple-value-bind (kind fd identity)
                       (funcall (store-operation io :open-search-entry) dirfd name path)
                     (when fd (incf opened))
                     (when (string= name "..")
                       (case injected-fault
                         (:not-directory
                          (setf kind :file))
                         (:no-descriptor
                          (when fd
                            (incf closed)
                            (funcall (store-operation io :close-search-directory) fd))
                          (setf fd nil))
                         (:identity-mismatch
                          (setf identity nil))))
                     (values kind fd identity)))
                 :close-search-directory
                 (lambda (fd)
                   (incf closed)
                   (funcall (store-operation io :close-search-directory) fd))))
              (tracked-store (make-store tracked-io (store-root store) "/tmp"))
              (checker
                (aitools.store.application:make-archive-symlink-prefix-checker
                 (aitools.store.application::%make-store-view tracked-store nil)
                 "" nil)))
         (unwind-protect
              (dolist (fault (list :not-directory :no-descriptor :identity-mismatch))
                (let ((cursor (funcall checker :start)))
                  (unwind-protect
                       (progn
                         (multiple-value-bind (next result)
                             (funcall checker :enter cursor "base")
                           (setf cursor next)
                           (expect result :to-be :clear))
                         (setf injected-fault fault)
                         (multiple-value-bind (next result)
                             (funcall checker :parent cursor)
                           (setf cursor next)
                           (expect result :to-be :outside)))
                    (setf injected-fault nil)
                    (funcall checker :finish cursor))))
           (funcall checker :finish))
         (expect opened :to-be closed)))
     :io nil))

  (it "checks archive cursor descriptor slot types"
    (let ((invalid-fd (read-from-string "bad"))
          (invalid-identity (read-from-string "1")))
      (signals type-error
        (aitools.store.application::make-%archive-prefix-cursor :current-fd invalid-fd))
      (signals type-error
        (aitools.store.application::make-%archive-prefix-cursor :current-identity invalid-identity)))))

(describe "archive prefix cursor handoff cleanup"
  (it "closes the opened directory when startup unwinds after transfer"
    (let ((closed nil)
          (tag (gensym "ARCHIVE-INTERRUPTED"))
          (template (aitools.store.application::make-%archive-prefix-cursor
                     :current-fd 9
                     :current-identity "root")))
      (let ((aitools.store.application::*fault-hook*
              (lambda (point &rest details)
                (declare (ignore details))
                (when (eq point :archive-prefix-handoff)
                  (throw tag :interrupted)))))
        (expect
         (catch tag
           (aitools.store.application::%archive-prefix-start
            template
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (values :directory 17 "root"))
            (lambda (fd) (push fd closed)))
           :returned)
         :to-be :interrupted))
      (expect closed :to-equal (list 17)))))

(in-package #:aitools.store.test)

(describe "archive prefix parent and Darwin descriptor branches"
  (it "restores a cursor without reopening an unmoved directory"
    (let* ((frame (aitools.store.application::make-%archive-prefix-frame
                   :moved-p nil))
           (cursor (aitools.store.application::make-%archive-prefix-cursor
                    :stack (list frame)))
           (opened 0)
           (closed 0))
      (multiple-value-bind (returned status)
          (aitools.store.application::%archive-prefix-parent
           cursor
           (lambda (&rest arguments)
             (declare (ignore arguments))
             (incf opened))
           (lambda (fd)
             (declare (ignore fd))
             (incf closed)))
        (expect returned :to-be cursor)
        (expect status :to-be :clear))
      (expect opened :to-be 0)
      (expect closed :to-be 0)
      (expect (aitools.store.application::%archive-prefix-cursor-stack cursor)
              :to-be nil)))
  #+darwin
  (it "classifies directory and file descriptors through Darwin metadata open"
    (call-with-temp-store
     (lambda (store)
       (let ((file-path (disk-path store "plain"))
             (io (store-boundaries store)))
         (with-open-file (stream file-path :direction :output)
           (write-string "file" stream))
         (multiple-value-bind (root-fd identity)
             (funcall (store-operation io :open-search-root) (store-root store))
           (declare (ignore identity))
           (unwind-protect
                (dolist (entry (list (list "." "." :directory)
                                     (list "plain" "plain" :file)))
                  (multiple-value-bind (kind fd child-identity)
                      (aitools.store.infrastructure::%open-entry-darwin-metadata
                       root-fd (first entry) (second entry))
                    (expect kind :to-be (third entry))
                    (if (eq kind :directory)
                        (progn
                          (expect fd :to-be-truthy)
                          (expect child-identity :to-be-truthy)
                          (funcall (store-operation io :close-search-directory) fd))
                        (progn
                          (expect fd :to-be nil)
                          (expect child-identity :to-be nil)))))
             (funcall (store-operation io :close-search-directory) root-fd)))))
     :io nil))
  #+darwin
  (it "rejects a regular file used as the search root"
    (call-with-temp-store
     (lambda (store)
       (let ((file-path (disk-path store "plain")))
         (with-open-file (stream file-path :direction :output)
           (write-string "file" stream))
         (signals aitools.store.application:store-io-error
           (aitools.store.infrastructure::%open-search-root file-path))))
     :io nil)))

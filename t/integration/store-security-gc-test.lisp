(in-package #:aitools.store.test)

(defun blob-listing (store)
  (sort (copy-list (funcall (store-io-list-directory (store-io-port store))
                            (blobs-directory (store-state-directory store))))
        #'string<))

(defun plant-blob-files (store &rest names)
  (dolist (name names)
    (with-open-file (out (sb-ext:parse-native-namestring
                          (join-path (blobs-directory (store-state-directory store)) name))
                         :direction :output :if-exists :supersede)
      (write-string "x" out))))

(defun referenced-store (store)
  "Commit a rewrite of a.txt so the journal references one blob, the
content before it; returns that blob's name."
  (put-file store "a.txt" "before")
  (commit store (list (write-file-request "a.txt" (bytes "after"))))
  (aitools.kernel.domain:content-hash (bytes "before")))

(defparameter *stray-hash* "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")

(describe "aitools.store garbage collection"
  (it "deletes stray temps and unreferenced blobs, keeping referenced blobs and foreign names"
    (with-temp-store (store)
      (let ((kept (referenced-store store))
            (tx-root (tx-root-directory (store-state-directory store))))
        (plant-blob-files store ".stale.tmp" "notes" *stray-hash*)
        (sb-posix:mkdir (join-path tx-root ".tx-x.deleting") #o700)
        (sb-posix:mkdir (join-path tx-root "junk") #o700)
        (collect-garbage store)
        (expect (blob-listing store) :to-equal (sort (list kept "notes") #'string<))
        (expect (funcall (store-io-list-directory (store-io-port store)) tx-root) :to-equal '("junk")))))

  (it-each (("an unreadable tx index" :tx-index)
            ("an unreadable journal line" :journal))
      "collects nothing when ~A leaves the references unknown"
      (label case)
    (declare (ignore label))
    (with-temp-store (store)
      (referenced-store store)
      (plant-blob-files store *stray-hash*)
      (ecase case
        (:tx-index
         (let ((tx (join-path (tx-root-directory (store-state-directory store)) (format-tx-id 3964000000 "00000000"))))
           (sb-posix:mkdir tx #o700)
           (with-open-file (out (sb-ext:parse-native-namestring (join-path tx "index.json")) :direction :output)
             (write-string "{" out))))
        (:journal
         (with-open-file (out (sb-ext:parse-native-namestring (journal-file-path (store-state-directory store)))
                              :direction :output :if-exists :append)
           (format out "{\"op_id\":\"x\"}~%"))))
      (collect-garbage store)
      (expect (member *stray-hash* (blob-listing store) :test #'string=) :to-be-truthy)
      (when (eq case :journal)
        ;; A write still succeeds; its own collection also keeps the blob.
        (expect (commit store (list (write-file-request "b.txt" (bytes "b")))) :to-be :committed)
        (expect (member *stray-hash* (blob-listing store) :test #'string=) :to-be-truthy))))

  (it "compacts the journal once it holds twice the retained entries"
    (with-temp-store (store)
      (flet ((journal-lines ()
               (count #\Newline (octets-string (funcall (store-io-read-file (store-io-port store))
                                                        (journal-file-path (store-state-directory store)))))))
        (loop for n from 1 to 40
              do (commit store (list (write-file-request "a.txt" (bytes (format nil "~D" n))))))
        (expect (journal-lines) :to-be 40)
        (commit store (list (write-file-request "a.txt" (bytes "41"))))
        (expect (journal-lines) :to-be 20)
        (expect (length (read-journal store)) :to-be 20))))

  (it "tolerates a removal that fails because the entry already vanished, and reports any other"
    (with-temp-store (store)
      (referenced-store store)
      (let* ((io (aitools.store.application::copy-store-io (store-io-port store)))
             (original-unlink #'store-io-unlink)
             (failing-store (make-store io (store-root store) (state-home-of store))))
        (flet ((collect-with-unlink (unlink)
         (cl-weave:with-mocked-functions
             (((symbol-function 'aitools.store.application:store-io-unlink)
               (lambda (candidate)
                 (if (eq candidate io)
                     unlink
                     (funcall original-unlink candidate)))))
           (collect-garbage failing-store))))
  (plant-blob-files store ".vanishing.tmp")
  (collect-with-unlink
   (lambda (path)
     (sb-posix:unlink path)
     (error 'store-io-error :operation "unlink" :path path :detail "raced")))
  (expect (member ".vanishing.tmp" (blob-listing store) :test #'string=) :to-be nil)
  (plant-blob-files store ".stuck.tmp")
  (let ((condition (handler-case
                       (progn
                         (collect-with-unlink
                          (lambda (path)
                            (error 'store-io-error :operation "unlink" :path path
                                   :detail "denied")))
                         nil)
                     (store-io-error (condition) condition))))
    (expect (store-io-error-detail condition) :to-equal "denied")
    (expect (store-io-error-path condition)
            :to-equal (join-path (blobs-directory (store-state-directory store))
                                 ".stuck.tmp")))))))
)


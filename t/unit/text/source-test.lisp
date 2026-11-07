;;;;
;;;; The text read flow over an in-memory TEXT-SOURCE: binary decided from
;;;; the first 8 KiB, before the rest of the file is read.
(in-package #:aitools.text.test)

(defun fake-source (files &optional reads (unreadable-path "/locked"))
  "A direct text boundary context over FILES, an alist of (path . octets)."
  (flet ((lookup (path) (cdr (assoc path files :test (function string=))))
         (note (operation path) (when reads (push (list operation path) (car reads)))))
    (cl-boundary-kit:make-boundary-context
     :text-read-sniffed
     (lambda (path count continue-p)
       (note :open path)
       (let ((bytes (lookup path)))
         (if (null bytes)
             (values nil (and (string= path unreadable-path) :unreadable))
             (let* ((prefix (subseq bytes 0 (min count (length bytes))))
                    (continue (funcall continue-p prefix (length bytes))))
               (if (and continue (= (length prefix) count))
                   (progn (note :rest path) bytes)
                   prefix))))))))

(defun read-outcome (source path &key max-bytes)
  (call-with-text-file/k source path
                         :max-bytes max-bytes
                         :on-text (lambda (bytes layout) (list :text (length bytes) (text-layout-line-ending layout)))
                         :on-binary (lambda (prefix size) (list :binary (length prefix) size))
                         :on-missing (lambda (path) (list :missing path))
                         :on-unreadable (lambda (path) (list :unreadable path))
                         :on-too-large (lambda (size) (list :too-large size))))

(describe "aitools.text.application call-with-text-file/k"
  (it "returns text with its layout"
    (expect (read-outcome (fake-source `(("/a" . ,(string-bytes (format nil "x~C~%" #\Return))))) "/a")
            :to-equal '(:text 3 :crlf)))

  (it "decides binary from the prefix without reading the whole file"
    (let* ((big (make-array 20000 :element-type '(unsigned-byte 8) :initial-element 65))
           (reads (list nil)))
      (setf (aref big 10) 0)
      (expect (read-outcome (fake-source `(("/b" . ,big)) reads) "/b") :to-equal '(:binary 8192 20000))
      (expect (car reads) :to-equal '((:open "/b")))))

  (it "reads a small file once and a large one in full after the sniff"
    (let ((reads (list nil)))
      (read-outcome (fake-source `(("/s" . ,(string-bytes "tiny"))) reads) "/s")
      (expect (car reads) :to-equal '((:open "/s"))))
    (let ((reads (list nil)))
      (read-outcome (fake-source `(("/l" . ,(make-array 9000 :element-type '(unsigned-byte 8) :initial-element 65))) reads)
                    "/l")
      (expect (reverse (car reads)) :to-equal '((:open "/l") (:rest "/l")))))

  (it "reports missing files and files over MAX-BYTES"
    (expect (read-outcome (fake-source '()) "/none") :to-equal '(:missing "/none"))
    (expect (read-outcome (fake-source `(("/c" . ,(string-bytes "12345")))) "/c" :max-bytes 4)
            :to-equal '(:too-large 5)))

  (it "reports a file that exists but cannot be read apart from a missing one"
    (let ((source (fake-source (quote ()))))
      (expect (read-outcome source "/locked") :to-equal '(:unreadable "/locked"))
      (expect (call-with-text-file/k source "/locked"
                                     :on-text (lambda (octets layout) (declare (ignore octets layout)) :text)
                                     :on-binary (lambda (prefix size) (declare (ignore prefix size)) :binary)
                                     :on-missing (lambda (path) (declare (ignore path)) :missing))
              :to-be :missing))))

(defun sniffed-outcome (source path &key max-bytes)
  (call-with-sniffed-octets/k source path
                              :max-bytes max-bytes
                              :on-text (lambda (bytes) (list :text (length bytes)))
                              :on-binary (lambda (prefix size) (list :binary (length prefix) size))
                              :on-missing (lambda (path) (list :missing path))
                              :on-unreadable (lambda (path) (list :unreadable path))
                              :on-too-large (lambda (size) (list :too-large size))))

(describe "aitools.text.application call-with-sniffed-octets/k"
  (it "reads a text file through one READ-SNIFFED call that goes on past the prefix"
    (let ((reads (list nil)))
      (expect (sniffed-outcome (fake-source `(("/t" . ,(make-array 9000 :element-type '(unsigned-byte 8)
                                                                             :initial-element 65))) reads)
                               "/t")
              :to-equal '(:text 9000))
      (expect (reverse (car reads)) :to-equal '((:open "/t") (:rest "/t")))))

  (it "stops a binary file and a file over MAX-BYTES at the prefix"
    (let ((big (make-array 20000 :element-type '(unsigned-byte 8) :initial-element 65))
          (reads (list nil)))
      (expect (sniffed-outcome (fake-source `(("/b" . ,(join-octets (subseq big 0 8191) (octets 0) (subseq big 8192)))) reads)
                               "/b")
              :to-equal '(:binary 8192 20000))
      (expect (sniffed-outcome (fake-source `(("/b" . ,big)) reads) "/b" :max-bytes 19999)
              :to-equal '(:too-large 20000))
      (expect (sniffed-outcome (fake-source `(("/b" . ,big)) reads) "/b" :max-bytes 20000)
              :to-equal '(:text 20000))
      (expect (reverse (car reads)) :to-equal '((:open "/b") (:open "/b") (:open "/b") (:rest "/b")))))

  (it "decides binary from the first 8 KiB only"
    (let ((big (join-octets (make-array 8192 :element-type '(unsigned-byte 8) :initial-element 65)
                            (octets 0)
                            (make-array 807 :element-type '(unsigned-byte 8) :initial-element 65))))
      (expect (sniffed-outcome (fake-source `(("/n" . ,big)) (list nil)) "/n") :to-equal '(:text 9000))))

  (it "tells missing from unreadable, with or without a READ-SNIFFED of the source's own"
    (let ((reads (list nil)))
      (expect (sniffed-outcome (fake-source '() reads) "/none") :to-equal '(:missing "/none"))
      (expect (sniffed-outcome (fake-source '() reads) "/locked") :to-equal '(:unreadable "/locked")))
    (expect (sniffed-outcome (fake-source '()) "/none") :to-equal '(:missing "/none"))
    (let ((locked (fake-source (quote ()) nil "/p")))
      (expect (sniffed-outcome locked "/p") :to-equal '(:unreadable "/p"))))

  (it "uses the direct :TEXT-READ-SNIFFED boundary operation"
    (let ((reads (list nil)))
      (expect (funcall (cl-boundary-kit:boundary-context-require (fake-source `(("/s" . ,(string-bytes "tiny"))) reads) :text-read-sniffed) "/s" 8192 (lambda (prefix size) (declare (ignore prefix size)) t))
              :to-equalp (string-bytes "tiny"))
      (expect (funcall (cl-boundary-kit:boundary-context-require (fake-source `(("/s" . ,(string-bytes "tiny"))) reads) :text-read-sniffed) "/s" 2 (lambda (prefix size) (declare (ignore prefix)) (expect size :to-be 4) nil))
              :to-equalp (string-bytes "ti"))
      (expect (funcall (cl-boundary-kit:boundary-context-require (fake-source `(("/s" . ,(string-bytes "tiny"))) reads) :text-read-sniffed) "/s" 2 (lambda (prefix size) (declare (ignore prefix size)) t))
              :to-equalp (string-bytes "tiny"))
      (expect (reverse (car reads)) :to-equal '((:open "/s") (:open "/s") (:open "/s") (:rest "/s")))))

  (it "fails fast when a size limit lacks an on-too-large continuation"
  (let ((result
         (handler-case
             (call-with-sniffed-octets/k
              (fake-source `(("/large" . ,(string-bytes "12345"))))
              "/large"
              :max-bytes 4
              :on-text (lambda (octets) (declare (ignore octets)) :text)
              :on-binary (lambda (prefix size) (declare (ignore prefix size)) :binary)
              :on-missing (lambda (path) (declare (ignore path)) :missing)
              :on-unreadable (lambda (path) (declare (ignore path)) :unreadable))
           (error (condition) (princ-to-string condition)))))
    (expect result :to-equal
            "No :ON-TOO-LARGE continuation was supplied for 5 bytes")))
  (it "fails fast when a text-file size limit lacks an on-too-large continuation"
      (let ((result
             (handler-case
                 (call-with-text-file/k
                  (fake-source `(("/large" . ,(string-bytes "12345"))))
                  "/large"
                  :max-bytes 4
                  :on-text (lambda (octets layout) (declare (ignore octets layout)) :text)
                  :on-binary (lambda (prefix size) (declare (ignore prefix size)) :binary)
                  :on-missing (lambda (path) (declare (ignore path)) :missing))
               (error (condition) (princ-to-string condition)))))
        (expect result :to-equal
                "No :ON-TOO-LARGE continuation was supplied for 5 bytes"))))

(describe "aitools.text.infrastructure make-host-text-source"
  (it "provides all file operations as boundary keys"
    (let ((context (aitools.text.infrastructure:make-host-text-source)))
      (dolist (key (list :text-file-size
                         :text-read-prefix
                         :text-read-octets
                         :text-call-with-chunks
                         :text-read-sniffed
                         :text-read-range))
        (expect (cl-boundary-kit:boundary-context-present-p context key) :to-be t)))))


;;;; t/unit/text/source-test.lisp
;;;;
;;;; The text read flow over an in-memory TEXT-SOURCE: binary decided from
;;;; the first 8 KiB, before the rest of the file is read.
(in-package #:aitools.text.test)

(defun fake-source (files &optional reads)
  "A TEXT-SOURCE over FILES, an alist of (path . octets). READS, when
given, is a cons whose car collects (operation path) records."
  (flet ((lookup (path) (cdr (assoc path files :test #'string=)))
         (note (operation path) (when reads (push (list operation path) (car reads)))))
    (make-text-source
     :file-size (lambda (path) (let ((bytes (lookup path))) (and bytes (length bytes))))
     :read-prefix (lambda (path count)
                    (note :prefix path)
                    (let ((bytes (lookup path))) (and bytes (subseq bytes 0 (min count (length bytes))))))
     :read-octets (lambda (path) (note :all path) (lookup path))
     :call-with-chunks (lambda (path size function)
                         (let ((bytes (lookup path)))
                           (and bytes
                                (loop for start from 0 below (length bytes) by size
                                      until (eq (funcall function (subseq bytes start (min (length bytes) (+ start size))))
                                                :stop)
                                      finally (return t))))))))

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
      (expect (car reads) :to-equal '((:prefix "/b")))))

  (it "reads a small file once and a large one in full after the sniff"
    (let ((reads (list nil)))
      (read-outcome (fake-source `(("/s" . ,(string-bytes "tiny"))) reads) "/s")
      (expect (car reads) :to-equal '((:prefix "/s"))))
    (let ((reads (list nil)))
      (read-outcome (fake-source `(("/l" . ,(make-array 9000 :element-type '(unsigned-byte 8) :initial-element 65))) reads)
                    "/l")
      (expect (reverse (car reads)) :to-equal '((:prefix "/l") (:all "/l")))))

  (it "reports missing files and files over MAX-BYTES"
    (expect (read-outcome (fake-source '()) "/none") :to-equal '(:missing "/none"))
    (expect (read-outcome (fake-source `(("/c" . ,(string-bytes "12345")))) "/c" :max-bytes 4)
            :to-equal '(:too-large 5)))

  (it "reports a file that exists but cannot be read apart from a missing one"
    (let ((source (make-text-source :file-size (lambda (path) (declare (ignore path)) (values nil :unreadable))
                                    :read-prefix (lambda (path count) (declare (ignore path count)) nil)
                                    :read-octets (lambda (path) (declare (ignore path)) nil)
                                    :call-with-chunks (lambda (path size function)
                                                        (declare (ignore path size function))
                                                        nil))))
      (expect (read-outcome source "/locked") :to-equal '(:unreadable "/locked"))
      (expect (call-with-text-file/k source "/locked"
                                     :on-text (lambda (octets layout) (declare (ignore octets layout)) :text)
                                     :on-binary (lambda (prefix size) (declare (ignore prefix size)) :binary)
                                     :on-missing (lambda (path) (declare (ignore path)) :missing))
              :to-be :missing))))

(defun sniffing-source (files reads)
  "FAKE-SOURCE's files through a READ-SNIFFED that notes (:open path) once per
call and (:rest path) when it goes on past the prefix."
  (let ((base (fake-source files)))
    (make-text-source
     :file-size (lambda (path) (source-file-size base path))
     :read-prefix (lambda (path count) (source-read-prefix base path count))
     :read-octets (lambda (path) (source-read-octets base path))
     :call-with-chunks (lambda (path size function) (source-call-with-chunks base path size function))
     :read-sniffed (lambda (path count continue-p)
                     (push (list :open path) (car reads))
                     (let ((bytes (cdr (assoc path files :test #'string=))))
                       (cond ((null bytes) (values nil (and (string= path "/locked") :unreadable)))
                             ((funcall continue-p (subseq bytes 0 (min count (length bytes))) (length bytes))
                              (push (list :rest path) (car reads))
                              bytes)
                             (t (subseq bytes 0 (min count (length bytes))))))))))

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
      (expect (sniffed-outcome (sniffing-source `(("/t" . ,(make-array 9000 :element-type '(unsigned-byte 8)
                                                                             :initial-element 65)))
                                                reads)
                               "/t")
              :to-equal '(:text 9000))
      (expect (reverse (car reads)) :to-equal '((:open "/t") (:rest "/t")))))

  (it "stops a binary file and a file over MAX-BYTES at the prefix"
    (let ((big (make-array 20000 :element-type '(unsigned-byte 8) :initial-element 65))
          (reads (list nil)))
      (expect (sniffed-outcome (sniffing-source `(("/b" . ,(join-octets (subseq big 0 8191) (octets 0) (subseq big 8192))))
                                                reads)
                               "/b")
              :to-equal '(:binary 8192 20000))
      (expect (sniffed-outcome (sniffing-source `(("/b" . ,big)) reads) "/b" :max-bytes 19999)
              :to-equal '(:too-large 20000))
      (expect (sniffed-outcome (sniffing-source `(("/b" . ,big)) reads) "/b" :max-bytes 20000)
              :to-equal '(:text 20000))
      (expect (reverse (car reads)) :to-equal '((:open "/b") (:open "/b") (:open "/b") (:rest "/b")))))

  (it "decides binary from the first 8 KiB only"
    (let ((big (join-octets (make-array 8192 :element-type '(unsigned-byte 8) :initial-element 65)
                            (octets 0)
                            (make-array 807 :element-type '(unsigned-byte 8) :initial-element 65))))
      (expect (sniffed-outcome (sniffing-source `(("/n" . ,big)) (list nil)) "/n") :to-equal '(:text 9000))))

  (it "tells missing from unreadable, with or without a READ-SNIFFED of the source's own"
    (let ((reads (list nil)))
      (expect (sniffed-outcome (sniffing-source '() reads) "/none") :to-equal '(:missing "/none"))
      (expect (sniffed-outcome (sniffing-source '() reads) "/locked") :to-equal '(:unreadable "/locked")))
    (expect (sniffed-outcome (fake-source '()) "/none") :to-equal '(:missing "/none"))
    (let ((locked (make-text-source :file-size (lambda (path) (declare (ignore path)) 5)
                                    :read-prefix (lambda (path count) (declare (ignore path count))
                                                   (values nil :unreadable))
                                    :read-octets (lambda (path) (declare (ignore path)) nil)
                                    :call-with-chunks (lambda (path size function)
                                                        (declare (ignore path size function))
                                                        nil))))
      (expect (sniffed-outcome locked "/p") :to-equal '(:unreadable "/p"))))

  (it "composes READ-SNIFFED from the other readers when a source has none"
    (let ((reads (list nil)))
      (expect (source-read-sniffed (fake-source `(("/s" . ,(string-bytes "tiny"))) reads) "/s" 8192
                                   (lambda (prefix size) (declare (ignore prefix size)) t))
              :to-equalp (string-bytes "tiny"))
      (expect (source-read-sniffed (fake-source `(("/s" . ,(string-bytes "tiny"))) reads) "/s" 2
                                   (lambda (prefix size) (declare (ignore prefix)) (expect size :to-be 4) nil))
              :to-equalp (string-bytes "ti"))
      (expect (source-read-sniffed (fake-source `(("/s" . ,(string-bytes "tiny"))) reads) "/s" 2
                                   (lambda (prefix size) (declare (ignore prefix size)) t))
              :to-equalp (string-bytes "tiny"))
      (expect (reverse (car reads)) :to-equal '((:prefix "/s") (:prefix "/s") (:prefix "/s") (:all "/s")))))

  (it "refuses a READ-SNIFFED that is not a function"
    (expect (handler-case (make-text-source :file-size #'identity :read-prefix #'identity :read-octets #'identity
                                            :call-with-chunks #'identity :read-sniffed 42)
              (error (condition) (princ-to-string condition)))
            :to-equal "make-text-source: READ-SNIFFED must be a function, got 42")))

(describe "aitools.text.application make-text-source"
  (it "names the argument that is not a function"
    (expect (handler-case (make-text-source :file-size 1 :read-prefix #'identity :read-octets #'identity
                                            :call-with-chunks #'identity)
              (error (condition) (princ-to-string condition)))
            :to-equal "make-text-source: FILE-SIZE must be a function, got 1")))

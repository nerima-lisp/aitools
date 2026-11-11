(in-package #:aitools.util.test)

(defun %util-source-directory ()
  (merge-pathnames "src/"
                   (asdf:system-source-directory "aitools/feature/util")))

(defun %util-lisp-files (directory)
  (append
   (remove-if-not (lambda (pathname)
                    (string-equal (or (pathname-type pathname) "") "lisp"))
                  (uiop:directory-files directory))
   (mapcan #'%util-lisp-files (uiop:subdirectories directory))))

(defun %util-production-files ()
  (cons (merge-pathnames "util.asd"
                         (asdf:system-source-directory "aitools/feature/util"))
        (%util-lisp-files (%util-source-directory))))

(defun %util-boundary-keys-in-form (form)
  (when (consp form)
    (append
     (when (and (eq (first form) 'cl-boundary-kit:boundary-context-require)
                (keywordp (third form)))
       (list (third form)))
     (%util-boundary-keys-in-form (car form))
     (%util-boundary-keys-in-form (cdr form)))))

(defun %util-required-boundary-keys ()
  (let ((keys '())
        (*package* (find-package '#:aitools.util.application))
        (*read-eval* nil)
        (eof (gensym "EOF")))
    (dolist (file (%util-lisp-files
                   (merge-pathnames "application/" (%util-source-directory))))
      (with-open-file (stream file)
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (setf keys
                       (nconc keys (%util-boundary-keys-in-form form))))))
    (remove-duplicates keys :test #'eq)))

(describe "aitools util boundary contract"
  (it "rejects legacy util adapter names and files"
    (let* ((source-files (%util-production-files))
           (legacy-adapter-symbols
             (list (concatenate (quote string) "util-" "port" "s")
                   (concatenate (quote string) "make-util-" "port" "s")
                   (concatenate (quote string) "make-util-" "port" "s-from-boundaries")
                   (concatenate (quote string) "make-production-util-" "port" "s")))
           (legacy-adapter-symbol-files
             (loop for file in source-files
                   when (some (lambda (symbol)
                                (search symbol (uiop:read-file-string file)
                                        :test #'char-equal))
                              legacy-adapter-symbols)
                     collect file))
           (legacy-adapter-files
             (remove-if-not (lambda (file)
                              (string-equal (or (pathname-name file) "")
                                            (concatenate (quote string) "port" "s")))
                            source-files)))
      (expect source-files :to-be-truthy)
      (expect legacy-adapter-symbol-files :to-equal nil)
      (expect legacy-adapter-files :to-equal nil)))

  (it "registers each capability required by util application"
    (let* ((required-keys (%util-required-boundary-keys))
           (expected-keys
             (list :util-random-octets :util-uuid-v4 :util-unix-ms
                   :util-read-file-octets :util-read-stdin-octets
                   :util-workspace-host :util-open-store))
           (workspace-host (cl-boundary-kit:make-boundary-context))
           (open-store (function identity))
           (boundaries
             (aitools.util.composition::make-util-dependencies
              :workspace-host workspace-host :open-store open-store)))
      (expect required-keys :to-be-truthy)
      (expect (length required-keys) :to-be (length expected-keys))
      (expect (every (lambda (key) (member key required-keys)) expected-keys)
              :to-be t)
      (expect (every (lambda (key) (member key expected-keys)) required-keys)
              :to-be t)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :util-workspace-host)
              :to-equal workspace-host)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :util-open-store)
              :to-equal open-store)
      (dolist (key (remove :util-workspace-host
                           (remove :util-open-store expected-keys)))
        (expect (functionp
                 (cl-boundary-kit:boundary-context-require boundaries key))
                :to-be t)))))

(describe "aitools.util.infrastructure os-random-source"
  (it "serves the cl-boundary-kit random-source protocol from the OS source"
    (let ((source (aitools.util.infrastructure::make-os-random-source)))
      (expect (length (cl-boundary-kit:random-source-bytes source 32)) :to-be 32)
      (expect (loop repeat 200 always (< (cl-boundary-kit:random-source-random source 62) 62)) :to-be-truthy)
      (expect (cl-boundary-kit:random-source-random source 1) :to-be 0)
      (expect (loop repeat 20 always (< (cl-boundary-kit:random-source-random source (expt 2 100)) (expt 2 100)))
              :to-be-truthy)))

  (it "does not repeat 16-byte draws"
    (let ((source (aitools.util.infrastructure::make-os-random-source)))
      (expect (equalp (cl-boundary-kit:random-source-bytes source 16) (cl-boundary-kit:random-source-bytes source 16))
              :to-be nil))))

(describe "aitools.util.infrastructure os-random-source errors"
  (it "reports a short read when its source reaches EOF"
    (let* ((aitools.util.infrastructure::+urandom-path+ "/dev/null")
           (source (aitools.util.infrastructure::make-os-random-source)))
      (unwind-protect
           (expect
            (handler-case
                (progn
                  (cl-boundary-kit:random-source-random source 2)
                  "no error")
              (error (condition) (princ-to-string condition)))
            :to-contain "short read from /dev/null")
        (when (aitools.util.infrastructure::%os-random-stream source)
          (close (aitools.util.infrastructure::%os-random-stream source)))))))

(in-package #:aitools.search.test)

(defun %search-feature-root ()
  (asdf:system-source-directory "aitools/feature/search"))

(defun %search-source-files (directory)
  (append
   (remove-if-not
    (lambda (pathname)
      (string-equal (or (pathname-type pathname) "") "lisp"))
    (uiop:directory-files directory))
   (mapcan #'%search-source-files (uiop:subdirectories directory))))

(defun %search-production-files ()
  (cons (merge-pathnames "search.asd" (%search-feature-root))
        (%search-source-files (merge-pathnames "src/" (%search-feature-root)))))

(defun %search-application-boundary-keys-in-form (form)
  (when (consp form)
    (append
     (when
         (and (eq (first form) 'cl-boundary-kit:boundary-context-require)
              (keywordp (third form)))
       (list (third form)))
     (%search-application-boundary-keys-in-form (car form))
     (%search-application-boundary-keys-in-form (cdr form)))))

(defun %search-required-boundary-keys ()
  (let ((keys '())
        (*package* (find-package '#:aitools.search.application))
        (*read-eval* nil)
        (eof (gensym "EOF")))
    (dolist
        (file
         (%search-source-files
          (merge-pathnames "application/"
                           (merge-pathnames "src/" (%search-feature-root)))))
      (with-open-file (stream file)
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (setf keys (nconc keys
                                   (%search-application-boundary-keys-in-form
                                    form))))))
    (remove-duplicates keys :test #'eq)))

(describe "aitools search boundary contract"
  (it "removes legacy search adapter symbols and production adapter files"
    (let* ((source-files (%search-production-files))
           (legacy-adapter-symbol-files
             (loop for file in source-files
                   when (search (concatenate (quote string) "search-" "port" "s")
                                (uiop:read-file-string file)
                                :test #'char-equal)
                     collect file))
           (legacy-adapter-files
             (remove-if-not (lambda (file)
                              (string-equal (or (pathname-name file) "")
                                            (concatenate (quote string) "port" "s")))
                            source-files)))
      (expect source-files :to-be-truthy)
      (expect legacy-adapter-symbol-files :to-equal nil)
      (expect legacy-adapter-files :to-equal nil)))

  (it "registers every capability required by application flows"
    (let* ((required-keys (%search-required-boundary-keys))
           (expected-keys (list :search-workspace-host :search-text-context
                                :search-open-store :search-unix-now
                                :search-read-stdin-octets))
           (workspace-host (cl-boundary-kit:make-boundary-context))
           (text-context
             (cl-boundary-kit:boundary-context-with
              (cl-boundary-kit:make-boundary-context)
              :text-call-with-chunks (function identity)))
           (open-store (function identity))
           (boundaries
             (aitools.search.composition::make-search-dependencies
              :workspace-host workspace-host
              :text-source text-context
              :open-store open-store)))
      (expect required-keys :to-be-truthy)
      (expect (length required-keys) :to-be (1+ (length expected-keys)))
      (expect (member :text-call-with-chunks required-keys) :to-be-truthy)
      (expect (every (lambda (key) (member key required-keys)) expected-keys)
              :to-be t)
      (expect (every (lambda (key) (member key expected-keys))
                     (remove :text-call-with-chunks required-keys))
              :to-be t)
      (dolist (key expected-keys)
        (expect (cl-boundary-kit:boundary-context-require boundaries key)
                :to-be-truthy))
      (expect (cl-boundary-kit:boundary-context-require
               text-context :text-call-with-chunks)
              :to-be-truthy)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :search-workspace-host)
              :to-equal workspace-host)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :search-text-context)
              :to-equal text-context)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :search-open-store)
              :to-equal open-store)
      (dolist (key (list :search-open-store :search-unix-now
                         :search-read-stdin-octets))
        (expect (functionp
                 (cl-boundary-kit:boundary-context-require boundaries key))
                :to-be t)))))

(in-package #:aitools.search.test)

(defun %allocation-cache-key ()
  (let* ((systems
           (list "aitools/data"
                 "aitools/core/workspace"
                 "aitools/core/store"
                 "aitools/feature/search"))
         (files
           (remove-duplicates
            (loop for system in systems
                  append (append
                          (uiop:symbol-call "CL-WEAVE" "ASDF-SYSTEM-FILES"
                                            system :include-dependencies t)
                          (uiop:symbol-call "CL-WEAVE" "ASDF-SYSTEM-DEFINITION-FILES"
                                            system :include-dependencies t)))
            :test (function string=)
            :key (function namestring))))
    (subseq
     (aitools.kernel.domain:content-hash
      (sb-ext:string-to-octets
       (with-output-to-string (manifest)
         (format manifest "lisp ~S ~S~%platform ~S ~S ~S~%"
                 (lisp-implementation-type)
                 (lisp-implementation-version)
                 (software-type)
                 (software-version)
                 (machine-type))
         (dolist (file (sort files (function string<) :key (function namestring)))
           (with-open-file (input file :direction :input
                                  :element-type (quote (unsigned-byte 8)))
             (let ((bytes (make-array (file-length input)
                                      :element-type (quote (unsigned-byte 8)))))
               (read-sequence bytes input)
               (format manifest "~A ~A~%"
                       (namestring file)
                       (aitools.kernel.domain:content-hash bytes))))))
       :external-format :utf-8))
     0
     24)))
(defun %allocation-cache-directory ()
  (let* ((key (%allocation-cache-key))
         (directory
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "aitools/search-allocation-fasl-~A/" key)
             (uiop:xdg-cache-home)))))
    (ensure-directories-exist
     (merge-pathnames "cache-entry" directory))
    directory))

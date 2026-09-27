;;;; packages/core/text/src/domain/language.lisp
;;;;
;;;; Lookup over the language table in data/domain/text/language-data.lisp.
;;;; The definition patterns stay strings here; the search and edit contexts
;;;; compile them with cl-regex-kit where they match.
(in-package #:aitools.text.domain)

(defstruct (language (:copier nil))
  (name "" :type string :read-only t)
  (extensions '() :type list :read-only t)
  (filenames '() :type list :read-only t)
  (line-comment nil :type (or null string) :read-only t)
  (block-comment nil :type list :read-only t)
  (extent :sexp :type (member :sexp :brace :indent :heading) :read-only t)
  (identifier "" :type string :read-only t)
  (definitions '() :type list :read-only t))

(defparameter *languages*
  (mapcar (lambda (plist)
            (make-language :name (getf plist :name)
                           :extensions (getf plist :extensions)
                           :filenames (getf plist :filenames)
                           :line-comment (getf plist :line-comment)
                           :block-comment (getf plist :block-comment)
                           :extent (getf plist :extent)
                           :identifier (getf plist :identifier)
                           :definitions (getf plist :definitions)))
          aitools.data:*text-languages*))

(defparameter *languages-by-filename*
  (let ((table (make-hash-table :test 'equal)))
    (dolist (language *languages* table)
      (dolist (filename (language-filenames language))
        (setf (gethash filename table) language)))))

(defparameter *languages-by-extension*
  (let ((table (make-hash-table :test 'equal)))
    (dolist (language *languages* table)
      (dolist (extension (language-extensions language))
        (setf (gethash extension table) language)))))

(defun language-names ()
  (mapcar #'language-name *languages*))

(defun find-language (name)
  "The LANGUAGE whose name is NAME (case-insensitive), or NIL."
  (find name *languages* :key #'language-name :test #'string-equal))

(defun language-for-path (path)
  "The LANGUAGE of PATH by exact base name, then by lowercase extension;
NIL for an unsupported file."
  (let ((name (subseq path (1+ (or (position #\/ path :from-end t) -1)))))
    (or (gethash name *languages-by-filename*)
        (let ((extension (%extension name)))
          (and extension (gethash extension *languages-by-extension*))))))

(defun language-path-predicate (name)
  "A predicate on paths accepting files of language NAME, suitable for the
workspace scan's :LANG argument; NIL when NAME is not a known language."
  (let ((language (find-language name)))
    (and language
         (lambda (path) (eq (language-for-path path) language)))))

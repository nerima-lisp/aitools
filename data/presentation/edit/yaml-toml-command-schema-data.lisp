(in-package #:aitools.data)

(defparameter *edit-structured-write-command-names*
  '("doc.set" "doc.delete" "doc.merge" "doc.patch" "doc.fmt"))

(defparameter *edit-structured-write-output-fields*
  '((:name "warnings" :description "For YAML/TOML writes, warnings lists dropped comments; always present, empty when none. JSON writes omit warnings.")))

(defparameter *edit-structured-write-error-codes*
  '("input.unsupported-value"))

(export '(*edit-structured-write-command-names*
         *edit-structured-write-output-fields*
         *edit-structured-write-error-codes*))

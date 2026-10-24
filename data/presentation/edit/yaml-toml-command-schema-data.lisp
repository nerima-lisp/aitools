(in-package #:aitools.data)

(defparameter *edit-yaml-toml-write-command-names*
  '("yaml.set" "yaml.delete" "yaml.merge" "yaml.patch" "yaml.fmt"
    "toml.set" "toml.delete" "toml.merge" "toml.patch" "toml.fmt"))

(defparameter *edit-yaml-toml-write-output-fields*
  '((:name "warnings" :description "[{code,message,count}] for dropped comments; always present, empty when none.")))

(defparameter *edit-yaml-toml-write-error-codes*
  '("input.unsupported-value"))

(export '(*edit-yaml-toml-write-command-names*
         *edit-yaml-toml-write-output-fields*
         *edit-yaml-toml-write-error-codes*))

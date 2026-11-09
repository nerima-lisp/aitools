(in-package #:aitools.data)

(defparameter *kernel-selector-catalog-data*
  (quote
   ((:old :content :ambiguous) (:range :position :fixed)
    (:symbol :position :ambiguous)
    (:between :content :ambiguous)
    (:match :content :select-all)))
  "Rows are KIND BASIS UNIQUENESS, in the order --old, --range, --symbol, --between, --match.")

(defparameter *kernel-selector-command-acceptance-data*
  (quote
   ((:edit . (:old :range :symbol :between :match))
    (:read . (:range :symbol :between :match))
    (:transform . (:range :symbol :between :match))
    (:move-lines . (:range :symbol :between :match))
    (:archive-read . (:range :symbol :between :match))
    (:insert . (:range :symbol :between :match))
    (:replace . (:range :symbol :between :match))))
  "The selectors each command accepts. A command absent from this alist accepts no selector at all.")

(export
 (quote
  (*kernel-selector-catalog-data* *kernel-selector-command-acceptance-data*)))

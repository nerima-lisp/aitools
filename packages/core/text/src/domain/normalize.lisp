;;;; packages/core/text/src/domain/normalize.lisp
;;;;
;;;; Unicode normalization for `transform --op nfc|nfkc` over SBCL's own
;;;; sb-unicode (NFKC folds full-width letters and joins a
;;;; separate voiced sound mark to its kana at SBCL 2.6.0).
(in-package #:aitools.text.domain)

(defun normalize-text (string form)
  "STRING in normalization FORM, one of :NFC, :NFD, :NFKC, :NFKD."
  (check-type form (member :nfc :nfd :nfkc :nfkd))
  (sb-unicode:normalize-string string form))

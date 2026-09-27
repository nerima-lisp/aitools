;;;; data/domain/protocol/selector-options-data.lisp
;;;;
;;;; The selector options (every selector but --old; one selector per
;;;; call), as the single data definition three presentation
;;;; sites build their cl-cli options from: edit (command-spec's :selectors
;;;; group), vcs (`git blame`/`git show`), and inspect (`read`, `archive
;;;; read`). Each entry names the option, its kind, an optional value-name for
;;;; help display, and the description; each site maps these through
;;;; MAKE-OPTION so the three cannot drift apart.
(in-package #:aitools.data)

(defparameter *selector-options*
  '((:key :range :name "range" :kind :value :value-name "S:E"
     :description "Lines S:E, S: or N (1-based, inclusive).")
    (:key :symbol :name "symbol" :kind :value :value-name "NAME"
     :description "The lines of a definition.")
    (:key :kind :name "kind" :kind :value :value-name "KIND"
     :description "Definition kind for --symbol.")
    (:key :between :name "between" :kind :pair :value-name "RE"
     :description "START-RE END-RE: a start line through the next end line.")
    (:key :exclusive :name "exclusive" :kind :flag
     :description "With --between: exclude both ends.")
    (:key :match :name "match" :kind :value :value-name "RE"
     :description "Every line matching RE.")
    (:key :invert :name "invert" :kind :flag
     :description "With --match: the lines that do not match."))
  "The selectors other than --old, one per call, in the order --range,
--symbol, --between, --match. :KIND is :VALUE, :FLAG, or :PAIR (a two-value option). :VALUE-NAME is
the help placeholder for value options; NIL for flags.")

(export '(*selector-options*))

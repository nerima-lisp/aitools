(in-package #:aitools.data)

(define-data-table +line-transform-ops+ :strings
  ("sort" "sort-numeric" "sort-version" "reverse" "shuffle" "unique" "delete-blank" "squeeze-blank"
    "strip-trailing" "indent" "dedent" "tabs-to-spaces" "spaces-to-tabs" "upper" "lower" "nfc" "nfkc"
    "wrap" "reflow" "comment" "uncomment"))

(define-data-table +whole-file-transform-ops+ :strings
  ("eol-lf" "eol-crlf" "final-newline" "no-final-newline" "strip-bom"))

(define-data-table +template-filters+ :strings
  ("upper" "lower" "capitalize" "snake" "camel" "kebab" "trim" "inc" "dec" "pad<N>")
  "The template filter names, as `schema` lists them.")

(export '(+line-transform-ops+ +whole-file-transform-ops+ +template-filters+))

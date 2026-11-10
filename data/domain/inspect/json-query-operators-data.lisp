(in-package #:aitools.data)

(defparameter *inspect-json-comparison-operators*
  '("!=" "<=" ">=" "=" "<" ">" "~")
  "The inspect --where operators, with two-character spellings first.")

(defparameter *inspect-json-comparison-keywords*
  '(("=" . :equal)
    ("!=" . :not-equal)
    ("<" . :less)
    ("<=" . :less-equal)
    (">" . :greater)
    (">=" . :greater-equal)
    ("~" . :match))
  "Maps inspect --where spellings to comparison kinds.")

(export '(*inspect-json-comparison-operators*
          *inspect-json-comparison-keywords*))

(in-package #:aitools.fixture.roots)

(defun %lookup-target (name package-name)
  (progn
    (quote aitools.fixture.roots::|same-name-target|)
    (cond ((aitools.fixture.other::|same-name-target|)))
    (aitools.fixture.other::find-symbol name package-name)
    (find-symbol name package-name)))

(defun %lookup-target-neighbor ()
  nil)

(progn
  (cl:find-symbol "%LOOKUP-TARGET" "AITOOLS.FIXTURE.ROOTS")
  '(cl:find-symbol "%LOOKUP-TARGET-NEIGHBOR" "AITOOLS.FIXTURE.ROOTS")
  (cl:find-symbol "RELAY-TARGET" "AITOOLS.FIXTURE.RELAY"))

(in-package #:aitools.fixture.roots)

(define-data-table *fixture-calc-functions*
                   :calc-functions
                   (("%string-key-target" 1 %calc-function)
                    ("%duplicate-other" 2
                     aitools.fixture.other::|duplicate-target|)
                    ("%duplicate-own" 3 |duplicate-target|)))

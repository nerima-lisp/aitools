(in-package #:aitools.util.test)

(describe "aitools.data define-data-table"
  (it "rejects duplicate string values"
    (signals error
      (macroexpand-1
       '(aitools.data::define-data-table *duplicate-schemes* :strings
          ("base64" "base64")))))
  (it "rejects repeated characters in alphabets"
    (signals error
      (macroexpand-1
       '(aitools.data::define-data-table *invalid-alphabet* :alphabet "aabc"))))
  (it "rejects repeated characters in string-pair alphabets"
    (signals error
      (macroexpand-1
       '(aitools.data::define-data-table *invalid-alphabets* :string-pairs
          (("hex" . "001122"))))))
  (it "rejects duplicate string-pair names"
    (signals error
      (macroexpand-1
       '(aitools.data::define-data-table *duplicate-alphabet-names* :string-pairs
          (("hex" . "0123") ("hex" . "4567"))))))
  (it "rejects nonpositive fixed calculation arity"
    (signals error
      (macroexpand-1
       '(aitools.data::define-data-table *invalid-functions* :calc-functions
          (("min" 0 cl:min))))))
  (it "rejects constant names"
    (signals error
      (macroexpand-1
       '(aitools.data::define-data-table nil :strings ("base64")))))
  (it "validates calculation handler declarations"
    (signals error
      (macroexpand-1
       '(aitools.data::define-data-table *unknown-functions* :calc-functions
          (("min" 1 :unknown-operation)))))
    (signals error
      (macroexpand-1
       '(aitools.data::define-data-table *unbound-functions* :calc-functions
          (("min" 1 cl:otherwise)))))
    (signals error
      (macroexpand-1
       '(aitools.data::define-data-table *non-cl-functions* :calc-functions
          (("min" 1 aitools.util.domain:codec-scheme-p)))))
    (signals error
      (macroexpand-1
       '(aitools.data::define-data-table *macro-functions* :calc-functions
          (("min" 1 cl:when)))))
    (signals error
      (macroexpand-1
       '(aitools.data::define-data-table *special-functions* :calc-functions
          (("min" 1 cl:if)))))
    (expect
     (first
      (macroexpand-1
       '(aitools.data::define-data-table *valid-functions* :calc-functions
          (("min" :variadic cl:min)
           ("round" 1 :round-half-away-from-zero)))))
     :to-be 'cl:defparameter)))

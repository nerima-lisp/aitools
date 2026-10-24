(in-package #:aitools.text.test)

(describe "aitools.text.domain zip DOS timestamp conversion"
  (it-each
   (("the first representable date" #x0000 #x0021 315532800)
    ("the last representable date and time" #xBF7D #xFF9F 4354819198)
    ("leap day" #x0000 #x585D 1709164800)
    ("February 30 normalizes to March 2" #x0000 #x565E 1677715200)
    ("zero month and day clamp to one" #x0000 #x5800 1704067200)
    ("month above December clamps to December" #x0000 #x59FF 1735603200)
    ("hour above 23 clamps to 23" #xF800 #x0021 315615600)
    ("minute above 59 clamps to 59" #x07E0 #x0021 315536340)
    ("second field above 29 clamps to 58" #x001F #x0021 315532858))
   "decodes ~A"
   (description time date expected)
   (declare (ignore description))
   (expect (aitools.text.domain::%dos-time-to-unix time date) :to-be expected))
  (it-each
   (("the first representable instant" 315532800 0 #x0021)
    ("an instant before 1980 clamps to the first representable instant" 0 0 #x0021)
    ("the last representable instant" 4354819198 #xBF7D #xFF9F)
    ("an instant after 2107 clamps to the last representable instant" 5000000000 #xBF7D #xFF9F)
    ("leap day" 1709164800 0 #x585D)
    ("an odd second rounds down to two-second resolution" 1700000001 #xB1AA #x576E))
   "encodes ~A"
   (description unix expected-time expected-date)
   (declare (ignore description))
   (expect (multiple-value-list (aitools.text.domain::%unix-to-dos-time unix))
           :to-equal
           (list expected-time expected-date))))

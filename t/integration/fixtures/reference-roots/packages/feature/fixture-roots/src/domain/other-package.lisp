(defpackage #:aitools.fixture.other
  (:use)
  (:shadow #:find-symbol)
  (:export #:relay-target))
(defpackage #:aitools.fixture.relay
  (:import-from #:aitools.fixture.other #:relay-target)
  (:export #:relay-target))
(defpackage #:aitools.fixture.empty-use (:use))
(defpackage #:aitools.fixture.default-use)

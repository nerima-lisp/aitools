(defpackage #:aitools.fixture.roots
  (:import-from #:aitools.protocol.application #:define-command)
  (:import-from #:aitools.inspect.presentation
                 #:define-inspect-command)
  (:import-from #:aitools.fixture.roots.api #:external-generic)
  (:shadow #:find-symbol))

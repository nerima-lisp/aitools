(in-package #:aitools.text.test)

(describe "ZIP32 size threshold"
  (it "accepts values below the sentinel and rejects the sentinel"
    (let ((limit #xFFFFFFFF))
      (expect (aitools.text.domain::%check-zip32-size
               (1- limit) "zip entry beyond 4GiB")
              :to-be nil)
      (signals archive-unsupported
        (aitools.text.domain::%check-zip32-size
         limit "zip entry beyond 4GiB")))))

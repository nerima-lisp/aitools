(in-package #:aitools.process.test)

(describe "aitools process host clock"
  (it "retries sleep until the requested interval has elapsed"
    (let ((times (list 0 1/2 1))
          (requests nil))
      (aitools.process.infrastructure::%sleep-for-at-least-ms
       1
       (lambda () (pop times))
       (lambda (milliseconds) (push milliseconds requests)))
      (expect (reverse requests) :to-equal (list 1 1/2))
      (expect times :to-equal nil))))

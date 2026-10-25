(defpackage #:aitools.fixture.struct-slots (:use #:cl) (:export #:entry))

(defstruct (live-record (:constructor nil) (:copier nil) (:predicate nil)) value)

(defstruct (orphan-record (:constructor nil) (:copier nil) (:predicate nil)) value)
(defun entry (record new-value)
  (setf (live-record-value record) new-value))

(in-package #:aitools.store.test)

(describe "aitools.store.domain layout boundaries"
  (it-each ((nil nil) ("relative" "also-relative"))
      "rejects invalid state/home values ~A and ~A"
      (xdg home)
    (signals error (state-home xdg home)))
  (it "handles the canonical root in workspace ids and joined paths"
    (let ((id (workspace-id "/")))
      (expect (subseq id 0 5) :to-equal "root-")
      (expect (length id) :to-be 21))
    (expect (join-path "/state" "") :to-equal "/state")
    (expect (join-path "/" "var" "log") :to-equal "/var/log"))
  (it "uses a safe root name for dot paths"
    (expect (subseq (workspace-id "/srv/.") 0 5) :to-equal "root-")))

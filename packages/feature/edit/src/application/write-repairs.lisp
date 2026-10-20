(in-package #:aitools.edit.application)

(defun %tx-not-found (fail tx)
  (funcall fail "input.not-found" (format nil "no open tx ~A" tx)
           :repairs (list (repair "list-tx" "List the open transactions." "aitools tx status"))))

(defun %damaged-state-repairs (tx)
  "Repairs for a STORE-FORMAT-ERROR: inspect or discard the transaction
whose record is damaged, or list open transactions when the write had no tx."
  (if tx
      (list (repair "tx-status" "Inspect the transaction's recorded state."
                    (format nil "aitools tx status ~A" (aitools.protocol.domain:shell-quote tx)))
            (repair "tx-abort" "Discard the damaged transaction and its staged writes."
                    (format nil "aitools tx abort ~A" (aitools.protocol.domain:shell-quote tx))))
      (list (repair "tx-status" "List open transactions and their state." "aitools tx status"))))

(in-package #:aitools.env.test)

(describe "aitools.env.domain network counters"
  (it "normalizes Linux and Darwin link rows without duplicating address rows"
    (dolist (fixture (list (cons :linux *network-linux-text*) #+darwin (cons :darwin *network-darwin-text*)))
      (multiple-value-bind (items valid) (parse-network-counters (cdr fixture) (car fixture))
        (expect valid :to-be t)
        (expect (length items) :to-be 2)
        (expect (mapcar (lambda (row) (field row "name")) items) :to-equal '("a0" "z0"))
        (expect (mapcar (lambda (name) (field (first items) name))
                        '("rx_bytes" "rx_packets" "rx_errors" "tx_bytes" "tx_packets" "tx_errors"))
                :to-equal '(700 8 9 1000 11 12)))))
  (it-each (("") ("Inter-| Receive | Transmit
 face |bytes
") ("Inter-| Receive | Transmit
 face |bytes
 a0: -1 2 3 0 0 0 0 0 4 5 6 0 0 0 0 0
") ("Inter-| Receive | Transmit
 face |bytes
 a0: 1 2
"))
      "rejects missing or malformed Linux network counters ~S" (text)
    (expect (nth-value 1 (parse-network-counters text :linux)) :to-be nil))
  (it-each ((:missing-separator) (:empty-interface-name) (:non-numeric-counter))
      "rejects malformed Linux counter rows: ~S" (kind)
    (let ((row
            (case kind
              (:missing-separator
               (format nil " a0 1 2 3 0 0 0 0 0 4 5 6 0 0 0 0 0~%"))
              (:empty-interface-name
               (format nil " : 1 2 3 0 0 0 0 0 4 5 6 0 0 0 0 0~%"))
              (:non-numeric-counter
               (format nil " a0: 1 2 invalid 0 0 0 0 0 4 5 6 0 0 0 0 0~%")))))
      (expect (nth-value 1
                         (parse-network-counters
                          (concatenate (quote string) *network-linux-text* row)
                          :linux))
              :to-be nil)))
  #+darwin (it-each (("a0 1500") ("a0 1500 192.0.2")
            ("a0 1500 192.0.2 192.0.2.1 x - 100 5 - 400 -"))
      "rejects malformed Darwin rows mixed with valid counters ~S" (row)
    (expect (nth-value 1 (parse-network-counters
                         (concatenate 'string *network-darwin-text* row) :darwin))
            :to-be nil))
  (it "rejects duplicate interfaces and malformed network headers or link rows"
    (expect (multiple-value-list
             (parse-network-counters
              (concatenate 'string *network-linux-text*
                           " a0: 1 2 3 0 0 0 0 0 4 5 6 0 0 0 0 0")
              :linux))
            :to-equal '(nil nil))
    #+darwin (expect (multiple-value-list
             (parse-network-counters "Name Mtu Network Address" :darwin))
            :to-equal '(nil nil))
    (expect (multiple-value-list
             (parse-network-counters
              (format nil "~A~% ~A~A~% ~A~%"
                      "Inter-| Missing | Transmit"
                      "face |bytes packets errs drop fifo frame compressed multicast|"
                      "bytes packets errs drop fifo colls carrier compressed"
                      "a0: 1 2 3 0 0 0 0 0 4 5 6 0 0 0 0 0")
              :linux))
            :to-equal '(nil nil))
    #+darwin (expect (multiple-value-list
             (parse-network-counters
              (format nil "~A~%~A~%"
                      "Alias Mtu Network Address Ipkts Ierrs Ibytes Opkts Oerrs Obytes Coll"
                      "z0 1500 <Link#2> 00:00:00:00:00:00 2 3 100 5 6 400 0")
              :darwin))
            :to-equal '(nil nil))
    #+darwin (expect (multiple-value-list
             (parse-network-counters
              (format nil "~A~%~A~%"
                      "Name Mtu Network Address Ipkts Ierrs Ibytes Opkts Oerrs Obytes Coll"
                      "a0* 16384 <Link#1> 8 9 700 11 12 1000 0 extra more")
              :darwin))
            :to-equal '(nil nil)))
  (it "rejects unsupported network counter operating systems"
    (expect (multiple-value-list
             (parse-network-counters *network-linux-text* :other))
            :to-equal '(nil nil)))
  (it "preserves colons in Linux interface aliases"
    (multiple-value-bind (items valid)
        (parse-network-counters
         (concatenate (quote string)
                      *network-linux-text*
                      (format nil " eth0:1: 50 1 0 0 0 0 0 0 60 2 0 0 0 0 0 0~%"))
         :linux)
      (expect valid :to-be t)
      (let ((row (find "eth0:1" items
                       :key (lambda (item) (field item "name"))
                       :test (function string=))))
        (expect row :to-be-truthy)
        (expect (field row "rx_bytes") :to-be 50)
        (expect (field row "tx_bytes") :to-be 60)))))

(describe "aitools.env.application network and disk"
  #+linux
  (it "limits sorted Linux interfaces and reports cumulative units"
    (multiple-value-bind (kind fields)
        (run-flow (function sys-network/k)
                  (make-fake-ports
                   :identity (list "Linux" "test" "generic")
                   :files (list (cons "/proc/net/dev" *network-linux-text*)))
                  :limit 1)
      (expect kind :to-be :partial)
      (expect (field fields "total") :to-be 2)
      (expect (field fields "truncated") :to-be t)
      (expect (field fields "basis") :to-equal "cumulative_interface_counters")
      (expect (object-field (field fields "units") "bytes") :to-equal "bytes")
      (expect (object-field (first (field fields "items")) "name") :to-equal "a0")))
  #+darwin (it "uses trusted Darwin netstat with fixed arguments despite PATH"
    (let ((host (make-fake-host :programs (list (cons "/usr/sbin/netstat" (exits-with *network-darwin-text*))))))
      (expect (run-flow #'sys-network/k (make-fake-ports :host host :environment '(("PATH" . "/untrusted")))) :to-be :ok)
      (expect (fake-host-runs host) :to-equal '(("/usr/sbin/netstat" ("-ibn"))))))
  #+darwin
  (it "limits sorted Darwin interfaces from trusted netstat"
    (let ((host (make-fake-host
                 :programs (list (cons "/usr/sbin/netstat"
                                       (exits-with *network-darwin-text*))))))
      (multiple-value-bind (kind fields)
          (run-flow (function sys-network/k)
                    (make-fake-ports :host host)
                    :limit 1)
        (expect kind :to-be :partial)
        (expect (field fields "source") :to-equal "netstat -ibn")
        (expect (field fields "total") :to-be 2)
        (expect (field fields "truncated") :to-be t)
        (expect (object-field (first (field fields "items")) "name")
                :to-equal "a0")
        (expect (object-field (first (field fields "items")) "rx_bytes")
                :to-be 700)
        (expect (fake-host-runs host)
                :to-equal (list (list "/usr/sbin/netstat" (list "-ibn")))))))
  (it-each ((0) (1001) (-1) (1.5)) "rejects out of range network limit ~S" (limit)
    (multiple-value-bind (kind error) (run-flow #'sys-network/k (make-fake-ports) :limit limit)
      (expect kind :to-be :error)
      (expect (first error) :to-equal "argument.invalid")))
  (it "does not turn unavailable or unsupported counters into an empty success"
    (dolist (identity '(("Other" "1" "generic") ("Linux" "1" "generic") ("Darwin" "1" "generic")))
      (multiple-value-bind (kind error) (run-flow #'sys-network/k (make-fake-ports :identity identity))
        (expect kind :to-be :error)
        (expect (first error) :to-equal "environment.unavailable"))))
  (it "reports one filesystem with bytes and an explicit availability basis"
    (multiple-value-bind (kind fields) (run-flow #'sys-disk/k (make-fake-ports))
      (expect kind :to-be :ok)
      (expect (field fields "path") :to-equal "/work")
      (expect (field fields "scope") :to-equal "filesystem_containing_path")
      (expect (field fields "total_bytes") :to-be 1000)
      (expect (field fields "available_bytes") :to-be 400)
      (expect (field fields "available_basis") :to-equal "unprivileged_available_blocks")))
  (it-each ((nil) ((100 101)) ((100 -1))) "rejects unavailable or inconsistent disk capacity ~S" (space)
    (multiple-value-bind (kind error) (run-flow #'sys-disk/k (make-fake-ports :space space))
      (expect kind :to-be :error)
      (expect (first error) :to-equal "environment.unavailable"))))

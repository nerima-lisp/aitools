;;;;
;;;; Fixture text follows the formats documented in proc(5) (Linux) and the
;;;; output captured from Darwin 25 `vm_stat`, `ps`, and `lsof`.
(in-package #:aitools.env.test)

(defparameter *network-linux-text*
  "Inter-| Receive | Transmit
 face |bytes packets errs drop fifo frame compressed multicast|bytes packets errs drop fifo colls carrier compressed

 z0: 100 2 3 0 0 0 0 0 400 5 6 0 0 0 0 0
 a0: 700 8 9 0 0 0 0 0 1000 11 12 0 0 0 0 0
")

(defparameter *network-darwin-text*
  "Name Mtu Network Address Ipkts Ierrs Ibytes Opkts Oerrs Obytes Coll
z0 1500 <Link#2> 00:00:00:00:00:00 2 3 100 5 6 400 0
z0 1500 192.0.2 192.0.2.1 2 - 100 5 - 400 -
a0* 16384 <Link#1> 8 9 700 11 12 1000 0
")

(describe "aitools.env.domain network counters"
  (it "normalizes Linux and Darwin link rows without duplicating address rows"
    (dolist (fixture (list (cons :linux *network-linux-text*) (cons :darwin *network-darwin-text*)))
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
  (it-each (("a0 1500") ("a0 1500 192.0.2")
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
    (expect (multiple-value-list
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
    (expect (multiple-value-list
             (parse-network-counters
              (format nil "~A~%~A~%"
                      "Alias Mtu Network Address Ipkts Ierrs Ibytes Opkts Oerrs Obytes Coll"
                      "z0 1500 <Link#2> 00:00:00:00:00:00 2 3 100 5 6 400 0")
              :darwin))
            :to-equal '(nil nil))
    (expect (multiple-value-list
             (parse-network-counters
              (format nil "~A~%~A~%"
                      "Name Mtu Network Address Ipkts Ierrs Ibytes Opkts Oerrs Obytes Coll"
                      "a0* 16384 <Link#1> 8 9 700 11 12 1000 0 extra more")
              :darwin))
            :to-equal '(nil nil)))
  (it "rejects unsupported network counter operating systems"
    (expect (multiple-value-list
             (parse-network-counters *network-linux-text* :other))
            :to-equal '(nil nil))))

(describe "aitools.env.domain metrics parsers"
  (it "computes aggregate CPU deltas without double-counting guest time"
    (let ((before (parse-cpu-counters "cpu 10 0 10 70 10 0 0 0 8 0"))
          (after (parse-cpu-counters "cpu 30 0 20 120 30 0 0 0 20 0")))
      (expect (length before) :to-be 8)
      (expect (cpu-busy-percent before after) :to-equal 30d0)
      (expect (cpu-busy-percent before before) :to-be nil)
      (expect (cpu-busy-percent after before) :to-be nil)))
  (it-each (("") ("cpu 1") ("cpu x 0 0 0 0 0 0 0") ("cpu0 1 2 3 4 5 6 7 8"))
      "rejects malformed aggregate CPU input ~S" (text)
    (expect (parse-cpu-counters text) :to-be nil))
  (it "accepts idle and saturated CPU intervals and rejects wrapped counters"
    (expect (cpu-busy-percent '(0 0 0 0 0 0 0 0) '(0 0 0 10 0 0 0 0)) :to-equal 0d0)
    (expect (cpu-busy-percent '(0 0 0 0 0 0 0 0) '(10 0 0 0 0 0 0 0)) :to-equal 100d0)
    (expect (cpu-busy-percent '(4294967295 0 0 0 0 0 0 0) '(0 0 0 10 0 0 0 0)) :to-be nil))
  (it-each (((30 0 20 120 9 0 0 0) 37.5d0)
            ((9 0 20 120 9 0 0 0) nil)
            ((10 0 10 70 9 0 0 0) nil))
      "permits only Linux iowait to decrease: ~S" (after expected)
    (let ((before '(10 0 10 70 10 0 0 0)))
      (expect (cpu-busy-percent before after) :to-be nil)
      (expect (cpu-busy-percent before after :iowait-may-decrease t) :to-equal expected)))
  (it "converts RSS to bytes and retains only the command basename"
    (let ((record (first (parse-process-metrics "42 125.5 2048 /usr/bin/worker"))))
      (expect (getf record :pid) :to-be 42)
      (expect (getf record :cpu-percent) :to-equal 125.5d0)
      (expect (getf record :rss-bytes) :to-be 2097152)
      (expect (getf record :command) :to-equal "worker")))
  (it-each (("")
            ("42")
            ("42 0")
            ("42 x 10 worker")
            ("42 0 -1 worker")
            ("42 0 10")
            ("0 0 10 worker"))
      "rejects malformed process metrics ~S" (text)
    (expect (parse-process-metrics text) :to-be nil))
  (it "does not invent available memory from missing page counters"
    (expect (parse-vm-stat "Mach Virtual Memory Statistics: (page size of 16384 bytes)" :strict t) :to-be nil)
    (expect (nth-value 1 (parse-meminfo (format nil "MemTotal: 1024 kB~%MemFree: 12 kB~%") :strict t)) :to-be nil))

  (describe "aitools.env.domain split-fields property"
    (cl-weave:around-each (next)
      (let ((cl-weave:*property-seed* 20261020)
            (cl-weave:*property-test-count* 64))
        (funcall next)))
    (cl-weave:it-property "round-trips generated non-whitespace field sequences"
        ((tokens (cl-weave:gen-list
                  (cl-weave:gen-string :min-length 1 :max-length 12
                                       :alphabet "abcXYZ09_-./")
                  :min-length 1 :max-length 12)))
      (expect (split-fields (format nil "~{~A~^ ~}" tokens))
              :to-equal tokens))))

(progn
(describe "aitools.env.domain host values at their edges"
  (it "joins onto an empty directory as an absolute path"
    (expect (join-directory "" "git") :to-equal "/git"))

  (it-each (("" nil) ("git" t) ("bin/git" nil) ("gi t" t))
      "judges the tool name ~S valid: ~A"
      (name valid)
    (expect (valid-tool-name-p name) :to-be valid))

  (it "refuses a tool name holding NUL and a zone name over 128 characters"
    (expect (valid-tool-name-p (format nil "git~C" (code-char 0))) :to-be nil)
    (expect (valid-zone-name-p (make-string 129 :initial-element #\A)) :to-be nil)
    (expect (valid-zone-name-p (make-string 128 :initial-element #\A)) :to-be-truthy))

  (it "searches $TZDIR first only when it is set and nonempty"
    (expect (first (zoneinfo-directories "/opt/zoneinfo")) :to-equal "/opt/zoneinfo")
    (expect (zoneinfo-directories "") :to-equal (zoneinfo-directories nil))
    (expect (zoneinfo-directories nil)
            :to-equal '("/usr/share/zoneinfo" "/usr/lib/zoneinfo" "/usr/share/lib/zoneinfo" "/etc/zoneinfo")))

  (it-each (("UTC" "UTC") (":Etc/UTC" "Etc/UTC") ("" nil) ("EST5EDT,M3.2.0,M11.1.0" nil) ("Europe/../x" nil))
      "reads the zone name from TZ=~S as ~S"
      (value name)
    (expect (zone-name-from-tz-variable value) :to-equal name)))
(describe "aitools.env.domain mutation and benchmark"
  (it "meets an 80 percent mutation score for network counter parsing"
    (let* ((source-path
             (asdf:system-relative-pathname
              "aitools" "packages/feature/env/src/domain/host-parsers.lisp"))
           (source-form
             (with-open-file (stream source-path)
               (let ((*package* (find-package "AITOOLS.ENV.DOMAIN")))
                 (loop for form = (read stream nil :eof)
                       until (eq form :eof)
                       when (and (consp form)
                                 (eq (first form) 'defun)
                                 (eq (second form) 'parse-network-counters))
                         return (list 'lambda (third form)
                                      (cons 'block
                                            (cons (second form) (cddddr form))))))))
           (expected '(("a0" 700 8 9 1000 11 12)
                       ("z0" 100 2 3 400 5 6))))
      (expect (not (null source-form)) :to-be t)
      (cl-weave:assert-mutation-score
       (cl-weave:run-mutations
        source-form
        (lambda (mutant mutation)
          (declare (ignore mutation))
          (let ((candidate (compile nil mutant)))
            (labels ((summary (rows)
                       (mapcar
                        (lambda (row)
                          (mapcar
                           (lambda (key)
                             (cdr (assoc key row :test #'equal)))
                           '("name" "rx_bytes" "rx_packets" "rx_errors"
                             "tx_bytes" "tx_packets" "tx_errors")))
                        rows))
                     (matches-p
                       (input os &optional (expected-rows expected))
                       (handler-case
                           (multiple-value-bind (rows valid)
                               (funcall candidate input os)
                             (and valid (equal (summary rows) expected-rows)))
                         (error () nil)))
                     (rejects-p (input os)
                       (handler-case
                           (multiple-value-bind (rows valid)
                               (funcall candidate input os)
                             (and (null rows) (null valid)))
                         (error () nil))))
              (and (matches-p
                (concatenate
                 'string *network-linux-text*
                 " eth0:1: 50 1 0 0 0 0 0 0 60 2 0 0 0 0 0 0~%")
                :linux
                (append
                 (list (first expected) '("eth0:1" 50 1 0 60 2 0))
                 (rest expected)))
                   (matches-p *network-darwin-text* :darwin)
                   (rejects-p
                    (concatenate
                     'string *network-linux-text*
                     " a0: 1 2 3 0 0 0 0 0 4 5 6 0 0 0 0 0~%")
                    :linux)
                   (rejects-p
                    (concatenate
                     'string
                     "Name Mtu Network Address Ipkts Ierrs Ibytes Opkts Oerrs Obytes Coll~%"
                     "z0 1500 <Link#2> 00:00:00:00:00:00 2 3 invalid 5 6 400 0~%")
                    :darwin)
                   (rejects-p *network-linux-text* :other))))))
       0.80)))
  (it "records one benchmark sample without using timing as a test threshold"
    (let ((result
            (cl-weave:benchmark (:warmup 0 :samples 1 :iterations 1)
              (parse-network-counters *network-linux-text* :linux))))
      (expect (cl-weave:benchmark-result-iterations result) :to-be 1)
      (expect (length (cl-weave:benchmark-result-samples result)) :to-be 1)))))

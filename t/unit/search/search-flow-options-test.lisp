;;;; t/unit/search/search-flow-options-test.lisp
;;;;
;;;; search/k over standard input and patterns, its next commands, and its
;;;; read, skip and scan options.
(in-package #:aitools.search.test)

(defun error-code (fields) (getf fields :code))

(defun error-message (fields) (getf fields :message))

(defun failing-stdin (message)
  (lambda (limit &key on-octets on-too-large on-failure)
    (declare (ignore limit on-octets on-too-large))
    (funcall on-failure message)))

(describe "aitools.search.application search/k standard input and patterns"
  (it "rejects a request with no pattern"
    (multiple-value-bind (kind fields) (search-in (list (list "/w/a.txt" "x")))
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (error-message fields) :to-contain "needs a pattern")))

  (it "drops one trailing CR LF from a --stdin pattern, and nothing more"
    (flet ((matches (stdin)
             (field (nth-value 1 (run-flow #'search/k
                                           (make-fake-ports :files (list (list "/w/a.txt" (format nil "ab~%ab ~%")))
                                                            :stdin stdin)
                                           :stdin t :fixed t :output :matches))
                    "total_matches")))
      (expect (matches (format nil "ab~C~%" #\Return)) :to-be 2)
      (expect (matches (format nil "ab ~C~%" #\Return)) :to-be 1))
    ;; A lone newline leaves the empty pattern, which selects every line.
    (expect (field (nth-value 1 (run-flow #'search/k (make-fake-ports :files (list (list "/w/a.txt" (format nil "x~%y~%")))
                                                                      :stdin (format nil "~%"))
                                          :stdin t :output :count))
                   "total_matches")
            :to-be 2))

  (it-each ((:invalid-utf8 "argument.invalid" "not UTF-8 (byte 1)")
            (:too-large "argument.invalid" "exceeds 1 MiB")
            (:failure "environment.io" "stdin closed")
            (:no-port "internal.unexpected" "without its workspace host or text source"))
      "maps a --stdin ~A to ~A"
      (case code message)
    (let ((ports (ecase case
                   (:invalid-utf8 (make-fake-ports :stdin (coerce #(97 255) '(vector (unsigned-byte 8)))))
                   (:too-large (make-fake-ports :stdin (make-array (1+ (* 1024 1024)) :element-type '(unsigned-byte 8)
                                                                                      :initial-element 97)))
                   (:failure (make-fake-ports :stdin-port (failing-stdin "stdin closed")))
                   (:no-port (make-fake-ports :stdin-port nil)))))
      (multiple-value-bind (kind fields) (run-flow #'search/k ports :stdin t)
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal code)
        (expect (error-message fields) :to-contain message))))

  (it "ends a search whose pattern can match empty on a file without a final newline"
    (destructuring-bind (kind fields)
        (%within-seconds 5 (lambda ()
                             (multiple-value-list
                              (search-in (list (list "/w/a.txt" "ab")) :patterns '("") :output :count))))
      (expect kind :to-be :ok)
      (expect (field fields "total_matches") :to-be 1))))

(describe "aitools.search.application search/k next commands"
  (it "reproduces every flag of a cut-short run with the full --limit"
    (multiple-value-bind (kind fields)
        (search-in (list (list "/w/a.lisp" (format nil "(hit it's)~%(HIT it's)~%")))
                   :patterns '("it's") :ignore-case t :word t :glob '("*.lisp") :lang "common-lisp" :no-ignore t
                   :skip-larger-than "1MiB" :newer "1000h" :limit 1 :context 0 :paths '("a.lisp"))
      (expect kind :to-be :partial)
      (expect (field fields "total_matches") :to-be 2)
      (expect (field fields "next_commands")
              :to-equal (list (format nil "aitools search --pattern 'it'\\''s' --ignore-case --word --before 0 --after 0 ~
                                           --limit 2 --glob '*.lisp' --lang common-lisp --no-ignore --skip-larger-than 1MiB ~
                                           --newer 1000h a.lisp")))))

  (it "reproduces --line-regexp, --invert, and --multiline"
    (multiple-value-bind (kind fields)
        (search-in (list (list "/w/a.txt" (format nil "x~%y~%z~%")))
                   :patterns '("x") :line-regexp t :invert t :multiline t :limit 1 :context 1)
      (expect kind :to-be :partial)
      (expect (field fields "next_commands")
              :to-equal '("aitools search --pattern x --line-regexp --invert --multiline --before 1 --after 1 --limit 2"))))

  (it "quotes an empty pattern in the next command"
    (multiple-value-bind (kind fields) (search-in (list (list "/w/a.txt" (format nil "x~%y~%"))) :patterns '("") :limit 1)
      (expect kind :to-be :partial)
      (expect (field fields "next_commands") :to-equal '("aitools search --pattern '' --before 2 --after 2 --limit 2"))))

  (it "bounds files mode by entries and asks for the entry total"
    (multiple-value-bind (kind fields)
        (search-in (list (list "/w/a.txt" (format nil "hit~%hit~%")) (list "/w/b.txt" "hit") (list "/w/c.txt" "hit"))
                   :patterns '("hit") :output :files :limit 1)
      (expect kind :to-be :partial)
      (expect (field fields "paths") :to-equal '("a.txt"))
      (expect (field fields "total") :to-be 3)
      (expect (field fields "total_matches") :to-be 4)
      (expect (field fields "next_commands") :to-equal '("aitools search --pattern hit --output files --limit 3")))))

(describe "aitools.search.application search/k reads, skips, and scan options"
  (it "skips a listed file that is gone or fails to read, and a directory it cannot list, as unreadable"
    (multiple-value-bind (kind fields)
        (run-flow #'search/k (make-fake-ports :files (list (list "/w/gone.txt" "hit") (list "/w/bad.txt" "hit")
                                                           (list "/w/locked/x.txt" "hit") (list "/w/ok.txt" "hit"))
                                              :unreadable '("/w/gone.txt") :failing '("/w/bad.txt")
                                              :unlistable '("/w/locked"))
                  :patterns '("hit"))
      (expect kind :to-be :ok)
      ;; An unlistable directory is reported during the walk, before the
      ;; files the pool reads, so compare the entries without their order.
      (expect (sort (mapcar (lambda (entry) (list (json-alist-value entry "path") (json-alist-value entry "reason")))
                            (field fields "skipped"))
                    #'string< :key #'first)
              :to-equal '(("bad.txt" "unreadable") ("gone.txt" "unreadable") ("locked" "unreadable")))
      (expect (field fields "files_scanned") :to-be 1)))

  (it "joins a file read in several chunks, and judges binary-ness by the first chunk alone"
    (let ((text (concatenate 'string (make-string 70000 :initial-element #\a) (string #\Nul) "hit")))
      (multiple-value-bind (kind fields)
          (run-flow #'search/k (make-fake-ports :files (list (list "/w/big.txt" text)) :split-chunks t)
                    :patterns '("hit") :output :matches)
        (expect kind :to-be :ok)
        (expect (field fields "skipped") :to-equal '())
        (expect (jfield (first (field fields "matches")) "col") :to-be 70002))))

  (it "reads an empty file as text with no lines"
    (multiple-value-bind (kind fields)
        (search-in (list (list "/w/empty.txt" "")) :patterns '("x") :output :files-without-match)
      (expect kind :to-be :ok)
      (expect (field fields "paths") :to-equal '("empty.txt"))
      (expect (field fields "files_scanned") :to-be 1)))

  (it "takes --newer as a duration before now when it names no path"
    (let ((files (list (list "/w/old.txt" "hit" :mtime 1000) (list "/w/new.txt" "hit" :mtime 99000))))
      (expect (mapcar (lambda (block) (jfield block "path"))
                      (field (nth-value 1 (search-in files :patterns '("hit") :newer "1h")) "blocks"))
              :to-equal '("new.txt"))))

  (it-each ((:lang "cobol" "unknown language cobol")
            (:skip-larger-than "huge" "--skip-larger-than: not a size: huge")
            (:newer "yesterday" "--newer: yesterday is neither an existing path nor a duration"))
      "rejects ~S ~S as argument.invalid"
      (option value message)
    (multiple-value-bind (kind fields) (search-in (list (list "/w/a.txt" "hit")) :patterns '("hit") option value)
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (error-message fields) :to-contain message)))

  (it "offers the use-known-language repair for an unknown --lang"
    (multiple-value-bind (kind fields)
        (search-in (list (list "/w/a.txt" "hit")) :patterns '("hit") :lang "cobol")
      (expect kind :to-be :error)
      (expect (getf (first (getf fields :repairs)) :action) :to-equal "use-known-language")
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools search --lang common-lisp")))

  (it "rejects a start path outside the workspace with a --root repair"
    (multiple-value-bind (kind fields)
        (search-in (list (list "/w/a.txt" "hit") (list "/elsewhere/b.txt" "hit")) :patterns '("hit") :paths '("/elsewhere"))
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (error-message fields) :to-equal "/elsewhere is outside the workspace root")
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools --root /elsewhere search")))

  (it "rejects a --root that is a file as argument.invalid"
    (multiple-value-bind (kind fields) (search-in (list (list "/w/a.txt" "hit")) :patterns '("hit") :root "/w/a.txt")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (error-message fields) :to-equal "workspace root /w/a.txt is not a directory")))

  (it "reports internal.unexpected when built without a workspace host, or asked for --tx without a store"
    (multiple-value-bind (kind fields) (run-flow #'search/k (make-search-ports) :patterns '("x"))
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "internal.unexpected")
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools schema search"))
    (multiple-value-bind (kind fields) (search-in (list (list "/w/a.txt" "x")) :patterns '("x") :tx "tx-1")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "internal.unexpected"))))

;;;; t/e2e/org-cases.lisp
(in-package #:aitools.e2e.test)

(defmacro define-org-read-case ((name workspace) &body body)
  `(it ,name
     (with-progress (,name)
       (let ((*current-case* (list 0 ,name)))
         (aitools-binary)
         (call-with-workspace (lambda (,workspace) ,@body))))))

(defun org-ok (workspace arguments command)
  (let ((envelope (run-ok workspace arguments)))
    (expect envelope :to-match-envelope "command" command "status" "ok")
    envelope))

(defun org-error (workspace arguments command code)
  (let* ((result (aitools workspace arguments))
         (envelope (aitools-envelope result))
         (error (jget envelope "error"))
         (repairs (jlist (jget error "repairs"))))
    (expect (aitools-exit-code result) :to-be 1)
    (expect (aitools-stream result) :to-be :stderr)
    (expect envelope :to-match-envelope "command" command "status" "error")
    (expect (jget error "code") :to-equal code)
    (expect (and (stringp (jget error "message"))
                 (plusp (length (jget error "message"))))
            :to-be t)
    (expect (jget error "exit_code") :to-be 1)
    (expect (plusp (length repairs)) :to-be t)
    (dolist (repair repairs)
      (dolist (field '("action" "detail" "command"))
        (let ((value (jget repair field)))
          (expect (and (stringp value) (plusp (length value))) :to-be t))))
    envelope))

(defun org-outline-summary (entries)
  (mapcar (lambda (entry)
            (list (jget entry "kind") (jget entry "name")
                  (jget entry "level") (jget entry "line")))
          (jlist entries)))

(defun expect-org-fragments (content fragments)
  (dolist (fragment fragments)
    (expect (search fragment content) :to-be-truthy)))

(defun expect-org-one-fragment (content fragments)
  (expect (some (lambda (fragment) (search fragment content)) fragments)
          :to-be-truthy))

(defparameter +org-outline-query-fixture+
  (format nil "#+TITLE: Query~%* TODO Build :work:~%** TODO Test :work:~%* DONE Ship :work:~%"))

(defparameter +org-export-basic-fixture+ (format nil "A *bold*.~%"))

(defparameter +org-export-coverage-fixture+
  (format nil
          (concatenate 'string
                       "#+TITLE: Report~%* Heading~%"
                       "A *bold* and /italic/ with =code=.~%- [X] checked~%1. ordered~%"
                       "| A | B |~%|---+---|~%| 1 | 2 |~%"
                       "#+BEGIN_SRC text~%echo hi~%#+END_SRC~%"
                       "[[https://example.test][external]]~%Reference[fn:note].~%"
                       "[fn:note] Definition.~%")))

(defparameter +org-export-refusal-fixtures+
  `(("file" . ,(format nil "* Links~%[[file:target.org][file]]~%"))
    ("id" . ,(format nil "* Links~%[[id:target][id]]~%"))
    ("internal" . ,(format nil "* Links~%[[#target][internal]]~%* Target~%:PROPERTIES:~%:CUSTOM_ID: target~%:END:~%"))
    ("diary" . ,(format nil "* Diary~%%%(diary-float t 4 2)~%"))))

(describe "aitools e2e org read commands"
  (define-org-read-case ("org outline returns ordered entries and applies entry limits" ws)
    (put ws "outline.org" +org-outline-query-fixture+)
    (let ((before (file-octets ws "outline.org")))
      (expect (plusp (length before)) :to-be t)
      (let* ((limited (org-ok ws '("org" "outline" "outline.org" "--limit" "2") "org.outline"))
             (entries (jlist (jget limited "entries"))))
        (expect (jget limited "total") :to-be 3)
        (expect (jget limited "truncated") :to-be t)
        (expect (org-outline-summary entries)
                :to-equal '(("headline" "Build" 1 2) ("headline" "Test" 2 3))))
      (let ((zero (org-ok ws '("org" "outline" "outline.org" "--limit" "0") "org.outline")))
        (expect (jlist (jget zero "entries")) :to-equal '())
        (expect (jget zero "total") :to-be 3)
        (expect (jget zero "truncated") :to-be t))
      (let* ((all (org-ok ws '("org" "outline" "outline.org") "org.outline"))
             (entries (jlist (jget all "entries"))))
        (expect (jget all "total") :to-be 3)
        (expect (jget all "truncated") :to-be nil)
        (expect (org-outline-summary entries)
                :to-equal '(("headline" "Build" 1 2)
                            ("headline" "Test" 2 3)
                            ("headline" "Ship" 1 4))))
      (expect (file-octets ws "outline.org") :to-equalp before)))
  (define-org-read-case ("org outline returns named source and unnamed example blocks in document order" ws)
    (put ws "blocks.org"
         (format nil "~{~A~%~}"
                 (list "#+NAME: code-sample"
                       "#+BEGIN_SRC lisp"
                       "(+ 1 2)"
                       "#+END_SRC"
                       "#+BEGIN_EXAMPLE"
                       "plain text"
                       "#+END_EXAMPLE")))
    (let* ((envelope (org-ok ws '("org" "outline" "blocks.org") "org.outline"))
           (entries (jlist (jget envelope "entries"))))
      (expect (jget envelope "total") :to-be 2)
      (expect (jget envelope "truncated") :to-be nil)
      (expect (org-outline-summary entries)
              :to-equal '(("src-block" "code-sample" nil 1)
                             ("example-block" "" nil 5)))))

  (define-org-read-case ("org query filters TODO headings and accepts the documented regex subset" ws)
    (put ws "query.org" +org-outline-query-fixture+)
    (let ((before (file-octets ws "query.org")))
      (dolist (match '("work/TODO" "{w.*}/TODO"))
        (let* ((envelope (org-ok ws
                                 (list "org" "query" "query.org" "--match" match
                                       "--at" "2026-10-19T09:00:00" "--tz" "UTC")
                                 "org.query"))
               (matches (jlist (jget envelope "matches"))))
          (expect (jget envelope "count") :to-be 2)
          (expect (mapcar (lambda (entry) (jget entry "line")) matches) :to-equal '(2 3))
          (expect (mapcar (lambda (entry) (jget entry "level")) matches) :to-equal (quote (1 2)))
          (expect (mapcar (lambda (entry) (jget entry "title")) matches)
                  :to-equal '("Build" "Test"))
          (expect (mapcar (lambda (entry) (jget entry "todo")) matches)
                  :to-equal '("TODO" "TODO"))
          (expect (mapcar (lambda (entry) (jlist (jget entry "tags"))) matches)
                  :to-equal '(("work") ("work")))
          (expect (mapcar (lambda (entry)
                            (mapcar (lambda (part)
                                      (list (jget part "title") (jget part "index")))
                                    (jlist (jget entry "heading_path"))))
                          matches)
                  :to-equal '((("Build" 1)) (("Build" 1) ("Test" 1))))))
      (expect (file-octets ws "query.org") :to-equalp before)))

  (define-org-read-case ("org query errors include a repair and source position" ws)
    (put ws "query.org" +org-outline-query-fixture+)
    (let* ((before (file-octets ws "query.org"))
           (envelope (org-error ws
                                '("org" "query" "query.org" "--match" "work/(TODO)"
                                  "--at" "2026-10-19T09:00:00" "--tz" "UTC")
                                "org.query" "input.syntax-error"))
           (diagnostics (jlist (jget envelope "error" "diagnostics")))
           (diagnostic (first diagnostics)))
      (expect (plusp (length diagnostics)) :to-be t)
      (expect (jget diagnostic "line") :to-be 1)
      (expect (and (integerp (jget diagnostic "col"))
                   (plusp (jget diagnostic "col")))
              :to-be t)
      (expect (and (stringp (jget diagnostic "message"))
                   (plusp (length (jget diagnostic "message"))))
              :to-be t)
      (expect (file-octets ws "query.org") :to-equalp before)))

  (define-org-read-case ("org query validates explicit time and applies DST policy" ws)
    (put ws "query.org" (format nil "* Scheduled~%SCHEDULED: <2026-03-08 Sun 02:00>~%"))
    (let ((before (file-octets ws "query.org")))
      (dolist (arguments
               '(("org" "query" "query.org" "--match" "work/TODO"
                  "--at" "2026-10-19T09:00:30" "--tz" "UTC")
                 ("org" "query" "query.org" "--match" "work/TODO"
                  "--at" "2026-10-19T09:00:00")
                 ("org" "query" "query.org" "--match" "work/TODO"
                  "--at" "2026-10-19T09:00:00" "--tz" "NotAZone/Nowhere")
                 ("org" "query" "query.org" "--match" "work/TODO" "--tz" "UTC")))
        (org-error ws arguments "org.query" "argument.invalid"))
      (labels ((query-arguments (at policy)
             (list "org" "query" "query.org" "--match"
                   "SCHEDULED<=\"<now>\"" "--at" at
                   "--tz" "America/New_York" "--dst-policy" policy))
           (match-count (at policy)
             (jget (org-ok ws (query-arguments at policy) "org.query") "count")))
        (expect (match-count "2026-03-08T02:30:00" "earlier") :to-be 0)
        (expect (match-count "2026-03-08T02:30:00" "compatible") :to-be 1)
        (org-error ws (query-arguments "2026-03-08T02:30:00" "strict")
                   "org.query" "argument.invalid")
        (dolist (policy '("compatible" "earlier" "later"))
          (expect (match-count "2026-11-01T01:30:00" policy) :to-be 1))
        (org-error ws (query-arguments "2026-11-01T01:30:00" "strict")
                   "org.query" "argument.invalid"))
      (expect (file-octets ws "query.org") :to-equalp before)))
  (define-org-read-case ("org export returns HTML, GFM, and ASCII strings" ws)
    (put ws "export.org" +org-export-basic-fixture+)
    (let ((before (file-octets ws "export.org")))
      (dolist (backend '("html" "gfm" "ascii"))
        (let* ((envelope (org-ok ws (list "org" "export" "export.org" "--backend" backend)
                                 "org.export"))
               (content (jget envelope "content")))
          (expect (jget envelope "backend") :to-equal backend)
          (expect (stringp content) :to-be t)
          (expect-org-fragments
           content
           (cond ((string= backend "html") '("A <b>bold</b>."))
                 ((string= backend "gfm") '("A **bold**"))
                 (t '("A *bold*." "bold"))))))
      (expect (file-octets ws "export.org") :to-equalp before)))

  (define-org-read-case ("org export preserves supported document elements in each backend" ws)
    (put ws "coverage.org" +org-export-coverage-fixture+)
    (let ((before (file-octets ws "coverage.org")))
      (dolist (backend '("html" "gfm" "ascii"))
        (let* ((envelope (org-ok ws
                                 (list "org" "export" "coverage.org" "--backend" backend)
                                 "org.export"))
               (content (jget envelope "content")))
          (expect (jget envelope "backend") :to-equal backend)
          (expect (stringp content) :to-be t)
          (expect-org-fragments
           content
           (cond ((string= backend "html")
                  '("<title>Report</title>" "<b>bold</b>" "<i>italic</i>" "<ul" "<li"
                    "<table" "<pre" "https://example.test" "external" "checked" "ordered"
                    "echo hi" "Reference" "Definition."))
                 ((string= backend "gfm")
                  '("# Heading" "**bold**" "italic" "`code`" "| A | B |" "~~~text"
                    "echo hi" "[external](<https://example.test>)" "[X] checked" "1.  ordered"
                    "Reference" "Definition"))
                 (t '("REPORT" "Heading" "*bold*" "checked" "ordered" "A" "B"
                      "1" "2" "echo hi" "https://example.test" "Definition."))))
          (when (string= backend "gfm")
            (expect-org-one-fragment content '("[x] checked" "[X] checked")))))
      (expect (file-octets ws "coverage.org") :to-equalp before)))

  (define-org-read-case ("org export rejects links and Diary syntax without partial content" ws)
    (dolist (fixture +org-export-refusal-fixtures+)
      (put ws "links.org" (cdr fixture))
      (let ((before (file-octets ws "links.org")))
        (dolist (backend '("html" "gfm" "ascii"))
          (let* ((envelope (org-error ws
                                      (list "org" "export" "links.org" "--backend" backend)
                                      "org.export" "input.unsupported-format"))
                 (content-present (nth-value 1 (gethash "content" envelope)))
                 (backend-present (nth-value 1 (gethash "backend" envelope))))
            (expect content-present :to-be nil)
            (expect backend-present :to-be nil))
          (expect (file-octets ws "links.org") :to-equalp before)))))
)

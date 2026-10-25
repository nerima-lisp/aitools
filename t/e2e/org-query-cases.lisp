(in-package #:aitools.e2e.test)
(describe "aitools e2e org read results and boundaries"
  (define-org-case ("org query reports sibling indices and nullable metadata" ws)
    (put ws "siblings.org"
         (format nil "~{~A~%~}"
                 (list "* Parent"
                       "** TODO Repeated :work:"
                       "** TODO Repeated :work:"
                       "* Other"
                       "** TODO Repeated :work:")))
    (let* ((before (file-octets ws "siblings.org"))
           (arguments (list "org" "query" "siblings.org" "--match" "work/TODO"
                            "--at" "2026-10-19T09:00:00" "--tz" "UTC"))
           (envelope (org-ok ws arguments "org.query"))
           (matches (jlist (jget envelope "matches"))))
      (expect (jget envelope "count") :to-be 3)
      (expect (mapcar (lambda (match) (jget match "title")) matches)
              :to-equal (list "Repeated" "Repeated" "Repeated"))
      (expect (mapcar (lambda (match) (jget match "line")) matches) :to-equal (list 2 3 5))
      (expect (mapcar (lambda (match) (jget match "level")) matches) :to-equal (list 2 2 2))
      (expect (mapcar (lambda (match)
                        (mapcar (lambda (part) (list (jget part "title") (jget part "index")))
                                (jlist (jget match "heading_path"))))
                      matches)
              :to-equal (list (list (list "Parent" 1) (list "Repeated" 1))
                              (list (list "Parent" 1) (list "Repeated" 2))
                              (list (list "Other" 1) (list "Repeated" 1))))
      (let* ((top-level
               (org-ok ws
                       (list "org" "query" "siblings.org" "--match" "LEVEL=1"
                             "--at" "2026-10-19T09:00:00" "--tz" "UTC")
                       "org.query"))
             (top-matches (jlist (jget top-level "matches")))
             (parent (first top-matches))
             (other (second top-matches)))
        (expect (jget top-level "count") :to-be 2)
        (expect (jget parent "title") :to-equal "Parent")
        (expect (jget other "title") :to-equal "Other")
        (expect (mapcar (lambda (part)
                          (list (jget part "title") (jget part "index")))
                        (jlist (jget other "heading_path")))
                :to-equal (list (list "Other" 1)))
        (multiple-value-bind (todo present) (gethash "todo" parent)
          (expect present :to-be t)
          (expect (null todo) :to-be-truthy))
        (multiple-value-bind (tags present) (gethash "tags" parent)
          (expect present :to-be t)
          (expect (jlist tags) :to-equal (list))))
      (let ((none (org-ok ws
                          (list "org" "query" "siblings.org" "--match" "missing/TODO"
                                "--at" "2026-10-19T09:00:00" "--tz" "UTC")
                          "org.query")))
        (expect (jget none "count") :to-be 0)
        (expect (jlist (jget none "matches")) :to-equal (list)))
      (expect (file-octets ws "siblings.org") :to-equalp before)))
  (define-org-case ("org outline names unnamed source blocks by language" ws)
    (put ws "unnamed.org"
         (format nil
                 "#+BEGIN_SRC lisp~%(+ 1 2)~%#+END_SRC~%#+BEGIN_EXAMPLE~%plain text~%#+END_EXAMPLE~%"))
    (let* ((before (file-octets ws "unnamed.org"))
           (envelope (org-ok ws '("org" "outline" "unnamed.org") "org.outline"))
           (entries (jlist (jget envelope "entries"))))
      (expect (jget envelope "total") :to-be 2)
      (expect (jget envelope "truncated") :to-be nil)
      (expect (org-outline-summary entries)
              :to-equal '(("src-block" "lisp" nil 1)
                          ("example-block" "" nil 4)))
      (expect (file-octets ws "unnamed.org") :to-equalp before)))
  (define-org-case ("org outline rejects NUL and UTF-16/UTF-32 inputs" ws)
    (dolist (fixture
             '(("nul.org" . #(#x2A #x20 #x54 #x69 #x74 #x6C #x65 #x00 #x0A))
               ("utf16be.org" . #(#xFE #xFF #x00 #x2A #x00 #x20 #x00 #x54))
               ("utf32le.org" . #(#xFF #xFE #x00 #x00 #x2A #x00 #x00 #x00))
               ("utf32be.org" . #(#x00 #x00 #xFE #xFF #x00 #x00 #x00 #x2A))))
      (let ((path (car fixture)))
        (put ws path (cdr fixture))
        (let ((before (file-octets ws path))
              (envelope (org-error ws (list "org" "outline" path)
                                   "org.outline" "input.unsupported-format")))
          (expect (nth-value 1 (gethash "content" envelope)) :to-be nil)
          (expect (file-octets ws path) :to-equalp before))))))

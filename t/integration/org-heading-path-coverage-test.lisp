(in-package #:aitools.edit.test)

(defparameter +hpc-lines+
  '("#+TODO: TODO | DONE"
    "Intro text"
    "* Parent"
    "body"
    "** Child"
    "** TODO Child :tag:"
    "** Other"
    "* Tres :x:"
    "** Child"
    "* Parent"
    "** Child"
    "*** Deep")
  "Fixture lines. Line numbers in the cases below are one-based indices into it.")

(defparameter +hpc-reason-json+ "it is not valid JSON")
(defparameter +hpc-reason-array+ "it must be a non-empty JSON array")
(defparameter +hpc-reason-segment+
  "each element must be an object with exactly a string title and a positive integer index")

(defun %hpc-source ()
  (format nil "~{~A~%~}" +hpc-lines+))

(defun %hpc-line-begin (line-number)
  "The character offset at which the one-based LINE-NUMBER of the fixture starts."
  (loop for line in +hpc-lines+
        repeat (1- line-number)
        sum (1+ (length line))))

(defun %hpc-engine ()
  (aitools.org.domain:org-document-engine-value
   (%org-coverage-document (%hpc-source))))

(defun %hpc-json (&rest segments)
  "A heading_path JSON array text from alternating title and index values."
  (format nil "[~{~A~^,~}]"
          (loop for (title index) on segments by #'cddr
                collect (format nil "{\"title\":\"~A\",\"index\":~A}" title index))))

(defun %hpc-title (node)
  (org-kit:node-property node :raw-value ""))

(defun %hpc-entries (json)
  (multiple-value-bind (entries reason)
      (aitools.org.domain:parse-org-heading-path json)
    (expect reason :to-be nil)
    entries))

(defun %hpc-resolve (engine json)
  "The values of %HEADING-AT-PATH for the JSON heading path."
  (multiple-value-list
   (aitools.org.infrastructure::%heading-at-path engine (%hpc-entries json))))

(defun %hpc-reason (json)
  (multiple-value-bind (entries reason)
      (aitools.org.domain:parse-org-heading-path json)
    (expect entries :to-be nil)
    reason))

(defparameter +hpc-resolution-cases+
  (list (list "first top-level duplicate" (%hpc-json "Parent" 1) 3)
        (list "second top-level duplicate" (%hpc-json "Parent" 2) 10)
        (list "tags are not part of the title" (%hpc-json "Tres" 1) 8)
        (list "first duplicate child" (%hpc-json "Parent" 1 "Child" 1) 5)
        (list "second duplicate child skips the TODO keyword and tag"
              (%hpc-json "Parent" 1 "Child" 2) 6)
        (list "unique child" (%hpc-json "Parent" 1 "Other" 1) 7)
        (list "child of another parent" (%hpc-json "Tres" 1 "Child" 1) 9)
        (list "child under the second duplicate parent" (%hpc-json "Parent" 2 "Child" 1) 11)
        (list "third level" (%hpc-json "Parent" 2 "Child" 1 "Deep" 1) 12))
  "ID, heading path, and the fixture line of the heading it names.")

(defparameter +hpc-miss-cases+
  (list (list "absent title" (%hpc-json "Missing" 1)
              "no heading titled \"Missing\" at this level of --heading-path")
        (list "title only exists one level deeper" (%hpc-json "Child" 1)
              "no heading titled \"Child\" at this level of --heading-path")
        (list "title only exists one level higher" (%hpc-json "Parent" 1 "Tres" 1)
              "no heading titled \"Tres\" at this level of --heading-path")
        (list "title is not a direct child" (%hpc-json "Parent" 1 "Deep" 1)
              "no heading titled \"Deep\" at this level of --heading-path")
        (list "title match is case sensitive" (%hpc-json "parent" 1)
              "no heading titled \"parent\" at this level of --heading-path")
        (list "index past plural duplicates" (%hpc-json "Parent" 3)
              "--heading-path index 3 is out of range: 2 headings titled \"Parent\" at this level")
        (list "index past a single heading" (%hpc-json "Tres" 2)
              "--heading-path index 2 is out of range: 1 heading titled \"Tres\" at this level")
        (list "index past duplicate children" (%hpc-json "Parent" 1 "Child" 3)
              "--heading-path index 3 is out of range: 2 headings titled \"Child\" at this level"))
  "ID, heading path, and the failure reason.")

(describe "Org --heading-path parsing"
  (it "parses a JSON array of title and index objects in order"
    (let ((entries (%hpc-entries
                    (format nil " [ {\"index\" : 2, \"title\":\"A \\\"q\\\" \\u00e9\\\\\"} ,~
                                   {\"title\":\"B\",\"index\":10} ] "))))
      (expect (length entries) :to-be 2)
      (expect (mapcar #'aitools.org.domain:org-heading-path-entry-title entries)
              :to-equal (list (format nil "A \"q\" ~C\\" (code-char #xe9)) "B"))
      (expect (mapcar #'aitools.org.domain:org-heading-path-entry-index entries)
              :to-equal '(2 10)))
    (let ((entries (%hpc-entries (%hpc-json "Only" 1))))
      (expect (length entries) :to-be 1)
      (expect (aitools.org.domain:org-heading-path-entry-title (first entries))
              :to-equal "Only")
      (expect (aitools.org.domain:org-heading-path-entry-index (first entries))
              :to-be 1)))
  (it "rejects text that is not JSON within the decode budget"
    (dolist (case (list (list "empty text" "")
                        (list "truncated object" "[{")
                        (list "bare word" "not json")
                        (list "duplicate key"
                              "[{\"title\":\"A\",\"title\":\"x\",\"index\":1}]")
                        (list "nesting beyond the depth budget" "[[[[[[[[[1]]]]]]]]]")
                        (list "input beyond the length budget"
                              (concatenate 'string "["
                                           (make-string 70000 :initial-element #\1)
                                           "]"))))
      (expect (%hpc-reason (second case)) :to-equal +hpc-reason-json+)))
  (it "rejects JSON that is not a non-empty array"
    (dolist (case (list (list "empty array" "[]")
                        (list "object" "{\"title\":\"A\",\"index\":1}")
                        (list "string" "\"A\"")
                        (list "number" "3")
                        (list "null" "null")
                        (list "true" "true")))
      (expect (%hpc-reason (second case)) :to-equal +hpc-reason-array+)))
  (it "rejects array elements that are not exact title and index objects"
    (dolist (case (list (list "non-object element" "[1]")
                        (list "string element" "[\"A\"]")
                        (list "nested array element" "[[]]")
                        (list "extra key" "[{\"title\":\"A\",\"index\":1,\"x\":true}]")
                        (list "missing index" "[{\"title\":\"A\"}]")
                        (list "missing title" "[{\"index\":1}]")
                        (list "two unrelated keys" "[{\"name\":\"A\",\"index\":1}]")
                        (list "numeric title" "[{\"title\":1,\"index\":1}]")
                        (list "null title" "[{\"title\":null,\"index\":1}]")
                        (list "fractional index" "[{\"title\":\"A\",\"index\":1.5}]")
                        (list "float-formatted index" "[{\"title\":\"A\",\"index\":1.0}]")
                        (list "exponent index" "[{\"title\":\"A\",\"index\":1e0}]")
                        (list "string index" "[{\"title\":\"A\",\"index\":\"1\"}]")
                        (list "null index" "[{\"title\":\"A\",\"index\":null}]")
                        (list "zero index" "[{\"title\":\"A\",\"index\":0}]")
                        (list "negative index" "[{\"title\":\"A\",\"index\":-1}]")
                        (list "bad element after a good one"
                              "[{\"title\":\"A\",\"index\":1},{\"title\":\"B\"}]")))
      (expect (%hpc-reason (second case)) :to-equal +hpc-reason-segment+))))

(describe "Org --heading-path resolution"
  (it "lists only the headlines directly below a node, looking through sections"
    (let* ((engine (%hpc-engine))
           (root (org-kit:document-root engine))
           (top (aitools.org.infrastructure::%child-headlines root)))
      (expect (mapcar #'org-kit:node-type (org-kit:node-children root))
              :to-equal '(:section :headline :headline :headline))
      (expect (mapcar #'org-kit:node-type top) :to-equal '(:headline :headline :headline))
      (expect (mapcar #'%hpc-title top) :to-equal '("Parent" "Tres" "Parent"))
      (expect (mapcar #'org-kit:node-begin top)
              :to-equal (mapcar #'%hpc-line-begin '(3 8 10)))
      (let ((children (aitools.org.infrastructure::%child-headlines (first top))))
        (expect (mapcar #'org-kit:node-type (org-kit:node-children (first top)))
                :to-equal '(:section :headline :headline :headline))
        (expect (mapcar #'%hpc-title children) :to-equal '("Child" "Child" "Other"))
        (expect (mapcar #'org-kit:node-begin children)
                :to-equal (mapcar #'%hpc-line-begin '(5 6 7))))
      (expect (aitools.org.infrastructure::%child-headlines (second (org-kit:node-children (first top))))
              :to-be nil)
      (expect (aitools.org.infrastructure::%child-headlines (second top))
              :to-equal (list (first (org-kit:node-children (second top)))))))
  (it "resolves each heading path to the headline on its fixture line"
    (let ((engine (%hpc-engine)))
      (dolist (case +hpc-resolution-cases+)
        (destructuring-bind (id json line) case
          (let ((values (%hpc-resolve engine json)))
            (expect (second values) :to-be nil)
            (expect (list id (org-kit:node-begin (first values)))
                    :to-equal (list id (%hpc-line-begin line))))))))
  (it "returns the document root for an empty entry list"
    (let ((engine (%hpc-engine)))
      (expect (aitools.org.infrastructure::%heading-at-path engine nil)
              :to-be (org-kit:document-root engine))))
  (it "resolves every heading_path that org-kit reports back to the same headline"
    (let ((engine (%hpc-engine))
          (seen 0))
      (org-kit:map-nodes
       (lambda (node)
         (when (eq (org-kit:node-type node) :headline)
           (incf seen)
           (let ((json (apply #'%hpc-json
                              (loop for segment in (org-kit:node-heading-path node)
                                    append (list (getf segment :title) (getf segment :index))))))
             (expect (first (%hpc-resolve engine json)) :to-be node))))
       (org-kit:document-root engine))
      (expect seen :to-be 9)))
  (it "reports a reason when no headline matches the path"
    (let ((engine (%hpc-engine)))
      (dolist (case +hpc-miss-cases+)
        (destructuring-bind (id json reason) case
          (let ((values (%hpc-resolve engine json)))
            (expect (list id (first values) (second values))
                    :to-equal (list id nil reason))))))))

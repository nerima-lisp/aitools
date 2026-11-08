;;;; Table-cell behavior and edge cases.
(in-package #:aitools.edit.test)

(describe "aitools.edit.domain table cells"
  (it "rewrites only the addressed cell, quoting when needed"
    (multiple-value-bind (text previous)
        (table-set-cell (format nil "name,note~%a,\"x, y\"~%b,z~%") #\, 2 "note" "has \"quotes\", commas")
      (expect text :to-equal (format nil "name,note~%a,\"x, y\"~%b,\"has \"\"quotes\"\", commas\"~%"))
      (expect previous :to-equal "z"))
    (expect (table-set-cell (format nil "k~Cv~%1~C2~%" #\Tab #\Tab) #\Tab 1 "v" "3")
            :to-equal (format nil "k~Cv~%1~C3~%" #\Tab #\Tab)))

  (it "refuses a missing row or column"
    (expect (refusal-code (lambda () (table-set-cell (format nil "a~%1~%") #\, 2 "a" "x"))) :to-equal "input.not-found")
    (expect (refusal-code (lambda () (table-set-cell (format nil "a~%1~%") #\, 1 "b" "x"))) :to-equal "input.not-found")))

(describe "aitools.edit.domain table cell edge cases"
  ;; The tables are FORMAT templates, with | standing for a carriage return.
  (it-each (("a quoted header name and escaped quotes" "\"na\"\"me\",v~%x,1~%" 1 "na\"me" "y" "\"na\"\"me\",v~%y,1~%" "x")
            ("a column given by 1-based index" "a,b~%1,2~%" 1 "2" "9" "a,b~%1,9~%" "2")
            ("CRLF records" "a,b|~%1,2|~%3,4|~%" 2 "a" "7" "a,b|~%1,2|~%7,4|~%" "3")
            ("a file without a final newline" "a~%1" 1 "a" "2" "a~%2" "1")
            ("an empty field" "a,b~%,2~%" 1 "a" "z" "a,b~%z,2~%" ""))
      "sets a cell with ~A"
      (name text row column value expected previous)
    (declare (ignore name))
    (flet ((expand (template) (substitute #\Return #\| (format nil template))))
      (multiple-value-bind (result old) (table-set-cell (expand text) #\, row column value)
        (expect result :to-equal (expand expected))
        (expect old :to-equal previous))))

  (it "replaces a malformed or unterminated quoted field whole, leaving the rest"
    (expect (table-set-cell (format nil "a,b~%\"q\"x,2~%") #\, 1 "a" "z") :to-equal (format nil "a,b~%z,2~%"))
    (expect (table-set-cell (format nil "a~%\"open") #\, 1 "a" "z") :to-equal (format nil "a~%z")))

  (it-each (("row 0" 0 "a") ("a column index past the header" 1 "3") ("a column index of 0" 1 "0")
            ("a short row" 2 "b"))
      "refuses ~A"
      (name row column)
    (declare (ignore name))
    (expect (refusal-code (lambda () (table-set-cell (format nil "a,b~%1,2~%3~%") #\, row column "x")))
            :to-equal "input.not-found"))

  (it "chooses the delimiter by extension, case-insensitively"
    (expect (table-delimiter-for-path "x.CSV") :to-be #\,)
    (expect (table-delimiter-for-path "dir.v/x.tsv") :to-be #\Tab)
    (expect (table-delimiter-for-path "x.txt") :to-be nil)
    (expect (table-delimiter-for-path "noext") :to-be nil)))

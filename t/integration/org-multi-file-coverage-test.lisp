(in-package #:aitools.edit.test)

(defun %multi-agenda-arguments (&rest arguments)
  (append arguments
          (list :from "2026-10-19" :through "2026-10-25" :future-repeats "all"
                :at "2026-10-19T09:00:00" :tz "UTC" :dst-policy "compatible"
                :root *root*)))

(defun %multi-query-arguments (&rest arguments)
  (append arguments
          (list :match "LEVEL=1" :at "2026-10-19T09:00:00" :tz "UTC"
                :dst-policy "compatible" :root *root*)))

(defun %multi-agenda (boundaries &rest arguments)
  (apply #'org-flow-ok #'aitools.org.application:org-agenda/k boundaries
         (apply #'%multi-agenda-arguments arguments)))

(defun %multi-query (boundaries &rest arguments)
  (apply #'org-flow-ok #'aitools.org.application:org-query/k boundaries
         (apply #'%multi-query-arguments arguments)))

(defun %multi-error (flow arguments boundaries)
  "(VALUES CODE MESSAGE) of the error FLOW reports for ARGUMENTS."
  (multiple-value-bind (kind fields) (apply #'run-org-flow flow boundaries arguments)
    (expect kind :to-be :error)
    (values (getf fields :code) (getf fields :message))))

(defun %multi-agenda-error (boundaries &rest arguments)
  (%multi-error #'aitools.org.application:org-agenda/k
                (apply #'%multi-agenda-arguments arguments) boundaries))

(defun %multi-query-error (boundaries &rest arguments)
  (%multi-error #'aitools.org.application:org-query/k
                (apply #'%multi-query-arguments arguments) boundaries))

(defun %multi-entry-paths (fields)
  (map 'list (lambda (entry) (json-field entry "path")) (field fields "entries")))

(defun %multi-match-paths (fields)
  (map 'list (lambda (entry) (json-field entry "path")) (field fields "matches")))

(defun %multi-scheduled (title)
  (format nil "#+TODO: TODO | DONE~%* TODO ~A~%SCHEDULED: <2026-10-20 Tue>~%" title))

(defun %multi-boundaries-with (&rest capabilities)
  (let ((boundaries (make-org-write-flow-boundaries)))
    (loop for (key value) on capabilities by #'cddr
          do (setf boundaries
                   (cl-boundary-kit:boundary-context-with boundaries key value)))
    boundaries))

(defun %multi-failure (message)
  (aitools.org.domain:make-org-failure :kind :argument :message message))

(describe "Org agenda and query reads over several files and directories"
  (it "merges the agenda of explicitly named files in path order"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "b.org" (%multi-scheduled "Beta"))
         (put "a.org" (%multi-scheduled "Alpha"))
         (let ((fields (%multi-agenda boundaries :paths '("b.org" "a.org"))))
           (expect (field fields "count") :to-be 2)
           (expect (%multi-entry-paths fields) :to-equal '("a.org" "b.org"))
           (expect (map 'list (lambda (entry) (json-field entry "title"))
                        (field fields "entries"))
                   :to-equal '("Alpha" "Beta")))))))
  (it "keeps the single-path argument working next to the paths argument"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "one.org" (%multi-scheduled "One"))
         (put "two.org" (%multi-scheduled "Two"))
         (expect (%multi-entry-paths (%multi-agenda boundaries :path "one.org"))
                 :to-equal '("one.org"))
         (expect (%multi-entry-paths
                  (%multi-agenda boundaries :path "one.org" :paths '("two.org")))
                 :to-equal '("two.org"))))))
  (it "expands a directory to its Org files recursively and skips other files"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "dir/x.org" (%multi-scheduled "X"))
         (put "dir/sub/y.org" (%multi-scheduled "Y"))
         (put "dir/notes.txt" (%multi-scheduled "Text"))
         (put "other/z.org" (%multi-scheduled "Z"))
         (let ((fields (%multi-agenda boundaries :paths '("dir"))))
           (expect (%multi-entry-paths fields)
                   :to-equal '("dir/sub/y.org" "dir/x.org"))
           (expect (field fields "count") :to-be 2))))))
  (it "reads a file once when it is named and also lies in a named directory"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "dir/x.org" (%multi-scheduled "X"))
         (put "solo.org" (%multi-scheduled "Solo"))
         (let ((fields (%multi-agenda boundaries
                                      :paths '("dir" "dir/x.org" "solo.org"))))
           (expect (%multi-entry-paths fields)
                   :to-equal '("dir/x.org" "solo.org")))))))
  (it "answers a query over a directory and a file with the matching path on each match"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "notes/one.org" (format nil "* Parent~%** Child~%"))
         (put "notes/two.org" (format nil "* Second~%"))
         (put "extra.org" (format nil "* Extra~%"))
         (let ((fields (%multi-query boundaries :paths '("extra.org" "notes"))))
           (expect (field fields "count") :to-be 3)
           (expect (%multi-match-paths fields)
                   :to-equal '("extra.org" "notes/one.org" "notes/two.org"))
           (expect (map 'list (lambda (match) (json-field match "title"))
                        (field fields "matches"))
                   :to-equal '("Extra" "Parent" "Second")))))))
  (it "ignores encrypted files named explicitly and found in directories"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "plain.org" (%multi-scheduled "Plain"))
         (put "vault.org.gpg" #(0 1 2 3))
         (put "dir/secret.org" (%multi-scheduled "Secret"))
         (let ((fields (%multi-agenda boundaries
                                      :paths '("vault.org.gpg" "plain.org"))))
           (expect (%multi-entry-paths fields) :to-equal '("plain.org")))
         (let ((fields (%multi-agenda boundaries :paths '("vault.org.gpg"))))
           (expect (field fields "count") :to-be 0))
         (sb-posix:symlink (disk "dir/secret.org") (disk "dir/link.org.gpg"))
         (let ((fields (%multi-agenda boundaries :paths '("dir/link.org.gpg"))))
           (expect (field fields "count") :to-be 0))
         (expect (aitools.org.application::%org-encrypted-path-p "a.org.gpg")
                 :to-be-truthy)
         (expect (aitools.org.application::%org-encrypted-path-p "A.ORG.GPG")
                 :to-be-truthy)
         (expect (aitools.org.application::%org-encrypted-path-p "a.org")
                 :to-be nil)
         (expect (aitools.org.application::%org-encrypted-path-p "a") :to-be nil)))))
  (it "requires every path to be a non-empty string"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "a.org" (%multi-scheduled "Alpha"))
         (expect (multiple-value-list (%multi-agenda-error boundaries))
                 :to-equal '("argument.invalid" "an Org file path is required"))
         (expect (%multi-agenda-error boundaries :paths '("a.org" ""))
                 :to-equal "argument.invalid")
         (expect (%multi-agenda-error boundaries :paths '("a.org" 7))
                 :to-equal "argument.invalid")
         (expect (multiple-value-list
                  (%multi-query-error boundaries :paths '("")))
                 :to-equal '("argument.invalid" "an Org file path is required")))))))

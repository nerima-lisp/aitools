(in-package #:aitools.edit.test)
(defun %org-checkbox-source (&rest lines)
  (format nil "* TODO Target~%~{~A~%~}" lines))
(defun %org-checkbox-toggle (source item &key (match "/TODO"))
  (let* ((document (%org-coverage-document source))
         (engine (aitools.org.domain:org-document-engine-value document))
         (options (list :match match :item item
                        :at "2026-10-19T09:00:00" :tz "UTC"
                        :dst-policy "compatible")))
    (multiple-value-bind (updated fields failure)
        (aitools.org.infrastructure::%org-checkbox-edit engine options)
      (values (and updated (org-kit:document-text updated)) fields failure
              (org-kit:document-text engine)))))
(defun %org-checkbox-failure (source item &key (match "/TODO"))
  (multiple-value-bind (text fields failure)
      (%org-checkbox-toggle source item :match match)
    (list text fields
          (and failure (aitools.org.domain:org-failure-kind failure))
          (and failure (aitools.org.domain:org-failure-message failure)))))
(defmacro %with-org-checkbox-adapter-error (condition &body body)
  `(cl-weave:with-mocked-functions
       (((symbol-function 'org-kit:set-checkbox-state)
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error ,condition))))
     ,@body))
(defparameter *org-checkbox-toggle-cases*
  (list
   (list "unchecked leaf becomes checked"
         (%org-checkbox-source "  - [ ] Item") 1
         (%org-checkbox-source "  - [X] Item"))
   (list "checked leaf becomes unchecked"
         (%org-checkbox-source "  - [X] Item") 1
         (%org-checkbox-source "  - [ ] Item"))
   (list "partial leaf without children becomes checked"
         (%org-checkbox-source "  - [-] Item") 1
         (%org-checkbox-source "  - [X] Item"))
   (list "toggling a parent restores the state its unchecked children imply"
         (%org-checkbox-source "  - [ ] P" "    - [ ] C") 1
         (%org-checkbox-source "  - [ ] P" "    - [ ] C"))
   (list "toggling a parent restores the state its checked children imply"
         (%org-checkbox-source "  - [X] P" "    - [X] C") 1
         (%org-checkbox-source "  - [X] P" "    - [X] C"))
   (list "toggling a parent restores partial for mixed children"
         (%org-checkbox-source "  - [-] P" "    - [X] C1" "    - [ ] C2") 1
         (%org-checkbox-source "  - [-] P" "    - [X] C1" "    - [ ] C2"))
   (list "toggling a parent adopts partial from a partial child"
         (%org-checkbox-source "  - [ ] P" "    - [-] C") 1
         (%org-checkbox-source "  - [-] P" "    - [-] C"))
   (list "checking the only child checks the parent"
         (%org-checkbox-source "  - [ ] P" "    - [ ] C") 2
         (%org-checkbox-source "  - [X] P" "    - [X] C"))
   (list "unchecking the only child unchecks the parent"
         (%org-checkbox-source "  - [X] P" "    - [X] C") 2
         (%org-checkbox-source "  - [ ] P" "    - [ ] C"))
   (list "checking one of two unchecked children makes the parent partial"
         (%org-checkbox-source "  - [ ] P" "    - [ ] C1" "    - [ ] C2") 2
         (%org-checkbox-source "  - [-] P" "    - [X] C1" "    - [ ] C2"))
   (list "unchecking one of two checked children makes the parent partial"
         (%org-checkbox-source "  - [X] P" "    - [X] C1" "    - [X] C2") 3
         (%org-checkbox-source "  - [-] P" "    - [X] C1" "    - [ ] C2"))
   (list "checking the last unchecked child checks a partial parent"
         (%org-checkbox-source "  - [-] P" "    - [X] C1" "    - [ ] C2") 3
         (%org-checkbox-source "  - [X] P" "    - [X] C1" "    - [X] C2"))
   (list "a parent that is already partial is left alone"
         (%org-checkbox-source "  - [-] P" "    - [X] C1" "    - [ ] C2"
                               "    - [ ] C3") 3
         (%org-checkbox-source "  - [-] P" "    - [X] C1" "    - [X] C2"
                               "    - [ ] C3"))
   (list "a partial sibling keeps the parent partial when a child is checked"
         (%org-checkbox-source "  - [ ] P" "    - [-] C1" "    - [ ] C2") 3
         (%org-checkbox-source "  - [-] P" "    - [-] C1" "    - [X] C2"))
   (list "a partial sibling keeps a checked parent partial when a child is unchecked"
         (%org-checkbox-source "  - [X] P" "    - [-] C1" "    - [X] C2") 3
         (%org-checkbox-source "  - [-] P" "    - [-] C1" "    - [ ] C2"))
   (list "checking the deepest item checks every ancestor"
         (%org-checkbox-source "  - [ ] A" "    - [ ] B" "      - [ ] C"
                               "        - [ ] D") 4
         (%org-checkbox-source "  - [X] A" "    - [X] B" "      - [X] C"
                               "        - [X] D"))
   (list "unchecking the deepest item unchecks every ancestor"
         (%org-checkbox-source "  - [X] A" "    - [X] B" "      - [X] C"
                               "        - [X] D") 4
         (%org-checkbox-source "  - [ ] A" "    - [ ] B" "      - [ ] C"
                               "        - [ ] D"))
   (list "a partial branch makes every ancestor partial"
         (%org-checkbox-source "  - [ ] A" "    - [ ] B" "      - [ ] C1"
                               "      - [ ] C2" "    - [ ] B2") 3
         (%org-checkbox-source "  - [-] A" "    - [-] B" "      - [X] C1"
                               "      - [ ] C2" "    - [ ] B2"))
   (list "only ancestors of the toggled item are recomputed"
         (%org-checkbox-source "  - [ ] P1" "    - [ ] C1" "  - [ ] P2"
                               "    - [ ] C2") 2
         (%org-checkbox-source "  - [X] P1" "    - [X] C1" "  - [ ] P2"
                               "    - [ ] C2"))
   (list "items without a checkbox are not numbered"
         (%org-checkbox-source "  - plain" "  - [ ] a" "  - [X] b") 2
         (%org-checkbox-source "  - plain" "  - [ ] a" "  - [ ] b"))
   (list "a parent item without a checkbox keeps no checkbox"
         (%org-checkbox-source "  - Group" "    - [ ] a" "    - [ ] b") 1
         (%org-checkbox-source "  - Group" "    - [X] a" "    - [ ] b"))
   (list "only direct checkbox children determine a checkbox parent"
         (%org-checkbox-source "  - [ ] Top" "    - Group" "      - [ ] a") 2
         (%org-checkbox-source "  - [ ] Top" "    - Group" "      - [X] a"))
   (list "items are numbered across lists and sub-headings are excluded"
         (%org-checkbox-source "  - [ ] a" "" "  Some text." ""
                               "  - [ ] b" "** Sub" "  - [ ] hidden") 2
         (%org-checkbox-source "  - [ ] a" "" "  Some text." ""
                               "  - [X] b" "** Sub" "  - [ ] hidden"))
   (list "statistics cookies follow the new state and other headings stay intact"
         (format nil "* Other [0/1]~%  - [ ] z~%* TODO Target [0/2]~%  - [ ] P [0/1]~%    - [ ] c~%  - [ ] d~%")
         2
         (format nil "* Other [0/1]~%  - [ ] z~%* TODO Target [1/2]~%  - [X] P [1/1]~%    - [X] c~%  - [ ] d~%"))
   (list "a nested heading is selected by its keyword"
         (format nil "* Parent~%** TODO Target [0%]~%  - [ ] a~%  - [ ] b~%")
         1
         (format nil "* Parent~%** TODO Target [50%]~%  - [X] a~%  - [ ] b~%"))))
(describe "Org checkbox edits"
  (it "toggles checkbox state and recomputes ancestors for every combination"
    (dolist (case *org-checkbox-toggle-cases*)
      (destructuring-bind (label source item expected) case
        (multiple-value-bind (text fields failure original)
            (%org-checkbox-toggle source item)
          (expect (list label failure text) :to-equal (list label nil expected))
          (expect (list label fields) :to-equal (list label (list (cons "item" item))))
          (expect (list label original) :to-equal (list label source))))))
  (it "returns the engine updated for a toggle without touching the input"
    (let* ((source (%org-checkbox-source "  - [ ] Item"))
           (document (%org-coverage-document source))
           (engine (aitools.org.domain:org-document-engine-value document)))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-checkbox-edit
           engine (list :match "/TODO" :item 1 :at "2026-10-19T09:00:00"
                        :tz "UTC" :dst-policy "compatible"))
        (expect failure :to-be nil)
        (expect fields :to-equal (list (cons "item" 1)))
        (expect (eq updated engine) :to-be nil)
        (expect (org-kit:document-text updated)
                :to-equal (%org-checkbox-source "  - [X] Item"))
        (expect (org-kit:document-text engine) :to-equal source))))
  (it "rejects item numbers that do not name a checkbox"
    (let ((out-of-range (list nil nil :selection-no-match
                              "checkbox number is out of range")))
      (expect (%org-checkbox-failure (%org-checkbox-source "  - [ ] Item") 2)
              :to-equal out-of-range)
      (expect (%org-checkbox-failure (%org-checkbox-source "  - plain item") 1)
              :to-equal out-of-range)
      (expect (%org-checkbox-failure (%org-checkbox-source "  body") 1)
              :to-equal out-of-range)
      (expect (%org-checkbox-failure
               (%org-checkbox-source "  - [ ] a" "** Sub" "  - [ ] hidden") 2)
              :to-equal out-of-range)))
  (it "returns selection failures before editing checkboxes"
    (let ((source (%org-checkbox-source "  - [ ] Item")))
      (expect (%org-checkbox-failure source 1 :match "/ABSENT")
              :to-equal (list nil nil :selection-no-match
                              "no heading matches --match"))
      (expect (%org-checkbox-failure
               (format nil "* TODO One~%  - [ ] a~%* TODO Two~%  - [ ] b~%") 1)
              :to-equal (list nil nil :selection-ambiguous
                              "more than one heading matches --match"))
      (expect (%org-checkbox-failure source 1 :match nil)
              :to-equal (list nil nil :argument
                              "exactly one of --match and --heading-path is required"))))
  (it "maps checkbox adapter failures"
    (let ((source (%org-checkbox-source "  - [ ] Item")))
      (dolist (case
                (list
                 (list (make-condition 'org-kit:org-edit-error
                                       :message "edit rejected"
                                       :reason :invalid-value)
                       :argument)
                 (list (make-condition 'org-kit:org-edit-error
                                       :message "edit missing"
                                       :reason :target-not-found)
                       :not-found)
                 (list (make-condition 'org-kit:org-kit-error
                                       :message "checkbox failed")
                       :internal)))
        (%with-org-checkbox-adapter-error (first case)
          (destructuring-bind (updated fields kind message)
              (%org-checkbox-failure source 1)
            (expect updated :to-be nil)
            (expect fields :to-be nil)
            (expect kind :to-be (second case))
            (expect (stringp message) :to-be t))))
      (%with-org-checkbox-adapter-error
          (make-condition 'org-kit:org-kit-error :message "checkbox failed")
        (expect (fourth (%org-checkbox-failure source 1))
                :to-equal "checkbox edit failed"))))
  (it "reports an exceeded output limit as too large"
    (let ((aitools.org.infrastructure::*org-kit-limits*
            (org-kit:make-limits :max-output-length 1)))
      (expect (%org-checkbox-failure (%org-checkbox-source "  - [ ] Item") 1)
              :to-equal (list nil nil :too-large
                              "checkbox edit exceeded a resource limit")))))

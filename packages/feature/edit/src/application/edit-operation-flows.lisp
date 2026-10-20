(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
  (%apply-edit/k context document path edit &key on-error on-applied)
  "One edit on DOCUMENT: ON-APPLIED (document strategy)."
  (declare (type function on-error on-applied))
  (let ((new (getf edit :new)))
    (if (getf edit :old)
        (aitools.kernel.domain:flow-call
          (apply-old-edit document (getf edit :old) new)
          (:on-edited (edited strategy line)
            (declare (ignore line))
            (aitools.kernel.domain:flow-continue
              :on-applied
              edited
              strategy))
          (:on-ambiguous (matches)
            (aitools.kernel.domain:flow-continue
              :on-error
              "selection.ambiguous"
              (format
                nil
                (concatenate 'string "--old matches ~D places in ~A; " "add surrounding lines to make it unique")
                (length matches)
                path)
              :candidates
              (lines-candidates matches)))
          (:on-no-match (candidates)
            (aitools.kernel.domain:flow-continue
              :on-error
              "selection.no-match"
              (format
                nil
                "--old matches nothing in ~A"
                path)
              :candidates
              (lines-candidates candidates))))
        (let ((selector (getf edit :selector)))
          (aitools.kernel.domain:flow-call
            (resolve-lines/k document selector path)
            (:on-error (code message &key candidates diagnostics conflicts repairs path)
              (flow-continue :on-error
                             code
                             message
                             :candidates
                             candidates
                             :diagnostics
                             diagnostics
                             :conflicts
                             conflicts
                             :repairs
                             repairs
                             :path
                             path))
            (:on-ranges (ranges)
              (flet ((apply-ranges ()
                       (aitools.kernel.domain:flow-continue
                         :on-applied
                         (replace-ranges document ranges (content-lines new))
                         nil)))
                (if (eq (aitools.kernel.domain:selector-kind selector) :match)
                    (let ((expected (or (getf edit :expect-count) (write-context-expect-count context))))
                      (unless (getf edit :expect-count)
                        (setf (write-context-selected-count context) (length ranges)))
                      (if (and expected (/= expected (length ranges)))
                          (aitools.kernel.domain:flow-continue
                            :on-error
                            "selection.count-mismatch"
                            (format
                              nil
                              "--match selects ~D line~:P, expected ~D"
                              (length ranges)
                              expected)
                            :diagnostics
                            (list (json-object "expected" expected "actual" (length ranges))))
                          (apply-ranges)))
                    (apply-ranges)))))))))

(aitools.kernel.domain:define-flow
  (%stdin-edits/k value &key on-error on-edits)
  "The edits of an `edit --stdin` VALUE: {old,new} or {edits:[...]}, each
edit a plist (:old text :new text) or (:selector selector :new text
:expect-count n). Only content-based selectors are accepted in edits[]."
  (declare (type function on-error on-edits))
  (labels ((text (object key)
             (let ((member (%json-member object key)))
               (and member (cdr member))))
           (invalid (control &rest arguments)
             (values nil (apply #'format nil control arguments)))
           (parse-item (item index)
             (cond
               ((not (json-obj-p item)) (invalid "edits[~D] is not an object" index))
               ((some
                 (lambda (key)
                   (%json-member item key))
                 '("range" "symbol"))
                (invalid
                 "edits[~D] uses ~A: edits[] accept only old, between and match, whose matches survive earlier edits"
                 index
                 (find-if
                  (lambda (key)
                    (%json-member item key))
                  '("range" "symbol"))))
               (t
                (let* ((new (text item "new"))
                       (given
                        (remove-if-not
                         (lambda (key)
                           (%json-member item key))
                         '("old" "between" "match"))))
                  (cond
                    ((not (stringp new)) (invalid "edits[~D] needs a string \"new\"" index))
                    ((/= (length given) 1) (invalid "edits[~D] needs exactly one of old, between, match" index))
                    ((string= (first given) "old")
                     (let ((old (text item "old")))
                       (if (and (stringp old) (plusp (length old)))
                           (values (list :old old :new new) nil)
                           (invalid "edits[~D].old must be a non-empty string" index))))
                    ((string= (first given) "between")
                     (let ((pair (text item "between")))
                       (if (and (vectorp pair)
                                (= (length pair) 2)
                                (every #'stringp pair)
                                (every #'plusp (map 'list #'length pair)))
                           (values
                            (list :selector
                                  (aitools.kernel.domain:make-between-selector (aref pair 0)
                                                                               (aref pair 1)
                                                                               :exclusive
                                                                               (eq (text item "exclusive") t))
                                  :new
                                  new)
                            nil)
                           (invalid "edits[~D].between must be [start-re, end-re]" index))))
                    (t
                     (let ((match (text item "match"))
                           (count (text item "expect_count")))
                       (cond
                         ((not (and (stringp match) (plusp (length match))))
                          (invalid "edits[~D].match must be a non-empty regex" index))
                         ((not (and (json-num-p count) (parse-count (json-num-text count))))
                          (invalid "edits[~D] uses match and needs \"expect_count\"" index))
                         (t
                          (values
                           (list :selector
                                 (aitools.kernel.domain:make-match-selector match :invert (eq (text item "invert") t))
                                 :new
                                 new
                                 :expect-count
                                 (parse-count (json-num-text count)))
                           nil)))))))))))
    (if (not (json-obj-p value))
        (aitools.kernel.domain:flow-continue
          :on-error
          "argument.invalid"
          "edit --stdin reads a JSON object")
        (let* ((edits-member (%json-member value "edits"))
               (edits (and edits-member (cdr edits-member)))
               (items
                (if edits-member
                    (and (vectorp edits) (not (stringp edits)) (plusp (length edits)) (coerce edits 'list))
                    (list value))))
          (cond
            ((and edits-member (null items))
             (aitools.kernel.domain:flow-continue
               :on-error
               "argument.invalid"
               "\"edits\" must be a non-empty array"))
            (t
             (let ((parsed '())
                   (failure nil))
               (loop for item in items
                     for index from 0
                     until failure
                     do (multiple-value-bind (edit message) (parse-item item index)
                          (if message
                              (setf failure message)
                              (push edit parsed))))
               (if failure
                   (aitools.kernel.domain:flow-continue
                     :on-error
                     "argument.invalid"
                     failure)
                   (aitools.kernel.domain:flow-continue
                     :on-edits
                     (nreverse parsed))))))))))

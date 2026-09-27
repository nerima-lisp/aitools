;;;; packages/feature/edit/src/application/edit-flows.lisp
;;;;
;;;; `edit` and `insert`.
(in-package #:aitools.edit.application)

(defun replace-ranges (document ranges new-lines)
  "DOCUMENT with each 1-based inclusive range of RANGES (ascending) replaced
by NEW-LINES; an empty range (end = start - 1) inserts before START."
  (let ((result document))
    (dolist (range (reverse ranges) result)
      (setf result (document-replace-lines result (1- (car range)) (cdr range) new-lines)))))

(defun %json-member (object key)
  (and (json-obj-p object) (assoc key (json-obj-members object) :test #'string=)))

(defun %stdin-edits/k (value fail on-edits)
  "The edits of an `edit --stdin` VALUE: {old,new} or {edits:[...]}, each
edit a plist (:old text :new text) or (:selector selector :new text
:expect-count n). Only content-based selectors are accepted in edits[]."
  (flet ((bad (control &rest arguments)
           (return-from %stdin-edits/k (funcall fail "argument.invalid" (apply #'format nil control arguments))))
         (text (object key)
           (let ((member (%json-member object key)))
             (and member (cdr member)))))
    (unless (json-obj-p value) (bad "edit --stdin reads a JSON object"))
    (let ((items (if (%json-member value "edits")
                     (let ((edits (text value "edits")))
                       (unless (and (vectorp edits) (not (stringp edits)) (plusp (length edits)))
                         (bad "\"edits\" must be a non-empty array"))
                       (coerce edits 'list))
                     (list value))))
      (funcall on-edits
               (loop for item in items
                     for index from 0
                     collect (progn
                               (unless (json-obj-p item) (bad "edits[~D] is not an object" index))
                               (dolist (key '("range" "symbol"))
                                 (when (%json-member item key)
                                   (bad "edits[~D] uses ~A: edits[] accept only old, between and match, whose matches survive earlier edits"
                                        index key)))
                               (let ((new (text item "new"))
                                     (given (remove-if-not (lambda (key) (%json-member item key)) '("old" "between" "match"))))
                                 (unless (stringp new) (bad "edits[~D] needs a string \"new\"" index))
                                 (unless (= (length given) 1)
                                   (bad "edits[~D] needs exactly one of old, between, match" index))
                                 (let ((key (first given)))
                                   (cond
                                     ((string= key "old")
                                      (let ((old (text item "old")))
                                        (unless (and (stringp old) (plusp (length old)))
                                          (bad "edits[~D].old must be a non-empty string" index))
                                        (list :old old :new new)))
                                     ((string= key "between")
                                      (let ((pair (text item "between")))
                                        (unless (and (vectorp pair) (= (length pair) 2) (every #'stringp pair)
                                                     (every #'plusp (map 'list #'length pair)))
                                          (bad "edits[~D].between must be [start-re, end-re]" index))
                                        (list :selector (aitools.kernel.domain:make-between-selector
                                                         (aref pair 0) (aref pair 1)
                                                         :exclusive (eq (text item "exclusive") t))
                                              :new new)))
                                     (t
                                      (let ((match (text item "match"))
                                            (count (text item "expect_count")))
                                        (unless (and (stringp match) (plusp (length match)))
                                          (bad "edits[~D].match must be a non-empty regex" index))
                                        (unless (and (json-num-p count) (parse-count (json-num-text count)))
                                          (bad "edits[~D] uses match and needs \"expect_count\"" index))
                                        (list :selector (aitools.kernel.domain:make-match-selector
                                                         match :invert (eq (text item "invert") t))
                                              :new new :expect-count (parse-count (json-num-text count))))))))))))))

(defun %apply-edit/k (context document path edit reject on-applied)
  "One edit on DOCUMENT: ON-APPLIED (document strategy)."
  (declare (type function reject on-applied))
  (let ((new (getf edit :new)))
    (if (getf edit :old)
        (apply-old-edit document (getf edit :old) new
                        :on-edited (lambda (edited strategy line)
                                     (declare (ignore line))
                                     (funcall on-applied edited strategy))
                        :on-ambiguous (lambda (matches)
                                        (funcall reject "selection.ambiguous"
                                                 (format nil "--old matches ~D places in ~A; add surrounding lines to make it unique"
                                                         (length matches) path)
                                                 :candidates (lines-candidates matches)))
                        :on-no-match (lambda (candidates)
                                       (funcall reject "selection.no-match"
                                                (format nil "--old matches nothing in ~A" path)
                                                :candidates (lines-candidates candidates))))
        (let ((selector (getf edit :selector)))
          (resolve-lines/k document selector path reject
                           (lambda (ranges)
                             (flet ((apply-ranges ()
                                      (funcall on-applied (replace-ranges document ranges (content-lines new)) nil)))
                               (if (eq (aitools.kernel.domain:selector-kind selector) :match)
                                   (let ((expected (or (getf edit :expect-count) (write-context-expect-count context))))
                                     (unless (getf edit :expect-count)
                                       (setf (write-context-selected-count context) (length ranges)))
                                     (if (and expected (/= expected (length ranges)))
                                         (funcall reject "selection.count-mismatch"
                                                  (format nil "--match selects ~D line~:P, expected ~D" (length ranges) expected)
                                                  :diagnostics (list (json-object "expected" expected "actual" (length ranges))))
                                         (apply-ranges)))
                                   (apply-ranges)))))))))

(defun %edit-plan (edits)
  (lambda (context commit reject)
    (block plan
     (let ((path (context-path context)))
      (read-document/k context path reject
                       (lambda (document)
                         (let ((strategy nil))
                           (dolist (edit edits)
                             (block one
                               (%apply-edit/k context document path edit
                                              (lambda (&rest rejection)
                                                (return-from plan (apply reject rejection)))
                                              (lambda (edited edit-strategy)
                                                (setf document edited strategy edit-strategy)
                                                (return-from one)))))
                           (commit-document context path document commit
                                            (and strategy (null (rest edits))
                                                 (list (cons "strategy" (string-downcase (symbol-name strategy)))))))))))))

(define-write-command "edit" (ports env positionals options on-plan fail)
  (let ((path (first positionals)))
    (flet ((plan (edits &key selector record-options inputs)
             (funcall on-plan
                      (make-write-plan
                       :command "edit"
                       :targets (list (make-write-target path))
                       :inputs inputs
                       :guard-requirements (selector-guards selector path)
                       :expect-hashes (getf options :expect-hash)
                       :expect-count (getf options :expect-count)
                       :replayable (and (content-selector-p selector) (null (getf options :expect-hash)))
                       :plan (%edit-plan edits)
                       :record-options record-options
                       :record-positionals (lambda (paths) (list (first paths)))))))
      (cond
        ((or (null path) (rest positionals))
         (funcall fail "argument.invalid" "edit takes exactly one path"))
        ((or (getf options :stdin) (getf options :stdin-data))
         (if (or (getf options :old) (getf options :new)
                 (some (lambda (key) (getf options key)) '(:range :symbol :between :match)))
             (funcall fail "argument.invalid" "--stdin carries old/new or edits[]; do not combine it with --old, --new or a selector")
             (read-stdin-json/k ports options
                                :on-json (lambda (value text)
                                           (%stdin-edits/k value fail
                                                           (lambda (edits)
                                                             (plan edits :record-options (inline-stdin-options options text)
                                                                         :inputs (list text)))))
                                :on-error fail)))
        (t
         (parse-selector/k :edit options
                           :extra-exclusive (list (cons "--old" (getf options :old)))
                           :on-error fail
                           :on-selector (lambda (selector)
                                          (let ((new (getf options :new)))
                                            (if (null new)
                                                (funcall fail "argument.invalid" "a selector needs --new ('' deletes the selection)")
                                                (plan (list (list :selector selector :new new))
                                                      :selector selector :record-options options :inputs (list new)))))
                           :on-none (lambda ()
                                      (let ((old (getf options :old)) (new (getf options :new)))
                                        (cond
                                          ((null old) (funcall fail "argument.invalid" "edit needs --old, a selector, or --stdin"))
                                          ((zerop (length old)) (funcall fail "argument.invalid" "--old must not be empty"))
                                          ((null new) (funcall fail "argument.invalid" "--old needs --new ('' deletes the match)"))
                                          (t (plan (list (list :old old :new new))
                                                   :record-options options :inputs (list old new))))))))))))

;;; ------------------------------------------------------------------ insert

(defun %insert-plan (text mode selector)
  "MODE is :START, :END, :BEFORE or :AFTER."
  (lambda (context commit reject)
    (let ((path (context-path context))
          (lines (content-lines text)))
      (read-document/k context path reject
                       (lambda (document)
                         (flet ((done (result inserted-at)
                                  (commit-document context path result commit
                                                   (list (cons "inserted_at" inserted-at)))))
                           (ecase mode
                             (:start (done (document-replace-lines document 0 0 lines) (list 1)))
                             (:end (let ((count (document-line-count document)))
                                     (done (document-replace-lines document count count lines) (list (1+ count)))))
                             ((:before :after)
                              (resolve-lines/k
                               document selector path reject
                               (lambda (ranges)
                                 (check-expect-count/k
                                  context (length ranges) reject
                                  (lambda ()
                                    (let ((positions (mapcar (lambda (range)
                                                               (if (eq mode :before) (1- (car range)) (cdr range)))
                                                             ranges))
                                          (result document))
                                      (dolist (position (reverse positions))
                                        (setf result (document-replace-lines result position position lines)))
                                      (done result
                                            (loop for position in positions
                                                  for index from 0
                                                  collect (+ position 1 (* index (length lines))))))))))))))))))

(define-write-command "insert" (ports env positionals options on-plan fail)
  (let* ((path (first positionals))
         (at (getf options :at))
         (modes (remove nil (list (and at :at) (and (getf options :before) :before) (and (getf options :after) :after)))))
    (cond
      ((or (null path) (rest positionals)) (funcall fail "argument.invalid" "insert takes exactly one path"))
      ((/= (length modes) 1) (funcall fail "argument.invalid" "insert needs exactly one of --at, --before, --after"))
      ((and at (not (member at '("start" "end") :test #'string=)))
       (funcall fail "argument.invalid" (format nil "--at ~S must be start or end" at)))
      (t
       (read-content-text/k
        ports options
        :on-error fail
        :on-text (lambda (text)
                   (flet ((plan (mode selector)
                            (funcall on-plan
                                     (make-write-plan
                                      :command "insert"
                                      :targets (list (make-write-target path))
                                      :inputs (list text)
                                      :guard-requirements (selector-guards selector path)
                                      :expect-hashes (getf options :expect-hash)
                                      :expect-count (getf options :expect-count)
                                      :replayable (and (content-selector-p selector) (null (getf options :expect-hash)))
                                      :plan (%insert-plan text mode selector)
                                      :record-options (inline-content-options options text)
                                      :record-positionals (lambda (paths) (list (first paths)))))))
                     (parse-selector/k :insert options
                                       :on-error fail
                                       :on-selector (lambda (selector)
                                                      (if at
                                                          (funcall fail "argument.invalid" "--at does not take a selector")
                                                          (plan (if (getf options :before) :before :after) selector)))
                                       :on-none (lambda ()
                                                  (if at
                                                      (plan (if (string= at "start") :start :end) nil)
                                                      (funcall fail "argument.invalid"
                                                               "--before/--after need a selector (--range, --symbol, --between, --match)")))))))))))

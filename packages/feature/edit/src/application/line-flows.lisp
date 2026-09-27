;;;; packages/feature/edit/src/application/line-flows.lisp
;;;;
;;;; `transform` and `move-lines`.
(in-package #:aitools.edit.application)

(defun %runs (ranges)
  "RANGES (1-based inclusive, ascending) merged into contiguous runs."
  (let ((runs '()))
    (dolist (range ranges (nreverse runs))
      (if (and runs (= (car range) (1+ (cdr (first runs)))))
          (setf (cdr (first runs)) (cdr range))
          (push (cons (car range) (cdr range)) runs)))))

(defun %transform-selection (document ranges function)
  "DOCUMENT with FUNCTION (a list of lines -> list of lines) applied to the
selected lines. A contiguous selection is replaced as one block; a
scattered one (--match) gets its lines back in place when FUNCTION keeps
their number, else FUNCTION runs on each contiguous run."
  (let ((runs (%runs ranges)))
    (flet ((lines-of (run)
             (loop for index from (1- (car run)) below (cdr run) collect (document-line document index))))
      (if (null (rest runs))
          (let ((run (first runs)))
            (document-replace-lines document (1- (car run)) (cdr run) (funcall function (lines-of run))))
          (let* ((selected (mapcan #'lines-of runs))
                 (result (funcall function selected)))
            (if (= (length result) (length selected))
                (let ((lines (copy-seq (text-document-lines document))))
                  (dolist (run runs)
                    (loop for index from (1- (car run)) below (cdr run)
                          do (setf (svref lines index) (pop result))))
                  (document-with-lines document lines))
                (let ((result document))
                  (dolist (run (reverse runs) result)
                    (setf result (document-replace-lines result (1- (car run)) (cdr run)
                                                         (funcall function (lines-of run))))))))))))

(defun %whole-file-op (document op)
  (cond
    ((string= op "eol-lf") (document-with-eol document +lf+))
    ((string= op "eol-crlf") (document-with-eol document +crlf+))
    ((string= op "final-newline") (document-with-final-newline document t))
    ((string= op "no-final-newline") (document-with-final-newline document nil))
    ((string= op "strip-bom") (document-without-bom document))))

(defun %transform-plan (ops selector keywords)
  (lambda (context commit reject)
    (let ((path (context-path context)))
      (read-document/k
       context path reject
       (lambda (document)
         (let ((original (document-line-count document)))
           (flet ((run-ops (ranges)
                    (let ((result document))
                      (if ranges
                          ;; A selection is resolved once, against the original, and
                          ;; every op runs on the selected lines in turn (whole-file ops
                          ;; never come with a selector).
                          (setf result (%transform-selection
                                        result ranges
                                        (lambda (lines)
                                          (dolist (op ops lines)
                                            (setf lines (apply #'transform-lines op lines keywords))))))
                          (dolist (op ops)
                            (setf result
                                  (cond
                                    ((member op +whole-file-transform-ops+ :test #'string=) (%whole-file-op result op))
                                    ((plusp (document-line-count result))
                                     (%transform-selection result (list (cons 1 (document-line-count result)))
                                                           (lambda (lines) (apply #'transform-lines op lines keywords))))
                                    (t result)))))
                      (commit-document context path result commit
                                       (and (some (lambda (op) (member op '("unique" "delete-blank" "squeeze-blank") :test #'string=)) ops)
                                            (list (cons "removed_lines" (max 0 (- original (document-line-count result))))))))))
             (if selector
                 (resolve-lines/k document selector path reject
                                  (lambda (ranges)
                                    (if (eq (aitools.kernel.domain:selector-kind selector) :match)
                                        (check-expect-count/k context (length ranges) reject (lambda () (run-ops ranges)))
                                        (run-ops ranges))))
                 (run-ops nil)))))))))

(define-write-command "transform" (ports env positionals options on-plan fail)
  (let ((path (first positionals))
        (ops (getf options :op)))
   (block transform-prepare
    (flet ((integer-option (name key)
             (let ((text (getf options key)))
               (cond ((null text) nil)
                     ((parse-count text))
                     (t (return-from transform-prepare
                          (funcall fail "argument.invalid" (format nil "--~A ~S must be a non-negative integer" name text))))))))
        (cond
          ((or (null path) (rest positionals)) (funcall fail "argument.invalid" "transform takes exactly one path"))
          ((null ops) (funcall fail "argument.invalid" "transform needs at least one --op"))
          (t
           (let ((unknown (find-if-not (lambda (op) (or (member op +line-transform-ops+ :test #'string=)
                                                         (member op +whole-file-transform-ops+ :test #'string=)))
                                       ops))
                 (width (integer-option "width" :width))
                 (key (integer-option "key" :key))
                 (columns (or (integer-option "columns" :columns) 80))
                 (seed (integer-option "seed" :seed)))
             (cond
               (unknown (funcall fail "argument.invalid"
                                 (format nil "unknown --op ~S; ops: ~{~A~^ ~}" unknown
                                         (append +line-transform-ops+ +whole-file-transform-ops+))))
               ((and (member "shuffle" ops :test #'string=) (null seed))
                (funcall fail "argument.invalid" "shuffle needs --seed N so the same input gives the same output"))
               ((and key (zerop key)) (funcall fail "argument.invalid" "--key counts fields from 1"))
               ((and (zerop columns)) (funcall fail "argument.invalid" "--columns must be positive"))
               (t
                (let ((language (and (intersection ops '("comment" "uncomment") :test #'string=)
                                     (aitools.text.domain:language-for-path path))))
                  (if (and (intersection ops '("comment" "uncomment") :test #'string=)
                           (or (null language)
                               (not (or (aitools.text.domain:language-line-comment language)
                                        (aitools.text.domain:language-block-comment language)))))
                      (funcall fail "input.unsupported-language"
                               (format nil "no comment syntax is known for ~A" path))
                      (compile-pattern/k
                       (or (getf options :delimiter) "\\s+") fail
                       (lambda (delimiter)
                         (parse-selector/k
                          :transform options :on-error fail
                          :on-selector (lambda (selector)
                                         (if (intersection ops +whole-file-transform-ops+ :test #'string=)
                                             (funcall fail "argument.invalid"
                                                      "whole-file ops (eol-lf eol-crlf final-newline no-final-newline strip-bom) take no selector")
                                             (%transform-plan-for on-plan path options ops selector
                                                                  (list :width width :key key :delimiter delimiter
                                                                        :columns columns :seed seed :language language))))
                          :on-none (lambda ()
                                     (%transform-plan-for on-plan path options ops nil
                                                          (list :width width :key key :delimiter delimiter
                                                                :columns columns :seed seed :language language)))))))))))))))))

(defun %transform-plan-for (on-plan path options ops selector keywords)
  (funcall on-plan
           (make-write-plan
            :command "transform"
            :targets (list (make-write-target path))
            :guard-requirements (selector-guards selector path)
            :expect-hashes (getf options :expect-hash)
            :expect-count (getf options :expect-count)
            :replayable (and (content-selector-p selector) (null (getf options :expect-hash)))
            :plan (%transform-plan ops selector keywords)
            :record-options options
            :record-positionals (lambda (paths) (list (first paths))))))

;;; -------------------------------------------------------------- move-lines

(defun %parse-to-position (text)
  "(values kind argument) of --to-position TEXT, or NIL."
  (cond
    ((string= text "start") (values :start nil))
    ((string= text "end") (values :end nil))
    (t (let ((colon (position #\: text)))
         (when colon
           (let ((kind (subseq text 0 colon)) (argument (subseq text (1+ colon))))
             (cond
               ((and (member kind '("after" "before") :test #'string=) (parse-count argument) (plusp (parse-count argument)))
                (values (if (string= kind "after") :after :before) (parse-count argument)))
               ((and (member kind '("after-symbol" "before-symbol") :test #'string=) (plusp (length argument)))
                (values (if (string= kind "after-symbol") :after-symbol :before-symbol) argument)))))))))

(defun %insertion-index/k (document path kind argument reject on-index)
  "The 0-based line index of DOCUMENT where moved lines go."
  (declare (type function reject on-index))
  (let ((count (document-line-count document)))
    (ecase kind
      (:start (funcall on-index 0))
      (:end (funcall on-index count))
      ((:after :before)
       (if (> argument count)
           (funcall reject "selection.no-match" (format nil "~A has ~D lines; line ~D does not exist" path count argument)
                    :candidates (list (json-object "line" count "text" (if (plusp count) (document-line document (1- count)) ""))))
           (funcall on-index (if (eq kind :after) argument (1- argument)))))
      ((:after-symbol :before-symbol)
       (resolve-lines/k document (aitools.kernel.domain:make-symbol-selector argument) path reject
                        (lambda (ranges)
                          (let ((range (first ranges)))
                            (funcall on-index (if (eq kind :after-symbol) (cdr range) (1- (car range)))))))))))

(defun %move-lines-plan (selector kind argument same-file)
  (lambda (context commit reject)
    (let ((source (context-path context 0))
          (destination (context-path context 1)))
      (read-document/k
       context source reject
       (lambda (document)
         (resolve-lines/k
          document selector source reject
          (lambda (ranges)
            (check-expect-count/k
             context (reduce #'+ ranges :key (lambda (range) (1+ (- (cdr range) (car range))))) reject
             (lambda ()
               (let* ((moved (loop for range in ranges
                                   append (loop for line from (car range) to (cdr range)
                                                collect (document-line document (1- line)))))
                      (removed (let ((result document))
                                 (dolist (range (reverse ranges) result)
                                   (setf result (document-replace-lines result (1- (car range)) (cdr range) '()))))))
                 (if (or same-file (string= source destination))
                     (%insertion-index/k
                      document source kind argument reject
                      (lambda (index)
                        (if (some (lambda (range) (< (1- (car range)) index (cdr range))) ranges)
                            (funcall reject "argument.invalid" "--to-position falls inside the lines being moved")
                            (let ((shift (loop for range in ranges
                                               when (<= (cdr range) index)
                                                 sum (1+ (- (cdr range) (car range))))))
                              (commit-document context source
                                               (document-replace-lines removed (- index shift) (- index shift) moved)
                                               commit)))))
                     (flet ((write-both (target-document)
                              (%insertion-index/k
                               target-document destination kind argument reject
                               (lambda (index)
                                 (funcall commit
                                          (list (write-document-request source removed)
                                                (write-document-request
                                                 destination
                                                 (document-replace-lines target-document index index moved))))))))
                       ;; A missing destination starts empty. Only --to-position
                       ;; start or end can reach one: a position needs the
                       ;; destination's --expect-hash, which no missing file matches.
                       (if (aitools.store.domain:entry-state-absent-p
                            (aitools.store.application:view-path-state (write-context-view context) destination))
                           (write-both (make-text-document "" :eol (text-document-eol document)))
                           (read-document/k context destination reject #'write-both))))))))))))))

(define-write-command "move-lines" (ports env positionals options on-plan fail)
  (let ((source (first positionals))
        (destination (or (getf options :to) (first positionals))))
    (multiple-value-bind (kind argument) (%parse-to-position (or (getf options :to-position) "end"))
      (cond
        ((or (null source) (rest positionals)) (funcall fail "argument.invalid" "move-lines takes exactly one source path"))
        ((null kind)
         (funcall fail "argument.invalid"
                  (format nil "--to-position ~S must be start, end, after:N, before:N, after-symbol:NAME or before-symbol:NAME"
                          (getf options :to-position))))
        (t
         (parse-selector/k
          :move-lines options :on-error fail
          :on-none (lambda () (funcall fail "argument.invalid" "move-lines needs a selector for the lines to move"))
          :on-selector
          (lambda (selector)
            (let ((positional (or (eq (aitools.kernel.domain:selector-basis selector) :position)
                                  (member kind '(:after :before :after-symbol :before-symbol))))
                  (same (null (getf options :to))))
              (funcall on-plan
                       (make-write-plan
                        :command "move-lines"
                        :targets (list (make-write-target source) (make-write-target destination))
                        :guard-requirements (append (and positional
                                                         (if same
                                                             (list (list :expect-hash source))
                                                             (list (list :expect-hash source) (list :expect-hash destination))))
                                                    (and (eq (aitools.kernel.domain:selector-kind selector) :match)
                                                         (list (list :expect-count))))
                        :expect-hashes (getf options :expect-hash)
                        :expect-count (getf options :expect-count)
                        :replayable (and (not positional) (null (getf options :expect-hash)))
                        :plan (%move-lines-plan selector kind argument same)
                        :record-options (lambda (paths)
                                          (let ((copy (copy-list options)))
                                            (if (getf options :to)
                                                (setf (getf copy :to) (second paths))
                                                (remf copy :to))
                                            copy))
                        :record-positionals (lambda (paths) (list (first paths)))))))))))))

(in-package #:aitools.edit.application)

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

(define-write-command "insert"
                      (ports env positionals options on-plan fail)
                      (let* ((path (first positionals))
                             (at (getf options :at))
                             (modes
                              (remove nil
                                      (list (and at :at)
                                            (and (getf options :before) :before)
                                            (and (getf options :after) :after)))))
                      (cond
                      ((or (null path) (rest positionals))
                       (funcall fail
                                "argument.invalid"
                                "insert takes exactly one path"))
                      ((/= (length modes) 1)
                       (funcall fail
                                "argument.invalid"
                                "insert needs exactly one of --at, --before, --after"))
                      ((and at
                            (not (member at '("start" "end") :test #'string=)))
                       (funcall fail
                                "argument.invalid"
                                (format nil "--at ~S must be start or end" at)))
                      (t
                       (read-content-text/k ports
                                            options
                                            :on-error
                                            fail
                                            :on-text
                                            (lambda (text)
                                            (flet ((plan (mode selector)
                                                   (funcall on-plan
                                                            (make-write-plan
                                                             :command
                                                             "insert"
                                                             :targets
                                                             (list
                                                              (make-write-target
                                                               path))
                                                             :inputs
                                                             (list text)
                                                             :guard-requirements
                                                             (selector-guards
                                                              selector
                                                              path)
                                                             :expect-hashes
                                                             (getf options
                                                                   :expect-hash)
                                                             :expect-count
                                                             (getf options
                                                                   :expect-count)
                                                             :replayable
                                                             (and
                                                              (content-selector-p
                                                               selector)
                                                              (null
                                                               (getf options
                                                                     :expect-hash)))
                                                             :plan
                                                             (%insert-plan text
                                                                           mode
                                                                           selector)
                                                             :record-options
                                                             (inline-content-options
                                                              options
                                                              text)
                                                             :record-positionals
                                                             (lambda (paths)
                                                             (list
                                                              (first paths)))))))
                                            (parse-selector/k :insert
                                                              options
                                                              :on-error
                                                              fail
                                                              :on-selector
                                                              (lambda (selector)
                                                              (if at
                                                              (funcall fail
                                                                       "argument.invalid"
                                                                       "--at does not take a selector")
                                                              (plan
                                                               (if (getf options
                                                                         :before)
                                                               :before
                                                               :after)
                                                               selector)))
                                                              :on-none
                                                              (lambda ()
                                                              (if at
                                                              (plan
                                                               (if (string= at
                                                                            "start")
                                                               :start
                                                               :end)
                                                               nil)
                                                              (funcall fail
                                                                       "argument.invalid"
                                                                       (concatenate
                                                                        'string
                                                                        "--before/--after need a "
                                                                        "selector (--range, "
                                                                        "--symbol, --between, "
                                                                        "--match)"))))))))))))


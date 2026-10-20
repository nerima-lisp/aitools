(in-package #:aitools.edit.application)

(defun %parse-to-position (text)
  "(values kind argument) of --to-position TEXT, or NIL."
  (cond
   ((string= text "start") (values :start nil))
   ((string= text "end") (values :end nil))
   (t (let ((colon (position #\: text)))
        (when colon
          (let ((kind (subseq text 0 colon)) (argument (subseq text (1+ colon))))
            (cond
             ((and (member kind '("after" "before") :test #'string=) (parse-count argument) (plusp (parse-count
                                                                                                    argument)))
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
           (funcall reject "selection.no-match" (format nil "~A has ~D lines; line ~D does not exist" path count
                                                        argument)
                    :candidates (list (json-object "line" count "text" (if (plusp count) (document-line
                                                                                          document (1- count)) ""))))
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

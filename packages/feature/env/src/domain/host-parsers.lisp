;;;; packages/feature/env/src/domain/host-parsers.lisp
;;;;
;;;; Pure parsers for the OS sources behind `sys info`, `sys procs`, and `sys
;;;; ports` (Linux reads /proc, Darwin runs sysctl, vm_stat, ps, and lsof).
;;;; Each takes the text the application layer already read
;;;; or captured, so the Linux parsers are tested on any host against
;;;; captured fixture text. Records are plists; the application layer turns
;;;; them into JSON.
(in-package #:aitools.env.domain)

(defun parse-network-counters (text os)
  "Return interface alists and a validity flag. Address rows are not counters."
  (let ((items nil) (seen (make-hash-table :test #'equal))
        (lines (%split-lines text)))
    (labels ((reject () (return-from parse-network-counters (values nil nil)))
             (add-row (name numbers)
               (when (or (zerop (length name)) (gethash name seen)
                         (some #'null numbers))
                 (reject))
               (setf (gethash name seen) t)
               (push (pairlis '("name" "rx_bytes" "rx_packets" "rx_errors"
                               "tx_bytes" "tx_packets" "tx_errors")
                             (cons name numbers)) items)))
      (case os
        (:linux
         (unless (and (>= (length lines) 2) (search "Receive" (first lines))
                      (search "Transmit" (first lines)) (search "bytes" (second lines)))
           (reject))
         (dolist (line (cddr lines))
           (unless (every #'%whitespace-p line)
             (let* ((colon (position #\: line :from-end t))
                    (fields (and colon (split-fields (subseq line (1+ colon))))))
               (unless (= (length fields) 16) (reject))
               (add-row (string-trim '(#\Space #\Tab) (subseq line 0 colon))
                        (mapcar (lambda (index) (%parse-field-integer (nth index fields)))
                                '(0 1 2 8 9 10)))))))
        (:darwin
         (let* ((header (split-fields (or (first lines) "")))
                (columns (mapcar (lambda (name) (position name header :test #'equal))
                                 '("Ibytes" "Ipkts" "Ierrs" "Obytes" "Opkts" "Oerrs"))))
           (unless (and (equal (first header) "Name") (notany #'null columns)) (reject))
           (dolist (line (rest lines))
             (let ((fields (split-fields line)))
               (when (and fields
                          (not (and (third fields) (search "<Link#" (third fields))))
                          (not (and (= (length fields) (length header))
                                    (%parse-field-integer (second fields))
                                    (every (lambda (index)
                                             (let ((value (nth index fields)))
                                               (or (equal value "-")
                                                   (%parse-field-integer value))))
                                           columns))))
                 (reject))
               (when (and (third fields) (search "<Link#" (third fields)))
                 ;; Loopback/tunnel rows omit Address. Numeric columns remain a suffix.
                 (let ((offset (- (length fields) (length header))))
                   (unless (member offset '(0 -1)) (reject))
                   (add-row (string-right-trim "*" (first fields))
                            (mapcar (lambda (index)
                                      (%parse-field-integer (nth (+ index offset) fields)))
                                    columns))))))))
        (otherwise (reject)))
      ;; A header alone must not turn a failed source into a healthy empty host.
      (values (sort items #'string< :key (lambda (row) (cdr (assoc "name" row :test #'equal))))
              (not (null items))))))

(defun %metric-decimal (text)
  (let ((dot (position #\. text)))
    (cond ((ascii-digits-p text) (parse-integer text))
          ((and dot (ascii-digits-p (subseq text 0 dot))
                (ascii-digits-p (subseq text (1+ dot))))
           (+ (parse-integer text :end dot)
              (/ (parse-integer text :start (1+ dot))
                 (expt 10 (- (length text) dot 1))))))))

(defun parse-cpu-counters (text)
  "Aggregate Linux counters, excluding guest counters already counted in user/nice."
  (let ((fields (and text (split-fields (first (%split-lines text))))))
    (when (and (equal (first fields) "cpu") (>= (length fields) 9)
               (every #'ascii-digits-p (subseq fields 1 9)))
      (mapcar #'parse-integer (subseq fields 1 9)))))

(defun cpu-busy-percent (before after &key iowait-may-decrease)
  (when (and before after)
    (let ((deltas (mapcar #'- after before)))
      (when iowait-may-decrease
        ;; Linux documents that iowait may decrease independently of CPU activity.
        (setf (nth 4 deltas) (max 0 (nth 4 deltas))))
      (let ((total (reduce #'+ deltas)))
        (when (and (every (lambda (value) (>= value 0)) deltas) (plusp total))
          (float (* 100 (/ (- total (nth 3 deltas) (nth 4 deltas)) total)) 1d0))))))

(defun parse-process-metrics (text)
  "NIL for an empty or malformed source; commands contain no argument vector."
  (block parse
    (loop for line in (and text (%split-lines text))
          for fields = (split-fields line)
          when fields
            collect (let* ((pid (and (first fields) (%parse-field-integer (first fields))))
                           (cpu (and (second fields) (%metric-decimal (second fields))))
                           (rss (and (third fields) (%parse-field-integer (third fields))))
                           (command (and (fourth fields) (format nil "~{~A~^ ~}" (nthcdr 3 fields)))))
                      (unless (and pid (plusp pid) cpu rss command) (return-from parse nil))
                      (list :pid pid :cpu-percent (float cpu 1d0) :rss-bytes (* rss 1024)
                            :command (subseq command (1+ (or (position #\/ command :from-end t) -1))))))))

(defun %split-lines (text)
  (with-input-from-string (stream text)
    (loop for line = (read-line stream nil nil) while line collect line)))

(defun %whitespace-p (char)
  (member char '(#\Space #\Tab #\Return #\Newline)))

(defun split-fields (line)
  "LINE split on runs of spaces and tabs."
  (let (fields (start nil))
    (loop for index from 0 to (length line)
          do (let ((blank (or (= index (length line)) (%whitespace-p (char line index)))))
               (cond ((and blank start) (push (subseq line start index) fields) (setf start nil))
                     ((and (not blank) (null start)) (setf start index)))))
    (nreverse fields)))

(defun %parse-field-integer (text)
  (and text (ascii-digits-p text) (parse-ascii-integer text 0 (length text))))

(defun %labelled-number (text label)
  "The first integer after `LABEL` at the start of a line of TEXT."
  (dolist (line (%split-lines text))
    (when (and (> (length line) (length label)) (string= label line :end2 (length label)))
      ;; vm_stat ends each count with a period ("2508908.").
      (let ((fields (mapcar (lambda (field) (string-right-trim "." field))
                            (split-fields (subseq line (length label))))))
        (return (%parse-field-integer (find-if #'ascii-digits-p fields)))))))

;;; memory

(defun parse-meminfo (text &key strict)
  "(VALUES TOTAL-BYTES AVAILABLE-BYTES) from /proc/meminfo. Kernels before
3.14 have no MemAvailable; MemFree + Buffers + Cached stands in there."
  (let ((total (%labelled-number text "MemTotal:"))
        (available (or (%labelled-number text "MemAvailable:")
                       (let ((free (%labelled-number text "MemFree:")))
                         (and free (or (not strict)
                                       (and (%labelled-number text "Buffers:")
                                            (%labelled-number text "Cached:")))
                              (+ free (or (%labelled-number text "Buffers:") 0)
                                      (or (%labelled-number text "Cached:") 0)))))))
    (values (and total (* total 1024)) (and available (* available 1024)))))

(defun parse-vm-stat (text &key strict)
  "Available bytes from Darwin `vm_stat`: free, inactive, and speculative
pages (the pages the kernel hands out without paging anything out) times
the page size from the header line."
  (let* ((header (first (%split-lines text)))
         (marker (and header (search "page size of " header)))
         (page-size (and marker
                         (%parse-field-integer (first (split-fields (subseq header (+ marker 13))))))))
    (when (and page-size (plusp page-size)
               (or (not strict)
                   (every (lambda (label) (%labelled-number text label))
                          '("Pages free:" "Pages inactive:" "Pages speculative:"))))
      (* page-size (+ (or (%labelled-number text "Pages free:") 0)
                      (or (%labelled-number text "Pages inactive:") 0)
                      (or (%labelled-number text "Pages speculative:") 0))))))

(defun parse-sysctl-values (text)
  "The lines of `sysctl -n a b ...` output, one value per requested name."
  (mapcar (lambda (line) (string-trim " " line)) (%split-lines text)))

(defun count-cpuinfo-processors (text)
  (count-if (lambda (line)
              (let ((fields (split-fields line)))
                (and fields (string= (first fields) "processor"))))
            (%split-lines text)))

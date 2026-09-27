;;;; packages/core/text/src/domain/line-index.lisp
;;;;
;;;; The byte-level line model. Lines are found by scanning for LF in
;;;; the undecoded bytes; a line's content excludes its LF and a CR right
;;;; before it. Nothing here allocates per line: MAP-LINES hands out byte
;;;; offsets, and callers decode only the lines they output. A final line
;;;; without a terminator counts; an empty file has zero lines.
(in-package #:aitools.text.domain)

(declaim (inline %content-end))
(defun %content-end (octets line-start lf)
  "The end of a line's content given the index of its LF."
  (declare (type octets octets) (type fixnum line-start lf))
  (if (and (> lf line-start) (= (aref octets (1- lf)) 13)) (1- lf) lf))

(declaim (inline map-lines))
(defun map-lines (function octets &key (start 0) end)
  "Call FUNCTION with (LINE-NUMBER CONTENT-START CONTENT-END) for each line
of OCTETS[START,END), numbering from 1. FUNCTION returning :STOP ends the
walk. Returns the number of lines visited. Pass START as the BOM length to
keep a BOM out of the first line. Declared inline so a caller's
DYNAMIC-EXTENT closure stays on the stack."
  (declare (type function function) (type octets octets) (type fixnum start))
  (let ((end (or end (length octets)))
        (line 0)
        (line-start start))
    (declare (type fixnum end line line-start)
             (optimize (speed 3) (safety 1)))
    (loop
      (when (>= line-start end) (return line))
      (let ((lf (position 10 octets :start line-start :end end)))
        (incf line)
        (if lf
            (let ((content-end (%content-end octets line-start lf)))
              (when (eq (funcall function line line-start content-end) :stop)
                (return line))
              (setf line-start (1+ lf)))
            (progn
              (funcall function line line-start end)
              (return line)))))))

(defmacro do-lines (((line start end) octets &rest keys) &body body)
  "Run BODY for each line of OCTETS with LINE, START, and END bound as in
MAP-LINES. BODY's value is the per-line continuation's value, so a body
evaluating to :STOP ends the walk. The closure is declared DYNAMIC-EXTENT."
  (let ((function (gensym "LINE-FUNCTION")))
    `(flet ((,function (,line ,start ,end)
              (declare (type fixnum ,line ,start ,end) (ignorable ,line ,start ,end))
              ,@body))
       (declare (dynamic-extent #',function))
       (map-lines #',function ,octets ,@keys))))

(defun count-lines (octets &key (start 0) end)
  (declare (type octets octets) (type fixnum start))
  (let ((end (or end (length octets))))
    (declare (type fixnum end))
    (if (>= start end)
        0
        (+ (count 10 octets :start start :end end)
           (if (= (aref octets (1- end)) 10) 0 1)))))

(defstruct (line-index (:constructor %make-line-index (starts end)) (:copier nil))
  "STARTS holds each line's first byte offset; END is the indexed range's
end. Built once per file for random access by line number (`read --range`,
`--tail`)."
  (starts nil :type (simple-array fixnum (*)) :read-only t)
  (end 0 :type fixnum :read-only t))

(defun build-line-index (octets &key (start 0) end)
  (declare (type octets octets) (type fixnum start))
  (let* ((end (or end (length octets)))
         (starts (make-array (count-lines octets :start start :end end) :element-type 'fixnum))
         (i 0))
    (declare (type fixnum i))
    (do-lines ((line line-start content-end) octets :start start :end end)
      (setf (aref starts i) line-start)
      (incf i)
      nil)
    (%make-line-index starts end)))

(defun line-index-count (index)
  (length (line-index-starts index)))

(defun line-index-bounds (index octets line)
  "(VALUES CONTENT-START CONTENT-END) of 1-based LINE, or NIL when LINE is
out of range."
  (declare (type octets octets) (type fixnum line))
  (let ((starts (line-index-starts index)))
    (when (<= 1 line (length starts))
      (let* ((start (aref starts (1- line)))
             (lf (position 10 octets :start start :end (line-index-end index))))
        (values start (if lf (%content-end octets start lf) (line-index-end index)))))))

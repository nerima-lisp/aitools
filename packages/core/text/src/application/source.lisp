;;;;
;;;; Text boundary flows. The composition root supplies the filesystem
;;;; operations through a cl-boundary-kit context. Paths are absolute native
;;;; strings, and line work happens in the domain over returned bytes.
(in-package #:aitools.text.application)

(defun %composed-read-sniffed (boundary-context path count continue-p &key max-bytes)
  "Compose a sniffed read from size, prefix, and octet boundary operations."
  (let ((file-size
          (cl-boundary-kit:boundary-context-require boundary-context :text-file-size))
        (read-prefix
          (cl-boundary-kit:boundary-context-require boundary-context :text-read-prefix))
        (read-octets
          (cl-boundary-kit:boundary-context-require boundary-context :text-read-octets)))
    (multiple-value-bind (size problem) (funcall file-size path)
      (if (null size)
          (values nil problem)
          (let ((read-count (if max-bytes (min count (1+ max-bytes)) count)))
            (multiple-value-bind (prefix problem)
                (funcall read-prefix path read-count)
              (if (null prefix)
                  (values nil problem)
                  (let ((continue (funcall continue-p prefix size)))
                    (cond ((and continue max-bytes (> (length prefix) max-bytes))
                           (values nil :too-large))
                          ((or (not continue) (< (length prefix) read-count))
                           prefix)
                          (max-bytes
                           (funcall read-octets path :max-bytes max-bytes))
                          (t (funcall read-octets path)))))))))))

(aitools.kernel.domain:define-flow
 (call-with-sniffed-octets/k
  boundary-context path &key max-bytes on-text on-binary on-missing on-unreadable
  (on-too-large
   (lambda (size)
     (error "No :ON-TOO-LARGE continuation was supplied for ~D bytes" size))))
 "The binary-before-read decision for PATH. :TEXT-READ-SNIFFED reads its
prefix and optional remainder on one open. When that operation is absent, the
flow composes :TEXT-FILE-SIZE, :TEXT-READ-PREFIX, and :TEXT-READ-OCTETS on
separate opens, so the fallback has no single-descriptor snapshot. For bounded
calls, the fallback forwards MAX-BYTES to the octet read; both paths stop at
the first excess byte if the file grows. Calls exactly one continuation:

ON-TEXT (octets): every byte of a text file.
ON-BINARY (prefix size): the sniffed prefix (enough for GUESS-MIME) and the
  byte size; nothing past the prefix is read.
ON-MISSING (path): PATH is absent or not a regular file.
ON-UNREADABLE (path): PATH exists but may not be read (EACCES).
ON-TOO-LARGE (size): fstat size when it already exceeds MAX-BYTES, or the
  first excess byte count after growth (only when MAX-BYTES is given)."
 (declare (type function on-text on-binary on-missing on-unreadable on-too-large))
 (let ((outcome :text) (file-size 0))
   (let* ((read-sniffed
           (if (cl-boundary-kit:boundary-context-present-p boundary-context :text-read-sniffed)
               (cl-boundary-kit:boundary-context-require boundary-context :text-read-sniffed)
             (lambda (path count continue-p &key max-bytes)
               (if max-bytes
                   (%composed-read-sniffed boundary-context path count continue-p
                                           :max-bytes max-bytes)
                 (%composed-read-sniffed boundary-context path count continue-p)))))
          (continue-p
           (lambda (prefix size)
             (setf file-size size
                   outcome (cond ((binary-octets-p prefix) :binary)
                                 ((and max-bytes (> size max-bytes)) :too-large)
                                 (t :text)))
             (eq outcome :text))))
     (multiple-value-bind (octets problem)
         (if max-bytes
             (funcall read-sniffed path +binary-sniff-length+ continue-p :max-bytes max-bytes)
           (funcall read-sniffed path +binary-sniff-length+ continue-p))
       (cond
        ((eq problem :too-large)
         (aitools.kernel.domain:flow-continue :on-too-large (1+ max-bytes)))
        ((null octets)
         (if (eq problem :unreadable)
             (aitools.kernel.domain:flow-continue :on-unreadable path)
           (aitools.kernel.domain:flow-continue :on-missing path)))
        ((eq outcome :binary)
         (aitools.kernel.domain:flow-continue :on-binary octets file-size))
        ((eq outcome :too-large)
         (aitools.kernel.domain:flow-continue :on-too-large file-size))
        (t
         (aitools.kernel.domain:flow-continue :on-text octets)))))))

(aitools.kernel.domain:define-flow
 (call-with-text-file/k
  boundary-context path &key max-bytes on-text on-binary on-missing
  (on-unreadable (lambda (path) (funcall on-missing path)))
  (on-too-large
   (lambda (size)
     (error "No :ON-TOO-LARGE continuation was supplied for ~D bytes" size))))
 "CALL-WITH-SNIFFED-OCTETS/K with the text's layout: ON-TEXT (octets
layout) gets the whole file and its TEXT-LAYOUT. ON-MISSING (path) is called
for an unreadable PATH when ON-UNREADABLE is not given."
 (declare (type function on-text on-binary on-missing on-too-large))
 (aitools.kernel.domain:flow-call
  (call-with-sniffed-octets/k boundary-context path :max-bytes max-bytes)
  (:on-text (octets)
            (aitools.kernel.domain:flow-continue
             :on-text octets (detect-text-layout octets)))
  (:on-binary (octets size)
              (aitools.kernel.domain:flow-continue :on-binary octets size))
  (:on-missing (missing-path)
               (aitools.kernel.domain:flow-continue :on-missing missing-path))
  (:on-unreadable (unreadable-path)
                  (aitools.kernel.domain:flow-continue :on-unreadable unreadable-path))
  (:on-too-large (size)
                 (aitools.kernel.domain:flow-continue :on-too-large size))))

;;;;
;;;; Text boundary flows. The composition root supplies the filesystem
;;;; operations through a cl-boundary-kit context. Paths are absolute native
;;;; strings, and line work happens in the domain over returned bytes.
(in-package #:aitools.text.application)

(aitools.kernel.domain:define-flow
 (call-with-sniffed-octets/k
  boundary-context path &key max-bytes on-text on-binary on-missing on-unreadable
  (on-too-large
   (lambda (size)
     (error "No :ON-TOO-LARGE continuation was supplied for ~D bytes" size))))
 "The binary-before-read decision over one open of PATH: the first
+BINARY-SNIFF-LENGTH+ bytes decide, and only a text file within MAX-BYTES is
read further, on the same descriptor. Calls exactly one continuation:

ON-TEXT (octets): every byte of a text file.
ON-BINARY (prefix size): the sniffed prefix (enough for GUESS-MIME) and the
  byte size; nothing past the prefix is read.
ON-MISSING (path): PATH is absent or not a regular file.
ON-UNREADABLE (path): PATH exists but may not be read (EACCES).
ON-TOO-LARGE (size): a text file larger than MAX-BYTES (only when MAX-BYTES
  is given); nothing past the prefix is read."
 (declare (type function on-text on-binary on-missing on-unreadable on-too-large))
 (let ((outcome :text) (file-size 0))
   (multiple-value-bind (octets problem)
       (funcall
        (cl-boundary-kit:boundary-context-require boundary-context :text-read-sniffed)
        path
        +binary-sniff-length+
        (lambda (prefix size)
          (setf file-size size
                outcome (cond ((binary-octets-p prefix) :binary)
                              ((and max-bytes (> size max-bytes)) :too-large)
                              (t :text)))
          (eq outcome :text)))
     (cond
      ((null octets)
       (if (eq problem :unreadable)
           (aitools.kernel.domain:flow-continue :on-unreadable path)
         (aitools.kernel.domain:flow-continue :on-missing path)))
      ((eq outcome :binary)
       (aitools.kernel.domain:flow-continue :on-binary octets file-size))
      ((eq outcome :too-large)
       (aitools.kernel.domain:flow-continue :on-too-large file-size))
      (t
       (aitools.kernel.domain:flow-continue :on-text octets))))))

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

(in-package #:aitools.edit.domain)

(defun document-with-eol (document eol)
  "Every terminator rewritten as EOL (`transform --op eol-lf|eol-crlf`)."
  (%make-text-document (text-document-lines document)
                       (map 'simple-vector (lambda (terminator) (if (plusp (length terminator)) eol ""))
                            (text-document-terminators document))
                       (text-document-bom-p document)
                       eol))

(defun document-with-final-newline (document final-newline)
  (let ((count (document-line-count document)))
    (if (zerop count)
        document
        (let ((terminators (copy-seq (text-document-terminators document))))
          (setf (svref terminators (1- count)) (if final-newline (text-document-eol document) ""))
          (%rebuild document (text-document-lines document) terminators)))))

(defun document-without-bom (document)
  (%rebuild document (text-document-lines document) (text-document-terminators document) :bom-p nil))

(defun document-with-lines (document lines)
  "DOCUMENT with its lines replaced one for one by LINES (same count), every
terminator kept."
  (%rebuild document lines (text-document-terminators document)))

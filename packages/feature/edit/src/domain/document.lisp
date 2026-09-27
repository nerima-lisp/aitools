;;;; packages/feature/edit/src/domain/document.lisp
;;;;
;;;; TEXT-DOCUMENT is a UTF-8 file as the text-editing commands see it:
;;;; its lines without terminators, each line's own terminator,
;;;; whether it starts with a BOM, and the terminator new lines get. Keeping
;;;; every line's terminator lets an edit of one line leave a mixed-EOL file
;;;; byte-identical everywhere else; the BOM never appears in line text.
(in-package #:aitools.edit.domain)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defstruct (text-document (:constructor %make-text-document (lines terminators bom-p eol))
                          (:copier nil))
  (lines #() :type simple-vector :read-only t)
  ;; "\n", "\r\n", or "" (only the last line, when the file has no final newline)
  (terminators #() :type simple-vector :read-only t)
  (bom-p nil :type boolean :read-only t)
  (eol (string #\Newline) :type string :read-only t))

(defparameter +lf+ (string #\Newline))
(defparameter +crlf+ (coerce (list #\Return #\Newline) 'string))
(defparameter +utf8-bom+ (coerce #(#xEF #xBB #xBF) 'octets))

(defun document-line-count (document)
  (length (text-document-lines document)))

(defun document-line (document index)
  "Line INDEX (0-based) without its terminator."
  (svref (text-document-lines document) index))

(defun document-final-newline-p (document)
  (let ((count (document-line-count document)))
    (and (plusp count)
         (plusp (length (svref (text-document-terminators document) (1- count)))))))

(defun %split-text (text)
  "(values lines terminators) of TEXT split at each line feed; a CR before
the line feed belongs to the terminator."
  (let ((lines '()) (terminators '()) (start 0) (length (length text)))
    (loop for newline = (position #\Newline text :start start)
          while newline
          do (let ((crlf (and (> newline start) (char= (char text (1- newline)) #\Return))))
               (push (subseq text start (if crlf (1- newline) newline)) lines)
               (push (if crlf +crlf+ +lf+) terminators)
               (setf start (1+ newline))))
    (when (< start length)
      (push (subseq text start) lines)
      (push "" terminators))
    (values (coerce (nreverse lines) 'simple-vector)
            (coerce (nreverse terminators) 'simple-vector))))

(defun %dominant-eol (terminators)
  (let ((crlf (count +crlf+ terminators :test #'string=))
        (lf (count +lf+ terminators :test #'string=)))
    (if (> crlf lf) +crlf+ +lf+)))

(defun make-text-document (text &key bom-p eol)
  "A document holding TEXT (a string without BOM). EOL defaults to the
terminator TEXT uses most."
  (multiple-value-bind (lines terminators) (%split-text text)
    (%make-text-document lines terminators (and bom-p t) (or eol (%dominant-eol terminators)))))

(defun decode-text-document/k (octets &key on-decoded on-binary on-invalid)
  "Decode a file's OCTETS for a text edit and call exactly one of
ON-DECODED (document), ON-BINARY () when the first 8 KiB hold a NUL, or
ON-INVALID (offset) at the first byte that is not valid UTF-8."
  (declare (type function on-decoded on-binary on-invalid))
  (let ((octets (coerce octets 'octets)))
    (if (aitools.text.domain:binary-octets-p octets)
        (funcall on-binary)
        (let ((start (aitools.text.domain:utf8-bom-length octets)))
          (aitools.text.domain:decode-utf8-strict/k
           octets :start start
           :on-decoded (lambda (text) (funcall on-decoded (make-text-document text :bom-p (plusp start))))
           :on-invalid on-invalid)))))

(defun document-text (document)
  "The document's text without BOM, terminators as stored."
  (with-output-to-string (out)
    (loop for line across (text-document-lines document)
          for terminator across (text-document-terminators document)
          do (write-string line out) (write-string terminator out))))

(defun document-logical-text (document)
  "The document's text with every terminator written as a line feed: the
form `--old` and `replace --multiline` match against, so a CRLF file
matches a pattern written with plain line feeds."
  (with-output-to-string (out)
    (loop for line across (text-document-lines document)
          for terminator across (text-document-terminators document)
          do (write-string line out)
             (when (plusp (length terminator)) (write-char #\Newline out)))))

(defun document-line-offsets (document)
  "A vector of each line's start offset in DOCUMENT-LOGICAL-TEXT, followed
by the text's length."
  (let* ((count (document-line-count document))
         (offsets (make-array (1+ count)))
         (offset 0))
    (dotimes (index count)
      (setf (svref offsets index) offset)
      (incf offset (+ (length (document-line document index))
                      (if (plusp (length (svref (text-document-terminators document) index))) 1 0))))
    (setf (svref offsets count) offset)
    offsets))

(defun offset-line-index (offsets offset)
  "The 0-based line holding logical-text OFFSET (OFFSETS from
DOCUMENT-LINE-OFFSETS)."
  (let ((low 0) (high (- (length offsets) 2)))
    (loop while (< low high)
          do (let ((middle (ceiling (+ low high) 2)))
               (if (<= (svref offsets middle) offset)
                   (setf low middle)
                   (setf high (1- middle)))))
    (max low 0)))

(defun render-document (document)
  "The document's bytes: BOM, then each line and its terminator."
  (let ((body (aitools.text.domain:encode-utf8 (document-text document))))
    (coerce (if (text-document-bom-p document)
                (concatenate 'octets +utf8-bom+ body)
                body)
            'octets)))

(defun content-lines (text)
  "TEXT split into line strings for a line-level write:
a missing final newline is implied, a CR before a line feed dropped, and
the empty string means no lines at all."
  (if (zerop (length text))
      '()
      (coerce (%split-text text) 'list)))

(defun %rebuild (document lines terminators &key (bom-p (text-document-bom-p document)))
  (%make-text-document (coerce lines 'simple-vector) (coerce terminators 'simple-vector)
                       bom-p (text-document-eol document)))

(defun document-replace-lines (document start end new-lines)
  "Replace lines [START, END) (0-based) with NEW-LINES (strings), which get
the document's EOL. Whether the file ends with a newline is kept;
an empty file counts as ending with one."
  (let* ((lines (text-document-lines document))
         (terminators (text-document-terminators document))
         (final-newline (or (zerop (length lines)) (document-final-newline-p document)))
         (eol (text-document-eol document))
         (new-lines (coerce new-lines 'list))
         (result-lines (append (coerce (subseq lines 0 start) 'list)
                               new-lines
                               (coerce (subseq lines end) 'list)))
         (result-terminators (append (coerce (subseq terminators 0 start) 'list)
                                     (make-list (length new-lines) :initial-element eol)
                                     (coerce (subseq terminators end) 'list))))
    (loop for cell on result-terminators
          while (rest cell)
          when (zerop (length (car cell)))
            do (setf (car cell) eol))
    (when result-lines
      (let ((last (last result-terminators)))
        (if final-newline
            (when (zerop (length (car last))) (setf (car last) eol))
            (setf (car last) ""))))
    (%rebuild document result-lines result-terminators)))

(defun document-with-logical-text (document text first-changed lines-after)
  "DOCUMENT with its logical text replaced by TEXT. Lines before
FIRST-CHANGED and the LINES-AFTER last lines keep their original
terminators; every other line gets the document's EOL. The final newline
follows TEXT."
  (multiple-value-bind (lines terminators) (%split-text text)
    (let* ((old-terminators (text-document-terminators document))
           (old-count (length old-terminators))
           (count (length lines))
           (eol (text-document-eol document)))
      (dotimes (index count)
        (let ((from-end (- count index)))
          (unless (zerop (length (svref terminators index)))
            (setf (svref terminators index)
                  (cond ((< index (min first-changed old-count)) (svref old-terminators index))
                        ((and (<= from-end lines-after) (>= index first-changed))
                         (let ((old (svref old-terminators (- old-count from-end))))
                           (if (plusp (length old)) old eol)))
                        (t eol))))))
      (%rebuild document lines terminators))))

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

;;;;
;;;; `code outline`, `code defs`, and `code refs` over the text context's
;;;; language table (data/domain/text/language-data.lisp). Every language's definition
;;;; patterns are compiled once, at load time, into a MATCHER, so they live
;;;; in the saved image (startup compiles nothing) and reuse the search matcher's
;;;; candidate-line strategy. Patterns are anchored at the line start, so
;;;; MAP-MATCHES' "leftmost, ties to the lower index" rule picks, per line,
;;;; the first table entry that matches: table order is priority order.
;;;;
;;;; A definition's end line is an estimate by the language's :EXTENT rule
;;;; (balanced parentheses, balanced braces, indentation, or the next heading
;;;; of the same or a higher level).
(in-package #:aitools.search.domain)

(defstruct (language-index (:constructor %make-language-index (language matcher kinds name-groups
                                                                        level-groups identifier))
                           (:copier nil) (:predicate nil))
  (language nil :read-only t)
  (matcher nil :type matcher :read-only t)
  ;; Per definition pattern: its kind string, the index of its `name`
  ;; group, and of its `level` group (or NIL).
  (kinds #() :type simple-vector :read-only t)
  (name-groups #() :type simple-vector :read-only t)
  (level-groups #() :type simple-vector :read-only t)
  ;; A byte regex matching one identifier character.
  (identifier nil :read-only t))

(defstruct (definition (:constructor %make-definition (line end-line kind name)) (:copier nil) (:predicate nil))
  (line 0 :type fixnum :read-only t)
  (end-line 0 :type fixnum :read-only t)
  (kind "" :type string :read-only t)
  (name "" :type string :read-only t))

(defun %build-language-index (language)
  (let ((definitions (language-definitions language)))
    (aitools.kernel.domain:flow-call
        (build-matcher/k (mapcar #'second definitions))
      (:on-built (matcher)
        (let ((programs (matcher-programs matcher)))
          (%make-language-index
           language matcher
           (map 'simple-vector #'first definitions)
           (map 'simple-vector
                (lambda (program)
                  (cl-regex-kit:regex-group-index
                   (program-regex program) "name"))
                programs)
           (map 'simple-vector
                (lambda (program)
                  (cl-regex-kit:regex-group-index
                   (program-regex program) "level"))
                programs)
           (cl-regex-kit:compile-byte-regex
            (format nil "(?:~A)" (language-identifier language))))))
      (:on-syntax-error (index pattern message)
        (error "language ~A: definition pattern ~D ~S does not compile: ~A"
               (language-name language) index pattern message)))))

(defparameter *language-indexes*
  (mapcar (lambda (name) (%build-language-index (aitools.text.domain:find-language name)))
          (aitools.text.domain:language-names)))

(defun language-index-for-path (path)
  "The LANGUAGE-INDEX of PATH's language, or NIL for an unsupported file."
  (let ((language (language-for-path path)))
    (and language (find language *language-indexes* :key #'language-index-language))))

;;; Definitions

(defun %group-text (octets hit group)
  (let ((start (and group (hit-group-start hit group))))
    (if start (%decode octets start (hit-group-end hit group)) "")))

(defun file-definitions (index octets)
  "Every definition in OCTETS (a whole file, BOM included or not) as
DEFINITIONs in line order."
  (declare (type octets octets))
  (let* ((octets (strip-bom octets))
         (language (language-index-language index))
         (extent (language-extent language))
         (fences (and (eq extent :heading) (%fence-lines octets)))
         (found '()))
    (flet ((on-match (pattern result line line-start)
             (unless (and fences (gethash line fences))
               (push (list line line-start pattern result) found))
             nil))
      (declare (dynamic-extent #'on-match))
      (map-matches #'on-match (language-index-matcher index) octets :line-mode t))
    (setf found (nreverse found))
    (loop for (line line-start pattern result) in found
          collect (%make-definition
                   line
                   (ecase extent
                     (:sexp (%line-of octets (%sexp-end octets (hit-start result)
                                                        (aitools.text.domain:language-block-comment language))))
                     (:brace (%line-of octets (%brace-end octets line-start
                                                          (aitools.text.domain:language-line-comment language)
                                                          (aitools.text.domain:language-block-comment language)
                                                          (language-name language))))
                     (:indent (%indent-end-line octets line line-start))
                     (:heading
                      (let* ((level-group (svref (language-index-level-groups index) pattern))
                             (level (length (%group-text octets result level-group)))
                             (next (find-if (lambda (entry)
                                              (let ((group (svref (language-index-level-groups index) (third entry))))
                                                (<= (length (%group-text octets (fourth entry) group)) level)))
                                            (rest (member line found :key #'first)))))
                        (if next (1- (first next)) (count-lines octets)))))
                   (svref (language-index-kinds index) pattern)
                   (%group-text octets result (svref (language-index-name-groups index) pattern))))))

(defun line-definition (index octets line-start line-end)
  "(VALUES kind name) of the definition the line [LINE-START, LINE-END) of
OCTETS opens, or NIL."
  (let ((programs (matcher-programs (language-index-matcher index))))
    (loop for program across programs
          for pattern from 0
          do (let ((result (scan-line (program-regex program) octets line-start line-end)))
               (when result
                 (return (values (svref (language-index-kinds index) pattern)
                                 (%group-text octets result (svref (language-index-name-groups index) pattern)))))))))

;;; References

(defun %char-bounds-before (octets position)
  "(VALUES start end) of the UTF-8 character ending at POSITION, or NIL."
  (when (plusp position)
    (let ((start (1- position)))
      (loop while (and (plusp start) (= (logand (aref octets start) #xC0) #x80))
            do (decf start))
      (values start position))))

(defun %char-bounds-at (octets position)
  (when (< position (length octets))
    (values position (next-char-boundary octets position))))

(defun %identifier-char-p (index octets &optional start end)
  (and start (cl-regex-kit:full-match-p (language-index-identifier index) (subseq octets start end))))

(defun map-word-occurrences (function index name octets)
  "Call FUNCTION with (LINE LINE-START LINE-END) once for each line of
OCTETS (BOM already removed) holding NAME (octets) with no identifier
character of INDEX's language right before or after it: `code refs`'s
word-bounded reference."
  (declare (type function function) (type octets name octets))
  (let ((pos 0) (line 1) (line-pos 0))
    (loop
      (let ((hit (octets-find name octets pos)))
        (unless hit (return))
        (let ((after (+ hit (length name))))
          (if (and (not (multiple-value-call #'%identifier-char-p index octets (%char-bounds-before octets hit)))
                   (not (multiple-value-call #'%identifier-char-p index octets (%char-bounds-at octets after))))
              (progn
                (incf line (count-newlines octets line-pos hit))
                (setf line-pos hit)
                (funcall function line (line-start-at octets hit) (line-content-end octets hit))
                (setf pos (next-line-start octets hit)))
              (setf pos (1+ hit))))))))

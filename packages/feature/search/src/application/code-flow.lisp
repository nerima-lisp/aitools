;;;; packages/feature/search/src/application/code-flow.lisp
;;;;
;;;; `code outline`, `code defs`, `code refs`. All three
;;;; read definitions through the language table the text context owns and
;;;; `--symbol` shares. `defs` and `refs` scan only files of a supported
;;;; language, and skip a file without reading its definitions when the name
;;;; does not occur in it.
(in-package #:aitools.search.application)

(defun %definition-json (definition &optional path)
  (json-object-from-alist (append (when path (list (cons "path" path)))
                       (list (cons "line" (definition-line definition))
                             (cons "end_line" (definition-end-line definition))
                             (cons "kind" (definition-kind definition))
                             (cons "name" (definition-name definition))))))

(defun %unsupported-language (on-error path)
  (funcall on-error "input.unsupported-language"
           (format nil "~A is not in a language `code` supports (~{~A~^, ~})" path
                   (aitools.text.domain:language-names))
           :repairs (list (%repair "read-instead" "Read the file directly."
                                   (command-line (list "aitools" "read" path))))))

(defun code-outline/k (ports &key path (limit 200) root tx on-ok on-partial on-error)
  "`code outline`: the definitions of the file PATH, in line order."
  (declare (type function on-ok on-partial on-error))
  (call-with-session/k
   ports "code outline" :root root :tx tx :on-error on-error
   :on-session
   (lambda (session)
     (let* ((absolute (%absolute session path))
            (relative (%relative session absolute))
            (index (language-index-for-path absolute)))
       (if (null index)
           (%unsupported-language on-error path)
           (%read-file/k
            session absolute relative
            :on-missing (lambda ()
                          (funcall on-error "input.not-found" (format nil "~A is not a readable file" path)
                                   :repairs (list (%repair "find-path" "Look for the file by name."
                                                           (command-line (list "aitools" "find"
                                                                               (aitools.workspace.domain:path-basename absolute)))))))
            :on-binary (lambda ()
                         (funcall on-error "input.unsupported-format" (format nil "~A is a binary file" path)
                                  :repairs (list (%repair "inspect" "Inspect the file instead."
                                                          (command-line (list "aitools" "info" path))))))
            :on-text
            (lambda (octets)
              (let* ((definitions (file-definitions index octets))
                     (total (length definitions))
                     (truncated (> total limit))
                     (fields (list (cons "path" (or relative absolute))
                                   (cons "lang" (aitools.text.domain:language-name (language-index-language index)))
                                   (cons "symbols" (mapcar #'%definition-json
                                                           (subseq definitions 0 (min limit total))))
                                   (cons "total" total)
                                   (cons "truncated" (json-boolean truncated)))))
                (if truncated
                    (%finish on-partial fields
                             (list (command-line (list "aitools" "code" "outline" path
                                                       "--limit" (princ-to-string total)))))
                    (%finish on-ok fields))))))))))

(defun %language-file-p (path)
  (and (language-index-for-path path) t))

(defun %code-scan/k (session command path worker emit on-done on-error)
  "Scan the supported-language files below PATH, WORKER running per file
on the pool, EMIT receiving (relative-path result) in path order."
  (aitools.workspace.application:call-with-workspace-scan/k
   (%host session) (session-root session)
   :paths (%start-paths session (and path (list path)))
   :lang #'%language-file-p
   :overlay (session-overlay session)
   :work (lambda (entry)
           (when (eq (aitools.workspace.application:scan-entry-kind entry) :file)
             (handler-case
                 (call-with-regex-budget/k
                  (lambda ()
                    (%read-file/k session
                               (aitools.workspace.application:scan-entry-absolute entry)
                               (aitools.workspace.application:scan-entry-path entry)
                               :on-text (lambda (octets)
                                          (funcall worker (aitools.workspace.application:scan-entry-path entry)
                                                   octets))
                               :on-binary (constantly nil)
                                  :on-missing (constantly nil)))
                  :on-exhausted (constantly nil))
               ((or stream-error file-error) () nil))))
   :emit (lambda (entry result)
           (when result (funcall emit (aitools.workspace.application:scan-entry-path entry) result))
           nil)
   :on-error (lambda (reason path) (scan-error on-error command reason path))
   :on-complete (lambda (source stopped) (declare (ignore stopped)) (funcall on-done source))))

(defun %name-octets (name)
  (coerce (aitools.text.domain:encode-utf8 name) 'octets))

(defun code-defs/k (ports &key name prefix kind path (limit 50) root tx on-ok on-partial on-error)
  "`code defs`: definitions named NAME (starting with NAME under PREFIX), of
KIND when given, in the supported-language files below PATH."
  (declare (type function on-ok on-partial on-error))
  (let ((needle (%name-octets name)) (items '()) (total 0))
    (call-with-session/k
     ports "code defs" :root root :tx tx :on-error on-error
     :on-session
     (lambda (session)
       (%code-scan/k
        session "code defs" path
        (lambda (relative octets)
          (when (octets-find needle octets 0)
            (remove-if-not (lambda (definition)
                             (and (if prefix
                                      (let ((found (definition-name definition)))
                                        (and (>= (length found) (length name)) (string= name found :end2 (length name))))
                                      (string= name (definition-name definition)))
                                  (or (null kind) (string= kind (definition-kind definition)))))
                           (file-definitions (language-index-for-path relative) octets))))
        (lambda (relative definitions)
          (dolist (definition definitions)
            (incf total)
            (when (<= total limit) (push (%definition-json definition relative) items))))
        (lambda (source)
          (declare (ignore source))
          (let* ((truncated (> total limit))
                 (fields (list (cons "defs" (nreverse items))
                               (cons "total" total)
                               (cons "truncated" (json-boolean truncated)))))
            (if truncated
                (%finish on-partial fields
                         (list (command-line (append (list "aitools" "code" "defs" name)
                                                     (when path (list path))
                                                     (when prefix (list "--prefix"))
                                                     (when kind (list "--kind" kind))
                                                     (list "--limit" (princ-to-string total))
                                                     (when tx (list "--tx" tx))))))
                (%finish on-ok fields))))
        on-error)))))

(defun code-refs/k (ports &key name path (limit 50) root tx on-ok on-partial on-error)
  "`code refs`: the lines holding NAME as a whole word in the supported-language
files below PATH; `kind` is `def` on a line that defines NAME."
  (declare (type function on-ok on-partial on-error))
  (let ((needle (%name-octets name)) (items '()) (total 0) (characters 0))
    (call-with-session/k
     ports "code refs" :root root :tx tx :on-error on-error
     :on-session
     (lambda (session)
       (%code-scan/k
        session "code refs" path
        (lambda (relative octets)
          (let ((index (language-index-for-path relative))
                (octets (strip-bom octets))
                (lines '()))
            (flet ((on-line (line start end)
                     (push (list line
                                 (if (equal (nth-value 1 (line-definition index octets start end)) name) "def" "ref")
                                 start end)
                           lines)
                     nil))
              (declare (dynamic-extent #'on-line))
              (map-word-occurrences #'on-line index needle octets))
            (and lines (cons octets (nreverse lines)))))
        (lambda (relative result)
          (destructuring-bind (octets . lines) result
            (loop for (line kind start end) in lines
                  do (incf total)
                     (when (<= total limit)
                       (let ((text (aitools.text.domain:decode-utf8 octets :start start :end end)))
                         (incf characters (length text))
                         (push (json-object-from-alist (list (cons "path" relative) (cons "line" line)
                                                  (cons "kind" kind) (cons "text" text)))
                               items))))))
        (lambda (source)
          (declare (ignore source))
          (let* ((truncated (> total limit))
                 (fields (list (cons "refs" (nreverse items))
                               (cons "total" total)
                               (cons "truncated" (json-boolean truncated))
                               (cons "approx_tokens" (aitools.kernel.domain:approx-token-count characters)))))
            (if truncated
                (%finish on-partial fields
                         (list (command-line (append (list "aitools" "code" "refs" name)
                                                     (when path (list path))
                                                     (list "--limit" (princ-to-string total))
                                                     (when tx (list "--tx" tx))))))
                (%finish on-ok fields))))
        on-error)))))

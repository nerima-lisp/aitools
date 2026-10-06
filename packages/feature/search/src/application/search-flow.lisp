;;;; packages/feature/search/src/application/search-flow.lisp
;;;;
;;;; `search`. The workspace scan hands each file to a worker on the ordered
;;;; worker pool; a worker reads the file and keeps only byte
;;;; offsets (SEARCH-FILE). Results arrive back in path order, and only then
;;;; are lines decoded, for as many as `--limit` still allows. Counting
;;;; continues to the end, so `total_matches` is exact even when the output
;;;; is cut short.
;;;;
;;;; The unit `--limit` and `total_matches` count depends on the mode:
;;;; selected lines for blocks, count, files, and files-without-match; matches
;;;; for matches. In count, files, and files-without-match `--limit` bounds
;;;; the entries returned and `total` gives their full number.
(in-package #:aitools.search.application)

(defconstant +stdin-pattern-limit+ (* 1024 1024)
  "Largest pattern `search --stdin` reads.")

(defparameter *output-modes*
  '((:blocks . "blocks") (:matches . "matches") (:count . "count") (:files . "files")
    (:files-without-match . "files-without-match")))

(defun %mode-name (mode)
  (cdr (assoc mode *output-modes*)))

(defun %search-command-line (request &key limit fixed)
  "The `aitools search` command line reproducing REQUEST (a plist of the
flow's keyword arguments), with LIMIT replacing `--limit` and FIXED adding
`--fixed`. The pattern is passed with `--pattern` so it reads back
unambiguously."
  (destructuring-bind (&key patterns paths ignore-case word line-regexp invert multiline
                         before after output glob lang no-ignore skip-larger-than newer tx
                       &allow-other-keys)
      request
    (command-line
     (append (list "aitools" "search")
             (loop for pattern in patterns append (list "--pattern" pattern))
             (when (or fixed (getf request :fixed)) (list "--fixed"))
             (when ignore-case (list "--ignore-case"))
             (when word (list "--word"))
             (when line-regexp (list "--line-regexp"))
             (when invert (list "--invert"))
             (when multiline (list "--multiline"))
             (when (eq output :blocks)
               (list "--before" (princ-to-string before) "--after" (princ-to-string after)))
             (unless (eq output :blocks) (list "--output" (%mode-name output)))
             (list "--limit" (princ-to-string limit))
             (loop for glob in glob append (list "--glob" glob))
             (when lang (list "--lang" lang))
             (when no-ignore (list "--no-ignore"))
             (when skip-larger-than (list "--skip-larger-than" skip-larger-than))
             (when newer (list "--newer" newer))
             (when tx (list "--tx" tx))
             paths))))

(defun %read-stdin-pattern/k (ports &key on-pattern on-error)
  "`search --stdin`: the pattern as raw UTF-8 text. One trailing newline
(LF or CR LF), as `echo` adds, is dropped; nothing else is changed."
  (declare (type function on-pattern on-error))
  (let ((reader (search-ports-read-stdin-octets ports)))
    (if (null reader)
        (%unavailable on-error "search")
        (funcall reader +stdin-pattern-limit+
                 :on-octets
                 (lambda (octets)
                   (let* ((end (length octets))
                          (end (if (and (plusp end) (= (aref octets (1- end)) 10)) (1- end) end))
                          (end (if (and (plusp end) (= (aref octets (1- end)) 13)) (1- end) end)))
                     (aitools.text.domain:decode-utf8-strict/k
                      (coerce octets '(simple-array (unsigned-byte 8) (*))) :end end
                      :on-decoded on-pattern
                      :on-invalid (lambda (position)
                                    (funcall on-error "argument.invalid"
                                             (format nil "the pattern on standard input is not UTF-8 (byte ~D)" position)
                                             :repairs (list (aitools.protocol.domain:repair "pass-pattern" "Pass the pattern as an argument."
                                                                     "aitools search --pattern PATTERN")))))))
                 :on-too-large
                 (lambda ()
                   (funcall on-error "argument.invalid" "the pattern on standard input exceeds 1 MiB"
                            :repairs (list (aitools.protocol.domain:repair "pass-pattern" "Pass the pattern as an argument."
                                                    "aitools search --pattern PATTERN"))))
                 :on-failure
                 (lambda (message)
                   (funcall on-error "environment.io" message
                            :repairs (list (aitools.protocol.domain:repair "pass-pattern" "Pass the pattern as an argument."
                                                    "aitools search --pattern PATTERN"))))))))

(defparameter *search-regex-budget-seconds* 20
  "Wall-clock seconds a single `search` run may spend matching before it gives
up on the remaining advanced-executor work and reports the file skipped
`regex-limit`. Generous enough that a linear scan of a large tree never
reaches it; tests rebind it small.")

(defun %run-deadline ()
  (and *search-regex-budget-seconds*
       (+ (get-internal-real-time)
          (* *search-regex-budget-seconds* internal-time-units-per-second))))

(defun %search-worker (session matcher mode keep deadline)
  "The per-file WORK function: a FILE-OUTCOME, or (:SKIPPED reason). DEADLINE
(an internal-real-time or NIL) is the run's cumulative regex budget, bound on
this worker thread so MAP-MATCHES can observe it."
  (lambda (entry)
    (when (eq (aitools.workspace.application:scan-entry-kind entry) :file)
      (handler-case
          (call-with-regex-budget/k
           (lambda ()
             (let ((*regex-run-deadline* deadline))
               (%read-file/k session
                             (aitools.workspace.application:scan-entry-absolute entry)
                             (aitools.workspace.application:scan-entry-path entry)
                             :on-text (lambda (octets) (search-file matcher octets mode keep))
                             :on-binary (lambda () (list :skipped "binary"))
                             :on-missing (lambda () (list :skipped "unreadable")))))
           :on-exhausted (lambda () (list :skipped "regex-limit")))
        ((or stream-error file-error) () (list :skipped "unreadable"))))))

(defun %run-search (session request matcher scan-options on-ok on-partial on-error)
  (destructuring-bind (&key paths output before after limit &allow-other-keys) request
    (let* ((programs (matcher-programs matcher))
           (with-index (> (length programs) 1))
           (items '()) (remaining limit) (truncated nil) (characters 0)
           (total 0) (entries 0) (files-scanned 0) (skipped '()))
      (flet ((skip (path reason)
               (push (json-object-from-alist (list (cons "path" path) (cons "reason" reason))) skipped))
             (entry-item (item)
               ;; count / files modes: one entry per file, bounded by LIMIT.
               (incf entries)
               (if (plusp remaining)
                   (progn (push item items) (decf remaining))
                   (setf truncated t))))
        (apply #'aitools.workspace.application:call-with-workspace-scan/k
               (%host session) (session-root session)
               :paths (%start-paths session paths)
               :work (%search-worker session matcher output limit (%run-deadline))
               :on-skip (lambda (entry reason)
                          (skip (aitools.workspace.application:scan-entry-path entry)
                                (if (eq reason :too-large) "too-large" "unreadable")))
               :on-error (lambda (reason path) (scan-error on-error "search" reason path))
               :emit
               (lambda (entry result)
                 (let ((path (aitools.workspace.application:scan-entry-path entry)))
                   (cond
                     ((null result))
                     ((and (consp result) (eq (first result) :skipped)) (skip path (second result)))
                     (t
                      (incf files-scanned)
                      (let ((selected (file-outcome-selected-count result)))
                        (ecase output
                          (:blocks
                           (incf total selected)
                           (when (plusp selected)
                             (let ((taken (min remaining selected)))
                               (when (< taken selected) (setf truncated t))
                               (when (plusp taken)
                                 (multiple-value-bind (blocks count)
                                     (build-blocks path (file-outcome-octets result)
                                                   (subseq (file-outcome-selected result) 0 taken)
                                                   before after)
                                   (incf characters count)
                                   (setf items (revappend blocks items))
                                   (decf remaining taken))))))
                          (:matches
                           (let ((count (file-outcome-match-count result)))
                             (incf total count)
                             (let ((taken (min remaining count)))
                               (when (< taken count) (setf truncated t))
                               (loop for match in (file-outcome-matches result)
                                     repeat taken
                                     do (multiple-value-bind (object count)
                                            (render-match path (file-outcome-octets result) match programs with-index)
                                          (incf characters count)
                                          (push object items)))
                               (decf remaining taken))))
                          (:count
                           (incf total selected)
                           (when (plusp selected)
                             (entry-item (json-object-from-alist (list (cons "path" path) (cons "count" selected))))))
                          (:files
                           (incf total selected)
                           (when (plusp selected) (entry-item path)))
                          (:files-without-match
                           (incf total selected)
                           (when (zerop selected) (entry-item path))))))))
                 nil)
               :on-complete
               (lambda (source stopped)
                 (declare (ignore stopped))
                 (let ((fields
                         (append
                          (list (cons "mode" (%mode-name output))
                                (cons (ecase output
                                        (:blocks "blocks") (:matches "matches") (:count "counts")
                                        ((:files :files-without-match) "paths"))
                                      (nreverse items))
                                (cons "total_matches" total))
                          (when (member output '(:count :files :files-without-match))
                            (list (cons "total" entries)))
                          (list (cons "files_scanned" files-scanned)
                                (cons "skipped" (nreverse skipped))
                                (cons "ignore_source" (%ignore-source-name source))
                                (cons "truncated" (json-boolean truncated))
                                (cons "approx_tokens" (aitools.kernel.domain:approx-token-count characters))))))
                   (if truncated
                       (%finish on-partial fields
                                (list (%search-command-line
                                       request :limit (if (member output '(:blocks :matches)) total entries))))
                       (%finish on-ok fields))))
               scan-options)))))

(defun search/k (ports &rest request
                 &key patterns stdin fixed ignore-case word line-regexp invert multiline
                   (context 2) before after (output :blocks) (limit 15) paths root tx
                   glob lang no-ignore skip-larger-than newer
                   on-ok on-partial on-error)
  "`search`. PATTERNS is the list of patterns (the positional pattern, or
each `--pattern`); with STDIN the one pattern is read from standard input
instead. PATHS are the scan starts. BEFORE and AFTER default to CONTEXT.
OUTPUT is :BLOCKS, :MATCHES, :COUNT, :FILES, or :FILES-WITHOUT-MATCH."
  (declare (ignore fixed ignore-case word line-regexp invert multiline paths
                   glob lang no-ignore skip-larger-than newer))
  (declare (type function on-ok on-partial on-error))
  (let ((request (list* :before (or before context) :after (or after context) :output output :limit limit
                        request)))
    (labels ((with-patterns (patterns)
               (cond
                 ((null patterns)
                  (%argument-error on-error "search needs a pattern (positional, --pattern, or --stdin)" "search"))
                 ((and (eq output :matches) (getf request :invert))
                  (%argument-error on-error "--output matches cannot be combined with --invert" "search"))
                 (t (run (list* :patterns patterns request)))))
             (run (request)
               (build-matcher/k
                (getf request :patterns)
                :fixed (getf request :fixed) :ignore-case (getf request :ignore-case)
                :word (getf request :word) :line-regexp (getf request :line-regexp)
                :multiline (getf request :multiline) :invert (getf request :invert)
                :on-syntax-error
                (lambda (index pattern message)
                  (declare (ignore index))
                  (funcall on-error "input.syntax-error" (format nil "invalid pattern ~S: ~A" pattern message)
                           :repairs (list (aitools.protocol.domain:repair "search-literally" "Search for the text literally."
                                                   (%search-command-line request :limit limit :fixed t)))))
                :on-built
                (lambda (matcher)
                  (call-with-session/k
                   ports "search" :root root :tx tx :on-error on-error
                   :on-session
                   (lambda (session)
                     (apply #'scan-options/k session "search"
                            :on-error on-error
                            :on-options (lambda (options)
                                          (%run-search session request matcher options on-ok on-partial on-error))
                            (loop for key in '(:glob :lang :no-ignore :skip-larger-than :newer)
                                  append (list key (getf request key))))))))))
      (if stdin
          (%read-stdin-pattern/k ports :on-pattern (lambda (pattern) (with-patterns (list pattern)))
                                       :on-error on-error)
          (with-patterns patterns)))))

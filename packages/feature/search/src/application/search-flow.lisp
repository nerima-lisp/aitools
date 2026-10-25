;;;;
;;;; SEARCH-FILE receives each whole file as octets. For blocks and matches,
;;;; it retains those octets and the byte offsets needed for output, without
;;;; decoding strings. Results arrive in path order; only output lines or
;;;; matches within `--limit` are decoded. Scanning continues to the end, so
;;;; `total_matches` stays exact even when output is cut short.
;;;;
;;;; For blocks, `--limit` counts selected lines; for matches, it counts
;;;; match occurrences. For count, files, and files-without-match, it bounds
;;;; returned file entries, and `total` reports the full entry count.
;;;; `total_matches` counts selected lines in all modes except matches, where
;;;; it counts match occurrences.
(in-package #:aitools.search.application)

(aitools.kernel.domain:define-flow
    (%read-stdin-pattern/k ports &key on-pattern on-error)
  "Read the raw UTF-8 pattern for search --stdin. One trailing newline
(LF or CR LF), as echo adds, is dropped; nothing else is changed."
  (let ((reader (search-ports-read-stdin-octets ports)))
    (if (null reader)
        (aitools.kernel.domain:flow-call
            (%unavailable "search")
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message :repairs repairs)))
        (let ((result (funcall reader +stdin-pattern-limit+)))
          (ecase (first result)
            (:octets
             (let* ((octets (second result))
                    (end (length octets))
                    (end (if (and (plusp end)
                                  (= (aref octets (1- end)) 10))
                             (if (and (> end 1)
                                      (= (aref octets (- end 2)) 13))
                                 (- end 2)
                                 (1- end))
                             end)))
               (aitools.text.domain:decode-utf8-strict/k
                (coerce octets (quote (simple-array (unsigned-byte 8) (*))))
                :end end
                :on-decoded
                (lambda (pattern)
                  (aitools.kernel.domain:flow-continue :on-pattern pattern))
                :on-invalid
                (lambda (position)
                  (aitools.kernel.domain:flow-continue
                   :on-error
                   "argument.invalid"
                   (format nil
                           "the pattern on standard input is not UTF-8 (byte ~D)"
                           position)
                   :repairs
                   (list
                    (repair
                     "pass-pattern"
                     "Pass the pattern as an argument."
                     "aitools search --pattern PATTERN")))))))
            (:too-large
             (aitools.kernel.domain:flow-continue
              :on-error
              "argument.invalid"
              "the pattern on standard input exceeds 1 MiB"
              :repairs
              (list
               (repair
                "pass-pattern"
                "Pass the pattern as an argument."
                "aitools search --pattern PATTERN"))))
            (:failure
             (aitools.kernel.domain:flow-continue
              :on-error
              "environment.io"
              (second result)
              :repairs
              (list
               (repair
                "pass-pattern"
                "Pass the pattern as an argument."
                "aitools search --pattern PATTERN")))))))));


(aitools.kernel.domain:define-flow
    (%search-patterns ports request limit &key on-ok on-partial on-error)
  (let ((patterns (getf request :patterns))
        (output (getf request :output)))
    (cond
      ((null patterns)
       (aitools.kernel.domain:flow-call
           (%argument-error
            "search needs a pattern (positional, --pattern, or --stdin)"
            "search")
         (:on-error (code message &key repairs)
           (aitools.kernel.domain:flow-continue
            :on-error code message :repairs repairs))))
      ((and (eq output :matches) (getf request :invert))
       (aitools.kernel.domain:flow-call
           (%argument-error
            "--output matches cannot be combined with --invert"
            "search")
         (:on-error (code message &key repairs)
           (aitools.kernel.domain:flow-continue
            :on-error code message :repairs repairs))))
      (t
       (aitools.kernel.domain:flow-call
           (build-matcher/k
            patterns
            :fixed (getf request :fixed)
            :ignore-case (getf request :ignore-case)
            :word (getf request :word)
            :line-regexp (getf request :line-regexp)
            :multiline (getf request :multiline)
            :invert (getf request :invert))
         (:on-syntax-error (index pattern message)
           (declare (ignore index))
           (aitools.kernel.domain:flow-continue
            :on-error
            "input.syntax-error"
            (format nil "invalid pattern ~S: ~A" pattern message)
            :repairs
            (list
             (repair
              "search-literally"
              "Search for the text literally."
              (%search-command-line request :limit limit :fixed t)))))
         (:on-built (matcher)
           (aitools.kernel.domain:flow-call
               (call-with-session/k
                ports
                "search"
                :root (getf request :root)
                :tx (getf request :tx))
             (:on-error (code message &key repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code message :repairs repairs))
             (:on-session (session)
               (aitools.kernel.domain:flow-call
                   (scan-options/k
                    session
                    "search"
                    :glob (getf request :glob)
                    :lang (getf request :lang)
                    :no-ignore (getf request :no-ignore)
                    :skip-larger-than (getf request :skip-larger-than)
                    :newer (getf request :newer))
                 (:on-error (code message &key repairs)
                   (aitools.kernel.domain:flow-continue
                    :on-error code message :repairs repairs))
                 (:on-options (options)
                   (aitools.kernel.domain:flow-call
                       (%run-search session request matcher options)
                     (:on-ok (fields)
                       (aitools.kernel.domain:flow-continue :on-ok fields))
                     (:on-partial (fields)
                       (aitools.kernel.domain:flow-continue
                        :on-partial fields))
                     (:on-error (code message &key repairs)
                       (aitools.kernel.domain:flow-continue
                        :on-error code message :repairs repairs)))))))))))))

(aitools.kernel.domain:define-flow
    (search/k ports &rest request
              &key patterns stdin fixed ignore-case word line-regexp invert multiline
                (context 2) before after (output :blocks) (limit 15) paths root tx
                glob lang no-ignore skip-larger-than newer
                on-ok on-partial on-error)
  "PATTERNS is the list of patterns (the positional pattern, or each
`--pattern`); with STDIN the one pattern is read from standard input
instead. PATHS are the scan starts. BEFORE and AFTER default to CONTEXT.
OUTPUT is :BLOCKS, :MATCHES, :COUNT, :FILES, or :FILES-WITHOUT-MATCH."
  (declare (ignore fixed ignore-case word line-regexp invert multiline paths
                   glob lang no-ignore skip-larger-than newer))
  (let ((request (list* :before (or before context)
                        :after (or after context)
                        :output output
                        :limit limit
                        request)))
    (if stdin
        (aitools.kernel.domain:flow-call
            (%read-stdin-pattern/k ports)
          (:on-pattern (pattern)
            (aitools.kernel.domain:flow-call
                (%search-patterns
                 ports (list* :patterns (list pattern) request) limit)
              (:on-ok (fields)
                (aitools.kernel.domain:flow-continue :on-ok fields))
              (:on-partial (fields)
                (aitools.kernel.domain:flow-continue :on-partial fields))
              (:on-error (code message &key repairs)
                (aitools.kernel.domain:flow-continue
                 :on-error code message :repairs repairs))))
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message :repairs repairs)))
        (aitools.kernel.domain:flow-call
            (%search-patterns
             ports (list* :patterns patterns request) limit)
          (:on-ok (fields)
            (aitools.kernel.domain:flow-continue :on-ok fields))
          (:on-partial (fields)
            (aitools.kernel.domain:flow-continue :on-partial fields))
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message :repairs repairs))))))

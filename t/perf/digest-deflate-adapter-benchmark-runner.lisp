(defun run-digest-deflate-adapter-benchmark ()
  (let* ((root (truename (%project-root)))
         (temporary (%temporary-directory root))
         (fasl-directory (merge-pathnames "fasl/" temporary))
         (sha-input (%make-input (* 16 1024 1024) #x61746f6f))
         (codec-input (%make-input (* 2 1024 1024) #x6465666c)))
    (unwind-protect
        (progn
          (ensure-directories-exist (merge-pathnames "output.fasl" fasl-directory))
          (asdf:initialize-source-registry
           (list :source-registry (list :directory root) :inherit-configuration))
          (asdf:initialize-output-translations
           (list :output-translations
                 (list (list root :**/ :*.*.*)
                       (list fasl-directory :**/ :*.*.*))
                 :inherit-configuration))
          (asdf:load-system "aitools")
          (labels ((assert-inside-workspace (pathname description)
                                            (unless (probe-file pathname)
                                              (error "Resolved ~A path does not exist: ~A"
                                                     description pathname))
                                            (let ((workspace-name
                                                   (namestring
                                                    (uiop:ensure-directory-pathname (truename root))))
                                                  (resolved-name (namestring (truename pathname))))
                                              (unless (and (>= (length resolved-name)
                                                               (length workspace-name))
                                                           (string= workspace-name resolved-name
                                                                    :end2 (length workspace-name)))
                                                (error "~A path resolves outside the workspace: ~A"
                                                       description resolved-name))))
                   (assert-system-component-in-workspace
                    (system-name component-path)
                    (let* ((system (asdf:find-system system-name))
                           (component-label (format nil "~{~A~^/~}" component-path))
                           (component (asdf:find-component system component-path)))
                      (assert-inside-workspace
                       (asdf:system-source-directory system)
                       (format nil "~A source directory" system-name))
                      (unless component
                        (error "Cannot find component ~A in ~A" component-label system-name))
                      (assert-inside-workspace
                       (asdf:component-pathname component)
                       (format nil "~A source file" component-label))
                      (multiple-value-bind (outputs translated-p)
                          (asdf:output-files (asdf:make-operation 'asdf:compile-op) component)
                        (declare (ignore translated-p))
                        (unless outputs
                          (error "ASDF returned no compile output for ~A" component-label))
                        (dolist (output outputs)
                          (assert-inside-workspace
                           output
                           (format nil "~A compiled output" component-label)))))))
            (assert-system-component-in-workspace
             "aitools/core/kernel" '("domain" "digest-kit"))
            (assert-system-component-in-workspace
             "aitools/core/text" '("domain" "codec-deflate"))
            (let* ((current-sha
                    (symbol-function (find-symbol "SHA256-HEX" "AITOOLS.KERNEL.DOMAIN")))
                   (current-deflate
                    (symbol-function (find-symbol "DEFLATE" "AITOOLS.TEXT.DOMAIN")))
                   (current-inflate
                    (symbol-function (find-symbol "INFLATE" "AITOOLS.TEXT.DOMAIN")))
                   (expected-sha (progn
                                   (setf *signature-hash-function* current-sha)
                                   (funcall current-sha sha-input)))
                   (shared-compressed (funcall current-deflate codec-input)))
              (%extract-baseline-source root temporary
                                        "packages/core/kernel/src/domain/digest.lisp"
                                        "baseline-digest")
              (%extract-baseline-source root temporary
                                        "packages/core/text/src/domain/codec-deflate.lisp"
                                        "baseline-codec-deflate")
              (let* ((old-sha
                      (symbol-function (find-symbol "SHA256-HEX" "AITOOLS.KERNEL.DOMAIN")))
                     (old-deflate
                      (symbol-function (find-symbol "DEFLATE" "AITOOLS.TEXT.DOMAIN")))
                     (old-inflate
                      (symbol-function (find-symbol "INFLATE" "AITOOLS.TEXT.DOMAIN"))))
                (format t "~&SBCL: ~A~%Platform: ~A / ~A / ~A~%Baseline source revision: ~A~%"
                        (lisp-implementation-version) (software-type)
                        (machine-type) (machine-version) *baseline-revision*)
                (format t "Protocol: one SBCL process; deterministic xorshift inputs; full GC before every timed call; order alternates within pairs; every pair checks SHA parity, DEFLATE round trips through both decoders, and inflate output/consumed length parity; no timing threshold.~%")
                (%assert-inflate-result
                 (multiple-value-list (funcall current-inflate shared-compressed))
                 codec-input shared-compressed)
                (%assert-sha-parity (list (funcall old-sha sha-input))
                                    (list expected-sha) expected-sha)
                (%run-workload
                 "SHA-256" (length sha-input)
                 (lambda () (funcall old-sha sha-input))
                 (lambda () (funcall current-sha sha-input))
                 (lambda (old current)
                   (%assert-sha-parity old current expected-sha)))
                (%run-workload
                 "deflate" (length codec-input)
                 (lambda () (funcall old-deflate codec-input))
                 (lambda () (funcall current-deflate codec-input))
                 (lambda (old current)
                   (%assert-deflate-parity old current codec-input
                                           old-inflate current-inflate)))
                (%run-workload
                 "inflate (2 MiB decoded)" (length codec-input)
                 (lambda () (funcall old-inflate shared-compressed))
                 (lambda () (funcall current-inflate shared-compressed))
                 (lambda (old current)
                   (%assert-inflate-parity old current codec-input
                                           shared-compressed))))))
          (when (probe-file temporary)
            (let ((directory-name (namestring temporary)))
              (uiop:delete-directory-tree
               temporary
               :validate
               (lambda (pathname)
                 (let ((pathname-name (namestring pathname)))
                   (and (>= (length pathname-name) (length directory-name))
                        (string= directory-name pathname-name
                                 :end2 (length directory-name)))))
               :if-does-not-exist :ignore)))))))

(defun %output-signature (values)
  (let* ((output (first values))
         (signature
          (cond
           ((stringp output)
            (format nil "string:~D:~A" (length output) output))
           ((typep output '(vector (unsigned-byte 8)))
            (format nil "octets:~D:sha256=~A"
                    (length output)
                    (funcall *signature-hash-function* output)))
           (t
            (error "Unsupported benchmark output type: ~A"
                   (type-of output)))))
         (secondary (second values)))
    (if (numberp secondary)
        (format nil "~A;value2=~D" signature secondary)
      signature)))

(defun %median-interval (values)
  (let* ((ordered (sort (copy-list values) #'<))
         (count (length ordered))
         (lower-rank 0))
    (loop for candidate from 1 to (floor count 2)
          for lower-tail =
          (loop with term = (expt 0.5d0 count)
                with total = 0.0d0
                for index from 0 below candidate
                do (incf total term)
                (setf term
                      (* term (/ (- count index) (1+ index))))
                finally (return total))
          while (<= (* 2.0d0 lower-tail) 0.05d0)
          do (setf lower-rank candidate))
    (if (zerop lower-rank)
        (values nil nil nil nil)
      (let ((upper-rank (1+ (- count lower-rank))))
        (values (nth (1- lower-rank) ordered)
                (nth (1- upper-rank) ordered)
                lower-rank upper-rank)))))

(defun %parse-command-line-options ()
  (labels ((parse-count (option value)
                        (multiple-value-bind (count end)
                            (parse-integer value :junk-allowed t)
                          (unless (and count (= end (length value)) (plusp count))
                            (error "~A expects a positive integer, got ~S."
                                   option value))
                          count)))
    (let ((arguments (uiop:command-line-arguments)))
      (when (equal (first arguments) "--")
        (pop arguments))
      (loop while arguments
            do (let ((option (pop arguments)))
                 (unless arguments
                   (error "Missing value for benchmark option ~A." option))
                 (let ((value (pop arguments)))
                   (cond
                    ((string= option "--mode")
                     (setf *benchmark-mode*
                           (cond ((string= value "compare") :compare)
                                 ((string= value "self") :self)
                                 (t (error "Unknown benchmark mode: ~A" value)))))
                    ((string= option "--self-arm")
                     (setf *self-arm*
                           (cond ((string= value "old") :old)
                                 ((string= value "current") :current)
                                 (t (error "Unknown self-comparison arm: ~A"
                                           value)))))
                    ((string= option "--warmup-pairs")
                     (setf *warmup-pairs* (parse-count option value)))
                    ((string= option "--sample-pairs")
                     (setf *sample-pairs* (parse-count option value)))
                    (t (error "Unknown benchmark option: ~A" option))))))
      (values *benchmark-mode* *self-arm*))))

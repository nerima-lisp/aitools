(require :asdf)

(defparameter *f0-revision* "1b20c3abdea1d95a049cdb0481afcfd72072b2a4")
(defparameter *warmup-pairs* 2)
(defparameter *sample-pairs* 15)

(defun %project-root ()
  (let ((root (uiop:ensure-directory-pathname (uiop:getcwd))))
    (unless (probe-file (merge-pathnames "aitools.asd" root))
      (error "Run this benchmark from the aitools project root."))
    root))
(defun %temporary-directory (root)
  (let ((directory
          (merge-pathnames
           (format nil ".u11-benchmark-~36R-~36R/"
                   (get-universal-time) (random (expt 36 10)))
           root)))
    (when (probe-file directory)
      (error "Benchmark temporary directory already exists: ~A" directory))
    (ensure-directories-exist (merge-pathnames "source.lisp" directory))
    directory))

(defun %extract-f0-source (root directory relative-path name)
  (let* ((source (merge-pathnames (format nil "~A.lisp" name) directory))
         (fasl (merge-pathnames (format nil "~A.fasl" name) directory))
         (text (uiop:run-program
                (list "jj" "file" "show" "--ignore-working-copy"
                      "-r" *f0-revision* relative-path)
                :directory root :output :string :error-output :string)))
    (unless (plusp (length text))
      (error "Historical source is empty: ~A" relative-path))
    (with-open-file (stream source :direction :output :if-exists :error
                                    :if-does-not-exist :create)
      (write-string text stream))
    (multiple-value-bind (compiled warnings-p failure-p)
        (compile-file source :output-file fasl)
      (declare (ignore warnings-p))
      (when failure-p
        (error "Could not compile F0 source ~A" relative-path))
      (load compiled))))

(defun %make-input (size seed)
  (let ((input (make-array size :element-type '(unsigned-byte 8)))
        (state seed))
    (loop for index below size
          do (setf state (logand #xffffffff (logxor state (ash state 13)))
                   state (logxor state (ash state -17))
                   state (logand #xffffffff (logxor state (ash state 5)))
                   (aref input index) (ldb (byte 8 0) state)))
    input))

(defun %timed-invoke (function)
  (sb-ext:gc :full t)
  (let ((start (get-internal-real-time)))
    (let ((values (multiple-value-list (funcall function))))
      (values (* 1000.0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))
              values))))

(defun %paired-invoke (old-function current-function old-first-p)
  (if old-first-p
      (multiple-value-bind (old-time old-values) (%timed-invoke old-function)
        (multiple-value-bind (current-time current-values)
            (%timed-invoke current-function)
          (values old-time old-values current-time current-values)))
      (multiple-value-bind (current-time current-values)
          (%timed-invoke current-function)
        (multiple-value-bind (old-time old-values) (%timed-invoke old-function)
          (values old-time old-values current-time current-values)))))
(defun %median (values)
  (nth (floor (length values) 2) (sort (copy-list values) #'<)))
(defun %relative-change-percent (old-time current-time)
  (unless (plusp old-time)
    (error "Old-arm elapsed time is zero; cannot compute a paired difference."))
  (* 100.0 (/ (- old-time current-time) old-time)))

(defun %run-workload (name input-size old-function current-function validator)
  (format t "~&Workload: ~A; input=~:D bytes; warmup pairs=~D; sample pairs=~D; order alternates; full GC before each call.~%"
          name input-size *warmup-pairs* *sample-pairs*)
  (dotimes (index *warmup-pairs*)
    (multiple-value-bind (old-time old-values current-time current-values)
        (%paired-invoke old-function current-function (evenp index))
      (declare (ignore old-time current-time))
      (funcall validator old-values current-values)))
  (let ((old-times nil)
        (current-times nil)
        (relative-changes nil)
        (current-wins 0))
    (dotimes (index *sample-pairs*)
      (multiple-value-bind (old-time old-values current-time current-values)
          (%paired-invoke old-function current-function (evenp (+ *warmup-pairs* index)))
        (funcall validator old-values current-values)
        (let ((relative (%relative-change-percent old-time current-time)))
          (push old-time old-times)
          (push current-time current-times)
          (push relative relative-changes)
          (when (< current-time old-time)
            (incf current-wins))
          (format t "  pair ~D order=~A old=~,3F ms current=~,3F ms relative=~,2F%~%"
                  (1+ index)
                  (if (evenp (+ *warmup-pairs* index)) "old/current" "current/old")
                  old-time current-time relative))))
    (setf old-times (nreverse old-times)
          current-times (nreverse current-times)
          relative-changes (nreverse relative-changes))
    (format t "  medians old=~,3F ms current=~,3F ms; paired median change=~,2F% (positive means current faster); current faster in ~D/~D pairs.~%"
            (%median old-times) (%median current-times)
            (%median relative-changes) current-wins *sample-pairs*)))

(defun %assert-sha-parity (old-values current-values expected)
  (let ((old-hash (first old-values))
        (current-hash (first current-values)))
    (unless (and (stringp old-hash) (stringp current-hash) (string= old-hash current-hash)
                 (string= current-hash expected))
      (error "SHA-256 outputs differ or failed the reference check."))))
(defun %assert-inflate-result (values expected encoded)
  (let ((output (first values))
        (consumed (second values)))
    (unless (and (equalp output expected) (eql consumed (length encoded)))
      (error "Inflate output or consumed length is incorrect."))))
(defun %assert-deflate-parity (old-values current-values input old-inflate current-inflate)
  (dolist (encoded (list (first old-values) (first current-values)))
    (unless (typep encoded '(simple-array (unsigned-byte 8) (*)))
      (error "Deflate did not return an octet vector."))
    (%assert-inflate-result (multiple-value-list (funcall old-inflate encoded)) input encoded)
    (%assert-inflate-result (multiple-value-list (funcall current-inflate encoded))
                            input encoded)))
(defun %assert-inflate-parity (old-values current-values input encoded)
  (%assert-inflate-result old-values input encoded)
  (%assert-inflate-result current-values input encoded)
  (unless (equalp (first old-values) (first current-values)) (error "Old and current inflate outputs differ.")))

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
                    (expected-sha (funcall current-sha sha-input))
                    (shared-compressed (funcall current-deflate codec-input)))
               (%extract-f0-source root temporary
                                   "packages/core/kernel/src/domain/digest.lisp"
                                   "f0-digest")
               (%extract-f0-source root temporary
                                   "packages/core/text/src/domain/codec-deflate.lisp"
                                   "f0-codec-deflate")
               (let* ((old-sha
                        (symbol-function (find-symbol "SHA256-HEX" "AITOOLS.KERNEL.DOMAIN")))
                      (old-deflate
                        (symbol-function (find-symbol "DEFLATE" "AITOOLS.TEXT.DOMAIN")))
                      (old-inflate
                        (symbol-function (find-symbol "INFLATE" "AITOOLS.TEXT.DOMAIN"))))
                 (format t "~&SBCL: ~A~%Platform: ~A / ~A / ~A~%F0 source revision: ~A~%"
                         (lisp-implementation-version) (software-type)
                         (machine-type) (machine-version) *f0-revision*)
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

(run-digest-deflate-adapter-benchmark)

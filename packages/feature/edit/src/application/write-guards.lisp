;;;; packages/feature/edit/src/application/write-guards.lisp
;;;;
;;;; The checks write-runtime.lisp runs around a plan: the
;;;; redaction-marker refusal for inputs and for the bytes about to be
;;;; written, parsing of --expect-count, --expect-hash and --lock-timeout,
;;;; the workspace boundary for targets and for paths a plan discovers itself,
;;;; and the change-detection hash --expect-hash is compared with.
(in-package #:aitools.edit.application)

(defparameter +redaction-placeholder+
  "[REDACTED_SECRET]")

(defun %contains-placeholder-p (input)
  (if (stringp input)
      (search +redaction-placeholder+ input)
      (search (aitools.text.domain:encode-utf8 +redaction-placeholder+) input)))

(defun %redacted-output-path (view requests)
  "The path of the first :WRITE request whose new bytes contain the
redaction marker while the file's current bytes do not. A marker in the raw
input is refused before planning (%CONTAINS-PLACEHOLDER-P); this catches one
introduced by decoding a JSON string, expanding a replacement template, or
transcoding, by comparing the bytes actually about to be written with the
bytes already there (an absent file counts as having none)."
  (let ((marker (aitools.text.domain:encode-utf8 +redaction-placeholder+)))
    (dolist (request requests)
      (when (eq (aitools.store.domain:change-request-op request) :write)
        (let ((new (aitools.store.domain:change-request-content request)))
          (when (and new (search marker new))
            (let ((old
                   (aitools.store.application:view-read-file view (aitools.store.domain:change-request-path request))))
              (unless (and old (search marker old))
                (return (aitools.store.domain:change-request-path request))))))))))

(defun parse-count (text)
  "TEXT as a non-negative integer, or NIL."
  (and (stringp text)
       (plusp (length text))
       (every
        (lambda (c)
          (char<= #\0 c #\9))
        text)
       (parse-integer text)))

(defun %parse-expect-hashes (arguments)
  "(values entries error-message)"
  (handler-case (values (mapcar #'aitools.kernel.domain:parse-expect-hash-argument arguments) nil)
    (error (condition)
      (values nil (princ-to-string condition)))))

(defun %lock-timeout-ms (text)
  "(values milliseconds valid-p) for the global --lock-timeout text."
  (if (null text)
      (values aitools.store.application:+default-lock-timeout-ms+ t)
      (handler-case (values (aitools.kernel.domain:duration-milliseconds (aitools.kernel.domain:parse-duration text)) t)
        (error ()
          (values nil nil)))))

(aitools.kernel.domain:define-flow
  (%boundary/k host root target temporary-root follow &key on-inside on-outside)
  "Check whether absolute TARGET is inside the workspace. ON-INSIDE receives
(real-path relative-path verdict), matching CALL-WITH-WORKSPACE-BOUNDARY/K.
With FOLLOW NIL, the last component is not resolved, so a symlink itself is
the write target."
  (let* ((lexical
          (aitools.workspace.domain:normalize-path
           (aitools.workspace.domain:join-path (aitools.workspace.application:workspace-root-path root) target)))
         (base (aitools.workspace.domain:path-basename lexical))
         (parent (aitools.workspace.domain:path-parent lexical)))
    (cond
      ((or follow (null parent) (member base (quote ("" "." "..")) :test (function string=)))
       (aitools.workspace.application:call-with-workspace-boundary/k host
                                                                     root
                                                                     target
                                                                     :temporary-root
                                                                     temporary-root
                                                                     :on-inside
                                                                     (lambda (path verdict)
                                                                       (aitools.kernel.domain:flow-continue
                                                                         :on-inside
                                                                         (aitools.kernel.domain:workspace-path-real
                                                                          path)
                                                                         (aitools.kernel.domain:workspace-path-relative
                                                                          path)
                                                                         verdict))
                                                                     :on-outside
                                                                     (lambda (verdict outside-lexical real)
                                                                       (aitools.kernel.domain:flow-continue
                                                                         :on-outside
                                                                         verdict
                                                                         outside-lexical
                                                                         real))))
      ((and temporary-root (equal (aitools.workspace.application:resolve-real-path host parent) temporary-root))
       (let ((real (aitools.workspace.domain:join-path temporary-root base)))
         (aitools.kernel.domain:flow-continue
           :on-inside
           real
           real
           :temporary)))
      (t
       (aitools.workspace.application:call-with-workspace-boundary/k host
                                                                     root
                                                                     parent
                                                                     :temporary-root
                                                                     temporary-root
                                                                     :on-inside
                                                                     (lambda (path verdict)
                                                                       (if (aitools.workspace.domain:git-metadata-name-p
                                                                            base)
                                                                           (aitools.kernel.domain:flow-continue
                                                                             :on-outside
                                                                             :git-directory
                                                                             lexical
                                                                             nil)
                                                                   (let ((relative
                                                                          (aitools.kernel.domain:workspace-path-relative
                                                                           path)))
                                                                             (aitools.kernel.domain:flow-continue
                                                                               :on-inside
                                                                             (aitools.workspace.domain:join-path
                                                                              (aitools.kernel.domain:workspace-path-real
                                                                               path)
                                                                              base)
                                                                               (if (eq verdict :temporary)
                                                                                   (aitools.workspace.domain:join-path
                                                                                    relative
                                                                                    base)
                                                                                   (if (zerop (length relative))
                                                                                       base
                                                                                       (concatenate (quote string)
                                                                                                    relative
                                                                                                    "/"
                                                                                                    base)))
                                                                               verdict))))
                                                                     :on-outside
                                                                     (lambda (verdict outside-lexical real)
                                                                       (aitools.kernel.domain:flow-continue
                                                                         :on-outside
                                                                         verdict
                                                                         outside-lexical
                                                                         real)))))))

(defun %outside-message (verdict target)
  (format nil
          "~A is outside the workspace (~A)"
          target
          (ecase verdict
            (:outside-root "outside the root")
            (:symlink-escape "a symlink leads outside the root")
            (:git-directory "inside .git")
            (:unresolvable "a symlink loop"))))

(aitools.kernel.domain:define-flow
  (resolve-extra-path/k context path &key (follow t) (base :root) on-inside on-outside)
  "The workspace boundary check for a path a plan discovers itself (a symlink target, a copy's
children): ON-INSIDE (relative) or ON-OUTSIDE (message). BASE is as for
MAKE-WRITE-TARGET, :ROOT by default; :CWD for a path the user typed (an
archive to read, split's --prefix). Only live runs call it: none of the
commands a `tx rebase` replays (+REPLAYABLE-COMMANDS+) discovers paths."
  (aitools.kernel.domain:flow-call
    (%boundary/k (write-context-host context)
                 (write-context-root context)
                 (%target-absolute (write-context-host context) (write-context-root context) path base)
                 (write-context-temporary-root context)
                 follow)
    (:on-inside (real relative verdict)
                (declare (ignore real))
                (if (eq verdict :inside)
                    (aitools.kernel.domain:flow-continue
                      :on-inside
                      relative)
                    (aitools.kernel.domain:flow-continue
                      :on-outside
                      (format nil "~A is in the mktemp area, not the workspace" path))))
    (:on-outside (verdict lexical real)
                 (declare (ignore real))
                 (aitools.kernel.domain:flow-continue
                   :on-outside
                   (%outside-message verdict lexical)))))

(defun view-hash (view path host)
  "The change-detection hash of PATH in VIEW, by the rule `info` reports
(AITOOLS.STORE.APPLICATION:PATH-HASH): a file's content hash; for a
symlink, the content hash of the regular file it leads to (followed through
HOST, the workspace host), else of its target text; NIL for a directory or
an absent path."
  (let ((state (aitools.store.application:view-path-state view path)))
    (case (aitools.store.domain:entry-state-kind state)
      (:file (aitools.store.domain:entry-state-hash state))
      (:symlink
       (let ((root (aitools.store.application:store-root (aitools.store.application:store-view-store view))))
         (aitools.store.application:path-hash host root view (aitools.workspace.domain:join-path root path))))
      (t nil))))

(defun %hash-conflict (path expected actual)
  (aitools.protocol.domain:json-object-from-alist
   (list (cons "path" path) (cons "kind" "write") (cons "base" expected) (cons "current" (or actual (json-null))))))

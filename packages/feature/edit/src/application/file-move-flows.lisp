(in-package #:aitools.edit.application)

(defun %per-path-move-requests (context source destination)
  "A directory move inside a tx, recorded per path: recreate every
entry below DESTINATION, then delete the originals, children first."
  (let ((creates (list (aitools.store.domain:mkdir-request destination)))
        (deletes (list (aitools.store.domain:delete-request source)))
        (view (write-context-view context)))
    (%walk-tree context source
                (lambda (path kind)
                  (let ((target (%replace-rebase path source destination))
                        (state (aitools.store.application:view-path-state view path)))
                    (push (aitools.store.domain:delete-request path) deletes)
                    (push (ecase kind
                            (:directory (aitools.store.domain:mkdir-request target))
                            (:file (aitools.store.domain:write-file-request
                                    target (aitools.store.application:view-read-file view path)
                                    :mode (aitools.store.domain:entry-state-mode state)))
                            (:symlink (aitools.store.domain:symlink-request
                                       target (aitools.store.domain:entry-state-target state))))
                          creates))))
    (append (nreverse creates) deletes)))

(define-write-command "move" (ports env positionals options on-plan fail)
  (if (/= (length positionals) 2)
      (funcall fail "argument.invalid" "move takes SRC and DST")
      (funcall on-plan
               (make-write-plan
                :command "move"
                :targets (list (make-write-target (first positionals) :follow nil)
                               (make-write-target (second positionals) :follow nil))
                :expect-hashes (getf options :expect-hash)
                :plan (lambda (context commit reject)
                        (let* ((source (context-path context 0))
                               (destination (context-path context 1))
                               (kind (%kind context source)))
                          (if (eq kind :absent)
                              (%not-found context source reject)
                              (%destination-check/k
                               context destination kind (getf options :overwrite) reject
                               (lambda ()
                                 (funcall commit
                                          (if (and (eq kind :directory) (write-context-tx context))
                                              (%per-path-move-requests context source destination)
                                              (list (aitools.store.domain:move-request source destination)))))))))
                :record-options options
                :record-positionals (lambda (paths) paths)))))

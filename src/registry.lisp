;;;; src/registry.lisp
;;;;
;;;; Turns the protocol command registry into cl-cli's app command list. The
;;;; registry itself lives in AITOOLS.PROTOCOL.APPLICATION so presentation
;;;; layers can register without referencing this package; this file is the
;;;; one place that interprets registered cli-command values as cl-cli
;;;; commands, wrapping same-group commands (`json get`, `json set`, ...) into
;;;; one parent cl-cli command per group.
(in-package #:aitools/cli)

(defun finalize-app-commands (registry)
  "Build top-level cl-cli commands, recursively nesting every registered group
path such as `json` or `org.todo`."
  (let ((groups (command-registry-group-commands registry)))
    (labels ((direct-child-groups (parent)
               (let ((prefix (concatenate 'string parent ".")))
                 (sort
                  (loop for candidate being the hash-keys of groups
                        when (and (> (length candidate) (length prefix))
                                  (string= prefix candidate :end2 (length prefix))
                                  (not (position #\. candidate :start (length prefix))))
                          collect candidate)
                  #'string<)))
             (compose-group (path name)
               (make-command
                :name name
                :subcommands
                (append
                 (reverse (gethash path groups))
                 (mapcar (lambda (child)
                           (compose-group
                            child
                            (car (last (uiop:split-string child :separator ".")))))
                         (direct-child-groups path))))))
      (append
       (reverse (command-registry-top-level registry))
       (loop for root in (sort (loop for group being the hash-keys of groups
                                     when (not (position #\. group))
                                       collect group)
                               #'string<)
             collect (compose-group root root))))))

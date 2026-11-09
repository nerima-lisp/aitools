;;;; The Git repository entries that can lie above a linked-worktree root.
;;;; The pinned Git version is 2.55.0, selected by this flake.
;;;; Repository entries, including nested worktree metadata, follow
;;;; gitrepository-layout(5):
;;;; https://github.com/git/git/blob/v2.55.0/Documentation/gitrepository-layout.adoc
;;;; Shared common-directory entries follow `common_list` in:
;;;; https://github.com/git/git/blob/v2.55.0/path.c
;;;; Git root refs follow `is_root_ref` in:
;;;; https://github.com/git/git/blob/v2.55.0/refs.c
;;;; The `sharedindex.*` and root-level `*_HEAD` patterns are classified by
;;;; packages/core/workspace/src/domain/boundary.lisp. Reftable and `description` are also
;;;; repository entries. The extension stores lfs and annex are included
;;;; alongside Git's svn store.
(in-package #:aitools.data)

(defparameter *git-common-root-metadata-names*
  '("objects" "refs"
              "packed-refs"
              "HEAD"
              "config"
              "config.worktree"
              "branches"
              "hooks"
              "common"
              "index"
              "info"
              "remotes"
              "logs"
              "shallow"
              "commondir"
              "modules"
              "worktrees"
              "gitdir"
              "locked"
              "description"
              "lost-found"
              "gc.pid"
              "rr-cache"
              "reftable"
              "lfs"
              "annex"
              "svn"
              ".git"
              "AUTO_MERGE"
              "BISECT_EXPECTED_REV"
              "NOTES_MERGE_PARTIAL"
              "NOTES_MERGE_REF"
              "MERGE_AUTOSTASH"))

(export '*git-common-root-metadata-names*)

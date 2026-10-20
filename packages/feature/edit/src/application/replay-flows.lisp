(in-package #:aitools.edit.application)

(defparameter +replayable-commands+
  '("edit" "insert" "replace" "apply" "transform" "move-lines"
    "json.set" "json.delete" "json.merge" "json.patch" "json.fmt")
  "The commands whose ops `tx rebase` may re-run: the ones registered with the
journal below, and the only ones REPLAY-EDIT-OP prepares without ports or a
workspace env.")

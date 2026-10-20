(in-package #:aitools.data)

(defparameter *store-action-names*
  '((:created . "created") (:modified . "modified")
    (:deleted . "deleted")
    (:moved . "moved")
    (:mode-changed . "mode-changed")
    (:linked . "linked")))

(export '(*store-action-names*))

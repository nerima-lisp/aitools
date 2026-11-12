(in-package #:aitools.store.application)

(progn
(defun %archive-prefix-finish-cursor (cursor close-directory)
  (unless (%archive-prefix-cursor-finished-p cursor)
    (let ((fd (%archive-prefix-cursor-current-fd cursor)))
      (unwind-protect
           (when fd (funcall close-directory fd))
        (setf (%archive-prefix-cursor-current-fd cursor) nil
              (%archive-prefix-cursor-stack cursor) nil
              (%archive-prefix-cursor-finished-p cursor) t))))
  nil)
(defun %archive-prefix-start (template open-entry close-directory)
  (let ((fd nil)
        (cursor nil)
        (started nil))
    (unwind-protect
         (progn
           (multiple-value-bind (kind opened identity)
               (funcall open-entry (%archive-prefix-cursor-current-fd template) "." ".")
             (setf fd opened)
             (unless (and (eq kind :directory)
                          (equal identity (%archive-prefix-cursor-current-identity template)))
               (error 'store-io-error :operation "open" :path "."
                                      :detail "directory cursor identity changed"))
             (setf cursor
                   (make-%archive-prefix-cursor
                    :current-fd fd
                    :current-identity identity
                    :folded-node (%archive-prefix-cursor-folded-node template)
                    :stage-node (%archive-prefix-cursor-stage-node template)
                    :symlink-p (%archive-prefix-cursor-symlink-p template)
                    :disk-live-p (%archive-prefix-cursor-disk-live-p template)
                    :stack (%archive-prefix-cursor-stack template)))
             (setf fd nil)
             (fault-point :archive-prefix-handoff)
             (setf started t))
           cursor)
      (unless started
        (if cursor
            (%archive-prefix-finish-cursor cursor close-directory)
            (when fd (funcall close-directory fd)))))))
(defun %archive-prefix-enter (cursor component open-entry close-directory deletions-visible)
  (let* ((folded-node (%archive-prefix-cursor-folded-node cursor))
         (stage-node (%archive-prefix-cursor-stage-node cursor))
         (folded-child
           (and folded-node
                (gethash (%archive-symlink-component-key component) folded-node)))
         (stage-child (and stage-node (gethash component stage-node)))
         (stage-present-p (and stage-child (nth-value 1 (gethash :kind stage-child))))
         (stage-kind (and stage-present-p (gethash :kind stage-child)))
         (frame
           (make-%archive-prefix-frame
            :folded-node folded-node
            :stage-node stage-node
            :symlink-p (%archive-prefix-cursor-symlink-p cursor)
            :disk-live-p (%archive-prefix-cursor-disk-live-p cursor)
            :parent-identity (%archive-prefix-cursor-current-identity cursor))))
    (push frame (%archive-prefix-cursor-stack cursor))
    (setf (%archive-prefix-cursor-folded-node cursor) folded-child
          (%archive-prefix-cursor-stage-node cursor) stage-child
          (%archive-prefix-cursor-symlink-p cursor)
          (or (%archive-prefix-cursor-symlink-p cursor)
              (and folded-child (gethash :symlink folded-child))
              (eq stage-kind :symlink)))
    (cond
      ((%archive-prefix-cursor-symlink-p cursor)
       (setf (%archive-prefix-cursor-disk-live-p cursor) nil))
      ((%archive-prefix-cursor-disk-live-p cursor)
       (multiple-value-bind (kind next-fd identity)
           (funcall open-entry
                    (%archive-prefix-cursor-current-fd cursor)
                    component
                    component)
         (cond
           ((eq kind :directory)
            (let ((previous-fd (%archive-prefix-cursor-current-fd cursor)))
              (setf (%archive-prefix-frame-moved-p frame) t
                    (%archive-prefix-cursor-current-fd cursor) next-fd
                    (%archive-prefix-cursor-current-identity cursor) identity)
              (funcall close-directory previous-fd)))
           ((eq kind :symlink)
            (setf (%archive-prefix-cursor-disk-live-p cursor) nil)
            (unless (and stage-present-p
                         (not (eq stage-kind :directory))
                         (or (not deletions-visible)
                             (not (eq stage-kind :absent))))
              (setf (%archive-prefix-cursor-symlink-p cursor) t)))
           (t
            (setf (%archive-prefix-cursor-disk-live-p cursor) nil)))))
      (t
       (setf (%archive-prefix-cursor-disk-live-p cursor) nil)))
    (values cursor
            (if (%archive-prefix-cursor-symlink-p cursor) :symlink :clear))))
(defun %archive-prefix-parent (cursor open-entry close-directory)
  (let ((frame (car (%archive-prefix-cursor-stack cursor))))
    (when (or (null frame) (%archive-prefix-cursor-symlink-p cursor))
      (return-from %archive-prefix-parent (values cursor :outside)))
    (when (%archive-prefix-frame-moved-p frame)
      (multiple-value-bind (kind parent-fd identity)
          (funcall open-entry (%archive-prefix-cursor-current-fd cursor) ".." "..")
        (unless (and (eq kind :directory)
                     parent-fd
                     (equal identity (%archive-prefix-frame-parent-identity frame)))
          (when parent-fd (funcall close-directory parent-fd))
          (return-from %archive-prefix-parent (values cursor :outside)))
        (let ((current-fd (%archive-prefix-cursor-current-fd cursor)))
          (unwind-protect
               (progn
                 (funcall close-directory current-fd)
                 (setf (%archive-prefix-cursor-current-fd cursor) parent-fd
                       (%archive-prefix-cursor-current-identity cursor) identity
                       parent-fd nil))
            (when parent-fd (funcall close-directory parent-fd))))))
    (pop (%archive-prefix-cursor-stack cursor))
    (setf (%archive-prefix-cursor-folded-node cursor)
          (%archive-prefix-frame-folded-node frame)
          (%archive-prefix-cursor-stage-node cursor)
          (%archive-prefix-frame-stage-node frame)
          (%archive-prefix-cursor-symlink-p cursor)
          (%archive-prefix-frame-symlink-p frame)
          (%archive-prefix-cursor-disk-live-p cursor)
          (%archive-prefix-frame-disk-live-p frame))
    (values cursor :clear))))

(progn
  (defstruct (%archive-prefix-frame (:copier nil))
    folded-node
    stage-node
    (symlink-p nil :type boolean)
    (disk-live-p t :type boolean)
    parent-identity
    (moved-p nil :type boolean))
  (defstruct (%archive-prefix-cursor (:copier nil))
    (current-fd nil :type (or null integer))
    (current-identity nil :type (or null string))
    folded-node
    stage-node
    (symlink-p nil :type boolean)
    (disk-live-p t :type boolean)
    (stack nil)
    (finished-p nil :type boolean)))

(progn
  (defun %archive-prefix-add-path (root path)
    (let ((node root))
      (dolist (component (aitools.workspace.domain:path-components path))
        (unless (string= component ".")
          (setf node (%archive-prefix-node node component))))
      (setf (gethash :symlink node) t)))
  (defun %archive-stage-node (node component)
    (let ((child (gethash component node)))
      (or child
          (setf (gethash component node) (make-hash-table :test 'equal)))))
  (defun %archive-stage-add-path (root path kind)
    (let ((node root))
      (dolist (component (aitools.workspace.domain:path-components path))
        (unless (string= component ".")
          (setf node (%archive-stage-node node component))))
      (setf (gethash :kind node) kind))))

(defun %archive-prefix-node (node component)
  (let* ((key (%archive-symlink-component-key component))
         (child (gethash key node)))
    (or child
        (setf (gethash key node) (make-hash-table :test 'equal)))))

(defun %archive-symlink-component-key (component)
  (aitools.text.domain:normalize-text
   (aitools.text.domain:casefold-text
    (aitools.text.domain:normalize-text component :nfd))
   :nfd))

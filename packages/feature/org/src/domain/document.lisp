(in-package #:aitools.org.domain)

(defstruct (org-document (:constructor make-org-document
                            (source-name octets engine-value))
                         (:copier nil))
  (source-name nil :read-only t)
  (octets #() :read-only t)
  (engine-value nil :read-only t))

(defstruct (org-failure (:constructor make-org-failure
                           (&key kind message line column offset))
                        (:copier nil))
  (kind nil :read-only t)
  (message "Org operation failed" :type string :read-only t)
  (line nil :type (or null integer) :read-only t)
  (column nil :type (or null integer) :read-only t)
  (offset nil :type (or null integer) :read-only t))

(defstruct (org-outline-entry (:constructor make-org-outline-entry
                                  (&key kind name level line))
                              (:copier nil) (:predicate nil))
  (kind nil :read-only t)
  (name "" :type string :read-only t)
  (level nil :type (or null integer) :read-only t)
  (line 1 :type integer :read-only t))

(defstruct (org-heading-path-entry (:constructor make-org-heading-path-entry
                                         (&key title index))
                                   (:copier nil) (:predicate nil))
  (title "" :type string :read-only t)
  (index 1 :type integer :read-only t))

(defstruct (org-query-match (:constructor make-org-query-match
                                 (&key line level title todo tags heading-path))
                            (:copier nil) (:predicate nil))
  (line 1 :type integer :read-only t)
  (level 1 :type integer :read-only t)
  (title "" :type string :read-only t)
  (todo nil :type (or null string) :read-only t)
  (tags '() :type list :read-only t)
  (heading-path '() :type list :read-only t))

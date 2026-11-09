(in-package #:aitools.md.domain)

(defstruct (md-heading (:constructor make-md-heading (section heading level line start-char end-char)) (:copier nil) (:predicate nil))
  (section 0 :type integer :read-only t)
  (heading "" :type string :read-only t)
  (level 0 :type integer :read-only t)
  (line 1 :type integer :read-only t)
  (start-char 0 :type integer :read-only t)
  (end-char 0 :type integer :read-only t))

(defstruct (md-document (:constructor make-md-document (source octets headings)) (:copier nil) (:predicate nil))
  (source "" :type string :read-only t)
  (octets #() :type vector :read-only t)
  (headings nil :type list :read-only t))

(defun md-document-heading (document section)
  (and (integerp section)
       (plusp section)
       (find section (md-document-headings document) :key (function md-heading-section) :test (function eql))))

(in-package #:aitools.data)

(defparameter *edit-option-groups*
  `((:write
     (:key :dry-run :name "dry-run" :kind :flag :description "Validate and show the diff; write nothing.")
     (:key :tx :name "tx" :kind :value :description "Stage the write in this tx instead of the working tree."))
    (:hash
     (:key :expect-hash :name "expect-hash" :kind :multi
      :description "Guard: HASH (the target) or PATH=HASH; refusal.target-changed when it differs. The hash is the one `aitools info PATH` reports: SHA-256 of the regular file the path leads to (symlinks followed), or for a symlink leading to no regular file, SHA-256 of its target text."))
    (:count
     (:key :expect-count :name "expect-count" :kind :value
      :description "Guard: the number of selected lines or replacements."))
    (:selectors
     ,@(mapcar (lambda (option)
                 (list :key (getf option :key) :name (getf option :name)
                       :kind (getf option :kind) :description (getf option :description)))
               *selector-options*))
    (:content
     (:key :content :name "content" :kind :value :description "Content text.")
     (:key :content-file :name "content-file" :kind :value :description "Content read as bytes from this file."))
    (:content-multi
     (:key :content :name "content" :kind :multi :description "Content text; repeat to concatenate.")
     (:key :content-file :name "content-file" :kind :multi :description "Content bytes from this file; repeat to concatenate."))
    (:stdin
     (:key :stdin :name "stdin" :kind :flag :description "Read the input from standard input (never read otherwise).")
     (:key :stdin-data :name "stdin-data" :kind :value
      :description "The --stdin input given inline; history records --stdin writes this way."))
    (:scan
     (:key :glob :name "glob" :kind :multi :description "Only paths matching this glob; repeatable.")
     (:key :lang :name "lang" :kind :value :description "Only files of this language.")
     (:key :no-ignore :name "no-ignore" :kind :flag :description "Include ignored files.")
     (:key :skip-larger-than :name "skip-larger-than" :kind :value :default "10MiB" :description "Skip larger files.")
     (:key :newer :name "newer" :kind :value :description "Only files newer than PATH or a duration ago.")))
  "The shared option groups a command spec's :INCLUDE names; the application
expands each into the command's option list. The :SELECTORS group is spliced
from *SELECTOR-OPTIONS* so the five selectors stay defined once.")

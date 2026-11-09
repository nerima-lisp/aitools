(in-package #:aitools.md.test)

(describe "Markdown domain heading selectors"
          (it "keeps duplicate headings distinct by one-based ordinal"
              (let* ((first (aitools.md.domain:make-md-heading 1 "Same" 1 2 0 8))
                     (second (aitools.md.domain:make-md-heading 2 "Same" 2 5 20 33))
                     (document (aitools.md.domain:make-md-document "source" #() (list first second))))
                (expect (aitools.md.domain:md-document-heading document 1) :to-be first)
                (expect (aitools.md.domain:md-document-heading document 2) :to-be second)
                (expect (aitools.md.domain:md-document-heading document 0) :to-be nil)
                (expect (aitools.md.domain:md-document-heading document 3) :to-be nil))))

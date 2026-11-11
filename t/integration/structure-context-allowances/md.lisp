(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      '(:context "md"
        :symbol-allowances
        (("md" (:infrastructure) "CL-MARKDOWN-KIT" "DOCUMENT-HEADINGS" "read parser heading records")
         ("md" (:infrastructure) "CL-MARKDOWN-KIT" "HEADING-RECORD-SECTION-SPAN" "locate section source bounds")
         ("md" (:infrastructure) "CL-MARKDOWN-KIT" "HEADING-RECORD-NODE" "read the heading source node")
         ("md" (:infrastructure) "CL-MARKDOWN-KIT" "NODE-SPAN" "read heading source bounds")
         ("md" (:infrastructure) "CL-MARKDOWN-KIT" "HEADING-RECORD-PLAIN-TEXT" "report visible heading text")
         ("md" (:infrastructure) "CL-MARKDOWN-KIT" "HEADING-RECORD-LEVEL" "report heading depth")
         ("md" (:infrastructure) "CL-PARSER-KIT" "SPAN-START-LINE" "report the heading source line")
         ("md" (:infrastructure) "CL-PARSER-KIT" "SPAN-START" "slice bytes at section start")
         ("md" (:infrastructure) "CL-PARSER-KIT" "SPAN-END" "slice bytes at section end")
         ("md" (:infrastructure) "CL-MARKDOWN-KIT" "PARSE-STRING" "parse decoded UTF-8 Markdown text")
         ("md" (:infrastructure) "CL-MARKDOWN-KIT" "DOCUMENT-SOURCE" "preserve parser source text")
         ("md" (:infrastructure) "CL-MARKDOWN-KIT" "MARKDOWN-RESOURCE-LIMIT-ERROR" "map parser resource limits")
         ("md" (:infrastructure) "CL-MARKDOWN-KIT" "MARKDOWN-KIT-ERROR-MESSAGE" "preserve parser error detail")
         ("md" (:infrastructure) "CL-MARKDOWN-KIT" "MARKDOWN-KIT-ERROR" "map Markdown parser errors"))))

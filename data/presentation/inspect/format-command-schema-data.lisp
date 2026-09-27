;;;; data/presentation/inspect/format-command-schema-data.lisp
;;;;
;;;; Schema text for the read side of the `json` and `table` groups.
(in-package #:aitools.data)

(defparameter *inspect-table-read-args*
  '((:name "--format" :type "enum" :choices ("csv" "tsv" "jsonl" "json" "ws" "sep" "lines")
     :description "Table format; default from the extension, then the content (ws, sep, lines are never guessed).")
    (:name "--delimiter" :type "string" :description "With --format sep: the fixed separator string (cut -d).")
    (:name "--pointer" :type "string" :description "With --format json: RFC 6901 pointer to the array of rows.")
    (:name "--no-header" :type "flag" :description "Treat the first row as data; columns are named 1, 2, ...")
    (:name "--ws-columns" :type "integer"
     :description "With --format ws: split into at most N columns, the last keeping the rest of the line (awk's $N). Without it every whitespace run splits.")
    (:name "--where" :type "string" :multiple t
     :description "<column><op><value>, op one of = != < <= > >= ~ (regex); repeat for AND. Columns by name or 1-based number.")
    (:name "--encoding" :type "enum" :choices ("utf-8" "shift_jis" "euc-jp" "iso-8859-1" "utf-16le" "utf-16be")
     :description "Decode from this encoding instead of UTF-8."))
  "Reading options shared by `table read` and `table agg`.")

(defparameter *inspect-format-command-schemas*
  `((:name "json.get"
     :summary "Get the value at a JSON pointer, its keys, length, or raw string."
     :description "RFC 6901 pointers (\"\" is the whole document; ~0 is ~ and ~1 is /). length is the array's elements, the object's keys, or the string's characters. A value whose JSON is longer than --max-bytes comes back as value_preview with truncated:true (status partial, exit 3)."
     :args ((:name "path" :kind "positional" :type "string" :required t :description "JSON file.")
            (:name "pointer" :kind "positional" :type "string" :required t :description "RFC 6901 pointer.")
            (:name "--max-bytes" :type "size" :default "16KiB" :description "Largest rendered value returned.")
            (:name "--keys" :type "flag" :description "Return the object's keys or the array's indexes instead of the value.")
            (:name "--raw" :type "flag" :description "Return text: a string value unescaped, anything else as JSON text."))
     :output-fields ((:name "pointer" :description "The pointer asked for.")
                     (:name "type" :description "object, array, string, number, boolean, or null.")
                     (:name "length" :description "Elements, keys, or characters; null for other types.")
                     (:name "value" :description "The value (default).")
                     (:name "keys" :description "With --keys.")
                     (:name "text" :description "With --raw.")
                     (:name "value_preview" :description "The start of the rendered value when --max-bytes was exceeded.")
                     (:name "truncated" :description "True when --max-bytes cut the value.")
                     (:name "approx_tokens" :description "ceil(characters of the returned value / 4).")
                     (:name "next_commands" :description "Narrower reads when truncated."))
     :error-codes ("argument.invalid" "input.not-found" "input.unsupported-format" "refusal.not-a-file"
                   "environment.busy" "environment.io"))
    (:name "json.select"
     :summary "Filter, sort, and project the elements of a JSON array."
     :description "--where <rel-pointer><op><json-value> (repeat for AND); op is = != < <= > >= or ~ (regex on strings). The right side is JSON when it parses as JSON, else a string. A missing member satisfies only !=. Ordering compares numbers with numbers and strings with strings."
     :args ((:name "path" :kind "positional" :type "string" :required t :description "JSON file.")
            (:name "pointer" :kind "positional" :type "string" :required t :description "Pointer to the array.")
            (:name "--where" :type "string" :multiple t :description "Condition on each element.")
            (:name "--pick" :type "string" :multiple t :description "Return {pointer: value} for these relative pointers instead of the element.")
            (:name "--sort-by" :type "string" :description "Relative pointer to order by (null first, then booleans, numbers, strings).")
            (:name "--desc" :type "flag" :description "Descending order.")
            (:name "--output" :type "enum" :choices ("items" "count") :default "items" :description "Sets mode.")
            (:name "--limit" :type "integer" :default 50 :description "Maximum items returned."))
     :output-fields ((:name "mode" :description "items or count.")
                     (:name "items" :description "[{pointer,value}] of the selected elements.")
                     (:name "count" :description "Selected elements, in count mode.")
                     (:name "total" :description "Selected elements, in items mode.")
                     (:name "truncated" :description "True when --limit cut items (status partial, exit 3)."))
     :error-codes ("argument.invalid" "input.not-found" "input.syntax-error" "input.unsupported-format"
                   "refusal.not-a-file" "environment.busy" "environment.io"))
    (:name "json.diff"
     :summary "List the add, remove, and replace operations between two JSON files."
     :description "Key order and whitespace are ignored; numbers compare by value. Array elements compare by index."
     :args ((:name "a" :kind "positional" :type "string" :required t :description "Old JSON file.")
            (:name "b" :kind "positional" :type "string" :required t :description "New JSON file.")
            (:name "--limit" :type "integer" :default 100 :description "Maximum operations returned."))
     :output-fields ((:name "identical" :description "No differences.")
                     (:name "ops" :description "[{op,pointer,old?,new?}], op add, remove, or replace.")
                     (:name "total" :description "Number of operations.")
                     (:name "truncated" :description "True when --limit cut ops (status partial, exit 3)."))
     :error-codes ("input.not-found" "input.unsupported-format" "refusal.not-a-file" "environment.io"))
    (:name "table.read"
     :summary "Read rows of a CSV, TSV, JSONL, JSON, whitespace, separator, or line table."
     :description "Columns get a type (integer, number, boolean, string, null; mixed for JSON sources). ws splits each line on whitespace runs, awk-style, so rows may differ in width; --ws-columns N bounds the fields and keeps the rest of the line in the last column. A first row of unique non-numeric cells is the header unless --no-header."
     :args ((:name "path" :kind "positional" :type "string" :required t :description "Table file.")
            (:name "--columns" :type "string" :description "Comma-separated column names or 1-based numbers.")
            (:name "--range" :type "string" :description "Rows S:E, S:, or N (1-based, after --where).")
            (:name "--limit" :type "integer" :default 50 :description "Maximum rows returned.")
            ,@*inspect-table-read-args*)
     :output-fields ((:name "format" :description "The format read.")
                     (:name "columns" :description "[{name,type}] of the returned columns.")
                     (:name "start_row" :description "Number of the first returned row.")
                     (:name "rows" :description "Arrays of cell values in column order.")
                     (:name "total_rows" :description "Rows after --where.")
                     (:name "truncated" :description "True when --limit stopped before the range end (status partial, exit 3).")
                     (:name "next_commands" :description "The command reading the next rows."))
     :error-codes ("argument.invalid" "input.not-found" "input.syntax-error" "input.unsupported-format"
                   "refusal.not-a-file" "environment.busy" "environment.io"))
    (:name "table.agg"
     :summary "Group table rows and count, sum, average, or take min, max, or distinct counts."
     :description "Without --group-by all rows form one group. --sum, --avg, --min, --max need a numeric column; other values fail with the offending rows in diagnostics. count is reported with --count or when no other aggregate is asked for. Groups are in key order unless --sort."
     :args ((:name "path" :kind "positional" :type "string" :required t :description "Table file.")
            (:name "--group-by" :type "string" :multiple t :description "Grouping column (repeatable).")
            (:name "--count" :type "flag" :description "Rows per group.")
            (:name "--sum" :type "string" :description "Column to sum.")
            (:name "--avg" :type "string" :description "Column to average.")
            (:name "--min" :type "string" :description "Column to take the minimum of.")
            (:name "--max" :type "string" :description "Column to take the maximum of.")
            (:name "--distinct" :type "string" :description "Column whose distinct values are counted.")
            (:name "--min-count" :type "integer" :description "Only groups with at least this many rows.")
            (:name "--sort" :type "string" :description "Output column to order by: count, sum, avg, min, max, distinct, or a --group-by column.")
            (:name "--desc" :type "flag" :description "Descending order.")
            (:name "--limit" :type "integer" :default 50 :description "Maximum groups returned.")
            ,@*inspect-table-read-args*)
     :output-fields ((:name "format" :description "The format read.")
                     (:name "groups" :description "[{key:{column:value},count?,sum?,avg?,min?,max?,distinct?}].")
                     (:name "total_groups" :description "Groups after --min-count.")
                     (:name "truncated" :description "True when --limit cut groups (status partial, exit 3)."))
     :error-codes ("argument.invalid" "input.not-found" "input.syntax-error" "input.unsupported-format"
                   "refusal.not-a-file" "environment.busy" "environment.io")))
  "The read commands of the `json` and `table` groups.")

(export '(*inspect-table-read-args* *inspect-format-command-schemas*))

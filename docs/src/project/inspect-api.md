# Inspect APIs for other contexts

The Inspect application package exposes selector helpers for Edit and VCS,
and YAML/TOML preflight helpers for Edit. Each helper reports its result
through exactly one continuation.

## Selector APIs

### `parse-selector-options/k`

```lisp
(parse-selector-options/k command
  :range range
  :symbol symbol :kind kind
  :between between :exclusive exclusive
  :match match :invert invert
  :extra-exclusive extra-exclusive
  :display-name display-name
  :on-selector on-selector
  :on-none on-none
  :on-invalid on-invalid)
```

`COMMAND` is a keyword such as `:read`, `:edit`, or `:archive-read`. The
line-based selector options are mutually exclusive:

| Option | Value | Meaning |
| --- | --- | --- |
| `RANGE` | string | `N`, `S:E`, or `S:`; 1-based inclusive line numbers using ASCII digits. `S:` runs through end of file. |
| `SYMBOL` | non-empty string | A definition name. Optional `KIND` filters by definition kind. |
| `BETWEEN` | two non-empty regex strings | Select from a line matching the first regex through the next line matching the second. `EXCLUSIVE` omits both delimiter lines. |
| `MATCH` | non-empty regex string | Select every matching line. `INVERT` selects non-matching lines instead. |

`KIND` is a definition kind string: `class`, `constant`, `enum`, `function`,
`impl`, `interface`, `macro`, `method`, `mode`, `module`, `namespace`,
`package`, `struct`, `trait`, `type`, or `variable`. Available kinds depend on
the selected file language.

`:read`, `:edit`, `:transform`, `:move-lines`, `:git-blame`, `:git-show`,
`:archive-read`, `:insert`, and `:replace` accept these four selector forms.
Edit's separate `--old` text replacement option is not an input to this API.
Exactly one selector may be present. `KIND`, `EXCLUSIVE`, and `INVERT` need
their corresponding selector. `EXTRA-EXCLUSIVE` is a list of
`(flag-text . value)` pairs; each non-NIL value adds that flag to the same
one-selector conflict check.

The helper calls exactly one continuation:

| Continuation | Arguments | When it runs |
| --- | --- | --- |
| `ON-SELECTOR` | one kernel selector | A valid selector was provided. |
| `ON-NONE` | none | No selector was provided. |
| `ON-INVALID` | message, repairs | Options conflict, a modifier is missing its selector, a selector is not accepted by the command, or its value is malformed. Malformed regular expressions are detected later by `resolve-selector/k`. |

### `resolve-selector/k`

```lisp
(resolve-selector/k lines selector
  :path path
  :on-selected on-selected
  :on-no-match on-no-match
  :on-ambiguous on-ambiguous
  :on-invalid on-invalid)
```

`LINES` is a simple vector of line strings. `SELECTOR` is a line selector of
kind `:range`, `:symbol`, `:between`, or `:match`. `:old` represents Edit's
content replacement option and cannot be resolved from lines alone. `PATH` is
optional; for a `:symbol` selector it chooses a language by exact base name,
then by lowercase file extension. Without a path, symbol resolution reports
`input.unsupported-language`.

| Continuation | Arguments | Result |
| --- | --- | --- |
| `ON-SELECTED` | ranges | A list of 1-based, inclusive `(start . end)` line ranges. |
| `ON-NO-MATCH` | candidates | Candidate locations when nothing matched. Line candidates have `line` and `text`; symbol candidates have `line`, `end_line`, `kind`, and `name`. |
| `ON-AMBIGUOUS` | candidates | Candidate spans when more than one symbol or between block matched. Between candidates have `line`, `end_line`, and `text`; symbol candidates have `line`, `end_line`, `kind`, and `name`. |
| `ON-INVALID` | code, message | A regex is invalid or exceeds the evaluation limit (`input.syntax-error`), or symbol resolution has no supported language (`input.unsupported-language`). |

## YAML and TOML write preflight

Edit calls these APIs to map source documents into Edit's JSON value model
through the Inspect application boundary. Inspect's YAML/TOML parser
conditions stay inside the Inspect feature; Edit receives only JSON text or a
public error code and message.

```lisp
(parse-yaml-write-document/k text :on-value on-value :on-error on-error)
(parse-toml-write-document/k text :on-value on-value :on-error on-error)
```

`TEXT` is decoded document text. On success, `ON-VALUE` receives compact JSON
text and the count of lexical comment spans returned by the corresponding kit
(`yaml-kit:comment-spans` or `cl-toml-kit:toml-comment-spans`). The parser maps
the supported source values to the JSON value model while preserving object
member and array order. Edit parses the JSON text into its own value model
before applying the requested operation. A comment span is one range returned
by the kit's comment scanner; the count lets Edit report comments that will be
dropped by serialization.

On failure, `ON-ERROR` receives a public error code and message. Unsupported
source values use `input.unsupported-value` and the message
`<FORMAT> value at JSON Pointer <pointer> uses unsupported <reason>.` YAML
reasons are `document-count`, `explicit-tag`, `anchor-or-alias`,
`non-string-key`, `duplicate-key`, and `non-finite-float`. TOML reasons are
`offset-date-time`, `local-date-time`, `local-date`, `local-time`, and
`non-finite-float`. The pointer follows JSON Pointer escaping; the document
root is the empty string, mapping-key violations point to their containing
mapping, and value violations point to the offending value.

```text
YAML value at JSON Pointer /key uses unsupported explicit-tag.
TOML value at JSON Pointer /key uses unsupported local-date.
```

Syntax failures use `input.syntax-error` with `YAML syntax error.` or
`TOML syntax error.` This includes TOML parser failures such as an integer
outside TOML's signed 64-bit range.

These APIs only parse and preflight source documents. After an edit, Edit
validates the resulting JSON value before serialization. YAML numbers must be
finite and representable by the serializer. TOML output must have an object
root, contain no null values, and use signed 64-bit integers. A non-table root
or a null yields `input.unsupported-value` with reason `non-table-root` or
`toml-null`; an unrepresentable YAML number or out-of-range TOML edit integer
yields `argument.invalid`. Edit then calls its serializer port; direct
serializer-kit calls live in Edit infrastructure.

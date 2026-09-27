;;;; packages/core/kernel/src/domain/token-estimate.lisp
;;;;
;;;; `approx_tokens` is a character-count approximation, not tied to
;;;; any model's real tokenizer. The formula -- one token
;;;; per four characters, rounded up -- is the same rough constant OpenAI and
;;;; Anthropic both publish as a ballpark for English/code text; it is
;;;; intentionally not language-aware (a CJK string is undercounted by this
;;;; measure), which is why the docs call it an approximation and not a count.
(in-package #:aitools.kernel.domain)

(declaim (ftype (function (fixnum) (integer 0)) approx-token-count))
(defun approx-token-count (character-count)
  "Approximate token count for a text of CHARACTER-COUNT characters: one
token per four characters, rounded up. CHARACTER-COUNT is the character
count, not the byte count -- a multi-byte UTF-8 character counts once."
  (declare (type fixnum character-count))
  (ceiling character-count 4))

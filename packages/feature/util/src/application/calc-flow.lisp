(in-package #:aitools.util.application)

(aitools.kernel.domain:define-flow
    (%calc-evaluate expression decimals &key on-ok on-error)
  (aitools.kernel.domain:flow-call
      (aitools.util.domain:evaluate-expression expression)
    (:on-value (value)
      (let ((exact (aitools.util.domain:format-exact value)))
        (aitools.kernel.domain:flow-continue
         :on-ok
         (append
          (list
           (cons "input" expression)
           (cons "result"
                 (aitools.util.domain:format-decimal value decimals)))
          (when exact (list (cons "exact" exact)))))))
    (:on-error (offset reason)
      (aitools.kernel.domain:flow-continue
       :on-error "input.syntax-error"
       (format nil "at character ~D: ~A" offset reason)
       :repairs (list (%schema-repair "util calc"))))))

(aitools.kernel.domain:define-flow
    (util-calc-flow ports expression stdin decimals &key on-ok on-error)
  "EXPRESSION is the positional expression or NIL; STDIN is `--stdin`.
Exactly one must be given. DECIMALS bounds the rounded `result`; `exact`
carries the unrounded rational."
  (declare (type function on-ok on-error))
  (cond
    ((not (<= 0 decimals aitools.util.domain:+calc-max-decimals+))
     (aitools.kernel.domain:flow-continue
      :on-error "argument.invalid"
      (format nil "--decimals must be between 0 and ~D"
              aitools.util.domain:+calc-max-decimals+)
      :repairs
      (list
       (repair
        "use-default" "Use the default precision."
        "aitools util calc '<expr>'"))))
    ((eq (null expression) (not stdin))
     (aitools.kernel.domain:flow-continue
      :on-error "argument.invalid"
      "pass exactly one of an expression argument or --stdin"
      :repairs
      (list
       (repair
        "pass-expression" "Pass the expression as one argument."
        "aitools util calc '<expr>'")
       (repair
        "pass-stdin" "Read the expression from standard input."
        "aitools util calc --stdin"))))
    (expression
     (aitools.kernel.domain:flow-call
         (%calc-evaluate expression decimals)
       (:on-ok (fields)
         (aitools.kernel.domain:flow-continue :on-ok fields))
       (:on-error (code message &key repairs)
         (aitools.kernel.domain:flow-continue
          :on-error code message :repairs repairs))))
    (t
     (aitools.kernel.domain:flow-call
         (resolve-input ports (make-input-request :stdin t) "util calc")
       (:on-input (octets &key text)
         (declare (ignore octets))
         (aitools.kernel.domain:flow-call
             (%calc-evaluate
              (string-trim '(#\Space #\Tab #\Newline #\Return) text)
              decimals)
           (:on-ok (fields)
             (aitools.kernel.domain:flow-continue :on-ok fields))
           (:on-error (code message &key repairs)
             (aitools.kernel.domain:flow-continue
              :on-error code message :repairs repairs))))
       (:on-error (code message &key repairs)
         (aitools.kernel.domain:flow-continue
          :on-error code message :repairs repairs))))))

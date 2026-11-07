(in-package #:aitools.protocol.test)

(defun %test-command-declaration (name builder &optional group)
  (aitools.protocol.application:make-command-declaration
   name :group group :command-builder builder
   :schema (make-command-schema name "Test command.")))

(describe "aitools.protocol.application define-command"
  (it-each (("non-list options" 42)
            ("odd-length options" (:schema))
            ("unknown option" (:command-builder #'identity :schema nil :unknown t))
            ("duplicate option" (:group nil :group nil :command-builder #'identity :schema nil))
            ("missing :command-builder" (:schema nil))
            ("missing :schema" (:command-builder #'identity)))
      "rejects ~A options during macro expansion"
      (case-name options)
    (signals error
      (macroexpand-1
        `(aitools.protocol.application:define-command
             "read" ,options))))

  (it-each (("an integer name" :name 7)
          ("an empty name" :name "")
          ("a character name" :name #\x)
          ("a keyword name" :name :read)
          ("a vector name" :name #())
          ("NIL as a name" :name nil)
          ("T as a name" :name t)
          ("an integer group" :group 7)
          ("an empty group" :group ""))
      "rejects ~A during macro expansion"
      (case-name field value)
    (let ((form
            (if (eq field :name)
                `(aitools.protocol.application:define-command
                     ,value (:command-builder #'identity :schema nil))
                `(aitools.protocol.application:define-command
                     "read" (:group ,value :command-builder #'identity :schema nil)))))
      (signals error (macroexpand-1 form))))

  (it "evaluates each dynamic form once in argument order"
    (let ((events nil)
          (schema (make-command-schema "util.echo" "Test command.")))
      (let ((declaration
              (aitools.protocol.application:define-command
                  (progn (push :name events) "util.echo")
                (:group (progn (push :group events) "util")
                 :command-builder (progn (push :builder events) #'identity)
                 :schema (progn (push :schema events) schema)))))
        (expect (reverse events) :to-equal '(:name :group :builder :schema))
        (expect (aitools.protocol.application:command-declaration-name declaration)
                :to-equal "util.echo")
        (expect (aitools.protocol.application:command-declaration-group declaration)
                :to-equal "util"))))
  (it "expands quoted and dynamic names and groups at runtime"
    (let* ((quoted-schema (make-command-schema "util.quoted" "Test command."))
           (quoted-declaration
             (eval
              (macroexpand-1
                `(aitools.protocol.application:define-command
                     '"util.quoted"
                   (:group '"util"
                    :command-builder #'identity
                    :schema ',quoted-schema)))))
           (dynamic-schema (make-command-schema "util.dynamic" "Test command."))
           (dynamic-declaration
             (eval
              (macroexpand-1
                `(aitools.protocol.application:define-command
                     (progn "util.dynamic")
                   (:group (progn "util")
                    :command-builder #'identity
                    :schema ',dynamic-schema))))))
      (expect (aitools.protocol.application:command-declaration-name quoted-declaration)
              :to-equal "util.quoted")
      (expect (aitools.protocol.application:command-declaration-group quoted-declaration)
              :to-equal "util")
      (expect (aitools.protocol.application:command-declaration-name dynamic-declaration)
              :to-equal "util.dynamic")
      (expect (aitools.protocol.application:command-declaration-group dynamic-declaration)
              :to-equal "util")
      (expect (nth-value
               1
               (macroexpand-1
                 '(aitools.protocol.application:define-command
                      (quote)
                    (:command-builder #'identity :schema nil))))
              :to-be t)
      (expect (nth-value
               1
               (macroexpand-1
                 '(aitools.protocol.application:define-command
                      (quote "util.extra" "extra")
                    (:command-builder #'identity :schema nil))))
              :to-be t)))

  (it "checks schema names and nested grouped names at runtime"
    (signals error
      (aitools.protocol.application:make-command-declaration
       "read" :command-builder #'identity
       :schema (make-command-schema "other" "Test command.")))
    (signals error
      (aitools.protocol.application:make-command-declaration
       "json.get" :group "table" :command-builder #'identity
       :schema (make-command-schema "json.get" "Test command.")))
    (signals error
      (aitools.protocol.application:make-command-declaration
       "read" :command-builder nil
       :schema (make-command-schema "read" "Test command.")))
    (signals error
      (aitools.protocol.application:make-command-declaration
       "read" :command-builder #'identity :schema nil))
    (signals error
      (aitools.protocol.application:make-command-declaration
       "" :command-builder #'identity
       :schema (make-command-schema "" "Test command.")))
    (signals error
      (aitools.protocol.application:make-command-declaration
       "org.todo.set" :group "org" :command-builder #'identity
       :schema (make-command-schema "org.todo.set" "Test command.")))
    (signals error
      (aitools.protocol.application:make-command-declaration
       "org..todo.set" :group "org..todo" :command-builder #'identity
       :schema (make-command-schema "org..todo.set" "Test command.")))
    (signals error
      (aitools.protocol.application:make-command-declaration
       "org.todo.set.extra" :group "org.todo" :command-builder #'identity
       :schema (make-command-schema "org.todo.set.extra" "Test command.")))
    (let ((registry (aitools.protocol.application:make-command-registry)))
      (aitools.protocol.application:register-command-declarations
       registry
       (list
        (%test-command-declaration
         "org.todo.set"
         (lambda (dependencies) (declare (ignore dependencies)) :set-command)
         "org.todo"))
       nil)
      (let ((groups (aitools.protocol.application:command-registry-group-commands registry)))
        (expect (nth-value 1 (gethash "org" groups)) :to-be t)
        (expect (gethash "org" groups) :to-equal nil)
        (expect (gethash "org.todo" groups) :to-equal '(:set-command)))
      (expect (mapcar #'command-schema-name
                      (aitools.protocol.application:all-command-schemas registry))
              :to-equal '("org.todo.set"))))
  (it-each (("a non-string group" 7 "read")
          ("an empty group" "" "read")
          ("a group starting with a dot" ".org" ".org.read")
          ("a group ending with a dot" "org." "org.read")
          ("a group with adjacent dots" "org..team" "org..team.read")
          ("a name starting with a dot" "org" ".org.read")
          ("a name ending with a dot" "org" "org.read.")
          ("a name with adjacent dots" "org" "org..read")
          ("a name with an extra command segment" "org" "org.read.extra")
          ("a name outside its group" "org" "util.read"))
      "rejects ~A at runtime"
      (case-name group name)
    (signals error
      (aitools.protocol.application:make-command-declaration
       name :group group :command-builder #'identity
       :schema (make-command-schema name "Test command.")))))

(describe "aitools.protocol.application command declaration registration"
  (it "builds a declaration once with the supplied dependencies"
    (let ((calls nil)
          (registry (aitools.protocol.application:make-command-registry)))
      (let ((declaration
              (%test-command-declaration
               "util.echo"
               (lambda (dependencies)
                 (push dependencies calls)
                 :cli-command)
               "util")))
        (aitools.protocol.application:register-command-declarations
         registry
         (list declaration)
         :dependencies)
        (expect calls :to-equal '(:dependencies))
        (expect (mapcar #'command-schema-name
                        (aitools.protocol.application:all-command-schemas registry))
                :to-equal '("util.echo")))))

  (it "rejects an existing command name before invoking its builder"
    (let ((calls 0)
          (registry (aitools.protocol.application:make-command-registry)))
      (aitools.protocol.application:register-command-declarations
       registry
       (list (%test-command-declaration "read" (lambda (dependencies) (declare (ignore dependencies)))))
       nil)
      (signals error
        (aitools.protocol.application:register-command-declarations
         registry
         (list (%test-command-declaration
                "read"
                (lambda (dependencies)
                  (declare (ignore dependencies))
                  (incf calls))))
         nil))
      (expect calls :to-be 0)
      (expect (mapcar #'command-schema-name
                      (aitools.protocol.application:all-command-schemas registry))
              :to-equal '("read"))))

  (it "checks the entire batch before invoking any builder"
    (let ((calls 0)
          (registry (aitools.protocol.application:make-command-registry)))
      (signals error
        (aitools.protocol.application:register-command-declarations
         registry
         (list (%test-command-declaration
                "read"
                (lambda (dependencies) (declare (ignore dependencies)) (incf calls)))
               (%test-command-declaration
                "read"
                (lambda (dependencies) (declare (ignore dependencies)) (incf calls))))
         nil))
      (expect calls :to-be 0)
      (expect (aitools.protocol.application:all-command-schemas registry)
              :to-equal nil)))

  (it "builds commands in declaration order"
    (let ((calls nil)
          (registry (aitools.protocol.application:make-command-registry)))
      (aitools.protocol.application:register-command-declarations
       registry
       (list (%test-command-declaration
              "first"
              (lambda (dependencies) (push (list :first dependencies) calls) :first-command))
             (%test-command-declaration
              "second"
              (lambda (dependencies) (push (list :second dependencies) calls) :second-command)))
       :dependencies)
      (expect (reverse calls) :to-equal '((:first :dependencies) (:second :dependencies)))
      (expect (mapcar #'command-schema-name
                      (aitools.protocol.application:all-command-schemas registry))
              :to-equal '("first" "second"))))

  (it "leaves the registry unchanged when a builder signals"
    (let ((calls nil)
          (registry (aitools.protocol.application:make-command-registry)))
      (signals error
        (aitools.protocol.application:register-command-declarations
         registry
         (list (%test-command-declaration
                "first"
                (lambda (dependencies) (declare (ignore dependencies)) (push :first calls) :first-command))
               (%test-command-declaration
                "second"
                (lambda (dependencies)
                  (declare (ignore dependencies))
                  (push :second calls)
                  (error "Builder failed."))))
         nil))
      (expect (reverse calls) :to-equal '(:first :second))
      (expect (aitools.protocol.application:all-command-schemas registry)
              :to-equal nil))))

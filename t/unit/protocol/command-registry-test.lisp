(in-package #:aitools.protocol.test)

(defun %test-command-declaration (name builder &optional group)
  (aitools.protocol.application:make-command-declaration
   name :group group :command-builder builder
   :schema (make-command-schema name "Test command.")))

(describe "aitools.protocol.application define-command"
  (it "rejects duplicate and unknown options during macro expansion"
    (signals error
      (macroexpand-1
       '(aitools.protocol.application:define-command
            "read"
          (:command-builder #'identity :schema nil :unknown t))))
    (signals error
      (macroexpand-1
       '(aitools.protocol.application:define-command
            "read"
          (:group nil :group nil :command-builder #'identity :schema nil))))
    (signals error
      (macroexpand-1
       '(aitools.protocol.application:define-command "read" (:schema nil))))
    (signals error
      (macroexpand-1
       '(aitools.protocol.application:define-command
            "read"
          (:command-builder #'identity)))))

  (it "rejects invalid literal names and groups during macro expansion"
    (signals error
      (macroexpand-1
       '(aitools.protocol.application:define-command
            7
          (:command-builder #'identity :schema nil))))
    (signals error
      (macroexpand-1
       '(aitools.protocol.application:define-command
            ""
          (:command-builder #'identity :schema nil))))
    (signals error
      (macroexpand-1
       '(aitools.protocol.application:define-command
            "json.get"
          (:group 7 :command-builder #'identity :schema nil))))
    (signals error
      (macroexpand-1
       '(aitools.protocol.application:define-command
            "json.get"
          (:group "" :command-builder #'identity :schema nil)))))

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

  (it "checks schema names and grouped command names at runtime"
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
       :schema (make-command-schema "" "Test command.")))))

(describe "aitools.protocol.application command declaration registration"
  (it "builds a declaration once with the supplied ports"
    (let ((calls nil)
          (registry (aitools.protocol.application:make-command-registry)))
      (let ((declaration
              (%test-command-declaration
               "util.echo"
               (lambda (ports)
                 (push ports calls)
                 :cli-command)
               "util")))
        (aitools.protocol.application:register-command-declarations
         registry
         (list declaration)
         :ports)
        (expect calls :to-equal '(:ports))
        (expect (mapcar #'command-schema-name
                        (aitools.protocol.application:all-command-schemas registry))
                :to-equal '("util.echo")))))

  (it "rejects an existing command name before invoking its builder"
    (let ((calls 0)
          (registry (aitools.protocol.application:make-command-registry)))
      (aitools.protocol.application:register-command-declarations
       registry
       (list (%test-command-declaration "read" (lambda (ports) (declare (ignore ports)))))
       nil)
      (signals error
        (aitools.protocol.application:register-command-declarations
         registry
         (list (%test-command-declaration
                "read"
                (lambda (ports)
                  (declare (ignore ports))
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
                (lambda (ports) (declare (ignore ports)) (incf calls)))
               (%test-command-declaration
                "read"
                (lambda (ports) (declare (ignore ports)) (incf calls))))
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
              (lambda (ports) (push (list :first ports) calls) :first-command))
             (%test-command-declaration
              "second"
              (lambda (ports) (push (list :second ports) calls) :second-command)))
       :ports)
      (expect (reverse calls) :to-equal '((:first :ports) (:second :ports)))
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
                (lambda (ports) (declare (ignore ports)) (push :first calls) :first-command))
               (%test-command-declaration
                "second"
                (lambda (ports)
                  (declare (ignore ports))
                  (push :second calls)
                  (error "Builder failed."))))
         nil))
      (expect (reverse calls) :to-equal '(:first :second))
      (expect (aitools.protocol.application:all-command-schemas registry)
              :to-equal nil))))

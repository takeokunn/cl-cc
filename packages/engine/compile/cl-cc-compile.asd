;;;; cl-cc-compile.asd --- independent ASDF system for the compilation engine
;;;;
;;;; Phase 3d extraction. Compile source files (context, closure, CPS,
;;;; codegen, builtin registry) are now part of this system.
;;;; Pipeline files (stdlib-source, pipeline, pipeline-repl etc.) remain
;;;; in cl-cc.asd because they use (in-package :cl-cc) which requires
;;;; the umbrella package to exist first.
;;;;
;;;; Files use (in-package :cl-cc/compile) and access VM/expand/ast symbols
;;;; via the :use list below.
;;;;
;;;; Dependency order: bootstrap → ast → prolog → parse → type → optimize
;;;;                   → vm → expand → compile

(asdf:defsystem :cl-cc-compile
  :description "Compilation engine: CPS, codegen, builtin registry"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-ast :cl-cc-prolog :cl-cc-parse :cl-cc-type
               :cl-cc-optimize :cl-cc-vm :cl-cc-emit :cl-cc-expand)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "context")
   (:file "closure")
    (:file "cps")
    (:file "cps-ast")
   (:file "cps-ast-control")          ; control flow forms (unwind-protect, block, return-from, tagbody, catch, throw)
   (:file "cps-ast-extended")         ; OOP/mutation forms (setq, defvar, make-instance, defclass…)
   (:file "cps-ast-functional")       ; functional forms + entry points (split from cps-ast-extended)
   (:file "builtin-registry-data")    ; Core alists: unary/binary/string-cmp/char-cmp
   (:file "builtin-registry-data-ext") ; Extended alists: I/O, stream, custom, ternary
   (:file "builtin-registry")         ; Registry struct + *builtin-registry* + registration
   (:file "builtin-registry-emitters") ; 27 emit-builtin-* emitter functions
   (:file "builtin-registry-dispatch") ; *builtin-emitter-table* + *convention-arity* + emit-registered-builtin
   (:file "codegen-core")             ; binop table + primitive/if compilation
   (:file "codegen-core-control")     ; block/return-from + tagbody/go + setq/quote/the
   (:file "codegen-core-let")         ; let-binding analysis: predicates + walkers
   (:file "codegen-core-let-emit")    ; let-binding emitters + compile-ast (ast-let)
   (:file "codegen-clos")             ; defclass + slot accessor compilation
   (:file "codegen-gf")               ; defgeneric/defmethod/make-instance/slot-value
   (:file "codegen-functions")        ; defmacro compilation (typed params now in expand)
   (:file "codegen-functions-params") ; parameter-list helpers + compile-function-body
   (:file "codegen-functions-emit")   ; lambda/defun/defvar compile-ast methods
   (:file "codegen-phase2")           ; Phase 2 AST-introspecting builtin handlers
   (:file "codegen-control")          ; control-flow + multiple-values compiler methods
   (:file "codegen-io")               ; Phase 2 stream/reader/printer handlers split from phase 2
   (:file "codegen-io-ext")           ; Phase 2 array/string/format/file handlers split from codegen-io
   (:file "codegen-hash-table")       ; hash-table handler cluster split from phase 2
   (:file "codegen-slot-predicates")  ; CLOS slot predicate handlers split from phase 2
   (:file "codegen-string-kwargs")    ; string comparison/case handlers split from phase 2
   (:file "codegen-fold")             ; compile-time constant fold + partial evaluator
   (:file "codegen-fold-optimize")    ; %loc macro + optimize-ast fold pass
   (:file "codegen")                  ; entry points + exception handling + multiple values
   (:file "codegen-calls")            ; function call compilation: %try-compile-* + ast-call method
   (:file "codegen-locals")))         ; local fn bindings: flet/labels/ast-function + emit-assembly

;;;; cl-cc-expand.asd --- independent ASDF system for the macro expansion subsystem
;;;;
;;;; Phase 3c extraction. All expand source files are now part of this system.
;;;; Files use (in-package :cl-cc) and access VM instruction types via the
;;;; umbrella package which uses both :cl-cc/expand and :cl-cc/vm.
;;;;
;;;; Dependency order: bootstrap → ast → prolog → parse → type → expand

(asdf:defsystem :cl-cc-expand
  :description "Macro expansion subsystem: macro-env, defmacro, macroexpand, lambda-list, LOOP"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-type)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "macro-lambda-list")   ; shared lambda-list parsing + destructuring helpers
   (:file "macro")               ; core: macro-env, defmacro machinery, macroexpansion
   (:file "macros-basic")        ; bootstrap: check-type/setf/list + value helpers
   (:file "macros-control-flow") ; bootstrap control-flow macros (when/unless/cond/do*)
   (:file "macros-control-flow-case") ; case/typecase macro expansion
   (:file "macros-mutation")     ; push/pop/incf/decf split from stdlib
   (:file "loop-data")           ; LOOP: grammar tables — the "Prolog database"
   (:file "loop-parser-for")     ; LOOP: token predicates, CPS utils, FOR sub-parsers
   (:file "loop-parser")         ; LOOP: CPS token parser → IR plist
   (:file "loop-emitters")       ; LOOP: registration macros + accumulation emitters
   (:file "loop-iter-emitters")  ; LOOP: iteration + condition emitters
   (:file "loop")                ; LOOP: generator — assembles tagbody from IR
   (:file "macros-setops")       ; list/set operations split from stdlib
   (:file "macros-list-utils")   ; ordering and list utility helpers
   (:file "macros-restarts")     ; restart/condition protocol split from stdlib
   (:file "macros-introspection") ; equalp and introspection helpers
   (:file "macros-stdlib")       ; stdlib: numeric/control macros (1+, ecase, rotatef...)
   (:file "macros-stdlib-ansi")  ; ANSI CL Phase 1 (psetf, assert, define-condition...)
   (:file "macros-stdlib-utils") ; list/tree/string/array utility macros
   (:file "macros-cxr")          ; algorithmic CXR accessor registration
   (:file "macros-hof")          ; higher-order list/search helpers (map/find/remove)
   (:file "macros-hof-search")   ; position/count/assoc search HOFs
   (:file "macros-filesystem")   ; file/IO/runtime helpers split from stdlib
    (:file "macros-sequence")     ; sequences: copy/fill/replace/mismatch/delete/substitute
    (:file "macros-sequence-fold") ; sequences: reduce/nsubstitute/map-into/merge/last/search
    (:file "macros-sequence-helpers")  ; list/sequence helper macros split from stdlib
    (:file "macros-plist")        ; property list helpers
   (:file "macros-package-system") ; package system and symbol-iteration macros
   (:file "macros-runtime-support") ; declarations, IO/hash/coerce/LTV/feature runtime macros
   (:file "macros-clos-protocol")  ; CLOS protocol: print-unreadable-object, describe, change-class
   (:file "macros-mop-support")   ; MOP introspection macros + parse-float + reinitialize-instance
    (:file "expander-data")       ; expander: grammar tables + dispatch table declarations
    (:file "expander-helpers")    ; expander: shared helper functions extracted from expander.lisp
    (:file "expander-defstruct-copy") ; expander: COPY-STRUCTURE expansion
     (:file "expander-defstruct-boa")  ; expander: BOA constructors for defstruct
     (:file "expander-defstruct-typed") ; expander: :TYPE list/vector defstruct forms
     (:file "expander-defstruct-clos")  ; expander: CLOS-backed defstruct forms
     (:file "expander-defstruct")       ; expander: defstruct dispatcher
    (:file "expander-typed-params") ; typed lambda-list helpers + *function-type-registry*
    (:file "expander-core")
    (:file "expander-definitions-helpers") ; expander: lambda-list default expansion helper
   (:file "expander-control-helpers") ; expander: binding helpers for control forms
   (:file "expander-setf-places-helpers") ; expander: setf-place cons access helper
   (:file "expander-setf-places") ; expander: setf compound-place registration table
   (:file "expander")
   (:file "expander-definitions-forms")
   (:file "expander-basic")      ; core application handlers split from expander.lisp
   (:file "expander-definitions")
   (:file "expander-control")
   (:file "expander-tail")
   (:file "expander-numeric")
   (:file "expander-sequence")))

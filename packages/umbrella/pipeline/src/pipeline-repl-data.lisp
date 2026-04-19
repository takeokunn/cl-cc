(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compile — REPL Host-Load Policy Data
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defparameter *host-only-top-level-form-names*
  '("EVAL-WHEN" "DEFPACKAGE" "DEFCLASS" "DEFSTRUCT" "DEFGENERIC"
    "DEFMETHOD" "DEFVAR" "DEFPARAMETER" "DEFCONSTANT")
  "Top-level definition forms better handled by host EVAL during project loads.")

(defparameter *host-only-top-level-macro-names*
  '("DEFINE-RT-PREDICATE"
    "DEFINE-RT-BINARY-PREDICATE"
    "DEFINE-RT-STREAM-OP"
    "DEFINE-EXPANDER-FOR"
    "DEFINE-LIST-LOWERER"
    "DEF-GRAMMAR-RULE"
    "DEFINE-PHP-STMT-PARSER"
    "DEF-FACT"
    "DEF-RULE"
    "DEFINE-PROLOG-DECLARATIVE-RULES"
    "DEFINE-PROLOG-INTEGER-BINOP-TYPE-RULES"
    "DEFINE-PROLOG-COMPARISON-TYPE-RULE"
    "DEFINE-VM-INSTRUCTION"
    "DEFINE-VM-UNARY-INSTRUCTION"
    "DEFINE-VM-BINARY-INSTRUCTION"
    "DEFINE-VM-CHAR-COMPARISON"
    "DEFINE-VM-STRING-COMPARISON"
    "DEFINE-VM-STRING-TRIM-INSTRUCTION"
    "DEFINE-VM-STRING-TRIM-EXECUTOR"
    "DEFINE-VM-FLOAT-ROUNDING-EXECUTORS"
    "DEFINE-VM-HASH-PROPERTY-EXECUTORS")
  "Project macros that should be host-evaluated during source loading.")

(defparameter *host-only-registration-symbol-names*
  '("*PHASE2-BUILTIN-HANDLERS*"
    "*STANDARD-LIBRARY-SOURCE*"
    "*EXPANDER-HEAD-TABLE*"
    "*VARIADIC-EXPANDER-SPECS*"
    "*SETF-COMPOUND-PLACE-HANDLERS*"
    "*INSTRUCTION-CONSTRUCTORS*"
    "*BUILTIN-PREDICATES*"
    "*LIST-LOWERING-TABLE*"
    "*%CONDITION-HANDLERS*"
    "*X86-64-TARGET*"
    "*AARCH64-TARGET*"
    "*RISCV64-TARGET*"
    "*WASM32-TARGET*"
    "FDEFINITION"
    "SET-FDEFINITION")
  "Symbols whose presence marks a top-level registration/mutation form.")

(defparameter *host-only-registration-helper-names*
  '("%REGISTER-STRING-CMP-HANDLER"
    "%REGISTER-STRING-CASE-HANDLER"
    "%REGISTER-SLOT-PREDICATE-HANDLER"
    "%REGISTER-BUILTINS"
    "%REGISTER-SLOTS-BUILTINS"
    "REGISTER-TARGET")
  "Helper forms whose presence marks a top-level registration/mutation form.")

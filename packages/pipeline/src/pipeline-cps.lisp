(in-package :cl-cc/pipeline)
;;;; Pipeline — CPS compile helpers shared by VM/native entrypoints

;;; CPS safety guards live in cl-cc/compile (context.lisp).
;;; They are accessible here via the (use-package :cl-cc/compile :cl-cc) bridge
;;; established in packages/umbrella-src/package.lisp.
;;; *enable-cps-vm-primary-path*, *compile-expression-cps-recursion-guard*,
;;; %cps-vm-compile-safe-ast-p, %cps-native-compile-safe-ast-p, and
;;; %cps-identity-entry-form are all inherited from cl-cc/compile.

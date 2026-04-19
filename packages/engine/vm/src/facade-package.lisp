;;;; packages/engine/vm/src/facade-package.lisp -- hook variables for cl-cc/vm

(in-package :cl-cc/vm)

;;; Hook variables for circular dependency resolution.
;;; These are set at load time by the respective upstream packages.
(defvar *vm-eval-hook* nil
  "Set by cl-cc/compile (pipeline.lisp). Called by the VM's vm-eval instruction.")
(defvar *vm-macroexpand-1-hook* nil
  "Set by cl-cc/expand (macro.lisp). Called by the VM's vm-macroexpand-1-inst instruction.")
(defvar *vm-macroexpand-hook* nil
  "Set by cl-cc/expand (macro.lisp). Called by the VM's vm-macroexpand-inst instruction.")
(defvar *vm-compile-string-hook* nil
  "Set by cl-cc/compile (pipeline.lisp). Called by run-string-with-io in io-runners.lisp.")
(defvar *vm-parse-forms-hook* nil
  "Set by cl-cc/parse (parser.lisp). Called by the VM's vm-read-sexp-inst instruction.")

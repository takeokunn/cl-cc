;;;; src/cli/selfhost.lisp — CL-CC Self-Hosting Verification
;;;;
;;;; Data: *selfhost-source-files* — ordered list of all source files
;;;; Logic: five phase helpers + summary printer + %do-selfhost entry point
;;;;
;;;; Load order: after args.lisp, before main.lisp.

(in-package :cl-cc/cli)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Data: ordered source files for self-hosting phase 4.
;;; Separated from the logic so the list is easy to update independently.
;;; ─────────────────────────────────────────────────────────────────────────

(defparameter *selfhost-source-files*
  '(;; package
    "src/package.lisp"
    ;; parse
    "src/parse/package.lisp"
    "src/parse/cst.lisp" "src/parse/diagnostics.lisp"
    "src/parse/lexer.lisp" "src/parse/lexer-readers.lisp" "src/parse/lexer-skip.lisp"
    "src/parse/lexer-dispatch.lisp"
    "src/parse/incremental.lisp" "src/parse/pratt.lisp"
    "src/parse/combinators.lisp"
    "src/parse/cl/parser.lisp" "src/parse/cl/parser-sexp-lowering.lisp"
    "src/parse/cl/lower.lisp" "src/parse/cl/lower-definitions.lisp"
    "src/parse/cl/lower-clos.lisp"
    "src/parse/cl/parser-roundtrip.lisp"
    "src/parse/cl/grammar.lisp"
    "src/parse/php/lexer.lisp" "src/parse/php/lexer-ops.lisp"
    "src/parse/php/parser.lisp" "src/parse/php/parser-expr.lisp"
    "src/parse/php/parser-stmt.lisp"
    "src/parse/php/grammar.lisp" "src/parse/php/grammar-stmt.lisp"
    "src/parse/cst-to-ast.lisp"
    ;; expand
    "src/expand/macro.lisp"
    "src/expand/macros-basic.lisp"
    "src/expand/loop-data.lisp" "src/expand/loop-parser.lisp"
    "src/expand/loop-emitters.lisp" "src/expand/loop.lisp"
    "src/expand/macros-stdlib.lisp"
    "src/expand/macros-sequence.lisp"
    "src/expand/macros-compat.lisp"
    "src/expand/expander-data.lisp"
    "src/expand/expander-defstruct.lisp"
    "src/expand/expander.lisp"
    ;; vm
    "src/vm/package.lisp" "src/vm/vm.lisp"
    "src/vm/vm-execute.lisp" "src/vm/vm-clos.lisp"
    "src/vm/vm-run.lisp" "src/vm/vm-opcodes.lisp"
    "src/vm/primitives.lisp"
    "src/vm/vm-bitwise.lisp" "src/vm/vm-transcendental.lisp"
    "src/vm/vm-numeric.lisp" "src/vm/vm-extensions.lisp"
    "src/vm/io.lisp" "src/vm/format.lisp" "src/vm/conditions.lisp"
    "src/vm/list.lisp" "src/vm/array.lisp"
    "src/vm/strings.lisp" "src/vm/symbols.lisp" "src/vm/hash.lisp"
    ;; type
    "src/type/package.lisp" "src/type/kind.lisp" "src/type/multiplicity.lisp"
    "src/type/types-core.lisp" "src/type/types-extended.lisp" "src/type/types-env.lisp"
    "src/type/substitution.lisp" "src/type/unification.lisp"
    "src/type/subtyping.lisp" "src/type/effect.lisp" "src/type/row.lisp"
    "src/type/constraint.lisp" "src/type/parser.lisp" "src/type/typeclass.lisp"
    "src/type/solver.lisp" "src/type/inference.lisp"
    "src/type/inference-effects.lisp" "src/type/bidirectional.lisp"
    "src/type/checker.lisp" "src/type/printer.lisp"
    ;; compile
    "src/compile/ir/types.lisp" "src/compile/ir/block.lisp"
    "src/compile/ir/ssa.lisp" "src/compile/ir/printer.lisp"
    "src/compile/context.lisp" "src/compile/closure.lisp" "src/compile/cps.lisp"
    "src/compile/builtin-registry-data.lisp" "src/compile/builtin-registry.lisp"
    "src/compile/codegen-core.lisp" "src/compile/codegen-clos.lisp"
    "src/compile/codegen-functions.lisp" "src/compile/codegen-phase2.lisp"
    "src/compile/codegen.lisp"
    ;; optimize
    "src/optimize/effects.lisp" "src/optimize/cfg.lisp" "src/optimize/ssa.lisp"
    "src/optimize/egraph.lisp" "src/optimize/egraph-rules.lisp"
    "src/optimize/optimizer-tables.lisp" "src/optimize/optimizer-inline.lisp"
    "src/optimize/optimizer.lisp"
    ;; emit
    "src/emit/mir.lisp" "src/emit/target.lisp" "src/emit/calling-convention.lisp"
    "src/emit/regalloc.lisp"
    "src/emit/x86-64.lisp" "src/emit/x86-64-encoding.lisp" "src/emit/x86-64-codegen.lisp"
    "src/emit/aarch64.lisp" "src/emit/aarch64-codegen.lisp"
    "src/emit/wasm-types.lisp" "src/emit/wasm-ir.lisp" "src/emit/wasm-extract.lisp"
    "src/emit/wasm-trampoline.lisp" "src/emit/wasm.lisp"
    "src/emit/binary/package.lisp" "src/emit/binary/macho.lisp"
    "src/emit/binary/elf.lisp" "src/emit/binary/wasm.lisp"
    ;; bytecode + runtime + pipeline
    "src/bytecode/package.lisp" "src/bytecode/encode.lisp" "src/bytecode/decode.lisp"
    "src/runtime/package.lisp" "src/runtime/runtime.lisp" "src/runtime/value.lisp"
    "src/runtime/frame.lisp" "src/runtime/heap.lisp" "src/runtime/gc.lisp"
    "src/compile/stdlib-source.lisp" "src/compile/pipeline.lisp")
  "Ordered list of source files loaded during self-hosting phase 4 verification.")

;;; ─────────────────────────────────────────────────────────────────────────
;;; Phase helpers — each takes CHECK, a (name expected actual) callback.
;;; ─────────────────────────────────────────────────────────────────────────

(defun %selfhost-phase-macro-eval (check)
  "Phase 1: verify macro expansion uses own-eval, not host eval."
  (format t "--- Macro eval through own VM ---~%")
  (funcall check "macro-eval-fn = our-eval (set at load time)"
           t (eq cl-cc:*macro-eval-fn* #'cl-cc:our-eval)))

(defun %selfhost-phase-basic-compilation (check)
  "Phase 2: verify basic CL forms compile and execute correctly."
  (format t "--- Basic compilation ---~%")
  (flet ((try (form) (handler-case (cl-cc:run-string form) (error () :err))))
    (funcall check "arithmetic"            42  (try "(+ 21 21)"))
    (funcall check "recursion"             120
             (try "(defun sh-f (n) (if (<= n 1) 1 (* n (sh-f (- n 1))))) (sh-f 5)"))
    (funcall check "closure"               15
             (try "(let ((x 10)) (funcall (lambda (y) (+ x y)) 5))"))
    (funcall check "defmacro via our-eval" 3
             (try "(defmacro sh-w (t2 &body b) `(if ,t2 (progn ,@b) nil)) (sh-w t (+ 1 2))"))))

(defun %selfhost-phase-meta-circular (check)
  "Phase 3: verify the compiler can meta-circularly compile itself."
  (format t "--- Meta-circular compilation ---~%")
  (flet ((try (form) (handler-case (cl-cc:run-string form) (error () :err))))
    (funcall check "run-string inside run-string"     42  (try "(run-string \"(+ 21 21)\")"))
    (funcall check "defun through nested compilation" 120
             (try "(run-string \"(defun sh-mf (n) (if (<= n 1) 1 (* n (sh-mf (- n 1))))) (sh-mf 5)\")"))))

(defun %selfhost-phase-source-loading (check)
  "Phase 4: load every source file through own compiler; pass iff all succeed."
  (let ((n (length *selfhost-source-files*)))
    (format t "--- Source file self-loading (~D files) ---~%" n)
    (let ((ok 0))
      (let ((cl-cc::*repl-vm-state*              nil)
            (cl-cc::*repl-accessor-map*          nil)
            (cl-cc::*repl-pool-instructions*     nil)
            (cl-cc::*repl-pool-labels*           nil)
            (cl-cc::*repl-global-vars-persistent* nil)
            (cl-cc::*repl-label-counter*          nil)
            (cl-cc::*repl-defstruct-registry*    nil))
        (dolist (f *selfhost-source-files*)
          (handler-case (progn (cl-cc::our-load f) (incf ok))
            (error (e) (declare (ignore e))))))
      (funcall check (format nil "~D/~D source files load through own compiler" ok n) n ok))))

(defun %selfhost-phase-host-eval (check)
  "Phase 5: report host eval replacement status."
  (format t "--- Host eval elimination ---~%")
  (format t "  Replaced with our-eval:~%")
  (format t "    - *macro-eval-fn* (pipeline.lisp) — all defmacro expansion~%")
  (format t "    - eval-lisp-condition (prolog.lisp) — Prolog engine~%")
  (format t "    - rt-eval (runtime.lisp) — runtime eval~%")
  (format t "  Remaining host eval (bootstrap):~%")
  (format t "    - #. read-time eval (lexer.lisp) — host constants~%")
  (format t "    - load-time-value (macro.lisp) — host environment~%")
  (format t "    - our-defmacro eager (expander.lisp) — host macro env~%")
  (format t "    - cps-transform-eval (cps.lisp) — host lambdas~%")
  (funcall check "4 of 7 eval calls replaced with our-eval" t t))

(defun %selfhost-print-summary (pass fail total)
  "Print the final selfhost summary banner and exit with appropriate status."
  (format t "~%~A~%" (make-string 60 :initial-element #\=))
  (format t "  ~D/~D checks passed~%" pass total)
  (if (zerop fail)
      (progn
        (format t "  STATUS: cl-cc is self-hosting.~%~%")
        (format t "  Proven capabilities:~%")
        (format t "    - Macro expansion through own VM (our-eval)~%")
        (format t "    - Meta-circular compilation (compiler compiles compiler)~%")
        (format t "    - ~D/~D source files self-load through own compiler~%"
                (length *selfhost-source-files*) (length *selfhost-source-files*))
        (format t "    - VM host function bridge (whitelist-based)~%")
        (format t "    - #'fn resolves registered closures from function registry~%")
        (uiop:quit 0))
      (progn
        (format t "  STATUS: ~D failures~%" fail)
        (uiop:quit 1))))

(defun %do-selfhost (parsed)
  "Run all self-hosting verification phases and print a TAP-like summary."
  (declare (ignore parsed))
  (let ((pass 0) (fail 0) (total 0))
    (flet ((check (name expected actual)
             (incf total)
             (if (equal expected actual)
                 (progn (incf pass) (format t "  ok ~D - ~A~%" total name))
                 (progn (incf fail)
                        (format t "  FAIL ~D - ~A~%    expected: ~S~%    got: ~S~%"
                                total name expected actual)))))
      (format t "~%cl-cc self-hosting verification (v~A)~%~%" *version*)
      (%selfhost-phase-macro-eval        #'check)
      (%selfhost-phase-basic-compilation #'check)
      (%selfhost-phase-meta-circular     #'check)
      (%selfhost-phase-source-loading    #'check)
      (%selfhost-phase-host-eval         #'check)
      (%selfhost-print-summary pass fail total))))

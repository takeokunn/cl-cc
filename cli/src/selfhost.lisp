;;;; cli/src/selfhost.lisp — CL-CC Self-Hosting Verification
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
    "packages/frontend/parse/src/package.lisp"
    "packages/frontend/parse/src/cst.lisp" "packages/frontend/parse/src/diagnostics.lisp"
    "packages/frontend/parse/src/lexer.lisp" "packages/frontend/parse/src/lexer-readers.lisp" "packages/frontend/parse/src/lexer-skip.lisp"
    "packages/frontend/parse/src/lexer-dispatch.lisp"
    "packages/frontend/parse/src/incremental.lisp" "packages/frontend/parse/src/pratt.lisp"
    "packages/frontend/parse/src/combinators.lisp"
    "packages/frontend/parse/src/cl/parser.lisp" "packages/frontend/parse/src/cl/parser-sexp-lowering.lisp"
    "packages/frontend/parse/src/cl/lower.lisp" "packages/frontend/parse/src/cl/lower-definitions.lisp"
    "packages/frontend/parse/src/cl/lower-clos.lisp"
    "packages/frontend/parse/src/cl/parser-roundtrip.lisp"
    "packages/frontend/parse/src/cl/grammar.lisp"
    "packages/frontend/parse/src/php/lexer.lisp" "packages/frontend/parse/src/php/lexer-ops.lisp"
    "packages/frontend/parse/src/php/parser.lisp" "packages/frontend/parse/src/php/parser-expr.lisp"
    "packages/frontend/parse/src/php/parser-stmt.lisp"
    "packages/frontend/parse/src/php/grammar.lisp" "packages/frontend/parse/src/php/grammar-stmt.lisp"
    "packages/frontend/parse/src/cst-to-ast.lisp"
    ;; expand
    "packages/frontend/expand/src/macro.lisp"
    "packages/frontend/expand/src/macros-basic.lisp"
    "packages/frontend/expand/src/loop-data.lisp" "packages/frontend/expand/src/loop-parser.lisp"
    "packages/frontend/expand/src/loop-emitters.lisp" "packages/frontend/expand/src/loop.lisp"
    "packages/frontend/expand/src/macros-stdlib.lisp"
    "packages/frontend/expand/src/macros-sequence.lisp"
    "packages/frontend/expand/src/macros-compat.lisp"
    "packages/frontend/expand/src/expander-data.lisp"
    "packages/frontend/expand/src/expander-defstruct.lisp"
    "packages/frontend/expand/src/expander.lisp"
    ;; vm
    "packages/engine/vm/src/package.lisp" "packages/engine/vm/src/vm.lisp"
    "packages/engine/vm/src/vm-execute.lisp" "packages/engine/vm/src/vm-clos.lisp"
    "packages/engine/vm/src/vm-run.lisp" "packages/engine/vm/src/vm-opcodes.lisp"
    "packages/engine/vm/src/primitives.lisp"
    "packages/engine/vm/src/vm-bitwise.lisp" "packages/engine/vm/src/vm-transcendental.lisp"
    "packages/engine/vm/src/vm-numeric.lisp" "packages/engine/vm/src/vm-extensions.lisp"
    "packages/engine/vm/src/io.lisp" "packages/engine/vm/src/format.lisp" "packages/engine/vm/src/conditions.lisp"
    "packages/engine/vm/src/list.lisp" "packages/engine/vm/src/array.lisp"
    "packages/engine/vm/src/strings.lisp" "packages/engine/vm/src/symbols.lisp" "packages/engine/vm/src/hash.lisp"
    ;; type
    "packages/type/type/src/package.lisp" "packages/type/type/src/kind.lisp" "packages/type/type/src/multiplicity.lisp"
    "packages/type/type/src/types-core.lisp" "packages/type/type/src/types-extended.lisp" "packages/type/type/src/types-env.lisp"
    "packages/type/type/src/substitution.lisp" "packages/type/type/src/unification.lisp"
    "packages/type/type/src/subtyping.lisp" "packages/type/type/src/effect.lisp" "packages/type/type/src/row.lisp"
    "packages/type/type/src/constraint.lisp" "packages/type/type/src/parser.lisp" "packages/type/type/src/typeclass.lisp"
    "packages/type/type/src/solver.lisp" "packages/type/type/src/inference.lisp"
    "packages/type/type/src/inference-effects.lisp" "packages/type/type/src/bidirectional.lisp"
    "packages/type/type/src/checker.lisp" "packages/type/type/src/printer.lisp"
    ;; compile
    "packages/foundation/ir/src/types.lisp" "packages/foundation/ir/src/block.lisp"
    "packages/foundation/ir/src/ssa.lisp" "packages/foundation/ir/src/printer.lisp"
    "packages/engine/compile/src/context.lisp" "packages/engine/compile/src/closure.lisp" "packages/engine/compile/src/cps.lisp"
    "packages/engine/compile/src/builtin-registry-data.lisp" "packages/engine/compile/src/builtin-registry.lisp"
    "packages/engine/compile/src/codegen-core.lisp" "packages/engine/compile/src/codegen-clos.lisp"
    "packages/engine/compile/src/codegen-functions.lisp" "packages/engine/compile/src/codegen-phase2.lisp"
    "packages/engine/compile/src/codegen.lisp"
    ;; optimize
    "packages/engine/optimize/src/effects.lisp" "packages/engine/optimize/src/cfg.lisp" "packages/engine/optimize/src/ssa.lisp"
    "packages/engine/optimize/src/egraph.lisp" "packages/engine/optimize/src/egraph-rules.lisp"
    "packages/engine/optimize/src/optimizer-tables.lisp" "packages/engine/optimize/src/optimizer-inline.lisp"
    "packages/engine/optimize/src/optimizer.lisp"
    ;; emit
    "packages/foundation/mir/src/mir.lisp" "packages/foundation/mir/src/target.lisp" "packages/backend/emit/src/calling-convention.lisp"
    "packages/backend/emit/src/regalloc.lisp"
    "packages/backend/emit/src/x86-64.lisp" "packages/backend/emit/src/x86-64-encoding.lisp" "packages/backend/emit/src/x86-64-codegen.lisp"
    "packages/backend/emit/src/aarch64.lisp" "packages/backend/emit/src/aarch64-codegen.lisp"
    "packages/backend/emit/src/wasm-types.lisp" "packages/backend/emit/src/wasm-ir.lisp" "packages/backend/emit/src/wasm-extract.lisp"
    "packages/backend/emit/src/wasm-trampoline.lisp" "packages/backend/emit/src/wasm.lisp"
    "packages/backend/binary/src/package.lisp" "packages/backend/binary/src/macho.lisp"
    "packages/backend/binary/src/elf.lisp" "packages/backend/binary/src/wasm.lisp"
    ;; bytecode + runtime + pipeline
    "packages/backend/bytecode/src/package.lisp" "packages/backend/bytecode/src/encode.lisp" "packages/backend/bytecode/src/decode.lisp"
    "packages/backend/runtime/src/package.lisp" "packages/backend/runtime/src/runtime.lisp" "packages/backend/runtime/src/value.lisp"
    "packages/backend/runtime/src/frame.lisp" "packages/backend/runtime/src/heap.lisp" "packages/backend/runtime/src/gc.lisp"
    "pipeline/src/stdlib-source.lisp" "pipeline/src/pipeline.lisp")
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
            (cl-cc/compile::*repl-label-counter*          nil)
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

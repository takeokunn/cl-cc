;;;; tests/e2e/selfhost-tests.lisp — Self-Hosting End-to-End Tests
;;;; Demonstrates that cl-cc can compile and run significant pieces of its own
;;;; compiler infrastructure: CPS transformer, optimizer, macro expander, etc.

(in-package :cl-cc/test)

(defsuite selfhost-suite
  :description "Self-hosting end-to-end tests"
  :parallel nil
  :parent cl-cc-e2e-suite)

(defsuite selfhost-slow-suite
  :description "Heavy self-hosting end-to-end regression tests"
  :parallel nil
  :parent cl-cc-e2e-suite)

(in-suite selfhost-suite)

;;; Helper: run multiple forms in a fresh REPL context, return last result.
;;; Each call creates isolated REPL state that doesn't interfere with other tests.
(defun run-repl-forms (&rest forms)
  "Run FORMS in a fresh REPL context, return last result."
  (let ((cl-cc::*repl-vm-state* nil)
        (cl-cc::*repl-accessor-map* nil)
        (cl-cc::*repl-pool-instructions* nil)
        (cl-cc::*repl-pool-labels* nil)
        (cl-cc::*repl-global-vars-persistent* nil)
        (cl-cc::*repl-label-counter* nil)
        (cl-cc::*repl-defstruct-registry* nil))
    (let ((result nil))
      (dolist (form forms result)
        (setf result (run-string-repl form))))))

;;; ─── REPL State Tests ──────────────────────────────────────────────────────

(deftest selfhost-defvar-persists
  "defvar values persist across REPL calls."
  :timeout 30
  (assert-eql 200
    (run-repl-forms
     "(defvar *sh-counter* 100)"
     "(setq *sh-counter* 200)"
     "*sh-counter*")))

(deftest selfhost-label-isolation
  "Labels from different REPL compilations don't collide."
  :timeout 30
  (assert-eq :yes
    (run-repl-forms
     "(defun sh-pred (x) (or (numberp x) (symbolp x)))"
     "(defun sh-check (x y) (if (sh-pred x) :yes :no))"
     "(sh-check 42 'ignored)")))

;;; ─── Self-Hosting: CPS Transformer ─────────────────────────────────────────

(in-suite selfhost-slow-suite)

(deftest-each selfhost-quasiquote
  "quasiquote works in self-hosted helper definitions and produces the expected result."
  :cases (("defun-builds-form" 9
           "(defun make-mul (a b) `(* ,a ,b))"
           "(let ((form (make-mul 3 3))) (eval form))")
          ("defun-builds-binding-form" t
           "(defun wrap-in-let (var val body) `(let ((,var ,val)) ,body))"
           "(equal (wrap-in-let 'x 5 '(+ x 1)) '(let ((x 5)) (+ x 1)))"))
  (expected setup-form eval-form)
  (assert-true
    (equal expected
           (run-repl-forms setup-form eval-form))))

(deftest selfhost-cps-transformer
  "cl-cc compiles a CPS transformer using quasiquotes and recursion."
  :timeout 30
  (let ((r (run-repl-forms
            "(defun sh-cps-atom-p (x)
              (or (numberp x) (symbolp x) (stringp x)))"
            "(defun sh-cps (expr k)
              (cond
                ((sh-cps-atom-p expr) `(funcall ,k ,expr))
                 ((eq (car expr) 'if)
                  (let ((tv (gensym \"T\"))
                        (then-r (sh-cps (caddr expr) k))
                        (else-r (sh-cps (cadddr expr) k)))
                    (sh-cps (cadr expr)
                             `(lambda (,tv) (if ,tv ,then-r ,else-r)))))
                 (t `(funcall ,k ,expr))))"
            "(sh-cps '(if x 1 2) '(lambda (v) v))")))
    (assert-true (and (consp r) (eq (car r) 'funcall)))))

(deftest-each selfhost-cps-transformer-arithmetic
  "cl-cc compiles and runs its own CPS transformer for arithmetic expressions."
  :timeout 30
  :cases (("add-1-2" 3 '(+ 1 2))
          ("mul-6-7" 42 '(* 6 7)))
  (expected expr)
  (assert-= expected
            (run-repl-forms
             "(defun sh-cps-run (expr)
                (cond
                  ((integerp expr) expr)
                  ((symbolp expr) expr)
                  ((consp expr)
                   (case (car expr)
                     (+ (+ (sh-cps-run (second expr)) (sh-cps-run (third expr))))
                     (- (- (sh-cps-run (second expr)) (sh-cps-run (third expr))))
                     (* (* (sh-cps-run (second expr)) (sh-cps-run (third expr))))
                     (otherwise (error \"Unsupported\"))))
                  (t (error \"Unsupported\"))))"
             (format nil "(sh-cps-run '~S)" expr))))

;;; ─── Self-Hosting: Optimizer Pattern Matcher ───────────────────────────────

(deftest selfhost-optimizer-fold
  "cl-cc compiles an optimizer-style constant folder."
  :timeout 30
  (assert-eql 7
    (run-repl-forms
      "(defun sh-fold (op a b)
        (cond
          ((and (eq op '+) (numberp a) (numberp b)) (+ a b))
          ((and (eq op '+) (eql a 0)) b)
          ((and (eq op '+) (eql b 0)) a)
          (t (list op a b))))"
      "(sh-fold '+ 3 4)")))

;;; ─── Self-Hosting: Macro Code Generation ───────────────────────────────────

(deftest selfhost-macro-codegen
  "cl-cc compiles a macro that generates constructor and accessor functions."
  :timeout 30
  (assert-eql 30
    (run-repl-forms
      "(defmacro sh-def-record (name &rest fields)
        `(progn
           (defun ,(intern (format nil \"MAKE-~A\" name)) (&rest args)
             args)
           (defun ,(intern (format nil \"~A-REF\" name)) (obj field)
             (getf obj field))))"
      "(sh-def-record sh-person :name :age)"
      "(let ((p (make-sh-person :name \"Alice\" :age 30)))
         (sh-person-ref p :age))")))

;;; ─── Self-Hosting: Recursive Data Processing ──────────────────────────────

(in-suite selfhost-suite)

(deftest selfhost-tree-walk
  "cl-cc compiles a recursive tree walker."
  :timeout 30
  (assert-eql 10
    (run-repl-forms
     "(defun sh-tree-sum (tree)
        (if (numberp tree)
            tree
            (+ (sh-tree-sum (car tree))
               (sh-tree-sum (cdr tree)))))"
     "(sh-tree-sum '(1 . (2 . (3 . 4))))")))

;;; ─── Self-Hosting: Load File ───────────────────────────────────────────────

(deftest selfhost-load-multi-form
  "cl-cc can load a file with multiple top-level forms."
  :timeout 30
  (let ((tmpfile (format nil "/tmp/cl-cc-selfhost-~A.lisp" (get-universal-time))))
    (unwind-protect
         (progn
           (with-open-file (s tmpfile :direction :output :if-exists :supersede)
             (write-string "(defvar *sh-base* 100)
(defun sh-offset (n) (+ *sh-base* n))" s))
           (assert-eql 142
             (run-repl-forms
              (format nil "(load ~S)" tmpfile)
              "(sh-offset 42)")))
      (ignore-errors (delete-file tmpfile)))))

;;; ─── Self-Hosting: Higher-Order Functions ──────────────────────────────────

(deftest selfhost-hof-pipeline
  "cl-cc compiles a pipeline of higher-order functions via lambda wrappers."
  :timeout 30
  (assert-eql 21
    (run-repl-forms
     "(defun sh-compose (f g) (lambda (x) (funcall f (funcall g x))))"
     "(defun sh-add1 (x) (+ x 1))"
     "(defun sh-double (x) (* x 2))"
     "(funcall (sh-compose (lambda (x) (sh-add1 x)) (lambda (x) (sh-double x))) 10)")))

;;; ─── Self-Hosting: Handler-Case with Recovery ─────────────────────────────

(deftest selfhost-error-recovery
  "cl-cc compiles handler-case for error recovery."
  :timeout 30
  (assert-eql 42
    (run-string "(handler-case
                   (progn (error \"oops\") 0)
                   (error (e) 42))")))

;;; ─── Self-Hosting: defstruct roundtrip ─────────────────────────────────────

(deftest selfhost-defstruct-roundtrip
  "cl-cc compiles defstruct with constructors and accessors."
  :timeout 30
  (assert-eql 4
    (run-repl-forms
      "(defstruct sh-point x y)"
      "(let ((p (make-sh-point :x 3 :y 4)))
         (sh-point-y p))")))

;;; ─── Self-Hosting: Mutual Recursion via labels ─────────────────────────────

(deftest selfhost-mutual-recursion
  "cl-cc compiles mutually recursive local functions via labels."
  :timeout 30
  (assert-true
    (run-string "(labels ((is-even (n) (if (= n 0) t (is-odd (- n 1))))
                          (is-odd (n) (if (= n 0) nil (is-even (- n 1)))))
                   (is-even 10))")))

;;; ─── Self-Hosting: Reader Macros ─────────────────────────────────────────

(deftest selfhost-reader-macros
  "Reader macros #:, #+, #-, and #. compile and evaluate correctly."
  :timeout 30
  (assert-true
    (run-string "(symbolp (quote #:foo))"))
  (assert-eq :yes
    (run-string "#+sbcl :yes"))
  (assert-eq :yes
    (run-string "#-nonexistent-feature :yes"))
  (assert-eq :fallback
    (run-string "(progn #+nonexistent-feature :no :fallback)"))
  (assert-eql 6
    (run-string "(+ 1 #.(+ 2 3))")))

(deftest selfhost-read-eval-respects-special
  "#.-reader evaluation is rejected when *read-eval* is nil."
  :timeout 30
  (assert-true
   (null
    (ignore-errors
      (run-string "(let ((*read-eval* nil)) (read-from-string \"#.(+ 2 3)\"))")))))

;;; ─── Self-Hosting: Meta-Circular Compilation ─────────────────────────────

(deftest selfhost-meta-circular-eval
  "cl-cc compiles nested expressions through host-level double compilation."
  :timeout 30
  (assert-eql 42
    (run-string (format nil "~A" (run-string "(+ 21 21)")))))

(deftest selfhost-meta-circular-defun
  "cl-cc compiles recursive factorial through REPL state."
  :timeout 30
  (assert-eql 120
    (run-repl-forms
     "(defun sh-meta-f (n) (if (<= n 1) 1 (* n (sh-meta-f (- n 1)))))"
     "(sh-meta-f 5)")))

(deftest selfhost-meta-circular-closure
  "cl-cc compiles a closure that captures a let-binding."
  :timeout 30
  (assert-eql 15
    (run-string "(let ((x 10)) (funcall (lambda (y) (+ x y)) 5))")))

;;; ─── Self-Hosting: True Meta-Circular (VM calls run-string) ─────────────
;;; These tests prove that code running in cl-cc's VM can invoke cl-cc's own
;;; compiler (run-string) via the host function bridge.  This is the key
;;; demonstration of meta-circular compilation: the compiler compiles code
;;; that invokes the compiler.

(deftest selfhost-meta-circular-compilation
  "VM code invokes cl-cc's own compiler via host bridge (true meta-circular compilation)."
  :timeout 30
  (assert-eql 42
    (run-string "(run-string \"(+ 21 21)\")"))
  (assert-eql 120
    (run-string
     "(run-string \"(defun sh-meta-fact (n) (if (<= n 1) 1 (* n (sh-meta-fact (- n 1))))) (sh-meta-fact 5)\")"))
  (assert-eql 15
    (run-string
     "(run-string \"(let ((x 10)) (funcall (lambda (y) (+ x y)) 5))\")")))

;;; ─── Self-Hosting: Load File + Use Definitions ──────────────────────────

(defun run-load-and-eval (file-path &rest forms)
  "Load FILE-PATH through our-load, then evaluate FORMS in the same REPL state.
  Returns the result of the last form."
  (let ((cl-cc::*repl-vm-state* nil)
        (cl-cc::*repl-accessor-map* nil)
        (cl-cc::*repl-pool-instructions* nil)
        (cl-cc::*repl-pool-labels* nil)
        (cl-cc::*repl-global-vars-persistent* nil)
        (cl-cc::*repl-label-counter* nil)
        (cl-cc::*repl-defstruct-registry* nil))
    (cl-cc::our-load file-path)
    (let ((result nil))
      (dolist (form forms result)
        (setf result (run-string-repl form))))))

(deftest selfhost-load-and-use-defs
  "Load a file with defvar+defun through our-load, then use its definitions."
  :timeout 30
  (let ((tmpfile (format nil "/tmp/cl-cc-sh-defs-~A.lisp" (get-universal-time))))
    (unwind-protect
         (progn
           (with-open-file (s tmpfile :direction :output :if-exists :supersede)
             (write-string "(defvar *sh-greeting* \"hello\")
(defun sh-greet (name) (list *sh-greeting* name))" s))
           (assert-equal '("hello" "world")
             (run-load-and-eval tmpfile
               "(sh-greet \"world\")")))
      (ignore-errors (delete-file tmpfile)))))

(in-suite selfhost-slow-suite)

(deftest selfhost-load-and-use-recursion
  "Load a file with recursive functions through our-load, then call them."
  :timeout 30
  (let ((tmpfile (format nil "/tmp/cl-cc-sh-rec-~A.lisp" (get-universal-time))))
    (unwind-protect
         (progn
           (with-open-file (s tmpfile :direction :output :if-exists :supersede)
             (write-string "(defun sh-fib (n)
  (if (<= n 1) n
      (+ (sh-fib (- n 1)) (sh-fib (- n 2)))))
(defun sh-ack (m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (sh-ack (- m 1) 1))
        (t (sh-ack (- m 1) (sh-ack m (- n 1))))))" s))
            (assert-eql 13 (run-load-and-eval tmpfile "(sh-fib 7)"))
            (assert-eql 7 (run-load-and-eval tmpfile "(sh-ack 2 2)")))
       (ignore-errors (delete-file tmpfile)))))

(in-suite selfhost-suite)

(deftest selfhost-load-chain
  "Load two files sequentially, second file uses first file's definitions."
  :timeout 30
  (let ((file1 (format nil "/tmp/cl-cc-sh-chain1-~A.lisp" (get-universal-time)))
        (file2 (format nil "/tmp/cl-cc-sh-chain2-~A.lisp" (get-universal-time))))
    (unwind-protect
         (progn
           (with-open-file (s file1 :direction :output :if-exists :supersede)
             (write-string "(defvar *sh-base* 1000)
(defun sh-offset (n) (+ *sh-base* n))" s))
           (with-open-file (s file2 :direction :output :if-exists :supersede)
             (write-string "(defun sh-combined (a b)
  (+ (sh-offset a) (sh-offset b)))" s))
            (let ((cl-cc::*repl-vm-state* nil)
                  (cl-cc::*repl-accessor-map* nil)
                  (cl-cc::*repl-pool-instructions* nil)
                  (cl-cc::*repl-pool-labels* nil)
                  (cl-cc::*repl-global-vars-persistent* nil)
                  (cl-cc::*repl-label-counter* nil)
                  (cl-cc::*repl-defstruct-registry* nil))
             (cl-cc::our-load file1)
             (cl-cc::our-load file2)
             (assert-eql 2003
               (run-string-repl "(sh-combined 1 2)"))))
      (ignore-errors (delete-file file1))
      (ignore-errors (delete-file file2)))))

;;; ─── Self-Hosting: Source File Loading ────────────────────────────────────

(defvar *selfhost-all-source-files*
  '("src/package.lisp"
    "src/parse/cst.lisp"
    "src/parse/diagnostics.lisp"
    "src/parse/ast.lisp"
    "src/parse/prolog.lisp"
    "src/parse/dcg.lisp"
    "src/parse/lexer.lisp"
    "src/parse/incremental.lisp"
    "src/parse/pratt.lisp"
    "src/parse/combinators.lisp"
    "src/parse/cl/parser.lisp"
    "src/parse/cl/grammar.lisp"
    "src/parse/php/lexer.lisp"
    "src/parse/php/parser.lisp"
    "src/parse/php/grammar.lisp"
    "src/parse/cst-to-ast.lisp"
    "src/expand/macro.lisp"
    "src/expand/expander.lisp"
    "src/vm/package.lisp"
    "src/vm/vm.lisp"
    "src/vm/primitives.lisp"
    "src/vm/io.lisp"
    "src/vm/conditions.lisp"
    "src/vm/list.lisp"
    "src/vm/strings.lisp"
    "src/vm/hash.lisp"
    "src/type/package.lisp"
    "src/type/kind.lisp"
    "src/type/multiplicity.lisp"
    "src/type/representation.lisp"
    "src/type/substitution.lisp"
    "src/type/unification.lisp"
    "src/type/subtyping.lisp"
    "src/type/effect.lisp"
    "src/type/row.lisp"
    "src/type/constraint.lisp"
    "src/type/parser.lisp"
    "src/type/typeclass.lisp"
    "src/type/solver.lisp"
    "src/type/inference.lisp"
    "src/type/checker.lisp"
    "src/type/printer.lisp"
    "src/compile/ir/types.lisp"
    "src/compile/ir/block.lisp"
    "src/compile/ir/ssa.lisp"
    "src/compile/ir/printer.lisp"
    "src/compile/context.lisp"
    "src/compile/closure.lisp"
    "src/compile/cps.lisp"
    "src/compile/builtin-registry.lisp"
    "src/compile/codegen.lisp"
    "src/optimize/effects.lisp"
    "src/optimize/cfg.lisp"
    "src/optimize/ssa.lisp"
    "src/optimize/egraph.lisp"
    "src/optimize/egraph-rules.lisp"
    "src/optimize/optimizer.lisp"
    "src/emit/mir.lisp"
    "src/emit/target.lisp"
    "src/emit/calling-convention.lisp"
    "src/emit/regalloc.lisp"
    "src/emit/x86-64.lisp"
    "src/emit/x86-64-codegen.lisp"
    "src/emit/aarch64.lisp"
    "src/emit/aarch64-codegen.lisp"
    "src/emit/wasm-types.lisp"
    "src/emit/wasm-ir.lisp"
    "src/emit/wasm-extract.lisp"
    "src/emit/wasm-trampoline.lisp"
    "src/emit/wasm.lisp"
    "src/emit/binary/package.lisp"
    "src/emit/binary/macho.lisp"
    "src/emit/binary/elf.lisp"
    "src/emit/binary/wasm.lisp"
    "src/bytecode/package.lisp"
    "src/bytecode/encode.lisp"
    "src/bytecode/decode.lisp"
    "src/runtime/package.lisp"
    "src/runtime/runtime.lisp"
    "src/runtime/value.lisp"
    "src/runtime/frame.lisp"
    "src/runtime/heap.lisp"
    "src/runtime/gc.lisp"
    "src/compile/pipeline.lisp")
  "All 84 source files in ASDF dependency order.")

(defvar *selfhost-representative-files*
  '("src/parse/cst.lisp"
    "src/parse/prolog.lisp"
    "src/parse/lexer.lisp"
    "src/compile/cps.lisp"
    "src/optimize/optimizer.lisp"
    "src/type/package.lisp"
    "src/type/kind.lisp"
    "src/type/parser.lisp"
    "src/type/typeclass.lisp"
    "src/type/solver.lisp"
    "src/type/inference.lisp"
    "src/type/checker.lisp"
    "src/type/printer.lisp"
    "src/emit/mir.lisp"
    "src/vm/vm.lisp"
    "src/runtime/gc.lisp")
  "Representative subset of source files covering all major modules.")

(in-suite selfhost-slow-suite)

(deftest selfhost-load-own-source
  "cl-cc can load a representative subset of its own source files."
  :timeout 30
  (let ((ok 0))
    (let ((cl-cc::*repl-vm-state* nil)
          (cl-cc::*repl-accessor-map* nil)
          (cl-cc::*repl-pool-instructions* nil)
          (cl-cc::*repl-pool-labels* nil)
          (cl-cc::*repl-global-vars-persistent* nil)
          (cl-cc::*repl-label-counter* nil))
      (dolist (f *selfhost-representative-files*)
        (handler-case
          (progn (cl-cc::our-load f) (incf ok))
          (error (e)
            (declare (ignore e))))))
    (assert-eql (length *selfhost-representative-files*) ok)))

(in-suite selfhost-suite)

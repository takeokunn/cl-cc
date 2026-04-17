# cl-cc

A self-hosting Common Lisp compiler and runtime implemented in pure Common Lisp — no C, no FFI.

cl-cc compiles ANSI Common Lisp to a register-based bytecode VM, and from there to native x86-64, AArch64, and WebAssembly. The compiler is itself written in Common Lisp, and its core design — CLOS dispatch, Prolog-based optimization, CPS transformation, Hindley–Milner type inference — is implemented using the same language features it compiles.

## Quick Start

```bash
nix develop
nix run .#test     # run the canonical unit / integration / e2e test plan
cl-cc repl         # interactive REPL
cl-cc run file.lisp
cl-cc eval "(+ 1 2)"
cl-cc compile file.lisp -o out --arch x86-64
```

## Language Support

### Core Special Forms

| Form | Status |
|---|---|
| `if`, `progn`, `block`/`return-from` | ✓ |
| `tagbody`/`go` | ✓ |
| `catch`/`throw`, `unwind-protect` | ✓ |
| `let`, `let*`, `setq`, `setf` | ✓ |
| `flet`, `labels` (mutual recursion) | ✓ |
| `lambda`, `defun`, `defvar`, `defparameter` | ✓ |
| `defmacro`, `macrolet` | ✓ |
| `quote`, `the`, `values` | ✓ |
| `multiple-value-bind`, `multiple-value-call` | ✓ |
| `eval-when` | ✓ |

### Closures & Higher-Order Functions

```lisp
;; Closures capture and mutate lexical variables
(let ((count 0))
  (let ((inc (lambda () (setq count (+ count 1))))
        (get (lambda () count)))
    (funcall inc)
    (funcall inc)
    (funcall get)))   ; => 2

;; Higher-order functions
(labels ((make-adder (n) (lambda (x) (+ x n))))
  (let ((add5 (make-adder 5)))
    (funcall add5 37)))  ; => 42
```

### CLOS — Object System

Full CLOS implementation including multiple dispatch, inheritance chains, and `call-next-method`.

```lisp
(defclass shape ()
  ((color :initarg :color :reader shape-color)))

(defclass circle (shape)
  ((radius :initarg :radius :reader circle-radius)))

(defclass rectangle (shape)
  ((width  :initarg :width  :reader rectangle-width)
   (height :initarg :height :reader rectangle-height)))

(defgeneric area (shape))

(defmethod area ((c circle))
  (* 3.14159 (circle-radius c) (circle-radius c)))

(defmethod area ((r rectangle))
  (* (rectangle-width r) (rectangle-height r)))

(let ((c (make-instance 'circle    :color :red   :radius 5))
      (r (make-instance 'rectangle :color :blue  :width 4 :height 6)))
  (list (area c) (area r)))   ; => (78.53975 24)
```

### Macros

```lisp
(defmacro my-when (condition &body body)
  `(if ,condition (progn ,@body) nil))

(defmacro swap! (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (setq ,a ,b)
       (setq ,b ,tmp))))
```

### Conditions & Error Handling

```lisp
(handler-case
    (error "something went wrong: ~A" 42)
  (error (e)
    (format nil "caught: ~A" e)))

(ignore-errors
  (/ 1 0))
```

### Structures

```lisp
;; defstruct expands to defclass + constructor + predicate
(defstruct point
  (x 0)
  (y 0))

(let ((p (make-point :x 3 :y 4)))
  (+ (point-x p) (point-y p)))   ; => 7
```

### Standard Library

**Lists**: `cons`, `car`, `cdr`, all 28 `c*r` forms, `list`, `append`, `nconc`, `reverse`, `nreverse`, `length`, `nth`, `nthcdr`, `last`, `butlast`, `copy-list`

**Sequences**: `mapcar`, `mapc`, `mapcan`, `every`, `some`, `notany`, `notevery`, `find`, `find-if`, `position`, `count`, `remove`, `remove-if`, `remove-duplicates`, `sort`, `stable-sort`, `reduce`, `merge`, `substitute`, `delete`

**Strings**: `string=`, `string<`, `string>`, `string-upcase`, `string-downcase`, `subseq`, `concatenate`, `string-trim`, `search`

**Characters**: `char-code`, `code-char`, `char-upcase`, `char-downcase`, `digit-char-p`, `alpha-char-p`, all comparison predicates

**Hash tables**: `make-hash-table`, `gethash`, `(setf gethash)`, `remhash`, `clrhash`, `maphash`, `hash-table-count`

**Arrays**: `make-array`, `aref`, `(setf aref)`, `vector-push-extend`, `array-rank`, `array-dimensions`, bit array operations

**Numbers**: full arithmetic, `floor`/`ceiling`/`truncate`/`round`, `mod`, `rem`, `abs`, `max`, `min`, `expt`, `sqrt`, `exp`, `log`, trig functions, `ash`, `logand`, `logior`, `logxor`, `lognot`

**I/O**: `format`, `print`, `princ`, `prin1`, `read-line`, `read-char`, `write-char`, `with-open-file`, `with-output-to-string`

**Predicates**: `numberp`, `integerp`, `stringp`, `symbolp`, `consp`, `null`, `listp`, `functionp`, `characterp`, `vectorp`, `hash-table-p`

### Multiple Values

```lisp
(multiple-value-bind (q r)
    (floor 17 5)
  (list q r))   ; => (3 2)

(values 1 2 3)
```

### Loop

```lisp
(loop for x in '(1 2 3 4 5)
      when (oddp x)
      collect (* x x))   ; => (1 9 25)

(loop for i from 1 to 10
      sum i)   ; => 55
```

## REPL

```
$ cl-cc repl
CL-CC 0.1.0  —  ANSI Common Lisp
Type a CL form and press Return. (exit) or Ctrl+D to quit.

* (defun factorial (n)
    (if (<= n 1) 1 (* n (factorial (- n 1)))))
=> FACTORIAL

* (factorial 10)
=> 3628800

* (defstruct point (x 0) (y 0))
=> POINT

* (point-x (make-point :x 7 :y 3))
=> 7

* (exit)
Goodbye.
```

Definitions persist across expressions within a session (function registry, class registry, and heap are preserved).

## Architecture

```
Source (.lisp)
    │
    ▼
Lexer + CST Parser (hand-written, incremental)
    │
    ▼
Macro Expander (defstruct→defclass, defconstant→defparameter, etc.)
    │
    ▼
AST (CLOS defstructs: ast-defun, ast-let, ast-defclass, …)
    │
    ▼
CPS Transform (optional)
    │
    ▼
Codegen → VM Bytecode
    │       (register-based, ~220 instruction types)
    │
    ├──► VM Interpreter  (SBCL-hosted; meta-circular eval)
    │
    ├──► MIR / SSA  (Braun et al. 2013)
    │       │
    │       ├──► x86-64 assembly text / Mach-O binary
    │       ├──► AArch64 assembly text
    │       └──► WebAssembly (wasm32)
    │
    └──► Bytecode encoder/decoder (portable format)

Optimizations:
  - Multi-pass: CSE, constant folding, copy propagation
  - Dead code elimination, strength reduction, jump threading
  - Prolog-based peephole optimizer
  - E-graph equality saturation
  - CFG construction + SSA form

Type System:
  - Hindley–Milner (Algorithm W) with gradual typing
  - Union type narrowing on conditionals
  - Parametric types: (List T), (Option T)
  - Typeclass stubs

Runtime:
  - 2-generation GC: Young (Cheney semi-space) + Old (tri-color mark-sweep)
  - SATB write barrier
  - Heap-allocated closures, cons cells, CLOS instances
```

## Self-Hosting

cl-cc is self-hosting in the meta-circular sense:

1. **Meta-circular `eval`**: `(eval '...)` inside a running program calls back into the cl-cc compiler pipeline, not the host SBCL.
2. **Macro expansion via `our-eval`**: `defmacro` and `define-compiler-macro` expansion is handled by cl-cc compiling and running its own bytecode.
3. **Compiler-in-the-compiler**: cl-cc can compile programs that themselves implement parsers, compilers, and evaluators — including CPS transformers and stack-machine compilers that mirror cl-cc's own internals.
4. **REPL state persistence**: Function, class, and accessor definitions persist across `run-string-repl` calls, enabling incremental REPL-driven development.
5. **Quasiquote in compiled code**: cl-cc correctly compiles `` ` `` and `,` in `defun` bodies, enabling it to run its own macro-generating functions — the same pattern used throughout `packages/engine/compile/src/cps.lisp`.

**Mini AST compiler** (run with `cl-cc run`):

```lisp
(defstruct ast-node kind value children)

(defun parse-expr (sexp)
  (cond
    ((integerp sexp)
     (make-ast-node :kind :lit :value sexp :children nil))
    ((and (consp sexp) (eq (car sexp) '+))
     (make-ast-node :kind :add :value nil
       :children (mapcar #'parse-expr (cdr sexp))))))

(defun eval-ast (node)
  (case (ast-node-kind node)
    (:lit  (ast-node-value node))
    (:add  (apply #'+ (mapcar #'eval-ast (ast-node-children node))))))

(eval-ast (parse-expr '(+ 1 (+ 2 (+ 3 4)))))  ; => 10
```

**CPS transformer with quasiquotes** — cl-cc compiles its own CPS transformation logic, using quasiquotes identical to `packages/engine/compile/src/cps.lisp`:

```lisp
;; This is the actual code from packages/engine/compile/src/cps.lisp — compiled by cl-cc itself
(defun %cps-sexp-binop (op a b k)
  (let ((va (gensym "A")) (vb (gensym "B")))
    (%cps-sexp-node a
      `(lambda (,va)
         ,(%cps-sexp-node b `(lambda (,vb) (funcall ,k (,op ,va ,vb))))))))

(defun %cps-sexp-node (node k)
  (cond
    ((integerp node) `(funcall ,k ,node))
    ((symbolp  node) `(funcall ,k ,node))
    ((consp node)
     (case (car node)
       ((+ - *) (%cps-sexp-binop (car node) (second node) (third node) k))
       (otherwise (error "Unsupported"))))))

(defun cps-transform (expr) `(lambda (k) ,(%cps-sexp-node expr 'k)))

;; The CPS form for (+ 1 2) — each subexpression gets its own continuation:
(cps-transform '(+ 1 2))
;; => (LAMBDA (K)
;;      (FUNCALL (LAMBDA (#:A1) (FUNCALL (LAMBDA (#:B2) (FUNCALL K (+ #:A1 #:B2))) 2)) 1))

;; Run it:
(eval (list (cps-transform '(* 6 7)) '(lambda (result) result)))  ; => 42
```

**Stack-machine compiler** — cl-cc compiles a program that compiles and executes expressions:

```lisp
(defun compile-expr (expr)
  (cond
    ((numberp expr) (list (list 'push expr)))
    ((eq (car expr) '+)
     (append (compile-expr (cadr expr))
             (compile-expr (caddr expr))
             (list '(add))))
    ((eq (car expr) '*)
     (append (compile-expr (cadr expr))
             (compile-expr (caddr expr))
             (list '(mul))))))

(defun execute (program)
  (let ((stack nil))
    (dolist (i program (car stack))
      (case (car i)
        (push (push (cadr i) stack))
        (add  (push (+ (pop stack) (pop stack)) stack))
        (mul  (push (* (pop stack) (pop stack)) stack))))))

(execute (compile-expr '(+ (* 2 3) (* 4 5))))  ; => 26
```

**CPS transformer** — implements cl-cc's own core technique inside a compiled program:

```lisp
(defun fib-cps (n k)
  (if (<= n 1)
      (funcall k n)
      (fib-cps (- n 1)
               (lambda (v1)
                 (fib-cps (- n 2)
                          (lambda (v2)
                            (funcall k (+ v1 v2))))))))

(defun identity (x) x)
(fib-cps 15 #'identity)  ; => 610
```

**REPL — persistent state across calls**:

```
$ cl-cc repl
* (defclass counter () ((n :initform 0 :accessor counter-n)))
=> #<HASH-TABLE ...>
* (defun bump (c) (setf (counter-n c) (+ (counter-n c) 1)) c)
=> #<VM-CLOSURE-OBJECT ...>
* (let ((c (make-instance 'counter))) (bump c) (bump c) (bump c) (counter-n c))
=> 3
```

## Known Limitations

- **Package system**: `defpackage`/`in-package`/`export` are no-ops at runtime. All symbols share the `:cl-cc` namespace. Multi-package programs are not yet supported.
- **Restarts**: `restart-case`, `abort`, `continue`, `muffle-warning` etc. are stub macros.
- **`format` directives**: Delegated to the host SBCL in the VM interpreter. Not available in native binaries.
- **Number tower**: `bignum`, `ratio`, `complex` work in the VM interpreter (host SBCL handles the arithmetic). Not represented in the native x86-64 backend (fixnum only).
- **Streams**: File handles are integers; first-class stream objects and stream predicates (`streamp`, `stream-element-type`) are not implemented.
- **`load` / `compile-file`**: Not yet implemented. Use `cl-cc run <file>` to execute files.

## Building & Testing

```bash
# Enter development environment
nix develop

# Run the canonical test plan (unit / integration / e2e)
nix run .#test

# Build the standalone ./cl-cc binary
nix run .#build

# CI-equivalent check (includes flake evaluation + the test plan)
nix flake check

# Clear FASL cache if tests misbehave
nix run .#clean
# or manually:
find ~/.cache/common-lisp/ -name "*.fasl" -delete
```

## CLI Reference

```
cl-cc run <file>          Compile and run a .lisp file
cl-cc compile <file>      Compile to native Mach-O binary
  --arch x86-64|arm64
  -o <output>
cl-cc eval "<expr>"       Evaluate a single expression
cl-cc repl                Interactive REPL (definitions persist)
  --stdlib                Include higher-order function library
cl-cc check <file>        Type-check without executing
  --strict                Treat type warnings as errors
```

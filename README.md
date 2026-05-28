# cl-cc

A self-hosting Common Lisp compiler and runtime implemented in pure Common Lisp — no project C source; the VM interpreter includes a minimal SBCL host-backed CFFI-compatible FFI shim.

cl-cc compiles ANSI Common Lisp to a register-based bytecode VM, and from there to native x86-64, AArch64, and WebAssembly. The compiler is itself written in Common Lisp, and its core design — CLOS dispatch, Prolog-based optimization, CPS transformation, Hindley–Milner type inference — is implemented using the same language features it compiles.

## Quick Start

```bash
nix develop
nix run .#test       # canonical fast unit plan
cl-cc repl           # interactive REPL
cl-cc run file.lisp
cl-cc eval "(+ 1 2)"
cl-cc compile file.lisp -o out --arch x86-64
```

`nix run .#test` maps to `cl-cc/test:run-tests` and executes the canonical
fast unit plan. Integration and self-hosting E2E suites are selected by suite
taxonomy and run explicitly; they no longer depend on `slow` names.
`nix flake check` invokes the same fast plan via `checks.tests`.

Timeouts are explicit for test execution. Set `CLCC_TEST_TIMEOUT` to override
the default per-test timeout (positive integer seconds; default: `10`). Set
`CLCC_SUITE_TIMEOUT` to override the whole-suite timeout (positive integer
seconds; default: `600`). Invalid, zero, or negative values are ignored and the
defaults are used. Individual tests may declare a positive `:timeout`, which
overrides `CLCC_TEST_TIMEOUT` for that test.
Stale FASLs (e.g. after switching between project paths) can be cleared with
`rm -rf ~/.cache/common-lisp/ && mkdir -p ~/.cache/common-lisp/`. The runner
otherwise keeps the warm cache to make repeat invocations fast.

## Language Support

### Core Special Forms

| Form                                         | Status |
| -------------------------------------------- | ------ |
| `if`, `progn`, `block`/`return-from`         | ✓      |
| `tagbody`/`go`                               | ✓      |
| `catch`/`throw`, `unwind-protect`            | ✓      |
| `let`, `let*`, `setq`, `setf`                | ✓      |
| `flet`, `labels` (mutual recursion)          | ✓      |
| `lambda`, `defun`, `defvar`, `defparameter`  | ✓      |
| `defmacro`, `macrolet`                       | ✓      |
| `quote`, `the`, `values`                     | ✓      |
| `multiple-value-bind`, `multiple-value-call` | ✓      |
| `eval-when`                                  | ✓      |

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

**Characters**: `char-code`, `code-char`, `char-upcase`, `char-downcase`, `digit-char-p`, `alpha-char-p`, `char-name`, `name-char`, all comparison predicates (`char=`, `char<`, `char>`, `char<=`, `char>=`, `char-equal`, `char-not-equal`, `char-lessp`, `char-greaterp`, `char-not-greaterp`, `char-not-lessp`)

**Hash tables**: `make-hash-table`, `gethash`, `(setf gethash)`, `remhash`, `clrhash`, `maphash`, `hash-table-count`

**Arrays**: `make-array` (adjustable, fill-pointer, displaced), `aref`, `(setf aref)`, `vector-push`, `vector-push-extend`, `vector-pop`, `fill-pointer`, `adjust-array`, `array-has-fill-pointer-p`, `adjustable-array-p`, `array-element-type`, `array-rank`, `array-dimensions`, `array-dimension`, `array-total-size`, `array-in-bounds-p`, `row-major-aref`, `array-displacement`, bit array operations

**Numbers**: full arithmetic, `floor`/`ceiling`/`truncate`/`round`, `mod`, `rem`, `abs`, `max`, `min`, `expt`, `sqrt`, `exp`, `log`, trig functions, `ash`, `logand`, `logior`, `logxor`, `lognot`

**I/O**: `format`, `print`, `princ`, `prin1`, `read-line`, `read-char`, `write-char`, `with-open-file`, `with-output-to-string`, `read-sequence`, `write-sequence`, `peek-char`, `unread-char`, `listen`, `clear-input`, `clear-output`, `finish-output`, `force-output`

**Strings**: `string=`, `string<`, `string>`, `string-upcase`, `string-downcase`, `string-capitalize`, `nstring-upcase`, `nstring-downcase`, `nstring-capitalize`, `subseq`, `concatenate`, `string-trim`, `string-left-trim`, `string-right-trim`, `search`

**Pretty Printer**: `pprint-logical-block`, `pprint-indent`, `pprint-newline`, `pprint-tab`, `pprint-dispatch-table`, `*print-pretty*`, `*print-level*`, `*print-length*`, `*print-circle*`, `*print-readably*`, `*print-base*`, `*print-radix*`

**Streams**: `broadcast-stream`, `concatenated-stream`, `echo-stream`, `synonym-stream`, `two-way-stream`, `string-input-stream`, `string-output-stream`, fundamental-stream (Gray Streams protocol)

**Unicode**: BMP General Category predicates, case folding, UTF-8 encoding/decoding, NFC/NFD normalization (Latin-1), syntax class determination

**Time**: `get-universal-time`, `get-internal-real-time`, `get-internal-run-time`, `sleep`, `encode-universal-time`, `decode-universal-time`, `time` macro

**Random**: `random-state` (MT19937), `make-random-state`, `*random-state*`, `random`

**Environment**: `lisp-implementation-type`, `lisp-implementation-version`, `machine-type`, `machine-version`, `software-type`, `software-version`, `room`, `apropos`, `apropos-list`

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

REPL history variables follow ANSI CL conventions:
- `*`, `**`, `***` — last 3 primary return values
- `+`, `++`, `+++` — last 3 input forms
- `/`, `//`, `///` — last 3 return value lists

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
CPS Transform (preferred lowering path for supported forms)
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
  - Prolog-backed rewrite stage (peephole + e-graph rule discovery)
  - E-graph equality saturation integrated into the main optimizer pipeline
  - CFG construction + SSA form

Type System:
  - Hindley–Milner (Algorithm W) with error-sentinel recovery paths
  - Union type narrowing on conditionals
  - Parametric types: (List T), (Option T)
  - Typeclasses (partial implementation)

Runtime:
  - 2-generation GC: Young (Cheney semi-space) + Old (tri-color mark-sweep)
  - SATB write barrier
  - Thread-Local Allocation Buffers (TLAB)
  - GC safepoint infrastructure with precise stack maps
  - Heap-allocated closures, cons cells, CLOS instances
  - `storage-condition` with 80/90/95% heap pressure warnings
  - Stack overflow guard (`*max-call-stack-depth*`)

Runtime Subsystems:
  - **Inline Caches**: Monomorphic, polymorphic, and megamorphic call site dispatch
  - **Type Feedback Vector (TFV)**: Runtime type profiling for Tier-1 compilation
  - **Concurrency**: Green threads (work-stealing scheduler), CSP channels, actor model, STM, futures/promises, structured task groups, fibers
  - **Memory Reclamation**: Epoch-Based Reclamation (EBR), Hazard Pointers, RCU, Quiescent-State-Based Reclamation (QSBR), MVCC
  - **Lock-Free**: Lock-free stack, queue, hash map, SPSC ring buffer
  - **Synchronization**: Mutex, RWLock, semaphore, condition variable, barrier, once-call
  - **OS Layer**: File I/O, process control, signal handling, mmap, socket/network (TCP/UDP), io_uring stubs, event loop
  - **FFI**: Foreign function calling, callback trampolines, native struct layout, inline assembly stubs
  - **Image**: Heap snapshot save/restore with magic/version/CRC32 verification
  - **Distributed**: Raft consensus (leader election, log replication), CRDTs (GCounter, PNCounter, LWWRegister), cluster membership
  - **Observability**: OpenTelemetry spans (JSON export), performance counters, structured logging, vector clocks, deadlock detector
  - **Atomic Operations**: CAS, swap, load, store, incf, memory barrier, load/store fences
```

## Self-Hosting

cl-cc is self-hosting in the meta-circular sense:

1. **Meta-circular `eval`**: `(eval '...)` inside a running program calls back into the cl-cc compiler pipeline, not the host SBCL.
2. **Macro expansion via `our-eval`**: `defmacro` and `define-compiler-macro` expansion is handled by cl-cc compiling and running its own bytecode.
3. **Compiler-in-the-compiler**: cl-cc can compile programs that themselves implement parsers, compilers, and evaluators — including CPS transformers and stack-machine compilers that mirror cl-cc's own internals.
4. **REPL state persistence**: Function, class, and accessor definitions persist across `run-string-repl` calls, enabling incremental REPL-driven development.
5. **Quasiquote in compiled code**: cl-cc correctly compiles `` ` `` and `,` in `defun` bodies, enabling it to run its own macro-generating functions — the same pattern used throughout `packages/cps/src/cps.lisp`.

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

**CPS transformer with quasiquotes** — cl-cc compiles its own CPS transformation logic, using quasiquotes identical to `packages/cps/src/cps.lisp`:

```lisp
;; This is the actual code from packages/cps/src/cps.lisp — compiled by cl-cc itself
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

ANSI CL conformance status: **8111 tests pass, 65 failures** (all pre-existing optimizer/backend/VM-CLOS regressions). See `docs/ansi-cl-lang.md` and `docs/ansi-cl-stdlib.md` for detailed feature requirement tracking.

- **Package system**: 14 runtime functions + 8 VM instructions added (Wave 1). Internal registry metadata exists; host CL is a bootstrap fallback. Multi-package self-hosting is partial. See `tests/conformance/package-conformance-tests.lisp` (18 expected-fail tests).
- **`format` directives**: Native `%vm-format-render` supports ~30 directives. `~_` (conditional newline) and `~I` (indent) added (Wave 2 — partial ANSI). Host SBCL fallback on error. Not available in native x86-64 binaries.
- **Number tower**: `bignum`, `ratio`, `complex` work in the VM interpreter (host SBCL arithmetic + JIT-callable bridges in `runtime-io.lisp`). Not represented in the native x86-64 backend (fixnum only). See `tests/conformance/number-conformance-tests.lisp` (24 expected-fail tests).
- **Native backend parity**: `load`, host-backed FFI, host-backed `format`, and host stream bridges not yet available in native binaries. 21 pathname/file/compound-stream/LOAD runtime functions added (Wave 4).
- **Unicode normalization**: NFC/NFD is implemented for Latin-1 characters. Full Unicode 15 normalization (NFKC/NFKD, UCA collation) requires the complete Unicode Character Database.
- **Concurrency**: Green threads, channels, actors, STM, lock-free structures, and EBR/RCU/QSBR are implemented as pure-CL primitives suitable for cooperative multitasking. Native OS thread scheduling and M:N threading are not yet integrated.
- **Distributed systems**: Raft consensus and CRDTs have proof-of-concept implementations. They lack full RPC integration, log persistence, and network transport layers needed for production use.
- **Standalone binary**: The `standalone` CLI option generates a native Mach-O/ELF binary with runtime linked, but full self-hosting (no host CL dependency at all) is not yet complete. Self-host pipeline exists in `packages/selfhost/src/pipeline-selfhost.lisp`.
- **I/O and streams**: 94 conformance tests in `tests/conformance/native-io-conformance-tests.lisp` (expected-fail). String streams, basic file I/O work via host CL bridging.

## Building & Testing

```bash
# Enter development environment
nix develop

# Run the canonical fast unit test plan
nix run .#test

# Build the standalone binary → ./result/bin/cl-cc
nix build

# Format the repo (nixfmt + deadnix + statix + prettier) via treefmt
nix fmt

# CI-equivalent check (flake evaluation + build + fast unit test plan)
nix flake check

# Clear FASL cache if tests misbehave (rm + mkdir avoids macOS SBCL race)
rm -rf ~/.cache/common-lisp/ && mkdir -p ~/.cache/common-lisp/
```

## CLI Reference

```
cl-cc run <file>          Compile and run a .lisp file
  --timeout <seconds>     Maximum execution time (default: 30)
  --no-timeout            Disable CLI timeout for debugging
cl-cc compile <file>      Compile to native Mach-O binary
  --arch x86-64|arm64
  -o <output>
  --timeout <seconds>     Maximum execution time (default: 30)
  --no-timeout            Disable CLI timeout for debugging
cl-cc eval "<expr>"       Evaluate a single expression
  --timeout <seconds>     Maximum execution time (default: 30)
  --no-timeout            Disable CLI timeout for debugging
cl-cc repl                Interactive REPL (definitions persist)
  --stdlib                Include higher-order function library
  --timeout <seconds>     Per-form execution timeout (default: 30)
  --no-timeout            Disable REPL form timeout for debugging
cl-cc check <file>        Type-check without executing
  --strict                Treat type warnings as errors
  --timeout <seconds>     Maximum execution time (default: 30)
  --no-timeout            Disable CLI timeout for debugging
cl-cc selfhost [file]     Run the self-hosting workload
  --timeout <seconds>     Maximum execution time (default: 30)
  --no-timeout            Disable CLI timeout for debugging
```

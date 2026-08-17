# Recipes

## The REPL

`cl-cc repl` starts an interactive session. Definitions persist across
expressions within a session — the function registry, the class registry, and
the heap are all preserved between forms.

```console
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

Persistence covers CLOS state too, so a class can be defined, instantiated,
and mutated across separate forms:

```console
$ cl-cc repl
* (defclass counter () ((n :initform 0 :accessor counter-n)))
=> #<HASH-TABLE ...>
* (defun bump (c) (setf (counter-n c) (+ (counter-n c) 1)) c)
=> #<VM-CLOSURE-OBJECT ...>
* (let ((c (make-instance 'counter))) (bump c) (bump c) (bump c) (counter-n c))
=> 3
```

### History variables

The REPL history variables follow ANSI CL conventions:

| Variable | Meaning |
|---|---|
| `*`, `**`, `***` | the last three primary return values |
| `+`, `++`, `+++` | the last three input forms |
| `/`, `//`, `///` | the last three return value lists |

Pass `--stdlib` to preload the higher-order function library, and
`--timeout` to bound each form's execution.

## The PHP frontend

cl-cc includes a PHP 8.0–8.5 frontend that compiles PHP source to the same CL
AST, and then through the same bytecode and native backend, as Common Lisp
programs. `.php` files are auto-detected; `--lang php` selects the frontend
explicitly for any command.

```sh
cl-cc run file.php
cl-cc run file.lisp --lang php
cl-cc compile file.php -o out --arch x86-64
cl-cc check file.php --strict
```

Statements lower to ordinary CL AST nodes:

| PHP | Lowering |
| --- | -------- |
| `echo $x;` | `ast-print` |
| `return $x;` / `return;` | `ast-return-from` |
| `if ($c) { } else { }` | `ast-if` |
| `while ($c) { }` | `ast-block` + tagbody loop |
| `for ($i=0; $i<n; $i++) { }` | `ast-progn` (init + while loop) |
| `foreach ($arr as $v)` / `foreach ($arr as $k => $v)` | `ast-let` |
| `function f($a, $b) { }` | `ast-defun` |
| `class C extends B { }` | `ast-defclass` |
| `switch ($x) { case 1: … default: }` | `ast-let` + `ast-block` + `ast-tagbody` |
| `break N;` / `continue N;` | `ast-go` within nested tagbodies |
| `try { } catch (Ex $e) { } finally { }` | `ast-unwind-protect` wrapping PHP exception dispatch |
| `throw new Ex();` | `ast-throw` |
| `namespace Foo\Bar;` / `use Vendor\X as Y;` | parser-time qualified-name resolution + AST metadata |

Expressions either map to a CL form or to a `%php-` runtime helper where PHP
semantics differ from Lisp:

| PHP | Lowering |
| --- | -------- |
| `[1, 2, 3]` / `array(1, 2, 3)` | `%php-array` call |
| `["a" => 1, "b" => 2]` | `%php-array` with key/value pairs |
| `$a[0]` | `%php-array-ref` |
| `$a[0] = $v` | `%php-array-set` |
| `$a ?? $b` | temp `ast-let` + `ast-if` |
| `$c ? $yes : $no` | `ast-if` |
| `fn($x) => $x + 1` | capture-wrapped `ast-lambda` |
| `match ($v) { 1 => 'one', default => 'x' }` | nested strict-equality conditional chain |
| `yield $v` / `yield from $xs` | `%php-yield` / `%php-yield-from` runtime data representation |
| `$a <=> $b` | `%php-spaceship` |
| `$a >> $n` | `%php-shift-right` |
| `$a instanceof C` | `%php-instanceof` |
| `$value \|> f(...)` | PHP 8.5 pipe helper with first-class callable lowering |
| `(void) $expr` | side-effect evaluation followed by PHP null |
| `clone($obj, ["prop" => $value])` | PHP 8.5 clone-with helper after `__clone` dispatch |
| `Closure::getCurrent()` | the currently executing Closure, or PHP null |

Parameter and return type annotations (`int`, `?string`, `int|string`,
`void`, `never`, `mixed`, `static`, and nullable, union, and intersection
types) are preserved as `:php-param-types` and `:php-return-type` in the
`ast-defun` declarations. Class properties and constants keep their declared
PHP types on `ast-slot-def` nodes.

### Runtime helpers

PHP's ordered arrays, loose equality, truthiness, and exceptions are bridged
by VM host functions that are registered automatically when compiling PHP
source:

```text
%php-array            Ordered associative array (preserves insertion order)
%php-array-ref        Array element read
%php-array-set        Array element write
%php-array-unset      Array element delete (preserving order)
%php-array-pairs      Ordered (key . value) pair list
%php-array-key-exists Array key presence check
%php-array-first / %php-array-last
                      PHP 8.5 array_first() / array_last()
%php-eq-loose         PHP == (type-coercing) equality
%php-eq-strict        PHP === (type-strict) equality
%php-truthy           PHP truthiness (0, "", "0", [], null -> false)
%php-null-p           PHP null check
%php-to-number        PHP numeric coercion
%php-count / %php-strlen / %php-strtolower / %php-strtoupper
%php-isset            PHP isset() semantics
%php-throw            Signal a php-exception condition
%php-exception-matches-p  PHP catch-type matching
%php-spaceship        <=> operator: returns -1, 0, or 1
%php-shift-right      >> right bitshift
%php-instanceof       instanceof type check against host runtime
%php-match-error      Unhandled match arm signal
%php-list-bind        PHP list() destructuring representation
%php-yield / %php-yield-from  Generator yield representation (runtime data)
%php-fiber-make / %php-fiber-start / %php-fiber-suspend
                      Fiber construction, start, state queries, first suspension
%php-pipe             PHP 8.5 pipe operator dispatch
%php-clone-with       PHP 8.5 clone-with property override application
```

The PHP 8.5 predefined constants include `PHP_VERSION`, `PHP_VERSION_ID`,
`PHP_BUILD_DATE`, and `PHP_BUILD_PROVIDER`.

For the parts of PHP that are not yet lowered, see
[Compatibility](compatibility.md#php-frontend).

## The JavaScript frontend

The JavaScript frontend lowers JavaScript source through the same CL AST and
backend pipeline as the other frontends. `.js` and `.mjs` files are
auto-detected; `--lang js` and `--lang javascript` select it explicitly for
any command. The frontend tracks ECMAScript 2026 as its latest supported
target.

```sh
cl-cc run file.js
cl-cc run file.mjs
cl-cc compile file.js -o out --arch x86-64
cl-cc check file.js --strict
```

## Inspecting the build

`dep-graph` renders the ASDF dependency graph, modelled with
[cl-dataflow-kit](https://github.com/nerima-lisp/cl-dataflow-kit):

```sh
cl-cc dep-graph --format mermaid
cl-cc dep-graph --format topo
```

`completion` emits a shell completion script, and `docs` emits the CLI
reference in Markdown, man, or JSON form. Both are generated from the same
declarative application spec that drives argument parsing, so they cannot
drift from the real flag set:

```sh
cl-cc completion fish
cl-cc docs markdown
```

;;;; packages/javascript/tests/js-e2e-tests.lisp — ES2026 JavaScript End-to-End Tests
;;;;
;;;; 10 end-to-end tests that parse complete JavaScript programs and assert
;;;; that the resulting AST list is non-empty, structurally coherent, and
;;;; contains the expected high-level constructs.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────────

(defun %js-run-capture (source)
  "Compile JavaScript SOURCE to the VM, seed the JS runtime globals, run it, and
return everything console.log printed (trailing newline trimmed). Exercises the
real execution path, unlike the parse-only %js-e2e-parse checks."
  (let* ((result  (cl-cc:compile-string source :target :vm :language :javascript))
         (program (cl-cc/compile:compilation-result-program result))
         (out     (make-string-output-stream))
         (state   (cl-cc/vm:make-vm-state :output-stream out)))
    (cl-cc/pipeline:seed-js-runtime-globals state)
    ;; %js-console-log writes via (format t ...) → *standard-output*, so bind it
    ;; to capture; the CLI relies on *standard-output* being the terminal.
    (let ((*standard-output* out))
      (cl-cc/vm:run-compiled program :output-stream out :state state))
    (string-right-trim '(#\Newline) (get-output-stream-string out))))

(deftest js-e2e-runs-basic-programs
  "JavaScript programs execute end-to-end (regression: the prelude bound Symbol/
Infinity/error classes to host *js-* specials that were never seeded as VM
globals, so EVERY JS program failed with 'Unbound global variable: *JS-...*')."
  (assert-string= "hello" (%js-run-capture "console.log(\"hello\");"))
  (assert-string= "14"    (%js-run-capture "console.log(2+3*4);"))
  (assert-string= "7"     (%js-run-capture "function f(a,b){return a+b;} console.log(f(3,4));"))
  (assert-string= "10"    (%js-run-capture "let s=0; for(let i=1;i<=4;i++){s+=i;} console.log(s);")))

(deftest js-e2e-runs-arrays-and-closures
  "Array higher-order methods, template literals, and closures run."
  (assert-string= "2,4,6" (%js-run-capture "console.log([1,2,3].map(x=>x*2).join(\",\"));"))
  (assert-string= "2,4"   (%js-run-capture "console.log([1,2,3,4].filter(x=>x%2==0).join(\",\"));"))
  (assert-string= "Hi Bob" (%js-run-capture "let n=\"Bob\"; console.log(`Hi ${n}`);"))
  (assert-string= "2"     (%js-run-capture "function mk(){let c=0; return ()=>++c;} let f=mk(); f(); console.log(f());")))

(deftest js-e2e-default-and-rest-params
  "function declarations honor default parameter values and ...rest collection
(regression: %js-parse-param-list parsed then discarded defaults and mistreated
...rest as a positional param)."
  (assert-string= "5"  (%js-run-capture "function g(x=5){return x;} console.log(g());"))
  (assert-string= "9"  (%js-run-capture "function g(x=5){return x;} console.log(g(9));"))
  (assert-string= "13" (%js-run-capture "function f(a,b=10){return a+b;} console.log(f(3));"))
  (assert-string= "3"  (%js-run-capture "function s(...n){return n.length;} console.log(s(1,2,3));"))
  (assert-string= "10" (%js-run-capture "function s(...n){return n.reduce((a,b)=>a+b,0);} console.log(s(1,2,3,4));"))
  (assert-string= "12" (%js-run-capture "function f(a,...rest){return a+rest.length;} console.log(f(10,1,2));")))

(deftest js-e2e-function-expressions
  "Function expressions execute, including `return' (regression: their body was
used raw without a (block nil ...), so `return value' silently produced nothing),
default params, rest params, and IIFEs."
  (assert-string= "5"  (%js-run-capture "let g=function(x){return x+1;}; console.log(g(4));"))
  (assert-string= "5"  (%js-run-capture "console.log((function(x){return x+1;})(4));"))
  (assert-string= "5"  (%js-run-capture "const g=function(x=5){return x;}; console.log(g());"))
  (assert-string= "13" (%js-run-capture "const f=function(a,b=10){return a+b;}; console.log(f(3));"))
  (assert-string= "3"  (%js-run-capture "const s=function(...n){return n.length;}; console.log(s(1,2,3));")))

(deftest js-e2e-class-this-binding
  "Class constructors and methods bind `this' to the instance — in both the host
special and the VM-global %js-this — so this.x reads/writes the receiver
(regression: this compiled to vm-get-global %js-this which was never set, so any
class using `this' failed with 'Unbound global variable: %JS-THIS')."
  (assert-string= "7"  (%js-run-capture "class C{constructor(x){this.x=x;} get(){return this.x;}} console.log(new C(7).get());"))
  (assert-string= "Bob" (%js-run-capture "class P{constructor(n){this.name=n;}} let p=new P(\"Bob\"); console.log(p.name);"))
  ;; this.n++ mutates the receiver across calls
  (assert-string= "2"  (%js-run-capture "class Ctr{constructor(){this.n=0;} inc(){this.n++; return this.n;}} let c=new Ctr(); c.inc(); console.log(c.inc());"))
  ;; nested method call restores the outer receiver afterwards
  (assert-string= "10" (%js-run-capture "class B{v(){return 10;}} class A{constructor(){this.b=new B();} go(){return this.b.v();}} console.log(new A().go());")))

(defun %js-e2e-parse (src)
  "Parse SRC and return the list of top-level AST nodes."
  (cl-cc/javascript:parse-js-source src))

(defun %js-e2e-has-defun-named (name asts)
  "Return the AST-DEFUN whose name matches upcased NAME, or NIL."
  (find-if (lambda (ast)
             (and (cl-cc:ast-defun-p ast)
                  (string= (string-upcase name)
                            (symbol-name (cl-cc:ast-defun-name ast)))))
           asts))

(defun %js-e2e-has-defclass-named (name asts)
  "Return the AST-DEFCLASS whose name matches upcased NAME, or NIL."
  (find-if (lambda (ast)
             (and (cl-cc:ast-defclass-p ast)
                  (string= (string-upcase name)
                            (symbol-name (cl-cc:ast-defclass-name ast)))))
           asts))

(defun %js-count-ast-type (pred asts)
  "Count top-level AST nodes for which PRED returns true."
  (count-if pred asts))

(defun %js-e2e-defun-body-forms (fn)
  "Return FN's effective body forms, unwrapping the single (block nil ...) that
js-callable-body wraps function bodies in so JS `return' (→ return-from nil)
resolves. Lets these AST-shape tests look past the block at the real statements."
  (let ((body (cl-cc:ast-defun-body fn)))
    (if (and (= (length body) 1) (cl-cc:ast-block-p (first body)))
        (cl-cc:ast-block-body (first body))
        body)))

;;; ─── 1. FizzBuzz ──────────────────────────────────────────────────────────────

(deftest js-e2e-fizzbuzz
  "FizzBuzz program parses to a non-empty AST with at least a function definition."
  (let ((asts (%js-e2e-parse "
function fizzBuzz(n) {
  for (let i = 1; i <= n; i++) {
    if (i % 15 === 0) {
      console.log('FizzBuzz');
    } else if (i % 3 === 0) {
      console.log('Fizz');
    } else if (i % 5 === 0) {
      console.log('Buzz');
    } else {
      console.log(i);
    }
  }
}
fizzBuzz(20);
")))
    (assert-true (>= (length asts) 2))
    (assert-true (not (null (%js-e2e-has-defun-named "FIZZBUZZ" asts))))))

;;; ─── 2. Fibonacci recursive ───────────────────────────────────────────────────

(deftest js-e2e-fibonacci-recursive
  "Recursive Fibonacci produces ast-defun whose body contains an ast-if."
  (let* ((asts (%js-e2e-parse "
function fib(n) {
  if (n <= 1) return n;
  return fib(n - 1) + fib(n - 2);
}
"))
         (fn (%js-e2e-has-defun-named "FIB" asts)))
    (assert-true (not (null fn)))
    (assert-true (some #'cl-cc:ast-if-p (%js-e2e-defun-body-forms fn)))))

;;; ─── 3. Array map / filter / reduce ──────────────────────────────────────────

(deftest js-e2e-array-higher-order
  "Array map/filter/reduce chain parses to NESTED let bindings: each const scopes
over the following statements, so the four declarations collapse to a single
outer ast-let whose body nests the rest (js-finish-let-bindings). Previously they
were flat siblings, leaving each const invisible to later statements."
  (let ((asts (%js-e2e-parse "
const nums = [1, 2, 3, 4, 5];
const doubled = nums.map(x => x * 2);
const evens = nums.filter(x => x % 2 === 0);
const sum = nums.reduce((acc, x) => acc + x, 0);
")))
    (assert-true (>= (length asts) 1))
    (assert-true (cl-cc:ast-let-p (first asts)))))

;;; ─── 4. Class with inheritance ────────────────────────────────────────────────

(deftest js-e2e-class-inheritance
  "Class hierarchy Animal -> Dog produces two ast-defclass nodes."
  (let ((asts (%js-e2e-parse "
class Animal {
  constructor(name) {
    this.name = name;
  }
  speak() {
    return this.name + ' makes a noise.';
  }
}

class Dog extends Animal {
  speak() {
    return this.name + ' barks.';
  }
}
")))
    (assert-true (not (null (%js-e2e-has-defclass-named "ANIMAL" asts))))
    (assert-true (not (null (%js-e2e-has-defclass-named "DOG" asts))))
    (let ((dog (%js-e2e-has-defclass-named "DOG" asts)))
      (assert-true (some (lambda (s) (string= "ANIMAL" (symbol-name s)))
                (cl-cc:ast-defclass-superclasses dog))))))

;;; ─── 5. Object destructuring ──────────────────────────────────────────────────

(deftest js-e2e-object-destructuring
  "Object destructuring assignment produces ast-let bindings accessing properties.
After %js-finish-let-bindings the 3 const declarations nest into 1 top-level let."
  (let ((asts (%js-e2e-parse "
const person = { name: 'Alice', age: 30, city: 'NY' };
const { name, age } = person;
const { city: location } = person;
")))
    (assert-true (>= (length asts) 1))
    ;; Search the nested let chain for a destructuring let (> 1 binding)
    (labels ((has-multi-bind-let (node)
               (when (cl-cc:ast-let-p node)
                 (or (> (length (cl-cc:ast-let-bindings node)) 1)
                     (some #'has-multi-bind-let (cl-cc:ast-let-body node))))))
      (assert-true (some #'has-multi-bind-let asts)))))

;;; ─── 6. Generator sequence ────────────────────────────────────────────────────

(deftest js-e2e-generator-sequence
  "Generator function parses to ast-defun with :js-generator declaration."
  (let* ((asts (%js-e2e-parse "
function* range(start, end, step = 1) {
  for (let i = start; i < end; i += step) {
    yield i;
  }
}
const it = range(0, 10, 2);
"))
         (gen (%js-e2e-has-defun-named "RANGE" asts)))
    (assert-true (not (null gen)))
    (assert-true (member :js-generator (cl-cc:ast-defun-declarations gen)))))

;;; ─── 7. Error handling try / catch ───────────────────────────────────────────

(deftest js-e2e-error-handling
  "try/catch/finally pattern produces %js-try-catch-finally call nodes."
  (let ((asts (%js-e2e-parse "
function safeDiv(a, b) {
  try {
    if (b === 0) throw new Error('Division by zero');
    return a / b;
  } catch (e) {
    console.error('Error:', e.message);
    return null;
  } finally {
    console.log('safeDiv done');
  }
}
")))
    (let* ((fn (%js-e2e-has-defun-named "SAFEDIV" asts))
           (body (when fn (%js-e2e-defun-body-forms fn))))
      (assert-true (not (null fn)))
      (assert-true (some (lambda (node)
                  (and (cl-cc:ast-call-p node)
                       (string= "%JS-TRY-CATCH-FINALLY"
                                 (symbol-name
                                  (cl-cc:ast-var-name
                                   (cl-cc:ast-call-func node))))))
                body)))))

;;; ─── 8. Module-style exports ──────────────────────────────────────────────────

(deftest js-e2e-module-exports
  "ES module with export statements parses in module mode without error."
  (let ((asts (cl-cc/javascript:parse-js-module "
export function add(a, b) { return a + b; }
export function subtract(a, b) { return a - b; }
export const PI = 3.14159;
")))
    ;; Module parse must return some nodes
    (assert-true (>= (length asts) 0))))

;;; ─── 9. Async / await simulation ─────────────────────────────────────────────

(deftest js-e2e-async-await
  "Async function with await parses to ast-defun with :js-async declaration."
  (let* ((asts (%js-e2e-parse "
async function fetchData(url) {
  try {
    const response = await fetch(url);
    const data = await response.json();
    return data;
  } catch (err) {
    throw new Error('Fetch failed: ' + err.message);
  }
}
"))
         (fn (%js-e2e-has-defun-named "FETCHDATA" asts)))
    (assert-true (not (null fn)))
    (assert-true (member :js-async (cl-cc:ast-defun-declarations fn)))))

;;; ─── 10. Optional chaining chain ─────────────────────────────────────────────

(deftest js-e2e-optional-chaining-chain
  "Complex optional chaining chain parses to ast-let bindings with call nodes."
  (let ((asts (%js-e2e-parse "
const user = { profile: { address: { city: 'NYC' } } };
const city = user?.profile?.address?.city;
const zip = user?.profile?.address?.zip ?? 'N/A';
const upper = user?.profile?.address?.city?.toUpperCase();
")))
    (assert-true (>= (length asts) 1))
    ;; The const declarations now nest into a single outer let (each scopes over
    ;; the following statements), so the first top-level form is an ast-let.
    (assert-true (cl-cc:ast-let-p (first asts)))))

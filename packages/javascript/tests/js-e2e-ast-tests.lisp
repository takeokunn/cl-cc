;;;; packages/javascript/tests/js-e2e-ast-tests.lisp — JS AST structural tests
;;;;
;;;; Parse-only tests that check the AST shape produced by the JS frontend
;;;; without running the program through the VM.
;;;;
;;;; Depends on: js-e2e-core-tests.lisp (loaded before this in serial ASDF module).

(in-package :cl-cc/test)
(in-suite cl-cc-javascript-e2e-serial-suite)

;;; ─── Parse helpers ───────────────────────────────────────────────────────────

(defun %js-e2e-parse (src)
  "Parse SRC and return the list of top-level AST nodes."
  (cl-cc/javascript:parse-js-source src))

(defun %js-e2e-has-defun-named (name asts)
  "Return the AST-DEFUN whose name matches NAME case-insensitively, or NIL.
JS identifiers are now interned case-preserving, so a defun named `fib' is the
symbol |fib|; compare with string-equal so these structural tests stay name-case
agnostic."
  (find-if (lambda (ast)
             (and (cl-cc:ast-defun-p ast)
                  (string-equal name
                                (symbol-name (cl-cc:ast-defun-name ast)))))
           asts))

(defun %js-e2e-has-defclass-named (name asts)
  "Return the AST-DEFCLASS whose name matches NAME case-insensitively, or NIL.
JS identifiers are interned case-preserving now, so compare with string-equal."
  (find-if (lambda (ast)
             (and (cl-cc:ast-defclass-p ast)
                  (string-equal name
                                (symbol-name (cl-cc:ast-defclass-name ast)))))
           asts))

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
      (assert-true (some (lambda (s) (string-equal "Animal" (symbol-name s)))
                (cl-cc:ast-defclass-superclasses dog))))))

;;; ─── 5. Object destructuring ──────────────────────────────────────────────────

(deftest js-e2e-object-destructuring
  "Object destructuring lowers to a nested let chain whose bindings access
properties via %js-get-prop. After %js-finish-let-bindings the const declarations
nest into single-binding lets for sequential scoping."
  (let ((asts (%js-e2e-parse "
const person = { name: 'Alice', age: 30, city: 'NY' };
const { name, age } = person;
const { city: location } = person;
")))
    (assert-true (>= (length asts) 1))
    (labels ((getprop-binding-p (b)
               (let ((v (cdr b)))
                 (and (cl-cc:ast-call-p v)
                      (cl-cc:ast-var-p (cl-cc:ast-call-func v))
                      (search "GET-PROP"
                              (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func v)))))))
             (chain-has-getprop (node)
               (when (cl-cc:ast-let-p node)
                 (or (some #'getprop-binding-p (cl-cc:ast-let-bindings node))
                     (some #'chain-has-getprop (cl-cc:ast-let-body node))))))
      (assert-true (some #'chain-has-getprop asts)))))

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

;;; ─── 9b. Async / await execution ────────────────────────────────────────────

(deftest js-e2e-async-await-execution
  "Async function declarations return a Promise; await unwraps it synchronously
in the simplified async model.  Regression: only async function EXPRESSIONS were
wrapped in %js-make-async; declarations only stored :js-async as metadata so
calling async functions ran the body synchronously and returned the raw value
instead of a Promise.  The fix wraps the body in %js-async (like generators use
%js-make-generator) so params are captured by closure."
  (assert-string= "10"
    (%js-run-capture
     "async function double(x){return x*2;}
async function main(){const r=await double(5); console.log(r);}
main();"))
  (assert-string= "6"
    (%js-run-capture
     "async function add(a,b){return a+b;}
async function run(){const s=await add(2,4); console.log(s);}
run();"))
  (assert-string= "7"
    (%js-run-capture
     "async function id(x){return await x;}
async function test(){console.log(await id(7));}
test();"))
  (assert-string= "caught"
    (%js-run-capture
     "async function fail(){throw new Error('oops');}
async function main(){
  try{await fail();}catch(e){console.log('caught');}
}
main();")))

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
    (assert-true (cl-cc:ast-let-p (first asts)))))

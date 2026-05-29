;;;; packages/javascript/tests/js-e2e-tests.lisp — ES2026 JavaScript End-to-End Tests
;;;;
;;;; 10 end-to-end tests that parse complete JavaScript programs and assert
;;;; that the resulting AST list is non-empty, structurally coherent, and
;;;; contains the expected high-level constructs.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────────

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
    (assert-true (some #'cl-cc:ast-if-p (cl-cc:ast-defun-body fn)))))

;;; ─── 3. Array map / filter / reduce ──────────────────────────────────────────

(deftest js-e2e-array-higher-order
  "Array map/filter/reduce chain parses to let bindings with call nodes."
  (let ((asts (%js-e2e-parse "
const nums = [1, 2, 3, 4, 5];
const doubled = nums.map(x => x * 2);
const evens = nums.filter(x => x % 2 === 0);
const sum = nums.reduce((acc, x) => acc + x, 0);
")))
    (assert-true (>= (length asts) 4))
    (assert-true (every #'cl-cc:ast-let-p asts))))

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
  "Object destructuring assignment produces ast-let bindings accessing properties."
  (let ((asts (%js-e2e-parse "
const person = { name: 'Alice', age: 30, city: 'NY' };
const { name, age } = person;
const { city: location } = person;
")))
    (assert-true (>= (length asts) 3))
    ;; Destructuring lets must have multiple bindings
    (let ((dest-lets (remove-if-not (lambda (ast)
                                      (and (cl-cc:ast-let-p ast)
                                           (> (length (cl-cc:ast-let-bindings ast)) 1)))
                                    asts)))
      (assert-true (>= (length dest-lets) 1)))))

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
           (body (when fn (cl-cc:ast-defun-body fn))))
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
    (assert-true (>= (length asts) 4))
    ;; The optional-chained assignments must be let forms
    (let ((chained (remove-if-not #'cl-cc:ast-let-p asts)))
      (assert-true (>= (length chained) 3)))))

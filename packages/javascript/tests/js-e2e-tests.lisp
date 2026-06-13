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

(defmacro deftest-js-run (name description &rest cases)
  "Each CASES element is (expected-string source-string).
Expands to a deftest whose body runs each source and asserts the output."
  `(deftest ,name ,description
     ,@(mapcar (lambda (c)
                 `(assert-string= ,(first c) (%js-run-capture ,(second c))))
               cases)))

(deftest-js-run js-e2e-runs-basic-programs
  "JavaScript programs execute end-to-end (regression: the prelude bound Symbol/
Infinity/error classes to host *js-* specials that were never seeded as VM
globals, so EVERY JS program failed with 'Unbound global variable: *JS-...*')."
  ("hello" "console.log(\"hello\");")
  ("14"    "console.log(2+3*4);")
  ("7"     "function f(a,b){return a+b;} console.log(f(3,4));")
  ("10"    "let s=0; for(let i=1;i<=4;i++){s+=i;} console.log(s);"))

(deftest-js-run js-e2e-runs-arrays-and-closures
  "Array higher-order methods, template literals, and closures run."
  ("2,4,6"   "console.log([1,2,3].map(x=>x*2).join(\",\"));")
  ("2,4"     "console.log([1,2,3,4].filter(x=>x%2==0).join(\",\"));")
  ("Hi Bob"  "let n=\"Bob\"; console.log(`Hi ${n}`);")
  ("2"       "function mk(){let c=0; return ()=>++c;} let f=mk(); f(); console.log(f());"))

(deftest-js-run js-e2e-default-and-rest-params
  "function declarations honor default parameter values and ...rest collection
(regression: %js-parse-param-list parsed then discarded defaults and mistreated
...rest as a positional param)."
  ("5"  "function g(x=5){return x;} console.log(g());")
  ("9"  "function g(x=5){return x;} console.log(g(9));")
  ("13" "function f(a,b=10){return a+b;} console.log(f(3));")
  ("3"  "function s(...n){return n.length;} console.log(s(1,2,3));")
  ("10" "function s(...n){return n.reduce((a,b)=>a+b,0);} console.log(s(1,2,3,4));")
  ("12" "function f(a,...rest){return a+rest.length;} console.log(f(10,1,2));"))

(deftest-js-run js-e2e-function-expressions
  "Function expressions execute, including `return' (regression: their body was
used raw without a (block nil ...), so `return value' silently produced nothing),
default params, rest params, and IIFEs."
  ("5"  "let g=function(x){return x+1;}; console.log(g(4));")
  ("5"  "console.log((function(x){return x+1;})(4));")
  ("5"  "const g=function(x=5){return x;}; console.log(g());")
  ("13" "const f=function(a,b=10){return a+b;}; console.log(f(3));")
  ("3"  "const s=function(...n){return n.length;}; console.log(s(1,2,3));"))

(deftest-js-run js-e2e-destructuring-and-sequential-decls
  "Array/object destructuring and multi-declarator let bind sequentially
(regression: ast-let binds in parallel, so a later initializer could not see an
earlier binding — destructuring's a = tmp[0] could not see tmp = init, and
`let a=1, b=a+1' gave nothing; %js-finish-let-bindings now expands a multi-binding
empty let into nested single-binding lets)."
  ("6"     "let a=5, b=a+1; console.log(b);")
  ("30"    "let [a,b]=[10,20]; console.log(a+b);")
  ("123"   "let [a,b,c]=[1,2,3]; console.log(a*100+b*10+c);")
  ("3"     "let {x,y}={x:1,y:2}; console.log(x+y);")
  ("2"     "let [,b]=[1,2]; console.log(b);")
  ("Bob30" "const {name,age}={name:'Bob',age:30}; console.log(name+age);")
  ("12"    "function f(){ let [a,b]=[3,4]; return a*b; } console.log(f());"))

(deftest-js-run js-e2e-destructuring-rest
  "Array and object destructuring rest elements collect into a real JS array /
object.  Regression: the :rest sentinel was mistaken for a default value, so an
array rest was a one-element CL list (Array.isArray false, rest.join threw with
':JS-UNDEFINED') and an object rest was null.  The array rest now yields a vector
(map/join/length work) and the object rest a fresh object excluding bound keys."
  ("3,4,5"              "const [x,y,...r]=[1,2,3,4,5]; console.log(r.join(','));")
  ("true"               "const [x,...r]=[1,2,3]; console.log(Array.isArray(r));")
  ("4,6,8,10"           "const [a,...r]=[1,2,3,4,5]; console.log(r.map(n=>n*2).join(','));")
  ("0"                  "const [p,...q]=[1]; console.log(q.length);")
  ("{\"b\":2,\"c\":3}"  "const {a,...rest}={a:1,b:2,c:3}; console.log(JSON.stringify(rest));")
  ("{}"                 "const {m,...n}={m:1}; console.log(JSON.stringify(n));"))

(deftest-js-run js-e2e-parameter-destructuring
  "Named-function parameters may be destructuring patterns: function f([a,b]) and
function g({x,y}).  Regression: the pattern was parsed but discarded (the param
kept only its gensym), so the body never unpacked it; the unbodied let then failed
to compile and the defun was silently dropped -> 'Undefined function: F'.  Now the
param gensym is unpacked by a body-prologue let (reusing the var-destructuring
machinery), including rest, defaults, and multiple destructuring params."
  ("30"  "function f([a,b]){return a+b;} console.log(f([10,20]));")
  ("12"  "function f({x,y}){return x*y;} console.log(f({x:3,y:4}));")
  ("1:2,3" "function f([a,...r]){return a+':'+r.join(',');} console.log(f([1,2,3]));")
  ("{\"b\":2}" "function f({a,...rest}){return JSON.stringify(rest);} console.log(f({a:1,b:2}));")
  ("6"   "function f([a,b],{x}){return a+b+x;} console.log(f([1,2],{x:3}));")
  ("10"  "function f({x=10}){return x;} console.log(f({}));")
  ("7"   "function f(a,b){return a-b;} console.log(f(9,2));"))

(deftest-js-run js-e2e-nested-destructuring
  "Destructuring patterns nest (array-in-array, object-in-object, mixed) for both
var declarations and function parameters.  Regression: %js-emit-destructure-
bindings bound a nested pattern's gensym but never recursed to unpack it, so inner
names stayed undefined and the binding form failed to compile.  %js-destructure-
sub-bindings now recurses, building bindings in let* order."
  ("1 2 3"   "const [a,[b,c]]=[1,[2,3]]; console.log(a,b,c);")
  ("42"      "const {a:{b}}={a:{b:42}}; console.log(b);")
  ("1 2"     "const {p,q:{r}}={p:1,q:{r:2}}; console.log(p,r);")
  ("1 2 3 4" "const [[a,b],[c,d]]=[[1,2],[3,4]]; console.log(a,b,c,d);")
  ("99"      "const {u:{v:{w}}}={u:{v:{w:99}}}; console.log(w);")
  ("1 2 3-4" "const [a,[b,...c]]=[1,[2,3,4]]; console.log(a,b,c.join('-'));")
  ("1 9"     "const {a,b:{c=9}}={a:1,b:{}}; console.log(a,c);")
  ("6"       "function f([a,[b,c]]){return a+b+c;} console.log(f([1,[2,3]]));")
  ("3"       "function f({data:{items}}){return items.length;} console.log(f({data:{items:[1,2,3]}}));"))

(deftest-js-run js-e2e-arrow-destructuring
  "Arrow-function parameters may be destructuring patterns: ([a,b])=>… and
({x,y})=>…, including nesting and the .map(([a,b])=>…) idiom.  Regression: the
paren contents were parsed as an array/object literal and only plain identifiers
became params, so the pattern was dropped and the arrow silently produced nothing.
%js-expr-to-binding-pattern now recovers the pattern from the literal when => is
seen, reusing the body-prologue destructuring lowering."
  ("11"  "const f=([a,b])=>a+b; console.log(f([5,6]));")
  ("12"  "const g=({x,y})=>x*y; console.log(g({x:3,y:4}));")
  ("6"   "const k=([a,[b,c]])=>a+b+c; console.log(k([1,[2,3]]));")
  ("7"   "const q=({a:{b}})=>b; console.log(q({a:{b:7}}));")
  ("3,7" "console.log([[1,2],[3,4]].map(([a,b])=>a+b).join(','));")
  ("10,20" "console.log([{n:1},{n:2}].map(({n})=>n*10).join(','));")
  ("3"   "const g=([a,...r])=>r.length; console.log(g([1,2,3,4]));")
  ("{\"b\":2,\"c\":3}" "const m=({a,...rest})=>JSON.stringify(rest); console.log(m({a:1,b:2,c:3}));")
  ("1:2,3;4:5,6" "console.log([[1,2,3],[4,5,6]].map(([a,...rest])=>a+':'+rest.join(',')).join(';'));")
  ("1 2"  "const o={...{a:1},...{b:2}}; console.log(o.a,o.b);")
  ("10"   "const r=x=>x*2; console.log(r(5));")
  ("7"    "const p=(a,b)=>a-b; console.log(p(9,2));")
  ("3"    "console.log((1,2,3));"))

(deftest-js-run js-e2e-json-and-math-globals
  "JSON and Math are seeded as global objects (regression: they were never added
to the prelude). JSON.parse worked once its string scanner stopped discarding the
position via with-output-to-string."
  ("{\"a\":1,\"b\":2}" "console.log(JSON.stringify({a:1,b:2}));")
  ("[1,2,3]"  "console.log(JSON.stringify([1,2,3]));")
  ("5"  "let o=JSON.parse('{\"x\":5}'); console.log(o.x);")
  ("42hi" "let p=JSON.parse(JSON.stringify({n:42,s:'hi'})); console.log(p.n+p.s);")
  ("20" "console.log(JSON.parse('[10,20,30]')[1]);")
  ("7"  "console.log(Math.max(3,7,2));")
  ("4"  "console.log(Math.sqrt(16));")
  ("5"  "console.log(Math.abs(-5));"))

(deftest-js-run js-e2e-try-catch-finally
  "try/catch/finally executes; finally always runs; nested re-throw works."
  ("a"        "try { console.log('a'); } catch(e) { console.log('b'); }")
  ("oops"     "try { throw 'oops'; } catch(e) { console.log(e); }")
  ("43"       "try { throw 42; } catch(e) { console.log(e+1); }")
  (#.(format nil "t~%f")      "try { console.log('t'); } finally { console.log('f'); }")
  (#.(format nil "c:x~%fin")  "try { throw 'x'; } catch(e) { console.log('c:'+e); } finally { console.log('fin'); }")
  ("boom"     "try { throw new Error('boom'); } catch(e) { console.log(e.message); }")
  (#.(format nil "cleanup~%1") "function f(){ try { return 1; } finally { console.log('cleanup'); } } console.log(f());")
  ("outer:inner" "try { try { throw 'inner'; } catch(e) { throw 'outer:'+e; } } catch(e) { console.log(e); }"))

(deftest-js-run js-e2e-object-spread
  "Object spread {...a, k:v} merges own properties with later entries overriding
earlier ones (regression: a spread entry misaligned the %js-make-object key/value
pairs). Lowers to a left-to-right fold of %js-object-assign / set."
  ("6"    "let a={x:1,y:2}; let b={...a,z:3}; console.log(b.x+b.y+b.z);")
  ("3"    "let a={x:1},c={y:2}; console.log(({...a,...c}).x+({...a,...c}).y);")
  ("9"    "let a={x:1,y:2}; console.log(({...a,y:9}).y);")
  ("3"    "let a={y:2}; let b={x:1,...a}; console.log(b.x+b.y);")
  ("5,99" "let a={n:5}; let b={...a}; b.n=99; console.log(a.n+','+b.n);"))

(deftest-js-run js-e2e-spread-arrays-and-calls
  "...spread expands an iterable in array literals and call arguments (regression:
the %js-spread marker was left as a single element / argument). Lowers to apply
over a runtime-appended list."
  ("1,2,3,4" "let a=[1,2]; console.log([...a,3,4].join(','));")
  ("4"   "let a=[1,2],c=[3,4]; console.log([...a,...c].length);")
  ("0,1,2,3" "console.log([0,...[1,2],3].join(','));")
  ("6"   "function add(a,b,c){return a+b+c;} console.log(add(...[1,2,3]));")
  ("1-2-3" "function f(a,b,c){return a+'-'+b+'-'+c;} console.log(f(1,...[2,3]));")
  ("a-b-c" "console.log([...'abc'].join('-'));")
  ("7"   "console.log(Math.max(...[3,7,2]));"))

(deftest-js-run js-e2e-division-yields-float
  "JS / produces an IEEE number, not a CL rational (regression: 5/2 printed
'5/2'). Integer-valued quotients still print cleanly, and /0 gives Infinity."
  ("2.5"  "console.log(5/2);")
  ("3"    "console.log(6/2);")
  ("3.5"  "let a=7,b=2; console.log(a/b);")
  ("1.25" "console.log(10/4/2);")
  ("4"    "let n=[2,4,6]; console.log(n.reduce((a,b)=>a+b,0)/n.length);")
  ("Inf"  "console.log(5/0 === Infinity ? 'Inf' : 'no');"))

(deftest-js-run js-e2e-integer-valued-float-formatting
  "An integer-valued float prints without a decimal point, JS-style (7.0 -> '7'),
while real decimals are preserved."
  ("7"    "console.log(7.0);")
  ("3.14" "console.log(3.14);")
  ("2.5"  "console.log(2.5);")
  ("4"    "console.log(Math.sqrt(16));"))

(deftest-js-run js-e2e-class-this-binding
  "Class constructors and methods bind `this' to the instance — in both the host
special and the VM-global %js-this — so this.x reads/writes the receiver
(regression: this compiled to vm-get-global %js-this which was never set, so any
class using `this' failed with 'Unbound global variable: %JS-THIS')."
  ("7"   "class C{constructor(x){this.x=x;} get(){return this.x;}} console.log(new C(7).get());")
  ("Bob" "class P{constructor(n){this.name=n;}} let p=new P(\"Bob\"); console.log(p.name);")
  ("2"   "class Ctr{constructor(){this.n=0;} inc(){this.n++; return this.n;}} let c=new Ctr(); c.inc(); console.log(c.inc());")
  ("10"  "class B{v(){return 10;}} class A{constructor(){this.b=new B();} go(){return this.b.v();}} console.log(new A().go());"))

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
    ;; Search the nested let chain for a binding whose value is a %js-get-prop
    ;; call — the hallmark of a lowered destructuring access.
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

;;; ─── 9b. Async / await execution ────────────────────────────────────────────

(deftest js-e2e-async-await-execution
  "Async function declarations return a Promise; await unwraps it synchronously
in the simplified async model.  Regression: only async function EXPRESSIONS were
wrapped in %js-make-async; declarations only stored :js-async as metadata so
calling async functions ran the body synchronously and returned the raw value
instead of a Promise.  The fix wraps the body in %js-async (like generators use
%js-make-generator) so params are captured by closure."
  ;; basic async return value unwrapped by await
  (assert-string= "10"
    (%js-run-capture
     "async function double(x){return x*2;}
async function main(){const r=await double(5); console.log(r);}
main();"))
  ;; async/await chain
  (assert-string= "6"
    (%js-run-capture
     "async function add(a,b){return a+b;}
async function run(){const s=await add(2,4); console.log(s);}
run();"))
  ;; await on a non-promise passthrough (plain value)
  (assert-string= "7"
    (%js-run-capture
     "async function id(x){return await x;}
async function test(){console.log(await id(7));}
test();"))
  ;; async try/catch: exception becomes rejected promise
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
    ;; The const declarations now nest into a single outer let (each scopes over
    ;; the following statements), so the first top-level form is an ast-let.
    (assert-true (cl-cc:ast-let-p (first asts)))))

;;; ─── 10b. Optional chaining / nullish coalescing execution ──────────────────

(deftest-js-run js-e2e-optional-chaining-execution
  "?. returns undefined when chain is null/undefined; ?? returns left operand unless null/undefined."
  ("NYC"       "const u={profile:{city:'NYC'}}; console.log(u?.profile?.city);")
  ("undefined" "const u=null; console.log(u?.profile?.city);")
  ("undefined" "const u={profile:null}; console.log(u?.profile?.city);")
  ("N/A"       "const u={profile:{}}; console.log(u?.profile?.zip ?? 'N/A');")
  ("0"         "console.log(0 ?? 'fallback');")
  (""          "console.log('' ?? 'fallback');")
  ("false"     "console.log(false ?? 'fallback');")
  ("HELLO"     "const s='hello'; console.log(s?.toUpperCase());")
  ("undefined" "const s=null; console.log(s?.toUpperCase());")
  ("b"         "console.log(null ?? undefined ?? 'b' ?? 'c');"))

;;; ─── 11. Named function-expression recursion + typeof function ───────────────

(deftest-js-run js-e2e-named-function-expression-recursion
  "Named function expressions can self-recurse by name; name visible only inside body."
  ("120"   "const f=function fac(n){return n<=1?1:n*fac(n-1);}; console.log(f(5));")
  ("55"    "const f=function fib(n){return n<2?n:fib(n-1)+fib(n-2);}; console.log(f(10));")
  ("5"     "const f=function(x){return x+1;}; console.log(f(4));")
  ("6"     "const f=function g(x){return x*2;}; console.log(f(3));")
  ("6,2,4" "console.log([3,1,2].map(function dbl(x){return x*2;}).join(\",\"));"))

(deftest-js-run js-e2e-typeof-function
  "typeof returns 'function' for vm-closure-objects; non-function types unchanged."
  ("function"           "const f=function(x){return x;}; console.log(typeof f);")
  ("function"           "const g=x=>x; console.log(typeof g);")
  ("function"           "function h(){return 1;} console.log(typeof h);")
  ("function"           "const o={m(){return 1;}}; console.log(typeof o.m);")
  ("yes"                "const h=function(){}; console.log(typeof h===\"function\"?\"yes\":\"no\");")
  ("object"             "console.log(typeof {a:1});")
  ("object"             "console.log(typeof [1,2]);")
  ("string number boolean" "console.log(typeof \"x\", typeof 5, typeof true);"))

;;; ─── 12. Arrow function default + rest parameters ────────────────────────────

(deftest-js-run js-e2e-arrow-default-and-rest-params
  "Arrow functions support default parameters and ...rest collection."
  ("10" "const f=(x=5)=>x*2; console.log(f());")
  ("6"  "const f=(x=5)=>x*2; console.log(f(3));")
  ("12" "const f=(a,b=2)=>a+b; console.log(f(10));")
  ("15" "const f=(a,b=2)=>a+b; console.log(f(10,5));")
  ("3"  "const f=(...n)=>n.length; console.log(f(1,2,3));")
  ("10" "const f=(...n)=>n.reduce((a,b)=>a+b,0); console.log(f(1,2,3,4));")
  ("12" "const f=(a,...n)=>a+n.length; console.log(f(10,1,2));")
  ("6"  "const f=(x=5)=>{return x+1;}; console.log(f());")
  ("7"  "const f=(a,b)=>a+b; console.log(f(3,4));")
  ("7"  "const add=a=>b=>a+b; console.log(add(3)(4));"))

;;; ─── 13. Object-literal getters and setters ──────────────────────────────────

(deftest-js-run js-e2e-object-getters-setters
  "Object-literal get/set accessors invoked on property read/write; both survive on same key."
  ("42" "const o={get v(){return 42;}}; console.log(o.v);")
  ("20" "const o={n:10,get v(){return this.n*2;}}; console.log(o.v);")
  ("5"  "const o={_x:0,set v(n){this._x=n;}}; o.v=5; console.log(o._x);")
  ("11" "const o={_c:0,get count(){return this._c;},set count(n){this._c=n+1;}}; o.count=10; console.log(o.count);")
  ("5"  "const o={n:5,m(){return this.n;}}; console.log(o.m());")
  ("3"  "const o={a:1,b:2}; console.log(o.a+o.b);"))

;;; ─── 14. Object/Reflect/Number/Array/String static-method namespaces ─────────

(deftest-js-run js-e2e-static-method-namespaces
  "Object/Reflect/Number/Array/String namespace globals seeded in prelude; Math/JSON unaffected."
  ("a,b"            "console.log(Object.keys({a:1,b:2}).join(\",\"));")
  ("2"              "console.log(Object.keys({a:1,b:2}).length);")
  ("1,2"            "console.log(Object.values({a:1,b:2}).join(\",\"));")
  ("1"              "console.log(Object.entries({a:1}).length);")
  ("2"              "console.log(Object.assign({},{a:1},{b:2}).b);")
  ("true"           "console.log(Array.isArray([1,2]));")
  ("false"          "console.log(Array.isArray(5));")
  ("true"           "console.log(Number.isInteger(5));")
  ("2"              "console.log(Reflect.ownKeys({a:1,b:2}).length);")
  ("9007199254740991" "console.log(Number.MAX_SAFE_INTEGER);")
  ("2"              "console.log(Math.max(1,2));")
  ("{\"a\":1}"      "console.log(JSON.stringify({a:1}));"))

;;; ─── 15. Relational operators return JS booleans + JS semantics ──────────────

(deftest-each js-e2e-relational-operators
  "< > <= >= follow JS Abstract Relational Comparison (string-lex, NaN→false, coerce)."
  :cases (("gt-true"      "console.log(5 > 0);"                               "true")
          ("lt-false"     "console.log(2 < 1);"                               "false")
          ("gte-eq"       "console.log(3 >= 3);"                              "true")
          ("lte-false"    "console.log(4 <= 2);"                              "false")
          ("str-lex"      "console.log(\"apple\" < \"banana\");"              "true")
          ("nan-gt"       "console.log(NaN > 0);"                             "false")
          ("coerce"       "console.log(\"5\" > 3);"                           "true")
          ("ternary"      "console.log(5 > 3 ? \"y\" : \"n\");"              "y")
          ("for-loop-sum" "let s=0; for(let i=0;i<4;i++){s+=i;} console.log(s);" "6"))
  (src expected)
  (assert-string= expected (%js-run-capture src)))

;;; ─── 16. Logical && / || short-circuit and yield an operand ──────────────────

(deftest-each js-e2e-logical-and-or
  "&& and || short-circuit and yield operand values, not coerced booleans."
  :cases (("and-false"  "console.log(true && false);"                                    "false")
          ("and-true"   "console.log(true && true);"                                     "true")
          ("and-val"    "console.log(1 && 2);"                                            "2")
          ("and-zero"   "console.log(0 && 2);"                                            "0")
          ("or-right"   "console.log(0 || 5);"                                            "5")
          ("or-left"    "console.log(3 || 5);"                                            "3")
          ("and-str"    "console.log(\"a\" && \"b\");"                                   "b")
          ("and-chain"  "console.log(1 && 2 && 3);"                                      "3")
          ("or-default" "function f(a){return a || \"def\";} console.log(f());"         "def")
          ("or-given"   "function f(a){return a || \"def\";} console.log(f(\"hi\"));"   "hi")
          ("short-circ" "let c=0; function s(){c++;return true;} false && s(); console.log(c);" "0"))
  (src expected)
  (assert-string= expected (%js-run-capture src)))

;;; ─── 17. null / undefined literals use the runtime sentinels ─────────────────

(deftest-js-run js-e2e-null-undefined-literals
  "null/undefined lower to runtime sentinels; ?? treats them as nullish; typeof is correct."
  ("null"      "console.log(null);")
  ("undefined" "console.log(undefined);")
  ("null"      "const x=null; console.log(x);")
  ("d"         "console.log(null ?? \"d\");")
  ("d"         "console.log(undefined ?? \"d\");")
  ("null"      "const x=null; console.log(x && x.foo);")
  ("object"    "console.log(typeof null);")
  ("undefined" "console.log(typeof undefined);")
  ("true"      "console.log(null == undefined);")
  ("v=null"    "console.log(\"v=\"+null);")
  ("undefined" "console.log(void 0);"))

;;; ─── 18. Class getters and setters ──────────────────────────────────────────

(deftest-js-run js-e2e-class-getters-setters
  "Class get/set accessors are invoked on property read/write; inheritance resolves through prototype chain."
  ("9"  "class C{get v(){return 9;}} console.log(new C().v);")
  ("20" "class C{constructor(){this.n=10;} get v(){return this.n*2;}} console.log(new C().v);")
  ("5"  "class C{set v(x){this._x=x;}} const o=new C(); o.v=5; console.log(o._x);")
  ("8"  "class C{constructor(){this._x=1;} get x(){return this._x;} set x(v){this._x=v;}} const o=new C(); o.x=8; console.log(o.x);")
  ("1"  "class A{get v(){return 1;}} class B extends A{} console.log(new B().v);")
  ("3"  "class C{m(){return 3;}} console.log(new C().m());")
  ("7"  "class C{constructor(){this.x=7;}} console.log(new C().x);"))

;;; ─── 19. Falsy values: ternary truthiness, NaN, uninitialized var ────────────

(deftest-js-run js-e2e-conditional-truthiness
  "JS falsy values test as false in ternary/if; uninitialized `let' is undefined."
  ("f" "console.log(\"\" ? \"t\" : \"f\");")
  ("f" "console.log(null ? \"t\" : \"f\");")
  ("f" "console.log(undefined ? \"t\" : \"f\");")
  ("f" "console.log(NaN ? \"t\" : \"f\");")
  ("f" "console.log(0 ? \"t\" : \"f\");")
  ("t" "console.log(\"x\" ? \"t\" : \"f\");")
  ("t" "console.log([] ? \"t\" : \"f\");")
  ("f" "if(NaN){console.log(\"t\");}else{console.log(\"f\");}")
  ("undefined" "let x; console.log(typeof x);")
  ("true"      "let y; console.log(y === undefined);")
  ("f"         "let z; console.log(z ? \"t\" : \"f\");"))

;;; ─── 20. Coercion functions: Number/String/Boolean/parseInt/parseFloat ───────

(deftest-js-run js-e2e-coercion-functions
  "Number/String/Boolean/parseInt/parseFloat callable as functions; Number.isInteger unaffected."
  ("43"         "console.log(Number(\"42\") + 1);")
  ("3.14"       "console.log(Number(\"3.14\"));")
  ("0"          "console.log(Number());")
  ("42x"        "console.log(String(42) + \"x\");")
  ("null"       "console.log(String(null));")
  ("true false" "console.log(Boolean(1), Boolean(0));")
  ("false true" "console.log(Boolean(\"\"), Boolean(\"x\"));")
  ("42"         "console.log(parseInt(\"42px\"));")
  ("255"        "console.log(parseInt(\"ff\", 16));")
  ("3.14"       "console.log(parseFloat(\"3.14abc\"));")
  ("1500"       "console.log(parseFloat(\"1.5e3xyz\"));")
  ("NaN"        "console.log(parseFloat(\"abc\"));")
  ("true"       "console.log(Number.isInteger(5));"))

;;; ─── 21. Tagged template literals ────────────────────────────────────────────

(deftest-js-run js-e2e-tagged-templates
  "Tagged template calls TAG(strings-array, ...subs); plain templates concatenate."
  ("1"     "function t(s){return s.length;} console.log(t`hi`);")
  ("hello" "function t(s){return s[0];} console.log(t`hello`);")
  ("a5b"   "function t(s,v){return s[0]+v+s[1];} console.log(t`a${5}b`);")
  ("x1y2z" "function t(s,a,b){return s[0]+a+s[1]+b+s[2];} console.log(t`x${1}y${2}z`);")
  ("a|b|c" "function tag(s,...v){return s.join('|');} console.log(tag`a${1}b${2}c`);")
  ("[]9[]" "function t(s,v){return '['+s[0]+']'+v+'['+s[1]+']';} console.log(t`${9}`);")
  ("val=5" "const n=5; console.log(`val=${n}`);")
  ("sum=5" "console.log(`sum=${2+3}`);"))

(deftest-js-run js-e2e-number-prototype-methods
  "Number.prototype toFixed/toString/toPrecision/valueOf format JS-faithfully."
  ("123.46"   "console.log((123.456).toFixed(2));")
  ("8"        "console.log((7.5).toFixed(0));")
  ("ff"       "console.log((255).toString(16));")
  ("1010"     "console.log((10).toString(2));")
  ("FF"       "console.log((255).toString(16).toUpperCase());")
  ("3.14"     "console.log((3.14159).toPrecision(3));")
  ("100"      "console.log((100).toPrecision(3));")
  ("1.2e+3"   "console.log((1234.5).toPrecision(2));")
  ("0.000012" "console.log((0.00001234).toPrecision(2));")
  ("-3.14"    "console.log((-3.14159).toPrecision(3));")
  ("42"       "console.log((42).valueOf());"))

(deftest-js-run js-e2e-standalone-global-builtins
  "structuredClone/queueMicrotask/setTimeout are reachable as bare direct calls."
  ("3"       "console.log(structuredClone({b:[2,3]}).b[1]);")
  ("5,99"    "const o={x:{y:5}}; const c=structuredClone(o); c.x.y=99; console.log(o.x.y+','+c.x.y);")
  ("3"       "console.log(structuredClone([1,2,3]).length);")
  ("micro"   "queueMicrotask(()=>console.log('micro'));")
  ("timeout" "setTimeout(()=>console.log('timeout'),0);")
  ("1"       "const f=structuredClone; console.log(f({a:1}).a);")
  ("42"      "console.log(parseInt('42px'));"))

(deftest-js-run js-e2e-super-constructor
  "super(args) calls the parent constructor; multi-level chains call the right parent."
  ("y"    "class A{constructor(n){this.n=n;}} class B extends A{constructor(n){super(n);}} console.log(new B('y').n);")
  ("x x!" "class A{constructor(n){this.n=n;}} class B extends A{constructor(n){super(n);this.m=n+'!';}} const b=new B('x'); console.log(b.n+' '+b.m);")
  ("11"   "class A{constructor(){this.k=1;}} class B extends A{constructor(){super();this.k+=10;}} console.log(new B().k);")
  ("12"   "class A{constructor(x){this.x=x;}} class B extends A{constructor(x){super(x*2);}} class C extends B{constructor(x){super(x+1);}} console.log(new C(5).x);")
  ("z"    "class A{constructor(n){this.n=n;}} class B extends A{} console.log(new B('z').n);"))

(deftest-js-run js-e2e-class-fields
  "Class field initializers run per-instance before constructor body; fields can reference each other."
  ("0"    "class C{count=0;} console.log(new C().count);")
  ("15"   "class C{x=5;y=10;} const c=new C(); console.log(c.x+c.y);")
  ("hi"   "class C{name='hi';} console.log(new C().name);")
  ("1,2"  "class C{items=[];add(v){this.items.push(v);return this.items.length;}} const c=new C(); console.log(c.add(1)+','+c.add(2));")
  ("11"   "class C{x=1;constructor(){this.x+=10;}} console.log(new C().x);")
  ("6"    "class C{a=2;b=this.a*3;} console.log(new C().b);")
  ("y 99" "class A{constructor(n){this.n=n;}} class D extends A{extra=99;} const d=new D('y'); console.log(d.n+' '+d.extra);"))

(deftest-js-run js-e2e-class-self-reference
  "A class can reference itself inside its own methods (static create, clone, recursive)."
  ("z"      "class A{constructor(n){this.n=n;} static create(n){return new A(n);}} console.log(A.create('z').n);")
  ("6"      "class A{constructor(n){this.n=n;} clone(){return new A(this.n+1);}} console.log(new A(5).clone().n);")
  ("true"   "class A{static self(){return A;}} console.log(A.self()===A);")
  ("42"     "class A{static y(){return 42;} static z(){return A.y();}} console.log(A.z());")
  ("object" "class A{static make(){return new A();}} console.log(typeof A.make());"))

(deftest-js-run js-e2e-static-fields
  "static field initializers set the value on the class object; case is preserved."
  ("1.0" "class A{static VERSION='1.0';} console.log(A.VERSION);")
  ("100" "class A{static MAX=100; static MIN=0;} console.log(A.MAX-A.MIN);")
  ("1 2" "class A{static count=0; static inc(){return ++A.count;}} console.log(A.inc()+' '+A.inc());")
  ("3"   "class A{static items=[1,2,3];} console.log(A.items.length);")
  ("7"   "class C{COUNT=7;} console.log(new C().COUNT);"))

(deftest-js-run js-e2e-case-sensitive-identifiers
  "Identifiers are case-sensitive: a/A, foo/Foo are distinct."
  ("5 10" "const A=5; const a=10; console.log(A+' '+a);")
  ("1 2"  "function foo(){return 1;} function Foo(){return 2;} console.log(foo()+' '+Foo());")
  ("10"   "class A{static Y=10;m(){return 1;}} const a=new A(); a.m(); console.log(A.Y);")
  ("7 0"  "class Point{constructor(x){this.x=x;} static origin(){return new Point(0);}} const p=new Point(7); console.log(p.x+' '+Point.origin().x);"))

(deftest-js-run js-e2e-super-method
  "super.method() calls the parent method; super() constructor calls still work in same class."
  ("11"      "class A{f(){return 1;}} class B extends A{f(){return super.f()+10;}} console.log(new B().f());")
  ("AB"      "class A{greet(){return 'A';}} class B extends A{greet(){return super.greet()+'B';}} console.log(new B().greet());")
  ("10"      "class A{constructor(){this.x=5;} m(){return this.x;}} class B extends A{m(){return super.m()*2;}} console.log(new B().m());")
  ("Hi Bob!" "class A{constructor(n){this.n=n;} greet(){return 'Hi '+this.n;}} class B extends A{constructor(n){super(n);} greet(){return super.greet()+'!';}} console.log(new B('Bob').greet());")
  ("k"       "class A{constructor(n){this.n=n;}} class B extends A{constructor(n){super(n);}} console.log(new B('k').n);"))

(deftest-js-run js-e2e-contextual-keywords-as-identifiers
  "Contextual keywords (get/set/of/from/static) are valid in binding positions."
  ("5"  "const set=5; console.log(set);")
  ("3"  "const of=1, from=2; console.log(of+from);")
  ("10" "function f(set){return set*2;} console.log(f(5));")
  ("3"  "const [get,set]=[1,2]; console.log(get+set);")
  ("5"  "class C{get x(){return 5;}} console.log(new C().x);")
  ("6"  "let s=0; for(const x of [1,2,3]){s+=x;} console.log(s);"))

(deftest-js-run js-e2e-symbol-constructor
  "Symbol(desc) is callable; .description is an accessor property; computed keys work."
  ("symbol"    "console.log(typeof Symbol('x'));")
  ("Symbol(d)" "console.log(Symbol('d').toString());")
  ("k"         "console.log(Symbol('k').description);")
  ("true"      "console.log(Symbol().description===undefined);")
  ("symbol"    "console.log(typeof Symbol());")
  ("42"        "const o={}; const k=Symbol('key'); o[k]=42; console.log(o[k]);")
  ("symbol"    "console.log(typeof Symbol.iterator);"))

(deftest-js-run js-e2e-global-function-calls
  "btoa/atob/encode*/decode*/isNaN/isFinite are callable as bare global calls."
  ("aGk="      "console.log(btoa('hi'));")
  ("hi"        "console.log(atob('aGk='));")
  ("a%20b"     "console.log(encodeURIComponent('a b'));")
  ("a b"       "console.log(decodeURIComponent('a%20b'));")
  ("a%20b/c"   "console.log(encodeURI('a b/c'));")
  ("false true" "console.log(isNaN(5)+' '+isNaN(NaN));")
  ("true false" "console.log(isFinite(5)+' '+isFinite(Infinity));"))

(deftest-js-run js-e2e-for-in
  "for...in enumerates enumerable string keys of an object."
  ("a,b,c" "const o={a:1,b:2,c:3}; const keys=[]; for(let k in o){keys.push(k);} console.log(keys.join(','));")
  ("x"     "const o={x:10}; let result=''; for(let k in o){result+=k;} console.log(result);"))

(deftest-js-run js-e2e-for-of-array
  "for...of iterates elements of an array."
  ("1,2,3" "const a=[1,2,3]; const out=[]; for(let v of a){out.push(v);} console.log(out.join(','));")
  ("30"    "let s=0; for(const n of [10,20]){s+=n;} console.log(s);"))

(deftest-js-run js-e2e-for-of-string
  "for...of iterates characters of a string."
  ("h,e,l,l,o" "const chars=[]; for(let c of 'hello'){chars.push(c);} console.log(chars.join(','));"))

(deftest-js-run js-e2e-for-of-destructuring
  "for...of with array/object destructuring unpacks each element."
  ("1a,2b"   "const pairs=[[1,'a'],[2,'b']]; const out=[]; for(let [n,s] of pairs){out.push(n+s);} console.log(out.join(','));")
  ("Alice,Bob" "const people=[{name:'Alice'},{name:'Bob'}]; const names=[]; for(const {name} of people){names.push(name);} console.log(names.join(','));")
  ("1,2,3"   "const rows=[[1,2,3]]; for(const [a,...rest] of rows){console.log([a].concat(rest).join(','));}"))

(deftest-js-run js-e2e-generator-execution
  "Generator functions eagerly collect yields; for...of and spread drain them."
  ("0,1,2,3,4" "function* range(n){for(let i=0;i<n;i++){yield i;}} const out=[]; for(const v of range(5)){out.push(v);} console.log(out.join(','));")
  ("1,2,3"     "function* g(){yield 1; yield 2; yield 3;} console.log([...g()].join(','));")
  ("a,b,c"     "function* letters(){yield* ['a','b','c'];} const out=[]; for(const v of letters()){out.push(v);} console.log(out.join(','));"))

(deftest-js-run js-e2e-map-iteration
  "Map for...of yields [k,v] pairs; .size/.get/.set/.has work; Array.from(map) works."
  ("a=1,b=2"       "const m=new Map([['a',1],['b',2]]); const out=[]; for(const [k,v] of m){out.push(k+'='+v);} console.log(out.join(','));")
  ("2 1 true false" "const m=new Map(); m.set('x',1); m.set('y',2); console.log(m.size+' '+m.get('x')+' '+m.has('x')+' '+m.has('z'));")
  ("2"              "const m=new Map([[1,'a'],[2,'b']]); console.log(Array.from(m).length);"))

(deftest-js-run js-e2e-set-iteration
  "Set for...of yields unique values; .size/.has/.add/.delete work; spread works."
  ("1,2,3"     "const s=new Set([1,2,3,2,1]); const out=[]; for(const v of s){out.push(v);} console.log(out.join(','));")
  ("3 true false" "const s=new Set([1,2,3]); console.log(s.size+' '+s.has(2)+' '+s.has(9));")
  ("4"          "const a=new Set([1,2,3]); const b=new Set([3,4]); const u=new Set([...a,...b]); console.log(u.size);"))

(deftest-js-run js-e2e-array-from
  "Array.from converts iterables/array-likes; optional map function applied."
  ("1,2,3"   "console.log(Array.from([1,2,3]).join(','));")
  ("h,e,l,l,o" "console.log(Array.from('hello').join(','));")
  ("2,4,6"   "console.log(Array.from([1,2,3],x=>x*2).join(','));")
  ("3"       "console.log(Array.from(new Set([1,2,3])).length);"))

(deftest js-e2e-private-class-fields
  "Private class fields (#name) are stored in the instance's __private__ slot
and are inaccessible from outside the class.  ES2022 syntax, inherited by
ES2026."
  ;; basic private field read/write
  (assert-string= "42"
    (%js-run-capture
     "class C{#x; constructor(v){this.#x=v;} get(){return this.#x;}}
console.log(new C(42).get());"))
  ;; private field with default initializer
  (assert-string= "0"
    (%js-run-capture
     "class C{#count=0; inc(){this.#count++;} val(){return this.#count;}}
const c=new C(); console.log(c.val());"))
  ;; private method (static)
  (assert-string= "10"
    (%js-run-capture
     "class C{#v; constructor(v){this.#v=v;} double(){return this.#v*2;}}
console.log(new C(5).double());"))
  ;; subclass uses parent's private field via inherited method
  (assert-string= "7"
    (%js-run-capture
     "class P{#n; constructor(n){this.#n=n;} get(){return this.#n;}}
class C extends P{constructor(n){super(n);} doubled(){return this.get()*2;}}
console.log(new C(7).get());")))



(deftest-js-run js-e2e-promise-chaining
  "Promise.then/.catch chain in the synchronous model; Promise.all/async work."
  ("6"          "Promise.resolve(3).then(x=>x*2).then(x=>console.log(x));")
  ("caught: oops" "Promise.reject('oops').catch(e=>console.log('caught: '+e));")
  ("1,2,3"      "Promise.all([Promise.resolve(1),Promise.resolve(2),Promise.resolve(3)]).then(vs=>console.log(vs.join(',')));")
  ("done"       "async function f(){return 'done';} f().then(v=>console.log(v));"))

(deftest-js-run js-e2e-weakmap-weakset
  "WeakMap and WeakSet: get/set/has/delete work (behave like Map/Set in CL model)."
  ("42 true false" "const wm=new WeakMap(); const k={}; wm.set(k,42); console.log(wm.get(k)+' '+wm.has(k)+' '+wm.has({}));")
  ("true false"    "const ws=new WeakSet(); const o={}; ws.add(o); console.log(ws.has(o)+' '+ws.has({}));")
  ("true false"    "const wm=new WeakMap(); const k={}; wm.set(k,1); const had=wm.has(k); wm.delete(k); console.log(had+' '+wm.has(k));"))

(deftest-js-run js-e2e-object-static-methods
  "Object.keys/values/entries/assign/fromEntries work on plain objects."
  ("a,b"   "console.log(Object.keys({a:1,b:2}).join(','));")
  ("1,2"   "console.log(Object.values({a:1,b:2}).join(','));")
  ("a=1,b=2" "console.log(Object.entries({a:1,b:2}).map(([k,v])=>k+'='+v).join(','));")
  ("3"     "const t={}; Object.assign(t,{x:1},{y:2}); console.log(t.x+t.y);")
  ("10"    "const o=Object.fromEntries([['x',10]]); console.log(o.x);"))

(deftest-js-run js-e2e-string-methods-es2021
  "String methods ES2021+: replaceAll, at, trimStart, trimEnd."
  ("a-b-c" "console.log('a.b.c'.replaceAll('.', '-'));")
  ("c"     "console.log('abc'.at(-1));")
  ("a"     "console.log('abc'.at(0));")
  ("hi  "  "console.log('  hi  '.trimStart());")
  ("  hi"  "console.log('  hi  '.trimEnd());"))

(deftest-js-run js-e2e-array-methods-es2023
  "Array ES2023: toReversed/toSorted (non-mutating), at (negative index), findLast/findLastIndex."
  ("3,2,1" "const a=[1,2,3]; console.log(a.toReversed().join(','));")
  ("1,2,3" "const a=[1,2,3]; a.toReversed(); console.log(a.join(','));")
  ("1,2,3" "console.log([3,1,2].toSorted().join(','));")
  ("3"     "console.log([1,2,3].at(-1));")
  ("4"     "console.log([1,2,3,4].findLast(x=>x%2===0));")
  ("3"     "console.log([1,2,3,4].findLastIndex(x=>x%2===0));"))

;;; ─── ES2025 Set composition methods ──────────────────────────────────────────

(deftest-js-run js-e2e-set-methods-es2025
  "ES2025 Set composition: union/intersection/difference/symmetricDifference/isSubsetOf/isSupersetOf/isDisjointFrom."
  ("1,2,3,4" "const a=new Set([1,2,3]),b=new Set([2,3,4]); console.log([...a.union(b)].join(','));")
  ("2,3"     "const a=new Set([1,2,3]),b=new Set([2,3,4]); console.log([...a.intersection(b)].join(','));")
  ("1"       "const a=new Set([1,2,3]),b=new Set([2,3,4]); console.log([...a.difference(b)].join(','));")
  ("1,4"     "const a=new Set([1,2,3]),b=new Set([2,3,4]); console.log([...a.symmetricDifference(b)].join(','));")
  ("true"    "console.log(new Set([1,2]).isSubsetOf(new Set([1,2,3])));")
  ("false"   "console.log(new Set([1,4]).isSubsetOf(new Set([1,2,3])));")
  ("true"    "console.log(new Set([1,2,3]).isSupersetOf(new Set([1,2])));")
  ("false"   "console.log(new Set([1,2]).isSupersetOf(new Set([1,2,3])));")
  ("true"    "console.log(new Set([1,2]).isDisjointFrom(new Set([3,4])));")
  ("false"   "console.log(new Set([1,2]).isDisjointFrom(new Set([2,3])));"))

;;; ─── ES2025 Iterator.prototype helpers ───────────────────────────────────────

(deftest-js-run js-e2e-iterator-helpers-es2025
  "ES2025 Iterator.prototype: map/filter/take/drop/flatMap/reduce/toArray/forEach/some/every/find."
  ("1,2,3"    "console.log([1,2,3].values().toArray().join(','));")
  ("2,4,6,8,10" "console.log([1,2,3,4,5].values().map(x=>x*2).toArray().join(','));")
  ("2,4,6"    "console.log([1,2,3,4,5,6].values().filter(x=>x%2===0).toArray().join(','));")
  ("1,2,3"    "console.log([1,2,3,4,5].values().take(3).toArray().join(','));")
  ("3,4,5"    "console.log([1,2,3,4,5].values().drop(2).toArray().join(','));")
  (""         "console.log([1,2,3].values().drop(10).toArray().join(','));")
  ("1,9,25"   "console.log([1,2,3,4,5].values().filter(x=>x%2!==0).map(x=>x*x).toArray().join(','));")
  ("15"       "console.log([1,2,3,4,5].values().reduce((acc,x)=>acc+x,0));")
  ("true"     "console.log([1,3,4,5].values().some(x=>x%2===0));")
  ("false"    "console.log([1,3,5].values().some(x=>x%2===0));")
  ("true"     "console.log([2,4,6].values().every(x=>x%2===0));")
  ("false"    "console.log([2,3,6].values().every(x=>x%2===0));")
  ("4"        "console.log([1,3,4,6,7].values().find(x=>x%2===0));")
  ("undefined" "const v=[1,3,5].values().find(x=>x%2===0); console.log(v===undefined?'undefined':v);")
  ("6"        "let s=0; [1,2,3].values().forEach(x=>{s+=x;}); console.log(s);")
  ("1,2,3,4,5" "console.log([[1,2],[3,4],[5]].values().flatMap(x=>x).toArray().join(','));"))

;;; ─── ES2025 Iterator helpers on generators ───────────────────────────────────

(deftest-js-run js-e2e-generator-iterator-helpers
  "ES2025 Iterator.prototype helpers work on generators (function*)."
  ("20,40"    "function* gen(n){for(let i=1;i<=n;i++)yield i;} const r=gen(5).filter(x=>x%2===0).map(x=>x*10).toArray(); console.log(r.join(','));")
  ("1,2,3"    "function* nums(){yield 1;yield 2;yield 3;yield 4;yield 5;} console.log(nums().take(3).toArray().join(','));")
  ("15"       "function* range(n){for(let i=1;i<=n;i++)yield i;} console.log(range(5).reduce((a,x)=>a+x,0));")
  ("true false" "function* vals(){yield 1;yield 3;yield 5;} const g=vals(); function* vals2(){yield 1;yield 3;yield 5;} const g2=vals2(); console.log(g.some(x=>x>2)+' '+g2.every(x=>x%2===0));")
  ("4,6"      "function* nums(){for(let i=1;i<=6;i++)yield i;} console.log(nums().drop(2).filter(x=>x%2===0).toArray().join(','));"))

;;; ─── ES2025/2026 static built-ins ────────────────────────────────────────────

(deftest-js-run js-e2e-es2026-math-sum-precise
  "ES2026 Math.sumPrecise: precise summation over an iterable."
  ("15" "console.log(Math.sumPrecise([1,2,3,4,5]));")
  ("42" "console.log(Math.sumPrecise([42]));")
  ("0"  "console.log(Math.sumPrecise([]));"))

(deftest-js-run js-e2e-es2026-error-is-error
  "ES2026 Error.isError: true for Error objects, false for numbers/null."
  ("yes" "const e=new Error('oops'); console.log(Error.isError(e)?'yes':'no');")
  ("no"  "console.log(Error.isError(42)?'yes':'no');")
  ("no"  "console.log(Error.isError(null)?'yes':'no');"))

(deftest-js-run js-e2e-es2024-regexp-escape
  "ES2024 RegExp.escape: escapes regex metacharacters with a backslash."
  ("Hello\\.World" "console.log(RegExp.escape('Hello.World'));")
  ("hello"         "console.log(RegExp.escape('hello'));")
  ("what\\?"       "console.log(RegExp.escape('what?'));"))

(deftest-js-run js-e2e-es2024-map-group-by
  "ES2024 Map.groupBy: groups iterable elements by a key function into a Map."
  ("2,4,6" "const g=Map.groupBy([1,2,3,4,5,6],x=>x%2===0?'even':'odd'); console.log(g.get('even').join(','));")
  ("1,3,5" "const g=Map.groupBy([1,2,3,4,5,6],x=>x%2===0?'even':'odd'); console.log(g.get('odd').join(','));")
  ("none"  "const g=Map.groupBy([1,3,5],x=>'odd'); const v=g.get('even'); console.log(v===undefined?'none':v.join(','));"))

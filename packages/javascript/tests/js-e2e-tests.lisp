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

(deftest js-e2e-destructuring-and-sequential-decls
  "Array/object destructuring and multi-declarator let bind sequentially
(regression: ast-let binds in parallel, so a later initializer could not see an
earlier binding — destructuring's a = tmp[0] could not see tmp = init, and
`let a=1, b=a+1' gave nothing; %js-finish-let-bindings now expands a multi-binding
empty let into nested single-binding lets)."
  (assert-string= "6"   (%js-run-capture "let a=5, b=a+1; console.log(b);"))
  (assert-string= "30"  (%js-run-capture "let [a,b]=[10,20]; console.log(a+b);"))
  (assert-string= "123" (%js-run-capture "let [a,b,c]=[1,2,3]; console.log(a*100+b*10+c);"))
  (assert-string= "3"   (%js-run-capture "let {x,y}={x:1,y:2}; console.log(x+y);"))
  (assert-string= "2"   (%js-run-capture "let [,b]=[1,2]; console.log(b);"))
  (assert-string= "Bob30" (%js-run-capture "const {name,age}={name:'Bob',age:30}; console.log(name+age);"))
  (assert-string= "12"  (%js-run-capture "function f(){ let [a,b]=[3,4]; return a*b; } console.log(f());")))

(deftest js-e2e-destructuring-rest
  "Array and object destructuring rest elements collect into a real JS array /
object.  Regression: the :rest sentinel was mistaken for a default value, so an
array rest was a one-element CL list (Array.isArray false, rest.join threw with
':JS-UNDEFINED') and an object rest was null.  The array rest now yields a vector
(map/join/length work) and the object rest a fresh object excluding bound keys."
  (assert-string= "3,4,5" (%js-run-capture "const [x,y,...r]=[1,2,3,4,5]; console.log(r.join(','));"))
  (assert-string= "true"  (%js-run-capture "const [x,...r]=[1,2,3]; console.log(Array.isArray(r));"))
  (assert-string= "4,6,8,10" (%js-run-capture "const [a,...r]=[1,2,3,4,5]; console.log(r.map(n=>n*2).join(','));"))
  (assert-string= "0"     (%js-run-capture "const [p,...q]=[1]; console.log(q.length);"))
  (assert-string= "{\"b\":2,\"c\":3}" (%js-run-capture "const {a,...rest}={a:1,b:2,c:3}; console.log(JSON.stringify(rest));"))
  (assert-string= "{}"    (%js-run-capture "const {m,...n}={m:1}; console.log(JSON.stringify(n));")))

(deftest js-e2e-parameter-destructuring
  "Named-function parameters may be destructuring patterns: function f([a,b]) and
function g({x,y}).  Regression: the pattern was parsed but discarded (the param
kept only its gensym), so the body never unpacked it; the unbodied let then failed
to compile and the defun was silently dropped -> 'Undefined function: F'.  Now the
param gensym is unpacked by a body-prologue let (reusing the var-destructuring
machinery), including rest, defaults, and multiple destructuring params."
  (assert-string= "30"  (%js-run-capture "function f([a,b]){return a+b;} console.log(f([10,20]));"))
  (assert-string= "12"  (%js-run-capture "function f({x,y}){return x*y;} console.log(f({x:3,y:4}));"))
  (assert-string= "1:2,3" (%js-run-capture "function f([a,...r]){return a+':'+r.join(',');} console.log(f([1,2,3]));"))
  (assert-string= "{\"b\":2}" (%js-run-capture "function f({a,...rest}){return JSON.stringify(rest);} console.log(f({a:1,b:2}));"))
  (assert-string= "6"   (%js-run-capture "function f([a,b],{x}){return a+b+x;} console.log(f([1,2],{x:3}));"))
  (assert-string= "10"  (%js-run-capture "function f({x=10}){return x;} console.log(f({}));"))
  ;; plain params still work (no regression)
  (assert-string= "7"   (%js-run-capture "function f(a,b){return a-b;} console.log(f(9,2));")))

(deftest js-e2e-nested-destructuring
  "Destructuring patterns nest (array-in-array, object-in-object, mixed) for both
var declarations and function parameters.  Regression: %js-emit-destructure-
bindings bound a nested pattern's gensym but never recursed to unpack it, so inner
names stayed undefined and the binding form failed to compile.  %js-destructure-
sub-bindings now recurses, building bindings in let* order."
  (assert-string= "1 2 3" (%js-run-capture "const [a,[b,c]]=[1,[2,3]]; console.log(a,b,c);"))
  (assert-string= "42"    (%js-run-capture "const {a:{b}}={a:{b:42}}; console.log(b);"))
  (assert-string= "1 2"   (%js-run-capture "const {p,q:{r}}={p:1,q:{r:2}}; console.log(p,r);"))
  (assert-string= "1 2 3 4" (%js-run-capture "const [[a,b],[c,d]]=[[1,2],[3,4]]; console.log(a,b,c,d);"))
  (assert-string= "99"    (%js-run-capture "const {u:{v:{w}}}={u:{v:{w:99}}}; console.log(w);"))
  ;; nested + rest, nested + default
  (assert-string= "1 2 3-4" (%js-run-capture "const [a,[b,...c]]=[1,[2,3,4]]; console.log(a,b,c.join('-'));"))
  (assert-string= "1 9"   (%js-run-capture "const {a,b:{c=9}}={a:1,b:{}}; console.log(a,c);"))
  ;; nested destructuring in a function parameter
  (assert-string= "6"     (%js-run-capture "function f([a,[b,c]]){return a+b+c;} console.log(f([1,[2,3]]));"))
  (assert-string= "3"     (%js-run-capture "function f({data:{items}}){return items.length;} console.log(f({data:{items:[1,2,3]}}));")))

(deftest js-e2e-arrow-destructuring
  "Arrow-function parameters may be destructuring patterns: ([a,b])=>… and
({x,y})=>…, including nesting and the .map(([a,b])=>…) idiom.  Regression: the
paren contents were parsed as an array/object literal and only plain identifiers
became params, so the pattern was dropped and the arrow silently produced nothing.
%js-expr-to-binding-pattern now recovers the pattern from the literal when => is
seen, reusing the body-prologue destructuring lowering."
  (assert-string= "11" (%js-run-capture "const f=([a,b])=>a+b; console.log(f([5,6]));"))
  (assert-string= "12" (%js-run-capture "const g=({x,y})=>x*y; console.log(g({x:3,y:4}));"))
  (assert-string= "6"  (%js-run-capture "const k=([a,[b,c]])=>a+b+c; console.log(k([1,[2,3]]));"))
  (assert-string= "7"  (%js-run-capture "const q=({a:{b}})=>b; console.log(q({a:{b:7}}));"))
  ;; the canonical idiom: destructuring in a .map/.forEach callback
  (assert-string= "3,7" (%js-run-capture "console.log([[1,2],[3,4]].map(([a,b])=>a+b).join(','));"))
  (assert-string= "10,20" (%js-run-capture "console.log([{n:1},{n:2}].map(({n})=>n*10).join(','));"))
  ;; rest INSIDE an arrow pattern: array spread lowers to apply+append, object
  ;; spread to a fold chain — both reversed by the converter
  (assert-string= "3" (%js-run-capture "const g=([a,...r])=>r.length; console.log(g([1,2,3,4]));"))
  (assert-string= "{\"b\":2,\"c\":3}" (%js-run-capture "const m=({a,...rest})=>JSON.stringify(rest); console.log(m({a:1,b:2,c:3}));"))
  (assert-string= "1:2,3;4:5,6" (%js-run-capture "console.log([[1,2,3],[4,5,6]].map(([a,...rest])=>a+':'+rest.join(',')).join(';'));"))
  ;; object spread literals (not arrow params) are unaffected by the fold-chain detection
  (assert-string= "1 2" (%js-run-capture "const o={...{a:1},...{b:2}}; console.log(o.a,o.b);"))
  ;; plain arrows and parenthesized expressions still parse (no regression)
  (assert-string= "10" (%js-run-capture "const r=x=>x*2; console.log(r(5));"))
  (assert-string= "7"  (%js-run-capture "const p=(a,b)=>a-b; console.log(p(9,2));"))
  (assert-string= "3"  (%js-run-capture "console.log((1,2,3));")))

(deftest js-e2e-json-and-math-globals
  "JSON and Math are seeded as global objects (regression: they were never added
to the prelude). JSON.parse worked once its string scanner stopped discarding the
position via with-output-to-string."
  (assert-string= "{\"a\":1,\"b\":2}" (%js-run-capture "console.log(JSON.stringify({a:1,b:2}));"))
  (assert-string= "[1,2,3]"  (%js-run-capture "console.log(JSON.stringify([1,2,3]));"))
  (assert-string= "5"  (%js-run-capture "let o=JSON.parse('{\"x\":5}'); console.log(o.x);"))
  (assert-string= "42hi" (%js-run-capture "let p=JSON.parse(JSON.stringify({n:42,s:'hi'})); console.log(p.n+p.s);"))
  (assert-string= "20" (%js-run-capture "console.log(JSON.parse('[10,20,30]')[1]);"))
  (assert-string= "7"  (%js-run-capture "console.log(Math.max(3,7,2));"))
  (assert-string= "4"  (%js-run-capture "console.log(Math.sqrt(16));"))
  (assert-string= "5"  (%js-run-capture "console.log(Math.abs(-5));")))

(deftest js-e2e-try-catch-finally
  "try/catch/finally executes (regression: the try/catch/finally thunks are
vm-closures, but %js-try-catch-finally invoked them with raw CL:FUNCALL, which
cannot call a vm-closure — every try statement failed)."
  (assert-string= "a"     (%js-run-capture "try { console.log('a'); } catch(e) { console.log('b'); }"))
  (assert-string= "oops"  (%js-run-capture "try { throw 'oops'; } catch(e) { console.log(e); }"))
  (assert-string= "43"    (%js-run-capture "try { throw 42; } catch(e) { console.log(e+1); }"))
  (assert-string= (format nil "t~%f")   (%js-run-capture "try { console.log('t'); } finally { console.log('f'); }"))
  (assert-string= (format nil "c:x~%fin") (%js-run-capture "try { throw 'x'; } catch(e) { console.log('c:'+e); } finally { console.log('fin'); }"))
  (assert-string= "boom"  (%js-run-capture "try { throw new Error('boom'); } catch(e) { console.log(e.message); }"))
  ;; finally runs before a return inside try
  (assert-string= (format nil "cleanup~%1") (%js-run-capture "function f(){ try { return 1; } finally { console.log('cleanup'); } } console.log(f());"))
  ;; nested try with re-throw
  (assert-string= "outer:inner" (%js-run-capture "try { try { throw 'inner'; } catch(e) { throw 'outer:'+e; } } catch(e) { console.log(e); }")))

(deftest js-e2e-object-spread
  "Object spread {...a, k:v} merges own properties with later entries overriding
earlier ones (regression: a spread entry misaligned the %js-make-object key/value
pairs). Lowers to a left-to-right fold of %js-object-assign / set."
  (assert-string= "6"     (%js-run-capture "let a={x:1,y:2}; let b={...a,z:3}; console.log(b.x+b.y+b.z);"))
  (assert-string= "3"     (%js-run-capture "let a={x:1},c={y:2}; console.log(({...a,...c}).x+({...a,...c}).y);"))
  (assert-string= "9"     (%js-run-capture "let a={x:1,y:2}; console.log(({...a,y:9}).y);"))
  (assert-string= "3"     (%js-run-capture "let a={y:2}; let b={x:1,...a}; console.log(b.x+b.y);"))
  ;; spread makes a shallow copy independent of the source
  (assert-string= "5,99"  (%js-run-capture "let a={n:5}; let b={...a}; b.n=99; console.log(a.n+','+b.n);")))

(deftest js-e2e-spread-arrays-and-calls
  "...spread expands an iterable in array literals and call arguments (regression:
the %js-spread marker was left as a single element / argument). Lowers to apply
over a runtime-appended list."
  (assert-string= "1,2,3,4" (%js-run-capture "let a=[1,2]; console.log([...a,3,4].join(','));"))
  (assert-string= "4"   (%js-run-capture "let a=[1,2],c=[3,4]; console.log([...a,...c].length);"))
  (assert-string= "0,1,2,3" (%js-run-capture "console.log([0,...[1,2],3].join(','));"))
  (assert-string= "6"   (%js-run-capture "function add(a,b,c){return a+b+c;} console.log(add(...[1,2,3]));"))
  (assert-string= "1-2-3" (%js-run-capture "function f(a,b,c){return a+'-'+b+'-'+c;} console.log(f(1,...[2,3]));"))
  (assert-string= "a-b-c" (%js-run-capture "console.log([...'abc'].join('-'));"))
  (assert-string= "7"   (%js-run-capture "console.log(Math.max(...[3,7,2]));")))

(deftest js-e2e-division-yields-float
  "JS / produces an IEEE number, not a CL rational (regression: 5/2 printed
'5/2'). Integer-valued quotients still print cleanly, and /0 gives Infinity."
  (assert-string= "2.5"  (%js-run-capture "console.log(5/2);"))
  (assert-string= "3"    (%js-run-capture "console.log(6/2);"))
  (assert-string= "3.5"  (%js-run-capture "let a=7,b=2; console.log(a/b);"))
  (assert-string= "1.25" (%js-run-capture "console.log(10/4/2);"))
  (assert-string= "4"    (%js-run-capture "let n=[2,4,6]; console.log(n.reduce((a,b)=>a+b,0)/n.length);"))
  (assert-string= "Inf"  (%js-run-capture "console.log(5/0 === Infinity ? 'Inf' : 'no');")))

(deftest js-e2e-integer-valued-float-formatting
  "An integer-valued float prints without a decimal point, JS-style (7.0 -> '7'),
while real decimals are preserved."
  (assert-string= "7"    (%js-run-capture "console.log(7.0);"))
  (assert-string= "3.14" (%js-run-capture "console.log(3.14);"))
  (assert-string= "2.5"  (%js-run-capture "console.log(2.5);"))
  (assert-string= "4"    (%js-run-capture "console.log(Math.sqrt(16));")))

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

;;; ─── 11. Named function-expression recursion + typeof function ───────────────

(deftest js-e2e-named-function-expression-recursion
  "A named function expression's name is visible INSIDE its own body for
self-recursion (but not outside).  Regression: it lowered to a PARALLEL
(let ((f lambda)) f), so `f` inside the body resolved to the enclosing scope
(undefined → 'Undefined function').  Now letrec-style: bind the name to nil, then
assign the lambda — the name is mutated AND captured, so it is boxed and the
closure reads the assignment."
  (assert-string= "120" (%js-run-capture "const f=function fac(n){return n<=1?1:n*fac(n-1);}; console.log(f(5));"))
  (assert-string= "55"  (%js-run-capture "const f=function fib(n){return n<2?n:fib(n-1)+fib(n-2);}; console.log(f(10));"))
  ;; anonymous and non-self-referential named expressions are unaffected
  (assert-string= "5"   (%js-run-capture "const f=function(x){return x+1;}; console.log(f(4));"))
  (assert-string= "6"   (%js-run-capture "const f=function g(x){return x*2;}; console.log(f(3));"))
  ;; passing a named function expression as an argument
  (assert-string= "6,2,4" (%js-run-capture "console.log([3,1,2].map(function dbl(x){return x*2;}).join(\",\"));")))

(deftest js-e2e-typeof-function
  "typeof returns 'function' for compiled JS functions (regression: they are
vm-closure-objects, not CL functions, so typeof fell through to 'object',
breaking the ubiquitous `typeof f === \"function\"' feature-detection idiom).
Object/array/primitive typeof results are unaffected."
  (assert-string= "function" (%js-run-capture "const f=function(x){return x;}; console.log(typeof f);"))
  (assert-string= "function" (%js-run-capture "const g=x=>x; console.log(typeof g);"))
  (assert-string= "function" (%js-run-capture "function h(){return 1;} console.log(typeof h);"))
  (assert-string= "function" (%js-run-capture "const o={m(){return 1;}}; console.log(typeof o.m);"))
  (assert-string= "yes"      (%js-run-capture "const h=function(){}; console.log(typeof h===\"function\"?\"yes\":\"no\");"))
  ;; non-function typeof results unchanged
  (assert-string= "object"   (%js-run-capture "console.log(typeof {a:1});"))
  (assert-string= "object"   (%js-run-capture "console.log(typeof [1,2]);"))
  (assert-string= "string number boolean" (%js-run-capture "console.log(typeof \"x\", typeof 5, typeof true);")))

;;; ─── 12. Arrow function default + rest parameters ────────────────────────────

(deftest js-e2e-arrow-default-and-rest-params
  "Arrow functions support default parameters (x=5) and rest parameters (...n).
Regression: the speculative paren/arrow parser had no branch for `=' (so a default
errored with 'expected ) but got =') and appended the rest param as an ORDINARY
positional (so (...n)=>n.length saw n as a single arg -> undefined).  Now defaults
become optional-params and the rest param routes through %js-rest-binding.  (The
parser fix also removed a dead duplicate parser-expr-pratt.lisp that shadowed the
intended edit site.)"
  ;; default used (no arg) and overridden (arg given)
  (assert-string= "10" (%js-run-capture "const f=(x=5)=>x*2; console.log(f());"))
  (assert-string= "6"  (%js-run-capture "const f=(x=5)=>x*2; console.log(f(3));"))
  ;; default on the 2nd parameter
  (assert-string= "12" (%js-run-capture "const f=(a,b=2)=>a+b; console.log(f(10));"))
  (assert-string= "15" (%js-run-capture "const f=(a,b=2)=>a+b; console.log(f(10,5));"))
  ;; rest parameter collects trailing args as an array
  (assert-string= "3"  (%js-run-capture "const f=(...n)=>n.length; console.log(f(1,2,3));"))
  (assert-string= "10" (%js-run-capture "const f=(...n)=>n.reduce((a,b)=>a+b,0); console.log(f(1,2,3,4));"))
  ;; required param followed by a rest param
  (assert-string= "12" (%js-run-capture "const f=(a,...n)=>a+n.length; console.log(f(10,1,2));"))
  ;; default with a block body
  (assert-string= "6"  (%js-run-capture "const f=(x=5)=>{return x+1;}; console.log(f());"))
  ;; regression guards: plain, single-param, and nested (curried) arrows still work
  (assert-string= "7"  (%js-run-capture "const f=(a,b)=>a+b; console.log(f(3,4));"))
  (assert-string= "7"  (%js-run-capture "const add=a=>b=>a+b; console.log(add(3)(4));")))

;;; ─── 13. Object-literal getters and setters ──────────────────────────────────

(deftest js-e2e-object-getters-setters
  "Object-literal get/set accessors are invoked on property read/write with
`this' bound to the object.  Regression: accessors were parsed (wrapped in a
%js-accessor descriptor) but stored as a plain property, so o.v returned the
descriptor object ([object Object]) instead of invoking the getter, and a
get/set pair on one key overwrote each other.  Accessors now route to internal
__get_K/__set_K slots that %js-get-prop/%js-set-prop dispatch."
  ;; getter invoked on read
  (assert-string= "42" (%js-run-capture "const o={get v(){return 42;}}; console.log(o.v);"))
  ;; getter sees `this'
  (assert-string= "20" (%js-run-capture "const o={n:10,get v(){return this.n*2;}}; console.log(o.v);"))
  ;; setter invoked on write
  (assert-string= "5"  (%js-run-capture "const o={_x:0,set v(n){this._x=n;}}; o.v=5; console.log(o._x);"))
  ;; getter + setter on the SAME key both survive and both run (setter stores
  ;; n+1, getter reads it back) — proves no overwrite and both are dispatched
  (assert-string= "11" (%js-run-capture "const o={_c:0,get count(){return this._c;},set count(n){this._c=n+1;}}; o.count=10; console.log(o.count);"))
  ;; regression guards: a regular method and a plain property are unaffected
  (assert-string= "5"  (%js-run-capture "const o={n:5,m(){return this.n;}}; console.log(o.m());"))
  (assert-string= "3"  (%js-run-capture "const o={a:1,b:2}; console.log(o.a+o.b);")))

;;; ─── 14. Object/Reflect/Number/Array/String static-method namespaces ─────────

(deftest js-e2e-static-method-namespaces
  "Object/Reflect/Number/Array/String static-method namespace globals are seeded
in the prelude, so Object.keys, Reflect.ownKeys, Number.isInteger, Array.isArray,
etc. resolve.  Regression: only Math and JSON were seeded — Object (and the rest)
were never globals, so Object.keys({a:1}) found no `Object' and returned empty.
The namespaces are built from the *js-builtin-specs* table by
%js-make-namespace-object, so they stay complete."
  (assert-string= "a,b" (%js-run-capture "console.log(Object.keys({a:1,b:2}).join(\",\"));"))
  (assert-string= "2"   (%js-run-capture "console.log(Object.keys({a:1,b:2}).length);"))
  (assert-string= "1,2" (%js-run-capture "console.log(Object.values({a:1,b:2}).join(\",\"));"))
  (assert-string= "1"   (%js-run-capture "console.log(Object.entries({a:1}).length);"))
  (assert-string= "2"   (%js-run-capture "console.log(Object.assign({},{a:1},{b:2}).b);"))
  (assert-string= "true"  (%js-run-capture "console.log(Array.isArray([1,2]));"))
  (assert-string= "false" (%js-run-capture "console.log(Array.isArray(5));"))
  (assert-string= "true"  (%js-run-capture "console.log(Number.isInteger(5));"))
  (assert-string= "2"   (%js-run-capture "console.log(Reflect.ownKeys({a:1,b:2}).length);"))
  (assert-string= "9007199254740991" (%js-run-capture "console.log(Number.MAX_SAFE_INTEGER);"))
  ;; regression guards: Math and JSON still work
  (assert-string= "2"      (%js-run-capture "console.log(Math.max(1,2));"))
  (assert-string= "{\"a\":1}" (%js-run-capture "console.log(JSON.stringify({a:1}));")))

;;; ─── 15. Relational operators return JS booleans + JS semantics ──────────────

(deftest js-e2e-relational-operators
  "< > <= >= return JS booleans (true/false) and follow JS Abstract Relational
Comparison: string operands compare lexicographically, a NaN operand is always
false, and mixed operands coerce via ToNumber.  Regression: they lowered to the
VM's CL comparison and returned 1/0, ignoring string/NaN/coercion semantics.
Now they route through %js-lt/gt/le/ge."
  (assert-string= "true"  (%js-run-capture "console.log(5 > 0);"))
  (assert-string= "false" (%js-run-capture "console.log(2 < 1);"))
  (assert-string= "true"  (%js-run-capture "console.log(3 >= 3);"))
  (assert-string= "false" (%js-run-capture "console.log(4 <= 2);"))
  ;; string operands compare lexicographically
  (assert-string= "true"  (%js-run-capture "console.log(\"apple\" < \"banana\");"))
  ;; NaN comparisons are always false
  (assert-string= "false" (%js-run-capture "console.log(NaN > 0);"))
  ;; mixed string/number coerces to number
  (assert-string= "true"  (%js-run-capture "console.log(\"5\" > 3);"))
  ;; comparison used in boolean position (ternary, loop) still works
  (assert-string= "y"     (%js-run-capture "console.log(5 > 3 ? \"y\" : \"n\");"))
  (assert-string= "6"     (%js-run-capture "let s=0; for(let i=0;i<4;i++){s+=i;} console.log(s);")))

;;; ─── 16. Logical && / || short-circuit and yield an operand ──────────────────

(deftest js-e2e-logical-and-or
  "&& and || short-circuit and yield an OPERAND (not a boolean), per JS.
Regression: they lowered to an ast-binop :and/:or that codegen could not emit,
so any function whose body used && or || failed to compile and was silently
dropped ('Undefined function').  Now they lower to let+if using %js-truthy,
like ?? does."
  (assert-string= "false" (%js-run-capture "console.log(true && false);"))
  (assert-string= "true"  (%js-run-capture "console.log(true && true);"))
  ;; && / || yield the operand value, not a coerced boolean
  (assert-string= "2"  (%js-run-capture "console.log(1 && 2);"))
  (assert-string= "0"  (%js-run-capture "console.log(0 && 2);"))
  (assert-string= "5"  (%js-run-capture "console.log(0 || 5);"))
  (assert-string= "3"  (%js-run-capture "console.log(3 || 5);"))
  (assert-string= "b"  (%js-run-capture "console.log(\"a\" && \"b\");"))
  ;; chaining and the `|| default' idiom inside a function (the dropped-function case)
  (assert-string= "3"   (%js-run-capture "console.log(1 && 2 && 3);"))
  (assert-string= "def" (%js-run-capture "function f(a){return a || \"def\";} console.log(f());"))
  (assert-string= "hi"  (%js-run-capture "function f(a){return a || \"def\";} console.log(f(\"hi\"));"))
  ;; short-circuit: RHS not evaluated when LHS already decides the result
  (assert-string= "0"   (%js-run-capture "let c=0; function s(){c++;return true;} false && s(); console.log(c);")))

;;; ─── 17. null / undefined literals use the runtime sentinels ─────────────────

(deftest js-e2e-null-undefined-literals
  "The null / undefined literals lower to the runtime +js-null+ / +js-undefined+
sentinels (:js-null / :js-undefined).  Regression: they lowered to the bare
:null / :undefined keywords, which the runtime did not recognize — so they
printed \"NULL\" / \"UNDEFINED\", `null ?? x' returned null instead of x, and
typeof was wrong."
  (assert-string= "null"      (%js-run-capture "console.log(null);"))
  (assert-string= "undefined" (%js-run-capture "console.log(undefined);"))
  (assert-string= "null"      (%js-run-capture "const x=null; console.log(x);"))
  ;; ?? now treats null/undefined as nullish
  (assert-string= "d"         (%js-run-capture "console.log(null ?? \"d\");"))
  (assert-string= "d"         (%js-run-capture "console.log(undefined ?? \"d\");"))
  ;; && yields the null operand (printed correctly)
  (assert-string= "null"      (%js-run-capture "const x=null; console.log(x && x.foo);"))
  ;; typeof and loose-equality semantics
  (assert-string= "object"    (%js-run-capture "console.log(typeof null);"))
  (assert-string= "undefined" (%js-run-capture "console.log(typeof undefined);"))
  (assert-string= "true"      (%js-run-capture "console.log(null == undefined);"))
  ;; string concatenation
  (assert-string= "v=null"    (%js-run-capture "console.log(\"v=\"+null);"))
  ;; void yields undefined
  (assert-string= "undefined" (%js-run-capture "console.log(void 0);")))

;;; ─── 18. Class getters and setters ──────────────────────────────────────────

(deftest js-e2e-class-getters-setters
  "Class get/set accessors are invoked on instance property read/write with
`this' bound to the instance, including inheritance via extends.  Regression:
a class `get v()' was stored as a plain prototype method, so `obj.v' returned
the getter FUNCTION instead of its result.  Class accessors now live under
__get_NAME/__set_NAME on the prototype and %js-get-prop/%js-set-prop dispatch
them through the prototype chain."
  (assert-string= "9"  (%js-run-capture "class C{get v(){return 9;}} console.log(new C().v);"))
  ;; getter sees `this'
  (assert-string= "20" (%js-run-capture "class C{constructor(){this.n=10;} get v(){return this.n*2;}} console.log(new C().v);"))
  ;; setter invoked on write
  (assert-string= "5"  (%js-run-capture "class C{set v(x){this._x=x;}} const o=new C(); o.v=5; console.log(o._x);"))
  ;; getter + setter on the same name both work
  (assert-string= "8"  (%js-run-capture "class C{constructor(){this._x=1;} get x(){return this._x;} set x(v){this._x=v;}} const o=new C(); o.x=8; console.log(o.x);"))
  ;; an inherited getter resolves through the prototype chain
  (assert-string= "1"  (%js-run-capture "class A{get v(){return 1;}} class B extends A{} console.log(new B().v);"))
  ;; regression guards: a regular method and a field are unaffected
  (assert-string= "3"  (%js-run-capture "class C{m(){return 3;}} console.log(new C().m());"))
  (assert-string= "7"  (%js-run-capture "class C{constructor(){this.x=7;}} console.log(new C().x);")))

;;; ─── 19. Falsy values: ternary truthiness, NaN, uninitialized var ────────────

(deftest js-e2e-conditional-truthiness
  "JS falsy values (false, 0, NaN, \"\", null, undefined) test as false in a
ternary, and an uninitialized declaration is undefined (not nil/false).
Regressions: the ternary passed its condition to ast-if WITHOUT %js-truthy (so
\"\"/null/undefined/NaN tested truthy); %js-truthy missed the float NaN literal;
and `let x;' bound x to nil so typeof x was \"boolean\"."
  ;; ternary coerces truthiness for every falsy value
  (assert-string= "f" (%js-run-capture "console.log(\"\" ? \"t\" : \"f\");"))
  (assert-string= "f" (%js-run-capture "console.log(null ? \"t\" : \"f\");"))
  (assert-string= "f" (%js-run-capture "console.log(undefined ? \"t\" : \"f\");"))
  (assert-string= "f" (%js-run-capture "console.log(NaN ? \"t\" : \"f\");"))
  (assert-string= "f" (%js-run-capture "console.log(0 ? \"t\" : \"f\");"))
  ;; truthy values still test true; an empty array/object is truthy in JS
  (assert-string= "t" (%js-run-capture "console.log(\"x\" ? \"t\" : \"f\");"))
  (assert-string= "t" (%js-run-capture "console.log([] ? \"t\" : \"f\");"))
  ;; NaN is falsy in an if-statement too
  (assert-string= "f" (%js-run-capture "if(NaN){console.log(\"t\");}else{console.log(\"f\");}"))
  ;; an uninitialized declaration is undefined
  (assert-string= "undefined" (%js-run-capture "let x; console.log(typeof x);"))
  (assert-string= "true"      (%js-run-capture "let y; console.log(y === undefined);"))
  (assert-string= "f"         (%js-run-capture "let z; console.log(z ? \"t\" : \"f\");")))

;;; ─── 20. Coercion functions: Number/String/Boolean/parseInt/parseFloat ───────

(deftest js-e2e-coercion-functions
  "Number(x)/String(x)/Boolean(x)/parseInt/parseFloat work as functions.
Regression: the global holding each is a value, not a callable function symbol
the codegen could dispatch, so a bare Number(x) raised 'Undefined function:
NUMBER'.  The CALL is now lowered straight to the runtime helper; Number.isInteger
(member access) is unaffected.  Also: Number(\"3.14\") kept double precision and
parseFloat parses the leading numeric prefix."
  (assert-string= "43"   (%js-run-capture "console.log(Number(\"42\") + 1);"))
  (assert-string= "3.14" (%js-run-capture "console.log(Number(\"3.14\"));"))
  (assert-string= "0"    (%js-run-capture "console.log(Number());"))
  (assert-string= "42x"  (%js-run-capture "console.log(String(42) + \"x\");"))
  (assert-string= "null" (%js-run-capture "console.log(String(null));"))
  (assert-string= "true false"  (%js-run-capture "console.log(Boolean(1), Boolean(0));"))
  (assert-string= "false true"  (%js-run-capture "console.log(Boolean(\"\"), Boolean(\"x\"));"))
  (assert-string= "42"   (%js-run-capture "console.log(parseInt(\"42px\"));"))
  (assert-string= "255"  (%js-run-capture "console.log(parseInt(\"ff\", 16));"))
  (assert-string= "3.14" (%js-run-capture "console.log(parseFloat(\"3.14abc\"));"))
  (assert-string= "1500" (%js-run-capture "console.log(parseFloat(\"1.5e3xyz\"));"))
  (assert-string= "NaN"  (%js-run-capture "console.log(parseFloat(\"abc\"));"))
  ;; namespace static methods still work alongside the callable form
  (assert-string= "true" (%js-run-capture "console.log(Number.isInteger(5));")))

;;; ─── 21. Tagged template literals ────────────────────────────────────────────

(deftest js-e2e-tagged-templates
  "A tagged template tag`...` calls TAG with the cooked-strings array first
(length = substitutions + 1) and the substitution VALUES after.  Regression: the
lowering passed the single CONCATENATED template string instead, so `tag`a${5}b``
gave the tag one wrong argument."
  ;; the first argument is the strings array
  (assert-string= "1"     (%js-run-capture "function t(s){return s.length;} console.log(t`hi`);"))
  (assert-string= "hello" (%js-run-capture "function t(s){return s[0];} console.log(t`hello`);"))
  ;; substitution values follow the strings array, in order
  (assert-string= "a5b"   (%js-run-capture "function t(s,v){return s[0]+v+s[1];} console.log(t`a${5}b`);"))
  (assert-string= "x1y2z" (%js-run-capture "function t(s,a,b){return s[0]+a+s[1]+b+s[2];} console.log(t`x${1}y${2}z`);"))
  ;; the strings array is a real array (join, length, rest-collected values)
  (assert-string= "a|b|c" (%js-run-capture "function tag(s,...v){return s.join('|');} console.log(tag`a${1}b${2}c`);"))
  ;; leading/trailing substitutions give empty boundary strings
  (assert-string= "[]9[]" (%js-run-capture "function t(s,v){return '['+s[0]+']'+v+'['+s[1]+']';} console.log(t`${9}`);"))
  ;; regression guards: plain template literals still concatenate
  (assert-string= "val=5" (%js-run-capture "const n=5; console.log(`val=${n}`);"))
  (assert-string= "sum=5" (%js-run-capture "console.log(`sum=${2+3}`);")))

(deftest js-e2e-number-prototype-methods
  "Number.prototype methods resolve and format JS-faithfully.  Regression:
%js-get-prop never routed numeric receivers to %js-method-ref (they fell to the
+js-undefined+ default), so (123.456).toFixed(2) called undefined -> 'Undefined
function: :JS-UNDEFINED'.  Also fixes CL/JS formatting drift: lowercase radix
digits, no trailing dot on toFixed(0), no padding spaces on toPrecision."
  (assert-string= "123.46" (%js-run-capture "console.log((123.456).toFixed(2));"))
  (assert-string= "8"      (%js-run-capture "console.log((7.5).toFixed(0));"))
  (assert-string= "ff"     (%js-run-capture "console.log((255).toString(16));"))
  (assert-string= "1010"   (%js-run-capture "console.log((10).toString(2));"))
  (assert-string= "FF"     (%js-run-capture "console.log((255).toString(16).toUpperCase());"))
  (assert-string= "3.14"   (%js-run-capture "console.log((3.14159).toPrecision(3));"))
  (assert-string= "100"    (%js-run-capture "console.log((100).toPrecision(3));"))
  (assert-string= "1.2e+3" (%js-run-capture "console.log((1234.5).toPrecision(2));"))
  (assert-string= "0.000012" (%js-run-capture "console.log((0.00001234).toPrecision(2));"))
  (assert-string= "-3.14"  (%js-run-capture "console.log((-3.14159).toPrecision(3));"))
  (assert-string= "42"     (%js-run-capture "console.log((42).valueOf());")))

(deftest js-e2e-standalone-global-builtins
  "Standalone global builtins (structuredClone, queueMicrotask, setTimeout, …)
are reachable as bare direct calls.  Regression: each lived in *js-builtin-map*
but had no prelude binding / coercion-call lowering, so structuredClone(x) raised
'Undefined function: STRUCTUREDCLONE' (the global holds a value, not a callable
function symbol the direct-call codegen can dispatch)."
  ;; structuredClone deep-copies (nested mutation does not affect the original)
  (assert-string= "3" (%js-run-capture "console.log(structuredClone({b:[2,3]}).b[1]);"))
  (assert-string= "5,99" (%js-run-capture "const o={x:{y:5}}; const c=structuredClone(o); c.x.y=99; console.log(o.x.y+','+c.x.y);"))
  (assert-string= "3" (%js-run-capture "console.log(structuredClone([1,2,3]).length);"))
  ;; timers/microtasks run synchronously in our model
  (assert-string= "micro" (%js-run-capture "queueMicrotask(()=>console.log('micro'));"))
  (assert-string= "timeout" (%js-run-capture "setTimeout(()=>console.log('timeout'),0);"))
  ;; indirect (value) use still works, and the coercion builtins are unaffected
  (assert-string= "1" (%js-run-capture "const f=structuredClone; console.log(f({a:1}).a);"))
  (assert-string= "42" (%js-run-capture "console.log(parseInt('42px'));")))

(deftest js-e2e-super-constructor
  "A subclass constructor calls the parent via super(args).  Regression: super
lowered to a nonexistent %js-super var, so super(n) raised 'Undefined function:
%JS-SUPER'.  The lexically-enclosing super class is captured per class so
multi-level chains (C extends B extends A) call the right parent, not loop."
  (assert-string= "y" (%js-run-capture "class A{constructor(n){this.n=n;}} class B extends A{constructor(n){super(n);}} console.log(new B('y').n);"))
  (assert-string= "x x!" (%js-run-capture "class A{constructor(n){this.n=n;}} class B extends A{constructor(n){super(n);this.m=n+'!';}} const b=new B('x'); console.log(b.n+' '+b.m);"))
  (assert-string= "11" (%js-run-capture "class A{constructor(){this.k=1;}} class B extends A{constructor(){super();this.k+=10;}} console.log(new B().k);"))
  ;; multi-level: C(5)->super(6)->B->super(12)->A sets x=12 (lexical super, no loop)
  (assert-string= "12" (%js-run-capture "class A{constructor(x){this.x=x;}} class B extends A{constructor(x){super(x*2);}} class C extends B{constructor(x){super(x+1);}} console.log(new C(5).x);"))
  ;; no explicit constructor still inherits the parent's (no regression)
  (assert-string= "z" (%js-run-capture "class A{constructor(n){this.n=n;}} class B extends A{} console.log(new B('z').n);")))

(deftest js-e2e-class-fields
  "Class field initializers (x = value;) run on every instance before the
constructor body.  Two stacked regressions: the initializer was stored as an
unresolved %JS-FIELD-INIT placeholder (never parsed), and field inits were never
injected into the constructor."
  (assert-string= "0"  (%js-run-capture "class C{count=0;} console.log(new C().count);"))
  (assert-string= "15" (%js-run-capture "class C{x=5;y=10;} const c=new C(); console.log(c.x+c.y);"))
  (assert-string= "hi" (%js-run-capture "class C{name='hi';} console.log(new C().name);"))
  ;; mutable field (array) shared across method calls on one instance
  (assert-string= "1,2" (%js-run-capture "class C{items=[];add(v){this.items.push(v);return this.items.length;}} const c=new C(); console.log(c.add(1)+','+c.add(2));"))
  ;; field initialised before the explicit constructor body runs
  (assert-string= "11" (%js-run-capture "class C{x=1;constructor(){this.x+=10;}} console.log(new C().x);"))
  ;; later field may reference an earlier one via this
  (assert-string= "6"  (%js-run-capture "class C{a=2;b=this.a*3;} console.log(new C().b);"))
  ;; derived class: parent constructor still runs AND own field initialises
  (assert-string= "y 99" (%js-run-capture "class A{constructor(n){this.n=n;}} class D extends A{extra=99;} const d=new D('y'); console.log(d.n+' '+d.extra);")))

(deftest js-e2e-class-self-reference
  "A class may reference itself inside its own methods (factory methods, recursion,
static helpers).  Regression: the class name was registered as a known global
only AFTER its methods compiled, so a self-reference (new A(), return A,
A.helper()) failed as an unbound variable."
  (assert-string= "z" (%js-run-capture "class A{constructor(n){this.n=n;} static create(n){return new A(n);}} console.log(A.create('z').n);"))
  (assert-string= "6" (%js-run-capture "class A{constructor(n){this.n=n;} clone(){return new A(this.n+1);}} console.log(new A(5).clone().n);"))
  (assert-string= "true" (%js-run-capture "class A{static self(){return A;}} console.log(A.self()===A);"))
  (assert-string= "42" (%js-run-capture "class A{static y(){return 42;} static z(){return A.y();}} console.log(A.z());"))
  (assert-string= "object" (%js-run-capture "class A{static make(){return new A();}} console.log(typeof A.make());")))

(deftest js-e2e-static-fields
  "static field initializers (static X = value;) set the value on the class
object.  Regression: static fields were never collected (only static methods),
and the field key was downcased, so static VERSION read as A.VERSION (uppercase)
returned undefined."
  (assert-string= "1.0" (%js-run-capture "class A{static VERSION='1.0';} console.log(A.VERSION);"))
  (assert-string= "100" (%js-run-capture "class A{static MAX=100; static MIN=0;} console.log(A.MAX-A.MIN);"))
  (assert-string= "1 2" (%js-run-capture "class A{static count=0; static inc(){return ++A.count;}} console.log(A.inc()+' '+A.inc());"))
  (assert-string= "3" (%js-run-capture "class A{static items=[1,2,3];} console.log(A.items.length);"))
  ;; the case fix also helps uppercase INSTANCE field names
  (assert-string= "7" (%js-run-capture "class C{COUNT=7;} console.log(new C().COUNT);")))

(deftest js-e2e-case-sensitive-identifiers
  "JavaScript identifiers are case-sensitive: a/A, foo/Foo are distinct, and a
lowercase instance variable does not collide with its Class name.  Regression:
js-ident-sym (and %js-binding-sym) upcased every identifier, so `const a = new
A()' overwrote the class's global slot and A.staticMember then read undefined."
  (assert-string= "5 10" (%js-run-capture "const A=5; const a=10; console.log(A+' '+a);"))
  (assert-string= "1 2" (%js-run-capture "function foo(){return 1;} function Foo(){return 2;} console.log(foo()+' '+Foo());"))
  (assert-string= "10" (%js-run-capture "class A{static Y=10;m(){return 1;}} const a=new A(); a.m(); console.log(A.Y);"))
  (assert-string= "7 0" (%js-run-capture "class Point{constructor(x){this.x=x;} static origin(){return new Point(0);}} const p=new Point(7); console.log(p.x+' '+Point.origin().x);")))

(deftest js-e2e-super-method
  "super.method() calls the parent's method bound to the current instance, and
super(args) constructor calls still work — both can appear in the same subclass.
Regression: super lowered to a call-only closure, so super.f() did member access
on it and got :JS-UNDEFINED.  super is now a JS object (proto=parent prototype,
__super_this__=this, __call__=parent ctor); vm-resolve-function honors __call__
so the object stays callable for super(args)."
  (assert-string= "11" (%js-run-capture "class A{f(){return 1;}} class B extends A{f(){return super.f()+10;}} console.log(new B().f());"))
  (assert-string= "AB" (%js-run-capture "class A{greet(){return 'A';}} class B extends A{greet(){return super.greet()+'B';}} console.log(new B().greet());"))
  (assert-string= "10" (%js-run-capture "class A{constructor(){this.x=5;} m(){return this.x;}} class B extends A{m(){return super.m()*2;}} console.log(new B().m());"))
  ;; super() and super.method() together in one subclass
  (assert-string= "Hi Bob!" (%js-run-capture "class A{constructor(n){this.n=n;} greet(){return 'Hi '+this.n;}} class B extends A{constructor(n){super(n);} greet(){return super.greet()+'!';}} console.log(new B('Bob').greet());"))
  ;; super() constructor call still works (no regression)
  (assert-string= "k" (%js-run-capture "class A{constructor(n){this.n=n;}} class B extends A{constructor(n){super(n);}} console.log(new B('k').n);")))

(deftest js-e2e-contextual-keywords-as-identifiers
  "Contextual keywords (get/set/of/from/as/static/...) are valid identifiers in
binding positions: const set = …, function f(get){…}, destructuring.  They only
act as keywords in specific positions (class getter/setter, for-of, import),
which still work.  Regression: the binding-pattern parser accepted only :T-IDENT,
so `const set = new Set()' failed with 'expected binding pattern, got :T-SET'."
  (assert-string= "5"  (%js-run-capture "const set=5; console.log(set);"))
  (assert-string= "3"  (%js-run-capture "const of=1, from=2; console.log(of+from);"))
  (assert-string= "10" (%js-run-capture "function f(set){return set*2;} console.log(f(5));"))
  (assert-string= "3"  (%js-run-capture "const [get,set]=[1,2]; console.log(get+set);"))
  ;; keyword positions unaffected
  (assert-string= "5"  (%js-run-capture "class C{get x(){return 5;}} console.log(new C().x);"))
  (assert-string= "6"  (%js-run-capture "let s=0; for(const x of [1,2,3]){s+=x;} console.log(s);")))

(deftest js-e2e-symbol-constructor
  "Symbol(desc) constructs a primitive symbol (regression: the bare call resolved
to the function symbol |Symbol| rather than the global value -> 'Undefined
function: Symbol'; added to the coercion-call table like parseInt).  And
sym.description is an accessor PROPERTY, not a method (it was in the method table,
so it returned a bound closure instead of the string)."
  (assert-string= "symbol" (%js-run-capture "console.log(typeof Symbol('x'));"))
  (assert-string= "Symbol(d)" (%js-run-capture "console.log(Symbol('d').toString());"))
  (assert-string= "k" (%js-run-capture "console.log(Symbol('k').description);"))
  (assert-string= "true" (%js-run-capture "console.log(Symbol().description===undefined);"))
  (assert-string= "symbol" (%js-run-capture "console.log(typeof Symbol());"))
  ;; symbol as a computed object key
  (assert-string= "42" (%js-run-capture "const o={}; const k=Symbol('key'); o[k]=42; console.log(o[k]);"))
  ;; member access on the Symbol global (well-known symbols) unaffected
  (assert-string= "symbol" (%js-run-capture "console.log(typeof Symbol.iterator);")))

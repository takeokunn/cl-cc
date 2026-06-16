;;;; packages/javascript/tests/js-e2e-core-tests.lisp — Core JS execution tests
;;;;
;;;; Shared test helpers (loaded first, so subsequent files can use them):
;;;;   %js-run-capture SOURCE  → string of captured console.log output
;;;;   deftest-js-run NAME DOC (EXPECTED SOURCE) ...
;;;;
;;;; Covers: basic programs, closures, function params, destructuring,
;;;;         JSON/Math globals, try/catch, object spread, division, class this.

(in-package :cl-cc/test)

(defsuite cl-cc-javascript-e2e-serial-suite
  :description "JavaScript end-to-end execution tests"
  :parent cl-cc-e2e-suite
  :parallel nil)

(in-suite cl-cc-javascript-e2e-serial-suite)

;;; ─── Shared helpers (available to all js-e2e-* files via serial load) ────────

(defun %js-run-capture (source)
  "Compile JavaScript SOURCE to the VM, seed the JS runtime globals, run it, and
return everything console.log printed (trailing newline trimmed). Exercises the
real execution path, unlike the parse-only %js-e2e-parse checks."
  (let* ((result  (cl-cc:compile-string source :target :vm :language :javascript))
         (program (cl-cc/compile:compilation-result-program result))
         (out     (make-string-output-stream))
         (state   (cl-cc/vm:make-vm-state :output-stream out)))
    (cl-cc/pipeline:seed-js-runtime-globals state)
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

(defparameter *js-e2e-batch-sentinel-prefix* "__CL_CC_JS_E2E_CASE_")

(defun %js-e2e-batch-sentinel (index)
  (format nil "~a~d__" *js-e2e-batch-sentinel-prefix* index))

(defun %js-source-ends-with-newline-p (source)
  (and (plusp (length source))
       (char= (char source (1- (length source))) #\Newline)))

(defun %js-lines-to-string (lines)
  (with-output-to-string (out)
    (loop for line in lines
          for first = t then nil
          do (unless first (terpri out))
             (write-string line out))))

(defun %js-run-capture-isolated-cases (sources)
  "Run independent JS SOURCES through one compile/run cycle.
Each case is wrapped in a function IIFE so lexical declarations in one case do
not collide with the next. Use this only for tests whose observable behavior is
independent of top-level var/function/global binding semantics."
  (let* ((sentinels (loop for index below (length sources)
                          collect (%js-e2e-batch-sentinel index)))
         (batched-source
           (with-output-to-string (out)
             (loop for source in sources
                   for sentinel in sentinels
                   do (write-string "(function(){" out)
                      (terpri out)
                      (write-string source out)
                      (unless (%js-source-ends-with-newline-p source)
                        (terpri out))
                      (write-string "})();" out)
                      (terpri out)
                      (format out "console.log(\"~a\");~%" sentinel)))))
    (let ((expected sentinels)
          (current '())
          (chunks '()))
      (dolist (line (uiop:split-string (%js-run-capture batched-source)
                                        :separator '(#\Newline)))
        (if (and expected (string= line (first expected)))
            (progn
              (push (%js-lines-to-string (nreverse current)) chunks)
              (setf current '()
                    expected (rest expected)))
            (unless (and (null expected) (string= line ""))
              (push line current))))
      (unless (and (null expected) (null current))
        (error "Batched JS E2E output did not contain the expected sentinels: ~S"
               expected))
      (nreverse chunks))))

(defmacro deftest-js-run-isolated-batch (name description &rest cases)
  "Like DEFTEST-JS-RUN, but compiles independent cases together.
Cases execute in separate function scopes and are split by sentinel output."
  `(deftest ,name ,description
     (let ((outputs (%js-run-capture-isolated-cases
                     (list ,@(mapcar #'second cases)))))
       ,@(loop for case in cases
               for index from 0
               collect `(assert-string= ,(first case) (nth ,index outputs))))))

;;; ─── Basic programs ──────────────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-runs-basic-programs
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

;;; ─── Function parameters ─────────────────────────────────────────────────────

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

;;; ─── Destructuring ───────────────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-destructuring-and-sequential-decls
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

(deftest-js-run-isolated-batch js-e2e-destructuring-rest
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

(deftest-js-run-isolated-batch js-e2e-parameter-destructuring
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

(deftest-js-run-isolated-batch js-e2e-nested-destructuring
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

(deftest-js-run-isolated-batch js-e2e-arrow-destructuring
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

;;; ─── JSON, Math, try/catch, spread, float formatting ─────────────────────────

(deftest-js-run-isolated-batch js-e2e-json-and-math-globals
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

(deftest-js-run-isolated-batch js-e2e-try-catch-finally
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

(deftest-js-run-isolated-batch js-e2e-spread-arrays-and-calls
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

;;; ─── Class this binding ──────────────────────────────────────────────────────

(deftest-js-run js-e2e-class-this-binding
  "Class constructors and methods bind `this' to the instance — in both the host
special and the VM-global %js-this — so this.x reads/writes the receiver
(regression: this compiled to vm-get-global %js-this which was never set, so any
class using `this' failed with 'Unbound global variable: %JS-THIS')."
  ("7"   "class C{constructor(x){this.x=x;} get(){return this.x;}} console.log(new C(7).get());")
  ("Bob" "class P{constructor(n){this.name=n;}} let p=new P(\"Bob\"); console.log(p.name);")
  ("2"   "class Ctr{constructor(){this.n=0;} inc(){this.n++; return this.n;}} let c=new Ctr(); c.inc(); console.log(c.inc());")
  ("10"  "class B{v(){return 10;}} class A{constructor(){this.b=new B();} go(){return this.b.v();}} console.log(new A().go());"))

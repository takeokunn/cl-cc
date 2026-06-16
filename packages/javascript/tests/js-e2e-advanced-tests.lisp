;;;; packages/javascript/tests/js-e2e-advanced-tests.lisp — JS advanced execution tests
;;;;
;;;; Optional chaining, named function expressions, typeof, arrow params,
;;;; getters/setters, static namespaces, operators, coercion, class features,
;;;; for-in/of loops, generators, Map/Set/WeakMap/WeakSet.
;;;;
;;;; Depends on: js-e2e-core-tests.lisp (%js-run-capture, deftest-js-run).

(in-package :cl-cc/test)
(in-suite cl-cc-javascript-e2e-serial-suite)

;;; ─── Optional chaining + nullish coalescing ──────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-optional-chaining-execution
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

;;; ─── Named function expressions + typeof ─────────────────────────────────────

(deftest-js-run js-e2e-named-function-expression-recursion
  "Named function expressions can self-recurse by name; name visible only inside body."
  ("120"   "const f=function fac(n){return n<=1?1:n*fac(n-1);}; console.log(f(5));")
  ("55"    "const f=function fib(n){return n<2?n:fib(n-1)+fib(n-2);}; console.log(f(10));")
  ("5"     "const f=function(x){return x+1;}; console.log(f(4));")
  ("6"     "const f=function g(x){return x*2;}; console.log(f(3));")
  ("6,2,4" "console.log([3,1,2].map(function dbl(x){return x*2;}).join(\",\"));"))

(deftest-js-run-isolated-batch js-e2e-typeof-function
  "typeof returns 'function' for vm-closure-objects; non-function types unchanged."
  ("function"           "const f=function(x){return x;}; console.log(typeof f);")
  ("function"           "const g=x=>x; console.log(typeof g);")
  ("function"           "function h(){return 1;} console.log(typeof h);")
  ("function"           "const o={m(){return 1;}}; console.log(typeof o.m);")
  ("yes"                "const h=function(){}; console.log(typeof h===\"function\"?\"yes\":\"no\");")
  ("object"             "console.log(typeof {a:1});")
  ("object"             "console.log(typeof [1,2]);")
  ("string number boolean" "console.log(typeof \"x\", typeof 5, typeof true);"))

;;; ─── Arrow function params ───────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-arrow-default-and-rest-params
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

;;; ─── Object-literal getters and setters ──────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-object-getters-setters
  "Object-literal get/set accessors invoked on property read/write; both survive on same key."
  ("42" "const o={get v(){return 42;}}; console.log(o.v);")
  ("20" "const o={n:10,get v(){return this.n*2;}}; console.log(o.v);")
  ("5"  "const o={_x:0,set v(n){this._x=n;}}; o.v=5; console.log(o._x);")
  ("11" "const o={_c:0,get count(){return this._c;},set count(n){this._c=n+1;}}; o.count=10; console.log(o.count);")
  ("5"  "const o={n:5,m(){return this.n;}}; console.log(o.m());")
  ("3"  "const o={a:1,b:2}; console.log(o.a+o.b);"))

;;; ─── Static-method namespace globals ─────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-static-method-namespaces
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

;;; ─── Relational operators ────────────────────────────────────────────────────

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

;;; ─── Logical &&/|| ──────────────────────────────────────────────────────────

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

;;; ─── null / undefined literals ───────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-null-undefined-literals
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

;;; ─── Class getters and setters ───────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-class-getters-setters
  "Class get/set accessors are invoked on property read/write; inheritance resolves through prototype chain."
  ("9"  "class C{get v(){return 9;}} console.log(new C().v);")
  ("20" "class C{constructor(){this.n=10;} get v(){return this.n*2;}} console.log(new C().v);")
  ("5"  "class C{set v(x){this._x=x;}} const o=new C(); o.v=5; console.log(o._x);")
  ("8"  "class C{constructor(){this._x=1;} get x(){return this._x;} set x(v){this._x=v;}} const o=new C(); o.x=8; console.log(o.x);")
  ("1"  "class A{get v(){return 1;}} class B extends A{} console.log(new B().v);")
  ("3"  "class C{m(){return 3;}} console.log(new C().m());")
  ("7"  "class C{constructor(){this.x=7;}} console.log(new C().x);"))

;;; ─── Conditional truthiness ──────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-conditional-truthiness
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

;;; ─── Coercion functions ──────────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-coercion-functions
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

;;; ─── Tagged templates + Number.prototype ─────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-tagged-templates
  "Tagged template calls TAG(strings-array, ...subs); plain templates concatenate."
  ("1"     "function t(s){return s.length;} console.log(t`hi`);")
  ("hello" "function t(s){return s[0];} console.log(t`hello`);")
  ("a5b"   "function t(s,v){return s[0]+v+s[1];} console.log(t`a${5}b`);")
  ("x1y2z" "function t(s,a,b){return s[0]+a+s[1]+b+s[2];} console.log(t`x${1}y${2}z`);")
  ("a|b|c" "function tag(s,...v){return s.join('|');} console.log(tag`a${1}b${2}c`);")
  ("[]9[]" "function t(s,v){return '['+s[0]+']'+v+'['+s[1]+']';} console.log(t`${9}`);")
  ("val=5" "const n=5; console.log(`val=${n}`);")
  ("sum=5" "console.log(`sum=${2+3}`);"))

(deftest-js-run-isolated-batch js-e2e-number-prototype-methods
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

;;; ─── Standalone global builtins ──────────────────────────────────────────────

(deftest-js-run js-e2e-standalone-global-builtins
  "structuredClone/queueMicrotask are reachable as bare direct calls."
  ("3"       "console.log(structuredClone({b:[2,3]}).b[1]);")
  ("5,99"    "const o={x:{y:5}}; const c=structuredClone(o); c.x.y=99; console.log(o.x.y+','+c.x.y);")
  ("3"       "console.log(structuredClone([1,2,3]).length);")
  ("micro"   "queueMicrotask(()=>console.log('micro'));")
  ("1"       "const f=structuredClone; console.log(f({a:1}).a);")
  ("42"      "console.log(parseInt('42px'));"))

;;; ─── Class features ──────────────────────────────────────────────────────────

(deftest-js-run js-e2e-super-constructor
  "super(args) calls the parent constructor; multi-level chains call the right parent."
  ("y"    "class A{constructor(n){this.n=n;}} class B extends A{constructor(n){super(n);}} console.log(new B('y').n);")
  ("x x!" "class A{constructor(n){this.n=n;}} class B extends A{constructor(n){super(n);this.m=n+'!';}} const b=new B('x'); console.log(b.n+' '+b.m);")
  ("11"   "class A{constructor(){this.k=1;}} class B extends A{constructor(){super();this.k+=10;}} console.log(new B().k);")
  ("12"   "class A{constructor(x){this.x=x;}} class B extends A{constructor(x){super(x*2);}} class C extends B{constructor(x){super(x+1);}} console.log(new C(5).x);")
  ("z"    "class A{constructor(n){this.n=n;}} class B extends A{} console.log(new B('z').n);"))

(deftest-js-run-isolated-batch js-e2e-class-fields
  "Class field initializers run per-instance before constructor body; fields can reference each other."
  ("0"    "class C{count=0;} console.log(new C().count);")
  ("15"   "class C{x=5;y=10;} const c=new C(); console.log(c.x+c.y);")
  ("hi"   "class C{name='hi';} console.log(new C().name);")
  ("1,2"  "class C{items=[];add(v){this.items.push(v);return this.items.length;}} const c=new C(); console.log(c.add(1)+','+c.add(2));")
  ("11"   "class C{x=1;constructor(){this.x+=10;}} console.log(new C().x);")
  ("6"    "class C{a=2;b=this.a*3;} console.log(new C().b);")
  ("y 99" "class A{constructor(n){this.n=n;}} class D extends A{extra=99;} const d=new D('y'); console.log(d.n+' '+d.extra);"))

(deftest-js-run-isolated-batch js-e2e-class-self-reference
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

(deftest-js-run-isolated-batch js-e2e-super-method
  "super.method() calls the parent method; super() constructor calls still work in same class."
  ("11"      "class A{f(){return 1;}} class B extends A{f(){return super.f()+10;}} console.log(new B().f());")
  ("AB"      "class A{greet(){return 'A';}} class B extends A{greet(){return super.greet()+'B';}} console.log(new B().greet());")
  ("10"      "class A{constructor(){this.x=5;} m(){return this.x;}} class B extends A{m(){return super.m()*2;}} console.log(new B().m());")
  ("Hi Bob!" "class A{constructor(n){this.n=n;} greet(){return 'Hi '+this.n;}} class B extends A{constructor(n){super(n);} greet(){return super.greet()+'!';}} console.log(new B('Bob').greet());")
  ("k"       "class A{constructor(n){this.n=n;}} class B extends A{constructor(n){super(n);}} console.log(new B('k').n);"))

;;; ─── Contextual keywords, Symbols, global functions ─────────────────────────

(deftest-js-run js-e2e-contextual-keywords-as-identifiers
  "Contextual keywords (get/set/of/from/static) are valid in binding positions."
  ("5"  "const set=5; console.log(set);")
  ("3"  "const of=1, from=2; console.log(of+from);")
  ("10" "function f(set){return set*2;} console.log(f(5));")
  ("3"  "const [get,set]=[1,2]; console.log(get+set);")
  ("5"  "class C{get x(){return 5;}} console.log(new C().x);")
  ("6"  "let s=0; for(const x of [1,2,3]){s+=x;} console.log(s);"))

(deftest-js-run-isolated-batch js-e2e-symbol-constructor
  "Symbol(desc) is callable; .description is an accessor property; computed keys work."
  ("symbol"    "console.log(typeof Symbol('x'));")
  ("Symbol(d)" "console.log(Symbol('d').toString());")
  ("k"         "console.log(Symbol('k').description);")
  ("true"      "console.log(Symbol().description===undefined);")
  ("symbol"    "console.log(typeof Symbol());")
  ("42"        "const o={}; const k=Symbol('key'); o[k]=42; console.log(o[k]);")
  ("symbol"    "console.log(typeof Symbol.iterator);"))

(deftest-js-run-isolated-batch js-e2e-global-function-calls
  "btoa/atob/encode*/decode*/isNaN/isFinite are callable as bare global calls."
  ("aGk="      "console.log(btoa('hi'));")
  ("hi"        "console.log(atob('aGk='));")
  ("a%20b"     "console.log(encodeURIComponent('a b'));")
  ("a b"       "console.log(decodeURIComponent('a%20b'));")
  ("a%20b/c"   "console.log(encodeURI('a b/c'));")
  ("false true" "console.log(isNaN(5)+' '+isNaN(NaN));")
  ("true false" "console.log(isFinite(5)+' '+isFinite(Infinity));"))

;;; ─── for-in / for-of loops ───────────────────────────────────────────────────

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

;;; ─── Generators + Map/Set + Array.from ──────────────────────────────────────

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

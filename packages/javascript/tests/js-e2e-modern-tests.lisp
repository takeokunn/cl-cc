;;;; packages/javascript/tests/js-e2e-modern-tests.lisp — ES2022+ execution tests
;;;;
;;;; Private class fields, Promise chaining, WeakMap/WeakSet, Object static
;;;; methods, ES2021 strings, ES2023 arrays, ES2025 Set/Iterator, ES2025/2026
;;;; static built-ins (Math.sumPrecise, Error.isError, RegExp.escape, Map.groupBy),
;;;; AbortController/AbortSignal, crypto.
;;;;
;;;; Depends on: js-e2e-core-tests.lisp (%js-run-capture, deftest-js-run).

(in-package :cl-cc/test)
(in-suite cl-cc-javascript-e2e-serial-suite)

;;; ─── ES2022: Private class fields ────────────────────────────────────────────

(deftest js-e2e-private-class-fields
  "Private class fields (#name) are stored in the instance's __private__ slot
and are inaccessible from outside the class.  ES2022 syntax, inherited by ES2026."
  (assert-string= "42"
    (%js-run-capture
     "class C{#x; constructor(v){this.#x=v;} get(){return this.#x;}}
console.log(new C(42).get());"))
  (assert-string= "0"
    (%js-run-capture
     "class C{#count=0; inc(){this.#count++;} val(){return this.#count;}}
const c=new C(); console.log(c.val());"))
  (assert-string= "10"
    (%js-run-capture
     "class C{#v; constructor(v){this.#v=v;} double(){return this.#v*2;}}
console.log(new C(5).double());"))
  (assert-string= "7"
    (%js-run-capture
     "class P{#n; constructor(n){this.#n=n;} get(){return this.#n;}}
class C extends P{constructor(n){super(n);} doubled(){return this.get()*2;}}
console.log(new C(7).get());")))

;;; ─── Promise chaining ────────────────────────────────────────────────────────

(deftest-js-run js-e2e-promise-chaining
  "Promise.then/.catch chain in the synchronous model; Promise.all/async work."
  ("6"          "Promise.resolve(3).then(x=>x*2).then(x=>console.log(x));")
  ("caught: oops" "Promise.reject('oops').catch(e=>console.log('caught: '+e));")
  ("1,2,3"      "Promise.all([Promise.resolve(1),Promise.resolve(2),Promise.resolve(3)]).then(vs=>console.log(vs.join(',')));")
  ("done"       "async function f(){return 'done';} f().then(v=>console.log(v));")
  ("10,21"      "Array.fromAsync([Promise.resolve(10),Promise.resolve(20)],(x,i)=>Promise.resolve(x+i)).then(vs=>console.log(vs.join(',')));"))

(deftest-js-run js-e2e-promise-static-builtins
  "Promise.try and Promise.withResolvers compile through static built-in dispatch."
  ("42" "Promise.try(()=>21*2).then(x=>console.log(x));")
  ("7"  "const r=Promise.withResolvers(); r.resolve(7); r.promise.then(x=>console.log(x));"))

(deftest-js-run js-e2e-import-meta
  "import.meta exposes host metadata and a resolve function."
  ("string function" "console.log((typeof import.meta.url)+' '+(typeof import.meta.resolve));")
  ("./dep.js"       "console.log(import.meta.resolve('./dep.js'));"))

(deftest-js-run js-e2e-dynamic-import-namespace
  "Dynamic import resolves to a host module namespace object."
  ("./dep.js ./dep.js function" "import('./dep.js').then(m=>console.log(m.__moduleName__+' '+m.url+' '+(typeof m.resolve)));")
  ("./child.js"                 "import('./dep.js').then(m=>console.log(m.resolve('./child.js')));"))

;;; ─── WeakMap and WeakSet ─────────────────────────────────────────────────────

(deftest-js-run js-e2e-weakmap-weakset
  "WeakMap and WeakSet: get/set/has/delete work (behave like Map/Set in CL model)."
  ("42 true false" "const wm=new WeakMap(); const k={}; wm.set(k,42); console.log(wm.get(k)+' '+wm.has(k)+' '+wm.has({}));")
  ("true false"    "const ws=new WeakSet(); const o={}; ws.add(o); console.log(ws.has(o)+' '+ws.has({}));")
  ("true false"    "const wm=new WeakMap(); const k={}; wm.set(k,1); const had=wm.has(k); wm.delete(k); console.log(had+' '+wm.has(k));"))

;;; ─── Object static methods ───────────────────────────────────────────────────

(deftest-js-run js-e2e-object-static-methods
  "Object.keys/values/entries/assign/fromEntries work on plain objects."
  ("a,b"   "console.log(Object.keys({a:1,b:2}).join(','));")
  ("1,2"   "console.log(Object.values({a:1,b:2}).join(','));")
  ("a=1,b=2" "console.log(Object.entries({a:1,b:2}).map(([k,v])=>k+'='+v).join(','));")
  ("3"     "const t={}; Object.assign(t,{x:1},{y:2}); console.log(t.x+t.y);")
  ("10"    "const o=Object.fromEntries([['x',10]]); console.log(o.x);"))

;;; ─── ES2021 string methods ───────────────────────────────────────────────────

(deftest-js-run js-e2e-string-methods-es2021
  "String methods ES2021+: replaceAll, at, trimStart, trimEnd."
  ("a-b-c" "console.log('a.b.c'.replaceAll('.', '-'));")
  ("c"     "console.log('abc'.at(-1));")
  ("a"     "console.log('abc'.at(0));")
  ("hi  "  "console.log('  hi  '.trimStart());")
  ("  hi"  "console.log('  hi  '.trimEnd());"))

;;; ─── ES2023 array methods ────────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-array-methods-es2023
  "Array ES2023: non-mutating array copy helpers, at, findLast/findLastIndex."
  ("3,2,1" "const a=[1,2,3]; console.log(a.toReversed().join(','));")
  ("1,2,3" "const a=[1,2,3]; a.toReversed(); console.log(a.join(','));")
  ("1,2,3" "console.log([3,1,2].toSorted().join(','));")
  ("3"     "console.log([1,2,3].at(-1));")
  ("4"     "console.log([1,2,3,4].findLast(x=>x%2===0));")
  ("3"     "console.log([1,2,3,4].findLastIndex(x=>x%2===0));")
  ("10,99,30" "console.log([10,20,30].with(1,99).join(','));")
  ("10,20,30" "const a=[10,20,30]; a.with(1,99); console.log(a.join(','));")
  ("1,9,9,4" "console.log([1,2,3,4].toSpliced(1,2,9,9).join(','));")
  ("1,2,3,4" "const a=[1,2,3,4]; a.toSpliced(1,2,9,9); console.log(a.join(','));")
  ("0:a|1:b" "console.log([...['a','b'].entries()].map(x=>x.join(':')).join('|'));")
  ("0,1,2" "console.log([...['a','b','c'].keys()].join(','));"))

;;; ─── ES2025 Set composition methods ─────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-set-methods-es2025
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

(deftest-js-run-isolated-batch js-e2e-iterator-helpers-es2025
  "ES2025 Iterator.prototype: map/filter/take/drop/flatMap/reduce/toArray/forEach/some/every/find."
  ("1,2,3"    "console.log([1,2,3].values().toArray().join(','));")
  ("2,3,4"    "console.log(Iterator.from([1,2,3]).map(x=>x+1).toArray().join(','));")
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
  "ES2025 RegExp.escape: escapes strings for literal use in RegExp patterns."
  ("\\x48ello\\.World" "console.log(RegExp.escape('Hello.World'));")
  ("\\x68ello"         "console.log(RegExp.escape('hello'));")
  ("\\x77hat\\?"       "console.log(RegExp.escape('what?'));")
  ("\\x66oo\\x2Dbar"   "console.log(RegExp.escape('foo-bar'));"))

(deftest-js-run js-e2e-es2024-map-group-by
  "ES2024 Map.groupBy: groups iterable elements by a key function into a Map."
  ("2,4,6" "const g=Map.groupBy([1,2,3,4,5,6],x=>x%2===0?'even':'odd'); console.log(g.get('even').join(','));")
  ("1,3,5" "const g=Map.groupBy([1,2,3,4,5,6],x=>x%2===0?'even':'odd'); console.log(g.get('odd').join(','));")
  ("none"  "const g=Map.groupBy([1,3,5],x=>'odd'); const v=g.get('even'); console.log(v===undefined?'none':v.join(','));"))

(deftest-js-run js-e2e-es2024-object-group-by
  "ES2024 Object.groupBy: groups iterable elements into object properties."
  ("2,4,6" "const g=Object.groupBy([1,2,3,4,5,6],x=>x%2===0?'even':'odd'); console.log(g.even.join(','));")
  ("1,3,5" "const g=Object.groupBy([1,2,3,4,5,6],x=>x%2===0?'even':'odd'); console.log(g.odd.join(','));")
  ("none"  "const g=Object.groupBy([1,3,5],x=>'odd'); console.log(g.even===undefined?'none':g.even.join(','));"))

(deftest-js-run js-e2e-es2024-array-grouping
  "ES2024 Array.prototype.group/groupToMap: groups array values through prototype dispatch."
  ("1,3,5|2,4,6" "const g=[1,2,3,4,5,6].group(x=>x%2===0?'even':'odd'); console.log(g.odd.join(',')+'|'+g.even.join(','));")
  ("1,3,5" "const g=[1,2,3,4,5,6].groupToMap(x=>x%2===0?'even':'odd'); console.log(g.get('odd').join(','));")
  ("none" "const g=[2,4,6].groupToMap(x=>x%2===0?'even':'odd'); const v=g.get('odd'); console.log(v===undefined?'none':v.join(','));"))

(deftest-js-run js-e2e-abort-controller
  "AbortController/AbortSignal globals work through the prelude."
  ("true why 1"
   "const c=new AbortController(); let n=0; c.signal.addEventListener('abort',()=>n++); c.abort('why'); c.abort('again'); console.log(c.signal.aborted+' '+c.signal.reason+' '+n);")
  ("true stop"
   "const s=AbortSignal.abort('stop'); console.log(s.aborted+' '+s.reason);")
  ("caught"
   "const s=AbortSignal.abort('caught'); try{s.throwIfAborted();}catch(e){console.log(e);}"))

(deftest-js-run js-e2e-crypto
  "crypto globals work through the prelude."
  ("true 8"
   "const bytes=new Uint8Array(8); const ret=crypto.getRandomValues(bytes); console.log((ret===bytes)+' '+bytes.length);")
  ("true"
   "const u=crypto.randomUUID(); const ok=u.length===36&&u===u.toLowerCase()&&u[8]==='-'&&u[13]==='-'&&u[18]==='-'&&u[23]==='-'&&u[14]==='4'&&'89ab'.indexOf(u[19])>=0; console.log(ok);"))

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defun %php-run-capture (source)
  "Compile PHP SOURCE to VM and run it, returning everything it echoed as a
string. compile-string with :language :php registers the PHP host bridges, so a
fresh VM state runs the program end-to-end."
  (let* ((result  (cl-cc:compile-string source :target :vm :language :php))
         (program (cl-cc/compile:compilation-result-program result))
         (out     (make-string-output-stream)))
    (cl-cc/vm:run-compiled program :output-stream out)
    ;; Trim a trailing newline the VM appends when flushing program output.
    (string-right-trim '(#\Newline) (get-output-stream-string out))))

(deftest php-e2e-constructor
  "new C(args) runs __construct with the instance as $this and the args
(regression: the constructor never ran — args were passed as :ARGn CLOS initargs
the class rejected). A class with no __construct is unaffected."
  (assert-string= "made" (%php-run-capture "<?php class C{ function __construct(){ echo 'made'; } } $o=new C();"))
  (assert-string= "9"    (%php-run-capture "<?php class C{ public $x; function __construct($v){ $this->x=$v; } } $o=new C(9); echo $o->x;"))
  (assert-string= "Bob:30" (%php-run-capture "<?php class P{ public $n; public $a; function __construct($n,$a){ $this->n=$n; $this->a=$a; } } $p=new P('Bob',30); echo $p->n.':'.$p->a;"))
  ;; constructor + a method that uses the constructed state
  (assert-string= "10"   (%php-run-capture "<?php class C{ public $x; function __construct($v){ $this->x=$v; } function dbl(){ return $this->x*2; } } $o=new C(5); echo $o->dbl();"))
  ;; a class with no constructor still constructs and uses property defaults
  (assert-string= "5"    (%php-run-capture "<?php class C{ public $x=5; } $o=new C(); echo $o->x;")))

(deftest php-e2e-instance-method-this
  "Instance methods bind $this to the receiver (regression: $this was an unbound
variable, so $this->x inside a method produced nothing). Implemented by giving
each instance method an implicit $this first parameter that the call site
($o->m(args)) fills with the receiver."
  (assert-string= "3"  (%php-run-capture "<?php class C{ public $x=3; function g(){ return $this->x; } } $o=new C(); echo $o->g();"))
  (assert-string= "15" (%php-run-capture "<?php class C{ public $b=10; function add($a){ return $this->b+$a; } } $o=new C(); echo $o->add(5);"))
  (assert-string= "7"  (%php-run-capture "<?php class C{ public $n=0; function setN($v){ $this->n=$v; } } $o=new C(); $o->setN(7); echo $o->n;"))
  (assert-string= "2"  (%php-run-capture "<?php class C{ public $c=0; function inc(){ $this->c=$this->c+1; return $this->c; } } $o=new C(); $o->inc(); echo $o->inc();"))
  ;; returning $this and chaining method calls
  (assert-string= "1"  (%php-run-capture "<?php class C{ public $v=1; function get(){ return $this->v; } function me(){ return $this; } } $o=new C(); echo $o->me()->get();"))
  ;; a method that does not use $this still works (receiver passed but unused)
  (assert-string= "42" (%php-run-capture "<?php class C{ function g(){ return 42; } } $o=new C(); echo $o->g();")))

(deftest php-e2e-class-property-access
  "Public properties read/write through $o->x with the correct slot name
(regression: a property declared via php-var-sym got slot |x| while $o->x looked
up X -> 'slot X is missing'), and a `= default' initializes the slot."
  (assert-string= "7" (%php-run-capture "<?php class C{ public $x=7; } $o=new C(); echo $o->x;"))
  (assert-string= "5" (%php-run-capture "<?php class C{ public $x; } $o=new C(); $o->x=5; echo $o->x;"))
  (assert-string= "x=7" (%php-run-capture "<?php class C{ public $x=7; } $o=new C(); echo \"x={$o->x}\";"))
  (assert-string= "hi" (%php-run-capture "<?php class C{ public $s='hi'; } $o=new C(); echo $o->s;")))

(deftest php-e2e-complex-string-interpolation
  "Curly-brace interpolation {$expr} supports array access, nested access, and
method calls — not just a bare variable (regression: the lexer required } right
after {$var, so {$a[\"k\"]} raised a lex error)."
  (assert-string= "v=5" (%php-run-capture "<?php $a=['k'=>5]; echo \"v={$a['k']}\";"))
  (assert-string= "n=9" (%php-run-capture "<?php $a=['x'=>['y'=>9]]; echo \"n={$a['x']['y']}\";"))
  (assert-string= "m=42" (%php-run-capture "<?php class C{ function g(){ return 42; } } $o=new C(); echo \"m={$o->g()}\";"))
  (assert-string= "Hi Bob, age 30!"
                  (%php-run-capture "<?php $n='Bob'; $a=['age'=>30]; echo \"Hi $n, age {$a['age']}!\";"))
  ;; simple interpolation and plain strings unaffected
  (assert-string= "Hi Bob!" (%php-run-capture "<?php $n='Bob'; echo \"Hi $n!\";"))
  (assert-string= "Hi Bob!" (%php-run-capture "<?php $n='Bob'; echo \"Hi {$n}!\";")))

(deftest php-e2e-foreach-over-array-literal
  "foreach over a PHP array literal iterates its values (regression: the non-key
foreach path used to assume a CL list and errored on the hash-table array)."
  (assert-string= "6" (%php-run-capture
                       "<?php $s=0; foreach ([1,2,3] as $v) { $s=$s+$v; } echo $s;")))

(deftest php-e2e-foreach-assoc-key-value
  "foreach ($arr as $k => $v) yields associative keys and values in order."
  (assert-string= "a=1;b=2;c=3;"
                  (%php-run-capture
                   "<?php $o=''; foreach (['a'=>1,'b'=>2,'c'=>3] as $k=>$v){ $o=$o.$k.'='.$v.';'; } echo $o;")))

(deftest php-e2e-generator-yield-foreach
  "A function containing yield becomes a generator; foreach drains its values."
  (assert-string= "6" (%php-run-capture
                       "<?php function g(){ yield 1; yield 2; yield 3; } $s=0; foreach (g() as $v){ $s=$s+$v; } echo $s;")))

(deftest php-e2e-generator-parametrized
  "A parametrized generator with a loop body yields the expected sequence."
  (assert-string= "15" (%php-run-capture
                        "<?php function c($n){ for($i=1;$i<=$n;$i++){ yield $i; } } $s=0; foreach (c(5) as $v){ $s=$s+$v; } echo $s;")))

(deftest php-e2e-generator-yield-from
  "yield from delegates to a nested generator, splicing its values in place."
  (assert-string= "0,1,2,3,"
                  (%php-run-capture
                   "<?php function inner(){ yield 1; yield 2; } function outer(){ yield 0; yield from inner(); yield 3; } $o=''; foreach (outer() as $v){ $o=$o.$v.','; } echo $o;")))

(deftest php-e2e-dynamic-closure-call
  "Calling a closure held in a variable, $f(args), invokes it (postfix LPAREN
lowers to a call on the value)."
  (assert-string= "50" (%php-run-capture
                        "<?php $f = function($x){ return $x*10; }; echo $f(5);")))

(deftest php-e2e-iife
  "An immediately-invoked function expression evaluates and calls in place."
  (assert-string= "10" (%php-run-capture
                        "<?php echo (function($x){ return $x*2; })(5);")))

(deftest php-e2e-closure-passed-to-user-function
  "A closure passed to a user function is callable inside it via $f(args)."
  (assert-string= "12" (%php-run-capture
                        "<?php function apply($f,$v){ return $f($v); } echo apply(function($x){ return $x*3; }, 4);")))

(deftest php-e2e-array-map-closure-callback
  "array_map invokes a PHP closure callback (routed back into the VM via
%vm-call-closure-sync). Closure bound to a variable to avoid the inline-closure +
inline-array-literal register-clobber edge case."
  (assert-string= "10,20,30"
                  (%php-run-capture
                   "<?php $f=function($x){ return $x*10; }; $a=[1,2,3]; $r=array_map($f,$a); echo $r[0].','.$r[1].','.$r[2];")))

(deftest php-e2e-array-filter-closure
  "array_filter invokes a PHP closure predicate per element (closure is the last
arg, so the inline-closure + array-literal register-clobber does not apply)."
  (assert-string= "3"
                  (%php-run-capture
                   "<?php $f=array_filter([0,1,0,2], function($x){ return $x; }); $s=0; foreach($f as $v){ $s=$s+$v; } echo $s;")))

(deftest php-e2e-equality-operators
  "== / != / === / !== compile and evaluate (regression: these lowered to an
unknown op symbol, so any function using them was silently dropped)."
  (assert-string= "1"  (%php-run-capture "<?php function f($x){ return $x == 4; } echo f(4);"))
  (assert-string= ""   (%php-run-capture "<?php function f($x){ return $x == 4; } echo f(3);"))
  (assert-string= "1"  (%php-run-capture "<?php function f($x){ return $x != 4; } echo f(3);"))
  (assert-string= "1"  (%php-run-capture "<?php function f($x){ return $x % 2 == 0; } echo f(4);")))

(deftest php-e2e-equality-type-juggling
  "PHP == juggles types ('5' == 5 is true) while === is strict ('5' === 5 false)."
  (assert-string= "loose"
                  (%php-run-capture "<?php $a='5'; if ($a == 5) { echo 'loose'; }"))
  (assert-string= "strictfail"
                  (%php-run-capture "<?php $a='5'; if ($a === 5) { echo 'x'; } else { echo 'strictfail'; }")))

(deftest php-e2e-echo-boolean-php-semantics
  "echo converts with PHP string semantics: true -> '1', false -> '' (regression:
echo went to the generic VM printer and printed CL T/NIL). Single echo per case —
ast-print appends a newline per statement, so values are combined with '.' / ','."
  (assert-string= "1"  (%php-run-capture "<?php echo true;"))
  (assert-string= ""   (%php-run-capture "<?php echo false;"))
  (assert-string= "1X" (%php-run-capture "<?php echo true . 'X';"))
  (assert-string= "X"  (%php-run-capture "<?php echo false . 'X';"))
  (assert-string= "1"  (%php-run-capture "<?php echo (5 == 5);"))
  (assert-string= "123" (%php-run-capture "<?php echo 1,2,3;")))

(deftest php-e2e-logical-operators
  "&& / || / ! evaluate to PHP booleans (regression: these lowered to unknown op
symbols / an undefined cl-cc/php::! function, so any expression using them failed
to compile)."
  (assert-string= "T" (%php-run-capture "<?php echo (true && true) ? 'T':'F';"))
  (assert-string= "F" (%php-run-capture "<?php echo (true && false) ? 'T':'F';"))
  (assert-string= "T" (%php-run-capture "<?php echo (false || true) ? 'T':'F';"))
  (assert-string= "F" (%php-run-capture "<?php echo (false || false) ? 'T':'F';"))
  (assert-string= "T" (%php-run-capture "<?php echo !false ? 'T':'F';"))
  (assert-string= "F" (%php-run-capture "<?php echo !5 ? 'T':'F';"))
  (assert-string= "1" (%php-run-capture "<?php echo true && true;")))

(deftest php-e2e-logical-in-conditions
  "Logical operators compose in if conditions and short-circuit."
  (assert-string= "both" (%php-run-capture "<?php $a=1; $b=2; if ($a && $b) { echo 'both'; }"))
  (assert-string= "mid"  (%php-run-capture "<?php $x=5; if ($x > 0 && $x < 10) { echo 'mid'; }"))
  (assert-string= "ok"   (%php-run-capture "<?php $a=true; $b=false; if (!$b && $a) { echo 'ok'; }")))

(deftest php-e2e-logical-short-circuit
  "&& does not evaluate its right operand when the left is false (the divide-by-
zero in boom() must not run)."
  (assert-string= "F"
                  (%php-run-capture
                   "<?php function boom(){ return 1/0; } $x=false; echo ($x && boom()) ? 'T':'F';")))

(deftest php-e2e-default-arguments
  "Function parameters with `= default` bind the default when the call omits them,
and the passed value otherwise (regression: php-parse-param-list parsed then
discarded the default tokens, so omitted args were left unset / 0)."
  (assert-string= "7"  (%php-run-capture "<?php function g($x=7){ return $x; } echo g();"))
  (assert-string= "3"  (%php-run-capture "<?php function g($x=7){ return $x; } echo g(3);"))
  (assert-string= "hi" (%php-run-capture "<?php function g($s='hi'){ return $s; } echo g();"))
  (assert-string= "15" (%php-run-capture "<?php function f($a,$b=10){ return $a+$b; } echo f(5);"))
  (assert-string= "7"  (%php-run-capture "<?php function f($a,$b=10){ return $a+$b; } echo f(5,2);"))
  (assert-string= "6"  (%php-run-capture "<?php function f($a=1,$b=2,$c=3){ return $a+$b+$c; } echo f();"))
  (assert-string= "15" (%php-run-capture "<?php function f($a=1,$b=2,$c=3){ return $a+$b+$c; } echo f(10);")))

(deftest php-e2e-default-arguments-all-callable-forms
  "Defaults work in closures, arrow functions, and methods, not just named functions."
  (assert-string= "9"  (%php-run-capture "<?php $f=function($x=9){ return $x; }; echo $f();"))
  (assert-string= "4"  (%php-run-capture "<?php $f=function($x=9){ return $x; }; echo $f(4);"))
  (assert-string= "16" (%php-run-capture "<?php $f=fn($x=8)=>$x*2; echo $f();"))
  (assert-string= "5"  (%php-run-capture "<?php class C{ function m($x=5){ return $x; } } $c=new C(); echo $c->m();"))
  (assert-string= "20" (%php-run-capture "<?php class C{ function m($x=5){ return $x; } } $c=new C(); echo $c->m(20);")))

(deftest php-e2e-array-append
  "$a[] = v appends to an array (regression: $a[] hit a parse error on the empty
subscript). Pushing onto the referenced hash-table is the whole effect."
  (assert-string= "3"  (%php-run-capture "<?php $a=[1,2]; $a[]=3; echo count($a);"))
  (assert-string= "3"  (%php-run-capture "<?php $a=[1,2]; $a[]=3; echo $a[2];"))
  (assert-string= "xy" (%php-run-capture "<?php $a=[]; $a[]='x'; $a[]='y'; echo $a[0].$a[1];"))
  (assert-string= "01020"
                  (%php-run-capture "<?php $a=[]; for($i=0;$i<3;$i++){ $a[]=$i*10; } echo $a[0].$a[1].$a[2];"))
  (assert-string= "12" (%php-run-capture "<?php $a=['k'=>1]; $a[]=2; echo $a['k'].$a[0];")))

(deftest php-e2e-array-subscript-set-still-works
  "Keyed and indexed subscript assignment are unaffected by the [] append path."
  (assert-string= "9" (%php-run-capture "<?php $a=[1]; $a[0]=9; echo $a[0];"))
  (assert-string= "5" (%php-run-capture "<?php $a=[]; $a['x']=5; echo $a['x'];")))

(deftest php-e2e-variadic-parameters
  "...$args collects the trailing arguments into a PHP array (regression: the
variadic param received the first arg directly instead of a collected array)."
  (assert-string= "10" (%php-run-capture "<?php function s(...$n){ return array_sum($n); } echo s(1,2,3,4);"))
  (assert-string= "4"  (%php-run-capture "<?php function s(...$n){ return count($n); } echo s(1,2,3,4);"))
  (assert-string= "0"  (%php-run-capture "<?php function s(...$n){ return count($n); } echo s();"))
  (assert-string= "x:3" (%php-run-capture "<?php function f($a,...$rest){ return $a.':'.count($rest); } echo f('x',1,2,3);"))
  (assert-string= "abc" (%php-run-capture "<?php function j(...$xs){ $r=''; foreach($xs as $x){ $r.=$x; } return $r; } echo j('a','b','c');")))

(deftest php-e2e-variadic-all-callable-forms
  "Variadics work in closures and arrow functions too."
  (assert-string= "6"  (%php-run-capture "<?php $s=function(...$n){ return array_sum($n); }; echo $s(1,2,3);"))
  (assert-string= "15" (%php-run-capture "<?php $s=fn(...$n)=>array_sum($n); echo $s(5,5,5);")))

(deftest php-e2e-spread-call
  "f(...$args) expands an array into positional arguments (regression: spread
lowered to an unconsumed %php-spread marker). Lowers to apply over a runtime arg
list."
  (assert-string= "5"  (%php-run-capture "<?php function add($a,$b){ return $a+$b; } $args=[2,3]; echo add(...$args);"))
  (assert-string= "6"  (%php-run-capture "<?php function add3($a,$b,$c){ return $a+$b+$c; } $x=[1,2,3]; echo add3(...$x);"))
  (assert-string= "xyz" (%php-run-capture "<?php function f($a,$b,$c){ return $a.$b.$c; } $r=['y','z']; echo f('x',...$r);"))
  (assert-string= "3"  (%php-run-capture "<?php $a=[3,1,2]; echo max(...$a);")))

(deftest php-e2e-spread-composes-with-variadic-and-closures
  "Spread expands into a variadic collector, and works on dynamic closure calls."
  (assert-string= "60" (%php-run-capture "<?php function s(...$n){ return array_sum($n); } $a=[10,20,30]; echo s(...$a);"))
  (assert-string= "20" (%php-run-capture "<?php $f=function($a,$b){ return $a*$b; }; $args=[4,5]; echo $f(...$args);")))

(deftest php-e2e-for-loop-continue
  "continue in a for loop runs the increment before re-testing (regression: the
for->while lowering put the continue target before the increment, so continue
skipped $i++ and looped forever)."
  (assert-string= "6"  (%php-run-capture "<?php $s=0; for($i=0;$i<6;$i++){ if($i%2)continue; $s+=$i; } echo $s;"))
  (assert-string= "6"  (%php-run-capture "<?php $s=0; for($i=0;$i<10;$i++){ if($i==5)break; if($i%2)continue; $s+=$i; } echo $s;"))
  (assert-string= "6"  (%php-run-capture "<?php $s=0; for($i=0;$i<3;$i++){ for($j=0;$j<3;$j++){ if($j==1)continue; $s++; } } echo $s;")))

(deftest php-e2e-for-loop-basics
  "for loops without continue still iterate correctly (no regression from the
dedicated for lowering)."
  (assert-string= "10"  (%php-run-capture "<?php $s=0; for($i=1;$i<=4;$i++){ $s+=$i; } echo $s;"))
  (assert-string= "3"   (%php-run-capture "<?php $s=0; for($i=0;$i<10;$i++){ if($i==3)break; $s+=$i; } echo $s;"))
  (assert-string= "321" (%php-run-capture "<?php $s=''; for($i=3;$i>0;$i--){ $s.=$i; } echo $s;")))

(deftest php-parser-cli-compile-path-for-php-files
  "Characterization: native compile path should auto-detect .php files and compile PHP source end-to-end."
  (let* ((tmp-dir (uiop:temporary-directory))
         (input (merge-pathnames "cl-cc-php-compile-gap.php" tmp-dir))
         (output (merge-pathnames "cl-cc-php-compile-gap" tmp-dir)))
    (with-open-file (stream input :direction :output :if-exists :supersede)
      (write-line "<?php echo match($x) { 1 => 'one', default => 'other' };" stream))
    (let ((result (cl-cc::compile-file-to-native input :output-file output)))
      (assert-equal output result)
      (assert-true (probe-file output)))))

(deftest php-runtime-array-unset-deletes-key
  "The PHP runtime helper for unset($array[$key]) removes the key and shrinks count."
  (let ((array (cl-cc/php:%php-array (list nil nil "a")
                                     (list nil nil "b"))))
    (assert-= 2 (cl-cc/php:%php-count array))
    (cl-cc/php:%php-array-unset array 0)
    (assert-= 1 (cl-cc/php:%php-count array))
    (assert-equal cl-cc/php:+php-null+ (cl-cc/php:%php-array-ref array 0))
    (assert-string= "b" (cl-cc/php:%php-array-ref array 1))))

(deftest php-runtime-yield-helper-preserves-value
  "The PHP yield helpers preserve yielded values for later generator lowering."
  (assert-equal '(:yield 42) (cl-cc/php:%php-yield 42))
  (assert-equal '(:yield-from (1 2)) (cl-cc/php:%php-yield-from '(1 2))))

(deftest php-runtime-exception-payload-matches-class
  "The PHP exception payload helper preserves class metadata for catch dispatch."
  (let ((payload (cl-cc/php:%php-make-exception 'ex :value)))
    (assert-true (cl-cc/php:%php-exception-object-p payload))
    (assert-eq :value (cl-cc/php:%php-exception-value payload))
    (assert-true (cl-cc/php:%php-exception-matches-p payload 'ex))
    (assert-true (cl-cc/php:%php-exception-matches-p payload '(other ex)))
    (assert-false (cl-cc/php:%php-exception-matches-p payload 'other))))

(deftest php-runtime-concat-stringifies-values
  "PHP interpolation concat stringifies null, booleans, numbers, and strings."
  (assert-string= "Hello 42" (cl-cc/php:%php-concat "Hello " 42))
  (assert-string= "x1" (cl-cc/php:%php-concat "x" t))
  (assert-string= "x" (cl-cc/php:%php-concat "x" cl-cc/php:+php-null+)))

(deftest php-runtime-enum-helpers
  "PHP enum runtime helpers return cases and backed lookups with PHP null fallback."
  (let* ((class (make-hash-table :test #'eq))
         (draft (cl-cc/php:%php-enum-make-case 'status 'draft 0))
         (published (cl-cc/php:%php-enum-make-case 'status 'published 1)))
    (setf (gethash :__class-slots__ class) '(draft published)
          (gethash 'draft class) draft
          (gethash 'published class) published)
    (assert-equal (list draft published) (cl-cc/php:%php-enum-cases class))
    (assert-eq published (cl-cc/php:%php-enum-from class 1))
    (assert-equal cl-cc/php:+php-null+ (cl-cc/php:%php-enum-try-from class 99))
    (assert-= 1 (cl-cc/php:%php-enum-case-value published))))

(deftest php-runtime-expression-operator-helpers
  "PHP operator helpers implement integer bitwise, shift, modulo, and spaceship behavior."
  (assert-= 3 (cl-cc/php:%php-modulo 7 4))
  (assert-= -1 (cl-cc/php:%php-modulo -7 3))
  (assert-= 8 (cl-cc/php:%php-shift-left 1 3))
  (assert-= -4 (cl-cc/php:%php-shift-right -8 1))
  (assert-= -1 (cl-cc/php:%php-spaceship 1 2))
  (assert-= 0 (cl-cc/php:%php-spaceship "2" 2))
  (assert-= 2 (cl-cc/php:%php-bitwise-and 6 3))
  (assert-= 7 (cl-cc/php:%php-bitwise-or 6 3))
  (assert-= 5 (cl-cc/php:%php-bitwise-xor 6 3))
  (assert-= -2 (cl-cc/php:%php-bitwise-not 1)))

(deftest php-compile-expression-operators
  "The PHP frontend compiles all expression operators added to the precedence chain."
  (let ((result (cl-cc:compile-string
                 "<?php $a = 2 ** 3 ** 2; $b = 7 % 4; $c = 1 << 3; $d = 8 >> 1; $e = 1 <=> 2; $f = 6 & 3; $g = 6 ^ 3; $h = 4 | 1; $i = ~1; $j = 1 + 2 . 3;"
                 :target :vm
                  :language :php)))
    (assert-true (typep result 'cl-cc/compile:compilation-result))))

(deftest php-compile-enum-static-builtins
  "PHP enums compile through static case and from/tryFrom/cases helper lowering."
  (let ((result (cl-cc:compile-string
                 "<?php enum Status: int { case Draft = 0; case Published = 1; } $a = Status::Published; $b = Status::from(1); $c = Status::tryFrom(99); $d = Status::cases();"
                 :target :vm
                 :language :php)))
    (assert-true (typep result 'cl-cc/compile:compilation-result))))

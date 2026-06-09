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

(deftest php-e2e-postfix-incdec-on-places
  "Postfix ++/-- mutate an object property and an array element (regression: only a
plain $var was mutated), yielding the OLD value, including $this->n++ in a method."
  (assert-string= "2"  (%php-run-capture "<?php class C{ public $n=0; } $o=new C(); $o->n++; $o->n++; echo $o->n;"))
  (assert-string= "10" (%php-run-capture "<?php class C{ public $n=10; } $o=new C(); echo $o->n++;"))
  (assert-string= "4"  (%php-run-capture "<?php class C{ public $n=5; } $o=new C(); $o->n--; echo $o->n;"))
  ;; the canonical counter: $this->n++ inside a method
  (assert-string= "3"  (%php-run-capture "<?php class C{ public $n=0; function inc(){ $this->n++; } } $o=new C(); $o->inc(); $o->inc(); $o->inc(); echo $o->n;"))
  (assert-string= "2"  (%php-run-capture "<?php $a=[0]; $a[0]++; $a[0]++; echo $a[0];"))
  ;; postfix yields the old element value (single echo: ast-print adds a newline
  ;; per statement, so combine with '.')
  (assert-string= "5-6" (%php-run-capture "<?php $a=[5]; $old=$a[0]++; echo $old.'-'.$a[0];")))

(deftest php-e2e-heredoc-nowdoc
  "Heredoc <<<EOT interpolates $vars and {$expr} like a double-quoted string;
nowdoc <<<'EOT' is literal.  Regression: the heredoc body was emitted as a raw
non-interpolated T-STRING (so $n printed literally), and the nowdoc label scan
swallowed the closing quote into the marker ('end marker EOT' not found').  Now
the heredoc body is routed through the shared interpolation lexer and the label
is scanned as an identifier."
  ;; heredoc interpolates a simple $var
  (assert-string= "Hi Bob"
                  (%php-run-capture (format nil "<?php $n=\"Bob\"; echo <<<EOT~%Hi $n~%EOT;~%")))
  ;; multiple vars and a {$expr} brace form
  (assert-string= "X-Y"
                  (%php-run-capture (format nil "<?php $a=\"X\";$b=\"Y\"; echo <<<EOT~%$a-$b~%EOT;~%")))
  (assert-string= "Hi Bob!"
                  (%php-run-capture (format nil "<?php $n=\"Bob\"; echo <<<EOT~%Hi {$n}!~%EOT;~%")))
  ;; multi-line body preserves interior newlines
  (assert-string= (format nil "line1~%val=Z")
                  (%php-run-capture (format nil "<?php $n=\"Z\"; echo <<<EOT~%line1~%val=$n~%EOT;~%")))
  ;; double-quoted heredoc label is still an (interpolating) heredoc
  (assert-string= "v=Q"
                  (%php-run-capture (format nil "<?php $n=\"Q\"; echo <<<\"EOT\"~%v=$n~%EOT;~%")))
  ;; nowdoc is literal — $n is NOT interpolated
  (assert-string= "Hi $n"
                  (%php-run-capture (format nil "<?php $n=\"Bob\"; echo <<<'EOT'~%Hi $n~%EOT;~%"))))

(deftest php-e2e-list-destructuring
  "list(...) = expr destructuring assignment works, mirroring the short [$a,$b]
form.  Regression: list($a,$b) lowered to a %php-list-bind call node that the
assignment parser did not recognize ('unsupported assignment target'); now it
parses to the same array-literal node as [$a,$b], flowing through
%php-lower-list-assign."
  (assert-string= "3"      (%php-run-capture "<?php list($a,$b)=[1,2]; echo $a+$b;"))
  (assert-string= "60"     (%php-run-capture "<?php list($a,$b,$c)=[10,20,30]; echo $a+$b+$c;"))
  ;; destructuring a function's array return
  (assert-string= "4-9"    (%php-run-capture "<?php function pair(){return [4,9];} list($x,$y)=pair(); echo $x.'-'.$y;"))
  (assert-string= "Bob:30" (%php-run-capture "<?php list($n,$a)=['Bob',30]; echo $n.':'.$a;"))
  ;; the short [$a,$b] form (and swap) keep working
  (assert-string= "1,2"    (%php-run-capture "<?php [$a,$b]=[1,2]; echo $a.','.$b;"))
  (assert-string= "2,1"    (%php-run-capture "<?php $a=1;$b=2; [$a,$b]=[$b,$a]; echo $a.','.$b;")))

(deftest php-e2e-list-destructuring-nested-keyed
  "Nested and keyed list destructuring.  %php-lower-list-assign previously bound
only flat top-level $var targets positionally, ignoring `key => $var' entries
and skipping nested [..] targets.  Now it recurses into nested targets and
accesses by key when a key is present."
  ;; nested
  (assert-string= "6"   (%php-run-capture "<?php [[$a,$b],$c]=[[1,2],3]; echo $a+$b+$c;"))
  (assert-string= "6"   (%php-run-capture "<?php list(list($a,$b),$c)=[[1,2],3]; echo $a+$b+$c;"))
  ;; keyed (order-independent access by key)
  (assert-string= "3"   (%php-run-capture "<?php ['x'=>$a,'y'=>$b]=['x'=>1,'y'=>2]; echo $a+$b;"))
  (assert-string= "1"   (%php-run-capture "<?php ['y'=>$a,'x'=>$b]=['x'=>1,'y'=>2]; echo $a-$b;")) ; a=2,b=1
  ;; nested + keyed combined
  (assert-string= "9,3" (%php-run-capture "<?php [['k'=>$a],$b]=[['k'=>9],3]; echo $a.','.$b;")))

(deftest php-e2e-static-members
  "Static methods and properties resolve via ClassName::member.  Regression: the
`static` modifier lexes as a T-TYPE (it doubles as the `static` return type), so
the class-body modifier parser skipped it — static props/methods were left
instance-allocated and ClassName::member found nothing on the class object
('Undefined function: NIL' / empty).  Static members are now class-allocated
(like class constants), so slot-value on the class object resolves them."
  ;; static method called by class name
  (assert-string= "25" (%php-run-capture "<?php class M{ static function sq($x){ return $x*$x; } } echo M::sq(5);"))
  ;; static property read
  (assert-string= "5"  (%php-run-capture "<?php class C{ static $n=5; } echo C::$n;"))
  ;; class constant still works alongside a static property in the same class
  (assert-string= "3|5" (%php-run-capture "<?php class C{ const PI=3; static $n=5; } echo C::PI.'|'.C::$n;"))
  ;; one static method consuming another's result, both by class name
  (assert-string= "21" (%php-run-capture "<?php class U{ static function inc($x){ return $x+1; } static function dbl($x){ return $x*2; } } echo U::inc(U::dbl(10));")))

(deftest php-e2e-self-static-parent
  "self::, static::, and parent:: resolve inside method bodies, including a class
referring to itself for recursion (ClassName::m / self::m).  Regression: methods
were compiled as class-slot initforms BEFORE the class registered its own global,
so a self-reference hit the 'Unbound variable' path and the whole class form was
dropped (silent empty output).  The class name is now registered as a global
before its method bodies compile."
  ;; self:: static method dispatch
  (assert-string= "9"  (%php-run-capture "<?php class C{ static function f(){ return self::g(); } static function g(){ return 9; } } echo C::f();"))
  ;; static:: (late static binding spelling) dispatch
  (assert-string= "7"  (%php-run-capture "<?php class C{ static function f(){ return static::g(); } static function g(){ return 7; } } echo C::f();"))
  ;; self::CONST inside a method
  (assert-string= "42" (%php-run-capture "<?php class C{ const X=42; static function get(){ return self::X; } } echo C::get();"))
  ;; recursion via ClassName::m
  (assert-string= "120" (%php-run-capture "<?php class F{ static function fac($n){ return $n<=1?1:$n*F::fac($n-1); } } echo F::fac(5);"))
  ;; recursion via self::m
  (assert-string= "720" (%php-run-capture "<?php class F{ static function fac($n){ return $n<=1?1:$n*self::fac($n-1); } } echo F::fac(6);"))
  ;; parent:: static dispatch to a superclass method
  (assert-string= "11" (%php-run-capture "<?php class A{ static function who(){ return 1; } } class B extends A{ static function who2(){ return parent::who()+10; } } echo B::who2();"))
  ;; a static method called from inside an instance method, by class name
  (assert-string= "109" (%php-run-capture "<?php class K{ public $base=100; static function tag(){ return 9; } function show(){ return $this->base+K::tag(); } } $o=new K(); echo $o->show();")))

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
    ;; %php-enum-case-list is the internal CL list; %php-enum-cases (ENUM::cases())
    ;; now returns a PHP array (hash-table) so count()/foreach work.
    (assert-equal (list draft published) (cl-cc/php:%php-enum-case-list class))
    (assert-true (hash-table-p (cl-cc/php:%php-enum-cases class)))
    (assert-= 2 (cl-cc/php:%php-count (cl-cc/php:%php-enum-cases class)))
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

(deftest php-e2e-match-and-relational-booleans
  "match(true){$cond=>…} works and relational operators return PHP booleans.
Two regressions: (1) match arms with multiple conditions (1,2=>…) built an
ast-binop :or that codegen could not emit, so the match produced nothing; now
they chain via nested ifs. (2) <,>,<=,>= lowered to the VM's integer compare
(1/0), so gettype(5>3) was \"integer\", (5>3)===true was false, and
match(true){$x>3=>…} never matched; now they go through %php-lt/gt/le/ge
(derived from %php-spaceship) and yield a real boolean."
  ;; match on a value (single and multiple conditions per arm)
  (assert-string= "b"   (%php-run-capture "<?php $x=2; echo match($x){1=>'a',2=>'b'};"))
  (assert-string= "low" (%php-run-capture "<?php $x=2; echo match($x){1,2=>'low',3=>'hi'};"))
  (assert-string= "hi"  (%php-run-capture "<?php $x=3; echo match($x){1,2=>'low',3=>'hi'};"))
  ;; match(true) with comparison conditions
  (assert-string= "big"   (%php-run-capture "<?php $x=5; echo match(true){$x>3=>'big',default=>'small'};"))
  (assert-string= "small" (%php-run-capture "<?php $x=1; echo match(true){$x>3=>'big',default=>'small'};"))
  ;; relational operators yield PHP booleans
  (assert-string= "boolean" (%php-run-capture "<?php echo gettype(5>3);"))
  (assert-string= "boolean" (%php-run-capture "<?php echo gettype(2<1);"))
  (assert-string= "T"       (%php-run-capture "<?php echo (5>3)===true ? 'T':'n';"))
  (assert-string= "boolean" (%php-run-capture "<?php echo gettype(true);"))
  ;; string comparison and loop conditions still work
  (assert-string= "y"  (%php-run-capture "<?php echo 'apple'<'banana' ? 'y':'n';"))
  (assert-string= "6"  (%php-run-capture "<?php $s=0; for($i=0;$i<4;$i++){$s+=$i;} echo $s;"))
  ;; spaceship still returns -1/0/1
  (assert-string= "-1" (%php-run-capture "<?php echo 1<=>2;")))

(deftest php-e2e-uninitialized-property-null
  "An untyped property declared without an initializer (public $x;) defaults to
PHP null, so is_null($o->x) is true and $o->x ?? d coalesces.  Regression: the
slot read as the unbound-slot-marker, so is_null was false and ?? did not fall
through to the default."
  (assert-string= "yes" (%php-run-capture "<?php class C{public $x;} $o=new C(); echo is_null($o->x)?'yes':'no';"))
  (assert-string= "def" (%php-run-capture "<?php class C{public $x;} $o=new C(); echo $o->x ?? 'def';"))
  ;; an explicit default is unaffected
  (assert-string= "7"   (%php-run-capture "<?php class C{public $x=7;} $o=new C(); echo $o->x ?? 'def';"))
  (assert-string= "hi"  (%php-run-capture "<?php class C{public $s='hi';} $o=new C(); echo $o->s;"))
  ;; writing then reading a no-default property still works
  (assert-string= "9"   (%php-run-capture "<?php class C{public $x;} $o=new C(); $o->x=9; echo $o->x;")))

(deftest php-e2e-nullish-coalescing-assignment
  "$x ??= v assigns only when $x is PHP null, for variables, object properties
and array elements.  Regression: %php-nullish-cond built an (ast-binop :op 'or)
that codegen could not emit, so EVERY ??= failed to compile and dropped the whole
program; it also wrongly treated false/0 as nullish."
  ;; variable: assigns when null, keeps an existing value
  (assert-string= "x"    (%php-run-capture "<?php $a=null; $a ??= 'x'; echo $a;"))
  (assert-string= "keep" (%php-run-capture "<?php $a='keep'; $a ??= 'x'; echo $a;"))
  ;; false and 0 are NOT nullish — ??= must not overwrite them
  (assert-string= "F"    (%php-run-capture "<?php $a=false; $a ??= 'x'; echo $a===false?'F':'o';"))
  (assert-string= "0"    (%php-run-capture "<?php $a=0; $a ??= 'x'; echo $a;"))
  ;; object property
  (assert-string= "y"    (%php-run-capture "<?php class C{public $x;} $o=new C(); $o->x ??= 'y'; echo $o->x;"))
  (assert-string= "5"    (%php-run-capture "<?php class C{public $x=5;} $o=new C(); $o->x ??= 'y'; echo $o->x;"))
  ;; array element
  (assert-string= "v"    (%php-run-capture "<?php $a=[]; $a['k'] ??= 'v'; echo $a['k'];"))
  (assert-string= "3"    (%php-run-capture "<?php $a=['k'=>3]; $a['k'] ??= 'v'; echo $a['k'];")))

(deftest php-e2e-compound-assign-undefined-var
  "A compound assignment on an undefined variable treats the missing left
operand as null (0 for arithmetic/bitwise, '' for concat, RHS for ??=) and
INTRODUCES the variable.  Regression: it read the unbound variable into a temp
and dropped the whole program."
  (assert-string= "3"  (%php-run-capture "<?php $a += 3; echo $a;"))
  (assert-string= "-3" (%php-run-capture "<?php $a -= 3; echo $a;"))
  (assert-string= "0"  (%php-run-capture "<?php $a *= 5; echo $a;"))
  (assert-string= "hi" (%php-run-capture "<?php $s .= 'hi'; echo $s;"))
  (assert-string= "y"  (%php-run-capture "<?php $a ??= 'y'; echo $a;"))
  ;; the known-variable path is unaffected
  (assert-string= "8"  (%php-run-capture "<?php $a=5; $a += 3; echo $a;"))
  (assert-string= "xy" (%php-run-capture "<?php $s='x'; $s .= 'y'; echo $s;")))

(deftest php-e2e-arithmetic-operand-coercion
  "PHP +, -, * coerce non-number operands (null->0, true->1, numeric string->
number) instead of erroring.  Regression: they lowered to raw CL arithmetic, so
null + 3, '5' + 3, true + 1 all signalled `not of type NUMBER'."
  (assert-string= "3"   (%php-run-capture "<?php echo null + 3;"))
  (assert-string= "8"   (%php-run-capture "<?php echo '5' + 3;"))
  (assert-string= "8"   (%php-run-capture "<?php echo '4' * '2';"))
  (assert-string= "2.5" (%php-run-capture "<?php echo '1.5' + 1;"))
  (assert-string= "2"   (%php-run-capture "<?php echo true + 1;"))
  (assert-string= "5"   (%php-run-capture "<?php echo 2 - -3;"))
  ;; pure-number arithmetic, precedence, compound and loops are unaffected
  (assert-string= "5"   (%php-run-capture "<?php echo 2 + 3;"))
  (assert-string= "14"  (%php-run-capture "<?php echo 2 + 3 * 4;"))
  (assert-string= "7"   (%php-run-capture "<?php $a=10; $a -= 3; echo $a;"))
  (assert-string= "10"  (%php-run-capture "<?php $s=0; for($i=1;$i<=4;$i++){$s+=$i;} echo $s;")))

(deftest php-e2e-float-stringification
  "PHP echoes/interpolates floats correctly: whole-valued floats as integers,
others with trailing zeros trimmed and no CL exponent marker; float literals
keep double precision; / yields a float (or an int when evenly divisible).
Regression: %php-stringify used princ-to-string (1.5 -> \"1.5d0\", 5/2 -> \"5/2\"),
literals were read as single-floats (3.14 -> 3.1400001…), and / returned a
rational."
  (assert-string= "3"       (%php-run-capture "<?php echo 1.5 * 2;"))
  (assert-string= "1.75"    (%php-run-capture "<?php echo 1.5 + 0.25;"))
  (assert-string= "3.14"    (%php-run-capture "<?php echo 3.14;"))
  (assert-string= "6"       (%php-run-capture "<?php echo 6.0;"))
  (assert-string= "1000000" (%php-run-capture "<?php echo 1000000.0;"))
  (assert-string= "2.5"     (%php-run-capture "<?php echo 10 / 4;"))
  (assert-string= "5"       (%php-run-capture "<?php echo 10 / 2;"))
  (assert-string= "x=1.5"   (%php-run-capture "<?php echo 'x=' . 1.5;"))
  (assert-string= "v=2.5"   (%php-run-capture "<?php $f=2.5; echo \"v=$f\";"))
  ;; sqrt returns a double (PHP 14-digit precision), whole roots print as int
  (assert-string= "1.4142135623731" (%php-run-capture "<?php echo sqrt(2);"))
  (assert-string= "2"       (%php-run-capture "<?php echo sqrt(4);")))

(deftest php-e2e-var-dump-and-export
  "var_dump PRINTS a type-annotated dump (it does not return a string), and
var_export prints a re-parseable representation.  Regressions: var_dump built
its string with with-output-to-string and returned it (so nothing was printed)
and used the raw CL float form; var_export was registered as a lambda, so the
builtin dispatch (which bridges a function SYMBOL) failed with 'Undefined
function'."
  (assert-string= "int(42)"        (%php-run-capture "<?php var_dump(42);"))
  (assert-string= "string(2) \"hi\"" (%php-run-capture "<?php var_dump('hi');"))
  (assert-string= "bool(true)"     (%php-run-capture "<?php var_dump(true);"))
  (assert-string= "float(1.5)"     (%php-run-capture "<?php var_dump(1.5);"))
  (assert-string= "NULL"           (%php-run-capture "<?php var_dump(null);"))
  ;; nested array dump in PHP's multi-line format
  (assert-string= (format nil "array(2) {~%  [0]=>~%  int(1)~%  [1]=>~%  int(2)~%}")
                  (%php-run-capture "<?php var_dump([1,2]);"))
  ;; var_export re-parseable forms
  (assert-string= "42"    (%php-run-capture "<?php var_export(42);"))
  (assert-string= "'hi'"  (%php-run-capture "<?php var_export('hi');"))
  (assert-string= "true"  (%php-run-capture "<?php var_export(true);"))
  (assert-string= (format nil "array (~%  0 => 1,~%  1 => 2,~%)")
                  (%php-run-capture "<?php var_export([1,2]);"))
  ;; return-mode var_export
  (assert-string= "7"     (%php-run-capture "<?php echo var_export(7, true);")))

(deftest php-e2e-enum-name-value-cases
  "Enum cases expose ->name and ->value, S::cases() is a PHP array, and
S::from/tryFrom resolve backed cases.  Regression: cases stored name/value under
CL symbol keys in an EQ hash-table, so $case->name (a string-key fallback) never
matched; cases() returned a CL list that broke count()/foreach."
  (assert-string= "A"  (%php-run-capture "<?php enum S{case A;} echo S::A->name;"))
  (assert-string= "a"  (%php-run-capture "<?php enum S:string{case A='a';case B='b';} echo S::A->value;"))
  (assert-string= "10" (%php-run-capture "<?php enum S:int{case A=10;} echo S::A->value;"))
  (assert-string= "2"  (%php-run-capture "<?php enum S{case A;case B;} echo count(S::cases());"))
  (assert-string= "AB" (%php-run-capture "<?php enum S{case A;case B;} $n=''; foreach(S::cases() as $c){$n.=$c->name;} echo $n;"))
  (assert-string= "B"  (%php-run-capture "<?php enum S:int{case A=1;case B=2;} echo S::from(2)->name;"))
  (assert-string= "null" (%php-run-capture "<?php enum S:int{case A=1;} echo S::tryFrom(9)===null?'null':'f';"))
  (assert-string= "y"  (%php-run-capture "<?php enum S{case A;} echo S::A===S::A?'y':'n';"))
  ;; match on enum cases
  (assert-string= "hearts" (%php-run-capture "<?php enum Suit:string{case H='h';case S='s';} $x=Suit::H; echo match($x){Suit::H=>'hearts',Suit::S=>'spades'};")))

(deftest php-e2e-enum-methods
  "Enum cases can call methods defined on the enum, with $this bound to the case.
Regression: enum methods lived on the class as instance-method slots that the
case singletons (plain payloads, not make-instance objects) never received, so
S::A->m() found no method ('Undefined function: NIL').  Enum methods are now
class-allocated and each case is linked to the enum class via __class__."
  (assert-string= "L" (%php-run-capture "<?php enum S{case A; public function label(){return 'L';}} echo S::A->label();"))
  ;; $this->value inside a method
  (assert-string= "A" (%php-run-capture "<?php enum S:string{case A='a'; public function up(){return strtoupper($this->value);}} echo S::A->up();"))
  ;; match($this) inside a method
  (assert-string= "red" (%php-run-capture "<?php enum Suit{case H;case S; public function color(){return match($this){Suit::H=>'red',Suit::S=>'black'};}} echo Suit::H->color();"))
  ;; method with an argument
  (assert-string= "6" (%php-run-capture "<?php enum S{case A; public function add($n){return $n+1;}} echo S::A->add(5);"))
  ;; methods coexist with name/value/const and don't break them
  (assert-string= "A" (%php-run-capture "<?php enum S{case A; public function x(){return 1;}} echo S::A->name;"))
  (assert-string= "5" (%php-run-capture "<?php enum S{case A; const X=5; public function y(){return 2;}} echo S::X;")))

(deftest php-e2e-nullsafe-operator
  "The ?-> nullsafe operator short-circuits to null when the receiver is null,
else reads the property / calls the method (passing the receiver as \$this).
Regression: the null check was an (ast-binop := ...) — CL NUMERIC equality — so
\$o?->x on an object raised 'not of type NUMBER'; the receiver was also evaluated
twice and ?->m() did not pass \$this."
  (assert-string= "5"       (%php-run-capture "<?php class C{public $x=5;} $o=new C(); echo $o?->x;"))
  (assert-string= "def"     (%php-run-capture "<?php class C{public $x=5;} $o=null; echo $o?->x ?? 'def';"))
  (assert-string= "7"       (%php-run-capture "<?php class C{function m(){return 7;}} $o=new C(); echo $o?->m();"))
  (assert-string= "n"       (%php-run-capture "<?php class C{function m(){return 7;}} $o=null; echo $o?->m() ?? 'n';"))
  ;; ?-> method passes the receiver as $this
  (assert-string= "3"       (%php-run-capture "<?php class C{public $v=3; function get(){return $this->v;}} $o=new C(); echo $o?->get();"))
  ;; chaining: a null link short-circuits the rest
  (assert-string= "none"    (%php-run-capture "<?php class C{public $n=null;} $o=new C(); echo $o?->n?->x ?? 'none';"))
  ;; a null property value coalesces
  (assert-string= "wasnull" (%php-run-capture "<?php class C{public $x;} $o=new C(); echo $o?->x ?? 'wasnull';")))

(deftest php-e2e-array-spread
  "The spread operator splices an array into an array literal ([...$a, 3]),
re-indexing integer keys and preserving string keys (PHP 8.1).  Regression: the
spread element lowered to a (%php-spread ...) call with no backing function
('Undefined function: %PHP-SPREAD'); %php-array now splices spread markers."
  (assert-string= "6"       (%php-run-capture "<?php $a=[1,2]; $b=[...$a,3]; echo array_sum($b);"))
  (assert-string= "1,2,3,4" (%php-run-capture "<?php $a=[2,3]; $b=[1,...$a,4]; echo implode(',',$b);"))
  (assert-string= "2"       (%php-run-capture "<?php $a=[1]; $b=[2]; $c=[...$a,...$b]; echo count($c);"))
  (assert-string= "11"      (%php-run-capture "<?php $a=[5,6]; $b=[...$a]; echo array_sum($b);"))
  ;; string keys are preserved
  (assert-string= "12"      (%php-run-capture "<?php $a=['x'=>1]; $b=[...$a,'y'=>2]; echo $b['x'].$b['y'];"))
  ;; integer keys are re-indexed from 0
  (assert-string= "12"      (%php-run-capture "<?php $a=[5=>1, 9=>2]; $b=[...$a]; echo $b[0].$b[1];"))
  ;; regression guards: plain arrays and call spread still work
  (assert-string= "6"       (%php-run-capture "<?php echo array_sum([1,2,3]);"))
  (assert-string= "6"       (%php-run-capture "<?php function s(...$n){return array_sum($n);} $a=[1,2,3]; echo s(...$a);")))

(deftest php-e2e-first-class-callable
  "f(...) creates a first-class callable (PHP 8.1) — a forwarding closure
fn(...$a) => f(...$a) — for user functions and builtins.  Regression: f(...)
parsed the ... as a %php-first-class-callable marker ARGUMENT, so f(...) called
f with the marker ('Undefined function: %PHP-FIRST-CLASS-CALLABLE')."
  (assert-string= "25" (%php-run-capture "<?php function sq($x){return $x*$x;} $f=sq(...); echo $f(5);"))
  ;; forwards multiple arguments
  (assert-string= "7"  (%php-run-capture "<?php function sub($a,$b){return $a-$b;} $f=sub(...); echo $f(10,3);"))
  ;; usable as a callable in higher-order functions
  (assert-string= "12" (%php-run-capture "<?php function dbl($x){return $x*2;} $f=dbl(...); echo array_sum(array_map($f,[1,2,3]));"))
  ;; works for builtins
  (assert-string= "5"  (%php-run-capture "<?php $f=strlen(...); echo $f('hello');"))
  ;; inline as an argument
  (assert-string= "14" (%php-run-capture "<?php function sq($x){return $x*$x;} echo array_sum(array_map(sq(...),[1,2,3]));"))
  ;; regression guards: ordinary calls and call-spread unaffected
  (assert-string= "25" (%php-run-capture "<?php function sq($x){return $x*$x;} echo sq(5);"))
  (assert-string= "3"  (%php-run-capture "<?php function s(...$n){return array_sum($n);} $a=[1,2]; echo s(...$a);")))

(deftest php-e2e-array-union
  "The + operator on two arrays is array UNION — the result has all of the left
array's entries plus the right's entries whose keys are not already present
(left wins), with keys preserved (NOT reindexed like array_merge).  Regression:
%php-add coerced both arrays to 0, so [1,2]+[3,4,5] gave 0 and count() errored."
  (assert-string= "3"     (%php-run-capture "<?php $a=[1,2]+[3,4,5]; echo count($a);"))
  (assert-string= "1,2,5" (%php-run-capture "<?php $a=[1,2]+[3,4,5]; echo implode(',',$a);"))
  ;; left operand wins on key conflicts; string keys preserved
  (assert-string= "12"    (%php-run-capture "<?php $a=['x'=>1]+['x'=>9,'y'=>2]; echo $a['x'].$a['y'];"))
  ;; integer keys preserved (not reindexed)
  (assert-string= "ac"    (%php-run-capture "<?php $a=[1=>'a']+[1=>'b',2=>'c']; echo $a[1].$a[2];"))
  (assert-string= "2"     (%php-run-capture "<?php $a=[]+[1,2]; echo count($a);"))
  ;; regression guards: numeric +, coercion, and array_merge are unaffected
  (assert-string= "5"     (%php-run-capture "<?php echo 2+3;"))
  (assert-string= "8"     (%php-run-capture "<?php echo '5'+3;"))
  (assert-string= "4"     (%php-run-capture "<?php echo count(array_merge([1,2],[3,4]));")))

(deftest php-e2e-intdiv-fdiv-array-is-list
  "intdiv (7.0), fdiv (8.0) and array_is_list (8.1) builtins.  intdiv and fdiv
were unregistered; array_is_list was registered as a LAMBDA, which the builtin
dispatch (resolving a function SYMBOL) could not call ('Undefined function')."
  (assert-string= "3"  (%php-run-capture "<?php echo intdiv(7,2);"))
  (assert-string= "-3" (%php-run-capture "<?php echo intdiv(-7,2);"))
  (assert-string= "3"  (%php-run-capture "<?php echo intdiv('10','3');"))
  (assert-string= "2.5" (%php-run-capture "<?php echo fdiv(10,4);"))
  (assert-string= "3"  (%php-run-capture "<?php echo fdiv(6,2);"))
  ;; array_is_list: sequential 0..n-1 keys
  (assert-string= "y"  (%php-run-capture "<?php echo array_is_list([1,2,3])?'y':'n';"))
  (assert-string= "n"  (%php-run-capture "<?php echo array_is_list([1=>'a',0=>'b'])?'y':'n';"))
  (assert-string= "n"  (%php-run-capture "<?php echo array_is_list(['x'=>1])?'y':'n';"))
  (assert-string= "y"  (%php-run-capture "<?php echo array_is_list([])?'y':'n';")))

(deftest php-e2e-math-non-cl-named-builtins
  "Math builtins whose names are NOT CL functions (fmod, atan2, log10, log2,
hypot, deg2rad, rad2deg, base_convert).  These were registered as LAMBDAs, so
the builtin dispatch — which lowers a call to a function SYMBOL and resolves it
only when fbound — left no fbound symbol and hit 'Undefined function'.  sin/cos/
log/exp escaped the bug only by colliding with inherited CL function names.  Now
registered by named %php- symbol."
  ;; fmod: remainder, sign follows the dividend (NOT CL mod, which would give 2)
  (assert-string= "1"  (%php-run-capture "<?php echo fmod(7,3);"))
  (assert-string= "-1" (%php-run-capture "<?php echo fmod(-7,3);"))
  ;; atan2(1,1) = pi/4
  (assert-string= "0.7854" (%php-run-capture "<?php echo round(atan2(1,1),4);"))
  (assert-string= "3"  (%php-run-capture "<?php echo log10(1000);"))
  (assert-string= "3"  (%php-run-capture "<?php echo log2(8);"))
  (assert-string= "5"  (%php-run-capture "<?php echo hypot(3,4);"))
  (assert-string= "3.14159" (%php-run-capture "<?php echo round(deg2rad(180),5);"))
  (assert-string= "180" (%php-run-capture "<?php echo round(rad2deg(3.141592653589793),2);"))
  ;; base_convert: lowercase digits, both directions
  (assert-string= "11111111" (%php-run-capture "<?php echo base_convert('ff',16,2);"))
  (assert-string= "ff" (%php-run-capture "<?php echo base_convert('255',10,16);"))
  ;; is_finite / is_infinite (also lambda-registered; same dispatch bug)
  (assert-string= "y" (%php-run-capture "<?php echo is_finite(1.5)?'y':'n';"))
  (assert-string= "y" (%php-run-capture "<?php echo is_infinite(fdiv(1,0))?'y':'n';")))

(deftest php-e2e-predefined-constants
  "Predefined PHP constants (PHP_EOL, PHP_INT_MAX, M_PI, STR_PAD_LEFT, SORT_*).
A bare identifier not followed by '(' was lowered to an ast-var — an undefined
global — so every constant read as the empty string.  Now php-parse-primary
consults *php-predefined-constants* and lowers a hit to its literal value."
  (assert-string= "9223372036854775807" (%php-run-capture "<?php echo PHP_INT_MAX;"))
  (assert-string= "8"       (%php-run-capture "<?php echo PHP_INT_SIZE;"))
  (assert-string= "3.14159" (%php-run-capture "<?php echo round(M_PI,5);"))
  (assert-string= "8.4.0"   (%php-run-capture "<?php echo PHP_VERSION;"))
  (assert-string= "2"       (%php-run-capture "<?php echo SORT_STRING;"))
  ;; PHP_EOL is a real newline
  (assert-string= "y" (%php-run-capture "<?php echo PHP_EOL===\"\\n\"?'y':'n';"))
  ;; STR_PAD_LEFT/RIGHT drive str_pad correctly (were empty -> broken padding)
  (assert-string= "005" (%php-run-capture "<?php echo str_pad('5',3,'0',STR_PAD_LEFT);"))
  (assert-string= "500" (%php-run-capture "<?php echo str_pad('5',3,'0',STR_PAD_RIGHT);"))
  ;; leading-backslash qualified reference resolves to the global constant
  (assert-string= "8" (%php-run-capture "<?php echo \\PHP_MAJOR_VERSION;"))
  ;; an UNKNOWN bare identifier still lowers to a var (no crash, empty value)
  (assert-string= "" (%php-run-capture "<?php echo NOT_A_REAL_CONSTANT_XYZ;")))

(deftest php-e2e-call-user-func
  "call_user_func / call_user_func_array over Closure, builtin-name, and
USER-function-name callables.  String callables for USER functions previously
failed ('Invalid function designator') because the resolver only checked the
builtin table; now %php-callable-user-function matches the VM function registry
by package-independent SYMBOL-NAME."
  ;; user function by string name
  (assert-string= "25" (%php-run-capture "<?php function sq($x){return $x*$x;} echo call_user_func('sq',5);"))
  (assert-string= "7"  (%php-run-capture "<?php function add($a,$b){return $a+$b;} echo call_user_func('add',3,4);"))
  ;; builtin by string name
  (assert-string= "HI" (%php-run-capture "<?php echo call_user_func('strtoupper','hi');"))
  ;; closure / arrow
  (assert-string= "101" (%php-run-capture "<?php $f=function($n){return $n+100;}; echo call_user_func($f,1);"))
  (assert-string= "21"  (%php-run-capture "<?php $g=fn($x)=>$x*3; echo call_user_func($g,7);"))
  ;; call_user_func_array spreads the array as positional args
  (assert-string= "30" (%php-run-capture "<?php function add($a,$b){return $a+$b;} echo call_user_func_array('add',[10,20]);"))
  (assert-string= "36" (%php-run-capture "<?php function sq($x){return $x*$x;} echo call_user_func_array('sq',[6]);")))

(deftest php-e2e-usort-in-place
  "usort/uasort/uksort take the array BY REFERENCE and sort IN PLACE.  The old
implementations built a fresh result array and returned it, so the caller's
variable was never updated and the sort silently did nothing.  Now they mutate
the passed hash-table (like sort/asort/ksort) and accept closure, arrow,
builtin-name and user-function-name comparators.  stable-sort matches PHP 8.0+."
  ;; usort with arrow comparators, ascending and descending
  (assert-string= "1,2,3" (%php-run-capture "<?php $a=[3,1,2]; usort($a, fn($x,$y)=>$x-$y); echo implode(',',$a);"))
  (assert-string= "3,2,1" (%php-run-capture "<?php $a=[3,1,2]; usort($a, fn($x,$y)=>$y-$x); echo implode(',',$a);"))
  ;; usort with a user-function-name string comparator
  (assert-string= "1,2,5,8" (%php-run-capture "<?php function cmp($a,$b){return $a-$b;} $c=[5,2,8,1]; usort($c,'cmp'); echo implode(',',$c);"))
  ;; usort with a builtin-name string comparator
  (assert-string= "apple,banana,cherry" (%php-run-capture "<?php $d=['banana','apple','cherry']; usort($d,'strcmp'); echo implode(',',$d);"))
  ;; uasort preserves keys, sorts by value
  (assert-string= "a,b,c" (%php-run-capture "<?php $e=['b'=>2,'a'=>1,'c'=>3]; uasort($e, fn($x,$y)=>$x-$y); echo implode(',',array_keys($e));"))
  ;; uksort sorts by key
  (assert-string= "apple,banana" (%php-run-capture "<?php $f=['banana'=>1,'apple'=>2]; uksort($f,'strcmp'); echo implode(',',array_keys($f));"))
  ;; stable: equal comparands keep insertion order
  (assert-string= "cab" (%php-run-capture "<?php $g=[['k'=>1,'v'=>'a'],['k'=>1,'v'=>'b'],['k'=>0,'v'=>'c']]; usort($g, fn($x,$y)=>$x['k']-$y['k']); echo $g[0]['v'].$g[1]['v'].$g[2]['v'];")))

(deftest php-e2e-serialize-unserialize
  "serialize/unserialize produce and parse PHP's native serialization format.
They were registered as LAMBDA stubs (serialize only handled strings;
unserialize just stringified) and, being non-CL-named, also failed dispatch
('Undefined function: SERIALIZE').  Now proper recursive %php-serialize /
%php-unserialize registered by symbol."
  ;; scalars
  (assert-string= "i:42;"        (%php-run-capture "<?php echo serialize(42);"))
  (assert-string= "s:5:\"hello\";" (%php-run-capture "<?php echo serialize('hello');"))
  (assert-string= "b:1;"         (%php-run-capture "<?php echo serialize(true);"))
  (assert-string= "b:0;"         (%php-run-capture "<?php echo serialize(false);"))
  (assert-string= "N;"           (%php-run-capture "<?php echo serialize(null);"))
  (assert-string= "d:3.14;"      (%php-run-capture "<?php echo serialize(3.14);"))
  ;; arrays — list and assoc, matching PHP's exact format
  (assert-string= "a:3:{i:0;i:1;i:1;i:2;i:2;i:3;}"
                  (%php-run-capture "<?php echo serialize([1,2,3]);"))
  (assert-string= "a:2:{s:1:\"a\";i:1;s:1:\"b\";i:2;}"
                  (%php-run-capture "<?php echo serialize(['a'=>1,'b'=>2]);"))
  ;; round-trip: scalars and nested arrays
  (assert-string= "50" (%php-run-capture "<?php echo unserialize(serialize(42))+8;"))
  (assert-string= "v"  (%php-run-capture "<?php $x=unserialize(serialize([1,2,['k'=>'v']])); echo $x[2]['k'];"))
  (assert-string= "T"  (%php-run-capture "<?php echo unserialize('b:1;')?'T':'F';"))
  ;; malformed input -> false
  (assert-string= "F"  (%php-run-capture "<?php echo unserialize('garbage')?'T':'F';")))

(deftest php-e2e-string-escape-preservation
  "PHP double-quoted strings keep the backslash for UNRECOGNIZED escapes (PHP
semantics), while still processing recognized ones.  Previously every unknown
escape dropped its backslash, so \"/\\d/\" lexed to \"/d/\" and regex character
classes (\\d \\w \\s) never matched."
  ;; unrecognized escapes keep the backslash
  (assert-string= "a\\.b"  (%php-run-capture "<?php echo \"a\\.b\";"))
  (assert-string= "\\d\\w" (%php-run-capture "<?php echo \"\\d\\w\";"))
  ;; recognized escapes still process
  (assert-string= "3" (%php-run-capture "<?php echo strlen(\"a\\nb\");"))   ; a + newline + b
  (assert-string= "A" (%php-run-capture "<?php echo \"\\x41\";"))           ; hex
  (assert-string= "H" (%php-run-capture "<?php echo \"\\u{48}\";"))         ; unicode
  (assert-string= "1" (%php-run-capture "<?php echo strlen(\"\\t\");")))    ; tab

(deftest php-e2e-preg-replace-callback
  "preg_replace_callback / _array.  preg_replace_callback called a non-existent
%php-regex-search and never stripped the /.../ delimiters, so every call raised
'The function %PHP-REGEX-SEARCH is undefined.'  preg_replace_callback_array had
the wrong arity (separate patterns/callbacks args instead of one map)."
  ;; the callback receives $matches[0] = full match and returns the replacement
  (assert-string= "a2b4" (%php-run-capture "<?php echo preg_replace_callback('/\\d/', fn($m)=>$m[0]*2, 'a1b2');"))
  (assert-string= "HI THERE" (%php-run-capture "<?php echo preg_replace_callback('/[a-z]+/', fn($m)=>strtoupper($m[0]), 'hi there');"))
  ;; limit caps the number of replacements
  (assert-string= "XX34" (%php-run-capture "<?php echo preg_replace_callback('/\\d/', fn($m)=>'X', '1234', 2);"))
  ;; works with the double-quoted pattern too (escape-preservation fix)
  (assert-string= "a2b4" (%php-run-capture "<?php echo preg_replace_callback(\"/\\d/\", fn($m)=>$m[0]*2, 'a1b2');"))
  ;; preg_replace_callback_array: a single [pattern => callback] map
  (assert-string= "LNLN" (%php-run-capture "<?php echo preg_replace_callback_array(['/\\d/'=>fn($m)=>'N','/[a-z]/'=>fn($m)=>'L'], 'a1b2');")))

(deftest php-e2e-preg-capture-groups
  "Capture groups in the regex engine: $1/$2/${1}/\\1/$0 backreferences in
preg_replace, and a corrected preg_match_all count.  The greedy non-backtracking
matcher now records each capturing group's span, so backreferences resolve."
  ;; $1/$2 swap and reorder
  (assert-string= "badc"  (%php-run-capture "<?php echo preg_replace('/(\\w)(\\w)/', '$2$1', 'abcd');"))
  (assert-string= "34/12" (%php-run-capture "<?php echo preg_replace('/(\\d+)-(\\d+)/', '$2/$1', '12-34');"))
  ;; $0 = whole match, ${1} brace form
  (assert-string= "a[12]b" (%php-run-capture "<?php echo preg_replace('/\\d+/', '[$0]', 'a12b');"))
  (assert-string= "hi!"    (%php-run-capture "<?php echo preg_replace('/(\\w+)/', '${1}!', 'hi');"))
  ;; nested groups number left-to-right by opening paren
  (assert-string= "4-2"    (%php-run-capture "<?php echo preg_replace('/((\\d)(\\d))/', '$2-$3', '42');"))
  ;; preg_match_all counts ALL matches (was 1 — anchored from index 0)
  (assert-string= "3" (%php-run-capture "<?php echo preg_match_all('/\\d/', '1a2b3');"))
  (assert-string= "3" (%php-run-capture "<?php echo preg_match_all('/\\w+/', 'foo bar baz');"))
  (assert-string= "0" (%php-run-capture "<?php echo preg_match_all('/\\d/', 'abc');")))

(deftest php-e2e-preg-match-out-param
  "preg_match($p,$s,$m) / preg_match_all populate the $matches out-parameter in
the caller's variable (a FRESH variable — no pre-declaration needed).  A
call-site transform assigns $m = the matches array (returned by value) and
yields the count.  Relies on ast-setq auto-declaring an unknown variable as a
global; a ref box could not be used (the VM copies host structs across the
bridge)."
  ;; capture groups land in $m[1], $m[2], full match in $m[0]
  (assert-string= "12|34" (%php-run-capture "<?php preg_match('/(\\d+)-(\\d+)/', '12-34', $m); echo $m[1].'|'.$m[2];"))
  (assert-string= "1:123:123" (%php-run-capture "<?php $r=preg_match('/(\\d+)/', 'abc123', $m); echo $r.':'.$m[0].':'.$m[1];"))
  (assert-string= "bob at host" (%php-run-capture "<?php preg_match('/(\\w+)@(\\w+)/', 'bob@host', $m); echo $m[1].' at '.$m[2];"))
  ;; the return value is still the count (0 when no match)
  (assert-string= "0" (%php-run-capture "<?php echo preg_match('/\\d/', 'abc', $m);"))
  ;; preg_match_all populates $matches in PREG_PATTERN_ORDER
  (assert-string= "1,2,3" (%php-run-capture "<?php preg_match_all('/(\\d)/', '1a2b3', $m); echo implode(',', $m[1]);"))
  (assert-string= "12,34" (%php-run-capture "<?php preg_match_all('/\\d+/', 'a12b34', $m); echo implode(',', $m[0]);")))

(deftest php-e2e-ucwords-delimiters
  "ucwords uppercases the first letter of each word.  The default delimiter set
was the CL literal \" \\t\\r\\n\\f\\v\", which CL does NOT escape — it read as the
letters trnfv, so any 'r' counted as a word boundary and ucwords('world')
returned 'WorLd'.  The defaults are now built from real control characters."
  (assert-string= "Hello World"      (%php-run-capture "<?php echo ucwords('hello world');"))
  ;; words containing r/t/n/f/v must NOT get interior capitals
  (assert-string= "World Order Roar" (%php-run-capture "<?php echo ucwords('world order roar');"))
  (assert-string= "Fluffy Vivid"     (%php-run-capture "<?php echo ucwords('fluffy vivid');"))
  ;; existing capitals are preserved (ucwords only touches word-initial chars)
  (assert-string= "The QUICK Brown"  (%php-run-capture "<?php echo ucwords('the QUICK brown');"))
  ;; explicit custom delimiter still works
  (assert-string= "Hello-World"      (%php-run-capture "<?php echo ucwords('hello-world', '-');")))

(deftest php-e2e-wordwrap-cut
  "wordwrap with the cut-long-words flag force-breaks a word longer than the
width into width-sized pieces.  The flag was declared ignored, so over-long
words were never broken."
  (assert-string= "aaa|aaa" (%php-run-capture "<?php echo wordwrap('aaaaaa',3,'|',true);"))
  (assert-string= "a|very|long|word|b" (%php-run-capture "<?php echo wordwrap('a verylongword b',4,'|',true);"))
  (assert-string= "A very|long|wooooooo|ord." (%php-run-capture "<?php echo wordwrap('A very long woooooooord.',8,'|',true);"))
  ;; without cut, an over-long word overflows its own line (no leading break)
  (assert-string= "aaaaaa" (%php-run-capture "<?php echo wordwrap('aaaaaa',3,'|',false);"))
  ;; ordinary space-wrapping unaffected
  (assert-string= "aaa|bbb|ccc" (%php-run-capture "<?php echo wordwrap('aaa bbb ccc',5,'|');"))
  (assert-string= "The quick|brown fox" (%php-run-capture "<?php echo wordwrap('The quick brown fox',10,'|');")))

(deftest php-e2e-json-encode-pretty
  "json_encode honours JSON_PRETTY_PRINT (4-space indent, newlines, space after
the object colon).  The flags argument was previously declared ignored, so the
flag produced compact output."
  ;; compact output unchanged (no flag)
  (assert-string= "{\"a\":1,\"b\":[2,3]}"
                  (%php-run-capture "<?php echo json_encode(['a'=>1,'b'=>[2,3]]);"))
  ;; pretty object with a nested array
  (assert-string= (format nil "{~%    \"a\": 1,~%    \"b\": [~%        2,~%        3~%    ]~%}")
                  (%php-run-capture "<?php echo json_encode(['a'=>1,'b'=>[2,3]],JSON_PRETTY_PRINT);"))
  ;; pretty list
  (assert-string= (format nil "[~%    1,~%    2,~%    3~%]")
                  (%php-run-capture "<?php echo json_encode([1,2,3],JSON_PRETTY_PRINT);"))
  ;; empty array stays inline even in pretty mode
  (assert-string= (format nil "{~%    \"x\": [],~%    \"y\": 1~%}")
                  (%php-run-capture "<?php echo json_encode(['x'=>[],'y'=>1],JSON_PRETTY_PRINT);")))

(deftest php-e2e-json-decode
  "json_decode parses JSON objects, strings, nested structures, and scalars.
The decoder was fundamentally broken: parse-string returned an empty fresh
stream (every string -> \"\"), and the parse functions never threaded the cursor
(faked it with (+ pos 1)), so objects and any multi-char/nested value failed.
Objects decode to associative arrays."
  (assert-string= "3"       (%php-run-capture "<?php $d=json_decode('{\"a\":1,\"b\":2}',true); echo $d['a']+$d['b'];"))
  (assert-string= "Bob-30"  (%php-run-capture "<?php $d=json_decode('{\"name\":\"Bob\",\"age\":30}',true); echo $d['name'].'-'.$d['age'];"))
  (assert-string= "3"       (%php-run-capture "<?php $d=json_decode('{\"x\":{\"y\":[1,2,3]}}',true); echo $d['x']['y'][2];"))
  (assert-string= "bb3"     (%php-run-capture "<?php $d=json_decode('[\"a\",\"bb\",\"ccc\"]'); echo $d[1].strlen($d[2]);"))
  ;; scalars and malformed input
  (assert-string= "y"       (%php-run-capture "<?php echo json_decode('true')?'y':'n';"))
  (assert-string= "null"    (%php-run-capture "<?php echo json_decode('not json')===null?'null':'x';"))
  ;; round-trip through encode
  (assert-string= "alice:editor:y"
                  (%php-run-capture "<?php $o=['user'=>'alice','roles'=>['admin','editor'],'active'=>true]; $r=json_decode(json_encode($o),true); echo $r['user'].':'.$r['roles'][1].':'.($r['active']?'y':'n');")))

(deftest php-e2e-number-format-rounding
  "number_format rounds half AWAY FROM ZERO (PHP semantics), not banker's
half-to-even.  It used CL ROUND, so number_format(2.5) gave 2 and
number_format(1234.5) gave 1,234 instead of 3 and 1,235."
  (assert-string= "1,235"  (%php-run-capture "<?php echo number_format(1234.5);"))
  (assert-string= "3"      (%php-run-capture "<?php echo number_format(2.5);"))
  (assert-string= "1"      (%php-run-capture "<?php echo number_format(0.5);"))
  (assert-string= "1,234"  (%php-run-capture "<?php echo number_format(1234.4);"))
  (assert-string= "-1,235" (%php-run-capture "<?php echo number_format(-1234.5);"))
  ;; decimals and custom separators unaffected
  (assert-string= "3.14"       (%php-run-capture "<?php echo number_format(3.14159,2);"))
  (assert-string= "1,234.57"   (%php-run-capture "<?php echo number_format(1234.567,2);"))
  (assert-string= "1.234,50"   (%php-run-capture "<?php echo number_format(1234.5,2,',','.');")))

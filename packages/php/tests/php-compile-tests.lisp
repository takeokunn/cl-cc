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

;;;; packages/php/tests/php-traits-tests.lisp — PHP Trait tests
;;;;
;;;; Coverage for trait declarations and class-use-trait via parse-php-source.
;;;; Exercises parser-trait.lisp: trait bodies, use-trait statements,
;;;; conflict resolution (insteadof), method aliasing (as), visibility change.
(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers (reuse %php-first from php-parser-tests.lisp) ──────────────────

;;; ─── Trait Definition ────────────────────────────────────────────────────────

(deftest php-trait-definition-produces-ast-defclass
  "trait T {} lowers to ast-defclass with :php-kind :trait."
  (let ((ast (first (cl-cc/php:parse-php-source "<?php trait Greetable { }"))))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-eq :trait (cl-cc:ast-defclass-php-kind ast))))

(deftest php-trait-name-is-upcased
  "trait name is stored as an upcased symbol consistent with class naming."
  (let ((ast (first (cl-cc/php:parse-php-source "<?php trait HasTimestamps { }"))))
    (assert-string= "HASTIMESTAMPS"
                    (symbol-name (cl-cc:ast-defclass-name ast)))))

(deftest php-trait-with-method-produces-slot-def
  "trait body with a public method lowers the method to an ast-slot-def."
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php trait Greetable { public function greet() { return 'hi'; } }")))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-true (plusp (length slots)))
    (assert-true (every #'cl-cc:ast-slot-def-p slots))))

(deftest php-trait-method-name-is-upcased
  "Method name inside a trait slot-def is stored as an upcased symbol."
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php trait Greetable { public function greet() { return 'hello'; } }")))
         (slot  (first (cl-cc:ast-defclass-slots ast))))
    (assert-string= "GREET" (symbol-name (cl-cc:ast-slot-name slot)))))

(deftest php-trait-with-abstract-method
  "An abstract method in a trait lowers to a slot-def without a concrete body."
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php trait HasLogger { abstract public function log(string $msg): void; }")))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-eq :trait (cl-cc:ast-defclass-php-kind ast))
    (assert-true (plusp (length slots)))))

(deftest php-trait-no-superclasses
  "Trait definitions never have a superclasses list."
  (let ((ast (first (cl-cc/php:parse-php-source "<?php trait Mixin { }"))))
    (assert-null (cl-cc:ast-defclass-superclasses ast))))

;;; ─── Class Using a Trait ─────────────────────────────────────────────────────

(deftest php-class-use-single-trait-produces-use-slot
  "class using one trait inserts a use-trait slot-def into the class body."
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait Greetable { } class Greeter { use Greetable; }"))
         (class (second asts))
         (slots (cl-cc:ast-defclass-slots class)))
    (assert-true (cl-cc:ast-defclass-p class))
    (assert-true (some (lambda (s)
                         (getf (cl-cc:ast-imports s) :php-trait-use))
                       slots))))

(deftest php-class-use-single-trait-records-trait-name
  "The use-trait slot-def records the upcased trait symbol in :php-trait-names."
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait Greetable { } class Greeter { use Greetable; }"))
         (class (second asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class))))
    (assert-true use-slot)
    (let ((names (getf (cl-cc:ast-imports use-slot) :php-trait-names)))
      (assert-= 1 (length names))
      (assert-string= "GREETABLE" (symbol-name (first names))))))

;;; ─── Multiple Traits ─────────────────────────────────────────────────────────

(deftest php-class-use-multiple-traits
  "class using two traits records both names in a single use-trait slot-def."
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { } trait B { } class C { use A, B; }"))
         (class (third asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (names (getf (cl-cc:ast-imports use-slot) :php-trait-names)))
    (assert-= 2 (length names))
    (assert-true (find "A" names :key #'symbol-name :test #'string=))
    (assert-true (find "B" names :key #'symbol-name :test #'string=))))

;;; ─── Conflict Resolution: insteadof ─────────────────────────────────────────

(deftest php-trait-insteadof-conflict-resolution
  "use A, B { A::hello insteadof B; } records the exclusion in :php-insteadof."
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { public function hello() { return 'A'; } }
                  trait B { public function hello() { return 'B'; } }
                  class C { use A, B { A::hello insteadof B; } }"))
         (class (third asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (insteadof-list (getf (cl-cc:ast-imports use-slot) :php-insteadof)))
    (assert-true (plusp (length insteadof-list)))
    (let ((entry (first insteadof-list)))
      (assert-string= "HELLO" (symbol-name (getf entry :method)))
      (assert-string= "A"     (symbol-name (getf entry :from))))))

(deftest php-trait-insteadof-records-excluded-traits
  "insteadof target trait names are recorded in the :exclude list of the entry."
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { public function hello() { return 'A'; } }
                  trait B { public function hello() { return 'B'; } }
                  class C { use A, B { A::hello insteadof B; } }"))
         (class (third asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (insteadof-list (getf (cl-cc:ast-imports use-slot) :php-insteadof))
         (excluded       (getf (first insteadof-list) :exclude)))
    (assert-true (find "B" excluded :key #'symbol-name :test #'string=))))

;;; ─── Method Aliasing: as ─────────────────────────────────────────────────────

(deftest php-trait-as-alias-records-alias-name
  "use A { A::hello as hi; } records the alias in :php-alias."
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { public function hello() { return 'A'; } }
                  class C { use A { A::hello as hi; } }"))
         (class (second asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (alias-list (getf (cl-cc:ast-imports use-slot) :php-alias)))
    (assert-true (plusp (length alias-list)))
    (let ((entry (first alias-list)))
      (assert-string= "HI" (symbol-name (getf entry :alias))))))

(deftest php-trait-as-alias-records-source-method
  "The :method field of an alias entry matches the original method name."
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { public function hello() { return 'A'; } }
                  class C { use A { A::hello as hi; } }"))
         (class (second asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (alias-list (getf (cl-cc:ast-imports use-slot) :php-alias))
         (entry (first alias-list)))
    (assert-string= "HELLO" (symbol-name (getf entry :method)))))

;;; ─── Visibility Change ───────────────────────────────────────────────────────

(deftest php-trait-as-visibility-change
  "use A { hello as protected; } records the new visibility in the alias entry."
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { public function hello() { return 'A'; } }
                  class C { use A { hello as protected; } }"))
         (class (second asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (alias-list (getf (cl-cc:ast-imports use-slot) :php-alias))
         (entry      (first alias-list)))
    (assert-eq :protected (getf entry :vis))))

(deftest php-trait-as-visibility-and-alias
  "use A { A::hello as protected greeting; } records both vis and alias."
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { public function hello() { return 'A'; } }
                  class C { use A { A::hello as protected greeting; } }"))
         (class (second asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (alias-list (getf (cl-cc:ast-imports use-slot) :php-alias))
         (entry      (first alias-list)))
    (assert-eq :protected (getf entry :vis))
    (assert-string= "GREETING" (symbol-name (getf entry :alias)))))

;;; ─── Trait Registry ──────────────────────────────────────────────────────────

(deftest php-trait-definition-registers-methods
  "Parsing a trait stores its method slots in *php-trait-registry*."
  (cl-cc/php:parse-php-source
   "<?php trait RegisterMe { public function act() { return 1; } }")
  (let ((registry-entry (gethash "REGISTERME" cl-cc/php:*php-trait-registry*)))
    (assert-true (listp registry-entry))
    (assert-true (plusp (length registry-entry)))))

(deftest php-trait-is-supported-by-check
  "Trait AST nodes pass php-check-supported-forms without signalling an error."
  (let ((ast (first (cl-cc/php:parse-php-source "<?php trait Mixin { }"))))
    (assert-eq :trait (cl-cc:ast-defclass-php-kind ast))
    ;; Must not signal an error — traits are supported
    (assert-true (cl-cc/php:php-check-supported-forms (list ast)))))

(deftest php-trait-with-property
  "A property slot inside a trait is parsed as an instance-allocation slot-def."
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php trait HasName { public string $name; }")))
         (slots (cl-cc:ast-defclass-slots ast))
         (prop  (first slots)))
    (assert-eq :trait (cl-cc:ast-defclass-php-kind ast))
    (assert-true (cl-cc:ast-slot-def-p prop))
    ;; Property slot names use the SAME symbol convention as member access
    ;; ($obj->name interns via php-ident-sym, upcased), so the declared slot name
    ;; matches how it is read/written. ($name -> NAME.) Without this they mismatched
    ;; and $obj->name raised "slot NAME is missing".
    (assert-string= "NAME" (symbol-name (cl-cc:ast-slot-name prop)))))

(deftest php-trait-multiple-methods
  "A trait with several methods produces one slot-def per method."
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php trait Multi {
                           public function foo() { return 1; }
                           public function bar() { return 2; }
                           public function baz() { return 3; }
                         }")))
         (slots (cl-cc:ast-defclass-slots ast))
         (names (mapcar (lambda (s) (symbol-name (cl-cc:ast-slot-name s))) slots)))
    (assert-eq :trait (cl-cc:ast-defclass-php-kind ast))
    (assert-= 3 (length slots))
    (assert-true (member "FOO" names :test #'string=))
    (assert-true (member "BAR" names :test #'string=))
    (assert-true (member "BAZ" names :test #'string=))))

(deftest php-class-use-trait-with-no-conflict-block
  "A simple use TraitName; without a conflict block leaves :php-insteadof empty."
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait Simple { public function act() { return 1; } }
                  class Worker { use Simple; }"))
         (class (second asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (insteadof-list (getf (cl-cc:ast-imports use-slot) :php-insteadof)))
    (assert-true use-slot)
    (assert-null insteadof-list)))

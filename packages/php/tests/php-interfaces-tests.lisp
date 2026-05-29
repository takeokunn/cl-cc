;;;; packages/php/tests/php-interfaces-tests.lisp — PHP Interface tests
;;;;
;;;; Coverage for interface declarations via parse-php-source.
;;;; Exercises parser-interface.lisp: definition, single/multiple implements,
;;;; interface extends, interface constants, abstract method signatures.
(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Interface Definition ────────────────────────────────────────────────────

(deftest php-interface-definition-produces-ast-defclass
  "interface I {} lowers to ast-defclass with :php-kind :interface."
  (let ((ast (first (cl-cc/php:parse-php-source "<?php interface Countable { }"))))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-eq :interface (cl-cc:ast-defclass-php-kind ast))))

(deftest php-interface-name-is-upcased
  "Interface name is stored as an upcased symbol consistent with class naming."
  (let ((ast (first (cl-cc/php:parse-php-source "<?php interface JsonSerializable { }"))))
    (assert-string= "JSONSERIALIZABLE"
                    (symbol-name (cl-cc:ast-defclass-name ast)))))

(deftest php-interface-is-supported-by-check
  "Interface AST nodes pass php-check-supported-forms without signalling an error."
  (let ((ast (first (cl-cc/php:parse-php-source "<?php interface Loggable { }"))))
    (assert-eq :interface (cl-cc:ast-defclass-php-kind ast))
    (assert-true (cl-cc/php:php-check-supported-forms (list ast)))))

;;; ─── Abstract Method Signatures ─────────────────────────────────────────────

(deftest php-interface-abstract-method-is-registered
  "An abstract method in an interface body is stored in *php-interface-registry*."
  (let* ((ast (first (cl-cc/php:parse-php-source
                      "<?php interface HasIdMeth { public function getId(): int; }")))
         ;; Use the exact symbol produced by the parser for registry lookup.
         (iface-sym (cl-cc:ast-defclass-name ast))
         (record (gethash iface-sym cl-cc/php:*php-interface-registry*)))
    (assert-true record)
    (assert-true (plusp (length (getf record :methods))))))

(deftest php-interface-method-signature-captures-name
  "The method signature plist stores the upcased method name symbol."
  (let* ((ast (first (cl-cc/php:parse-php-source
                      "<?php interface RenderableIface { public function render(): string; }")))
         (iface-sym (cl-cc:ast-defclass-name ast))
         (sigs (getf (gethash iface-sym cl-cc/php:*php-interface-registry*) :methods)))
    (assert-true sigs)
    (assert-true (some (lambda (s) (string= "RENDER" (symbol-name (getf s :name)))) sigs))))

(deftest php-interface-method-signature-captures-return-type
  "The method signature plist preserves the declared return type string."
  (let* ((ast (first (cl-cc/php:parse-php-source
                      "<?php interface LabeledIface { public function label(): string; }")))
         (iface-sym (cl-cc:ast-defclass-name ast))
         (sigs (getf (gethash iface-sym cl-cc/php:*php-interface-registry*) :methods))
         (sig  (first sigs)))
    (assert-true (stringp (getf sig :return-type)))))

(deftest php-interface-empty-body-has-no-methods
  "An interface with an empty body registers with an empty methods list."
  (let* ((ast (first (cl-cc/php:parse-php-source "<?php interface EmptyIfaceX { }")))
         (iface-sym (cl-cc:ast-defclass-name ast))
         (record (gethash iface-sym cl-cc/php:*php-interface-registry*)))
    (assert-true record)
    (assert-null (getf record :methods))))

;;; ─── Interface Constants ─────────────────────────────────────────────────────

(deftest php-interface-constant-is-slot-def
  "Interface constants are stored as :class allocation slot-defs with :php-class-constant."
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php interface HasVersion { const VERSION = '1.0'; }")))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-eq :interface (cl-cc:ast-defclass-php-kind ast))
    (assert-true (plusp (length slots)))
    (let ((const-slot (first slots)))
      (assert-true (cl-cc:ast-slot-def-p const-slot))
      (assert-eq :class (cl-cc:ast-slot-allocation const-slot))
      (assert-true (getf (cl-cc:ast-imports const-slot) :php-class-constant)))))

(deftest php-interface-constant-name-is-upcased
  "Interface constant name is stored as an upcased symbol."
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php interface Colors { const RED = 'red'; }")))
         (slot  (first (cl-cc:ast-defclass-slots ast))))
    (assert-string= "RED" (symbol-name (cl-cc:ast-slot-name slot)))))

(deftest php-interface-typed-constant
  "Interface typed constants preserve the type annotation on the slot-def."
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php interface Spec { const int LIMIT = 100; }")))
         (slot  (first (cl-cc:ast-defclass-slots ast))))
    (assert-true (stringp (cl-cc:ast-slot-type slot)))))

;;; ─── Interface Extends ───────────────────────────────────────────────────────

(deftest php-interface-extends-single-parent
  "interface B extends A records A as a parent in the superclasses slot."
  (let* ((asts   (cl-cc/php:parse-php-source
                  "<?php interface A { } interface B extends A { }"))
         (iface-b (second asts))
         (supers  (cl-cc:ast-defclass-superclasses iface-b)))
    (assert-eq :interface (cl-cc:ast-defclass-php-kind iface-b))
    (assert-= 1 (length supers))
    (assert-string= "A" (symbol-name (first supers)))))

(deftest php-interface-extends-multiple-parents
  "Interfaces may extend multiple parent interfaces (multiple inheritance)."
  (let* ((asts   (cl-cc/php:parse-php-source
                  "<?php interface A { } interface B { } interface C extends A, B { }"))
         (iface-c (third asts))
         (supers  (cl-cc:ast-defclass-superclasses iface-c)))
    (assert-= 2 (length supers))
    (assert-true (find "A" supers :key #'symbol-name :test #'string=))
    (assert-true (find "B" supers :key #'symbol-name :test #'string=))))

;;; ─── Class Implements ────────────────────────────────────────────────────────

(deftest php-class-implements-single-interface
  "class Foo implements Bar records BAR in the superclasses list."
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php class Foo implements Bar { }")))
         (supers (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-true (member "BAR" supers :test #'string=))))

(deftest php-class-implements-multiple-interfaces
  "class Foo implements A, B records both A and B in superclasses in order."
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php class Foo implements IfaceA, IfaceB { }")))
         (names (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (assert-equal '("IFACEA" "IFACEB") names)))

(deftest php-class-extends-and-implements
  "class Foo extends Bar implements A, B records all in superclasses: Bar then A, B."
  (let* ((ast    (first (cl-cc/php:parse-php-source
                         "<?php class Box extends Base implements Storable, Countable { }")))
         (supers (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (assert-true (member "BASE"       supers :test #'string=))
    (assert-true (member "STORABLE"   supers :test #'string=))
    (assert-true (member "COUNTABLE"  supers :test #'string=))))

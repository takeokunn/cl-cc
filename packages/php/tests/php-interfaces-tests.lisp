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

(deftest php-interface-method-signature-captures-return-by-reference
  "Interface method signatures preserve PHP's function &name() return marker."
  (let* ((ast (first (cl-cc/php:parse-php-source
                      "<?php interface RefSourceIface { public function &current(): mixed; }")))
         (iface-sym (cl-cc:ast-defclass-name ast))
         (sigs (getf (gethash iface-sym cl-cc/php:*php-interface-registry*) :methods))
         (sig (first sigs)))
    (assert-true sig)
    (assert-string= "CURRENT" (symbol-name (getf sig :name)))
    (assert-equal "mixed" (getf sig :return-type))
    (assert-true (getf sig :returns-by-ref))))

(deftest php-interface-method-signature-captures-parameter-metadata
  "Interface method signatures preserve modern PHP parameter metadata."
  (let* ((ast (first (cl-cc/php:parse-php-source
                      "<?php interface SinkIface { #[Trace] public function write(#[Sensitive] string &$message, int $limit = 10, ...$rest): void; }")))
         (iface-sym (cl-cc:ast-defclass-name ast))
         (sigs (getf (gethash iface-sym cl-cc/php:*php-interface-registry*) :methods))
         (sig (first sigs))
         (params (getf sig :params))
         (param-types (getf sig :param-types))
         (param-defaults (getf sig :param-defaults))
         (default-ast (cdr (assoc (second params) param-defaults :test #'eq))))
    (assert-true sig)
    (assert-equal '(:public) (getf sig :modifiers))
    (assert-= 2 (length params))
    (assert-equal '(0) (getf sig :by-ref-indices))
    (assert-string= "message" (symbol-name (first params)))
    (assert-string= "rest" (symbol-name (getf sig :variadic-param)))
    (assert-equal "string" (cdr (assoc (first params) param-types :test #'eq)))
    (assert-equal "int" (cdr (assoc (second params) param-types :test #'eq)))
    (assert-true (cl-cc:ast-int-p default-ast))
    (assert-= 10 (cl-cc:ast-int-value default-ast))
    (assert-eq (first params) (first (first (getf sig :param-attributes))))
    (assert-true (getf (rest (first (getf sig :param-attributes))) :php-attributes))
    (assert-string= "Trace" (cl-cc/php:php-attribute-name (first (getf sig :attributes))))))

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

(deftest php-interface-multiple-constants-in-one-declaration
  "Interface const declarations may contain several constants sharing one optional type."
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php interface SpecMulti { const int MIN = 1, MAX = 10; }")))
         (slots (cl-cc:ast-defclass-slots ast))
         (record (gethash (cl-cc/php::php-ident-sym "SpecMulti")
                          cl-cc/php:*php-interface-registry*)))
    (assert-= 2 (length slots))
    (assert-equal '("MIN" "MAX")
                  (mapcar (lambda (slot)
                            (symbol-name (cl-cc:ast-slot-name slot)))
                          slots))
    (assert-equal '("int" "int") (mapcar #'cl-cc:ast-slot-type slots))
    (assert-= 2 (length (getf record :constants)))))

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

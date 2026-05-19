;;;; tests/unit/compile/builtin-registry-data-tests.lisp — Builtin Registry Data tests
(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest-each builtin-registry-data-table-sizes
  "The raw registry tables retain representative sizes."
  :cases (("unary"      cl-cc/compile::*builtin-unary-entries*      100)
          ("binary"     cl-cc/compile::*builtin-binary-entries*     20)
          ("string-cmp" cl-cc/compile::*builtin-string-cmp-entries* 10))
  (table min-size)
  (assert-true (> (length table) min-size)))

;; "car" and "mod" ctors are covered by builtin-registry-constructor-symbols
;; in builtin-registry-tests.lisp. "set" and "string=" are not covered there.
(deftest-each builtin-registry-data-representative-mappings
  "Representative raw mappings stay wired to the expected constructors."
  :cases (("set"    'set    cl-cc/compile::*builtin-binary-entries*     'cl-cc::make-vm-set-symbol-value)
          ("string=" 'string= cl-cc/compile::*builtin-string-cmp-entries* 'cl-cc::make-vm-string=)
          ("find-package" 'find-package cl-cc/compile::*builtin-unary-entries* 'cl-cc::make-vm-find-package))
  (sym table expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym table))))

(deftest-each builtin-registry-data-float-mappings
  "Float builtin raw mappings resolve to the VM constructors exported for compile-time lookup."
  :cases (("float" 'float 'cl-cc::make-vm-float-inst)
          ("float-precision" 'float-precision 'cl-cc::make-vm-float-precision)
          ("float-radix" 'float-radix 'cl-cc::make-vm-float-radix)
          ("float-sign" 'float-sign 'cl-cc::make-vm-float-sign)
          ("float-digits" 'float-digits 'cl-cc::make-vm-float-digits)
          ("decode-float" 'decode-float 'cl-cc::make-vm-decode-float)
           ("integer-decode-float" 'integer-decode-float 'cl-cc::make-vm-integer-decode-float))
  (sym expected-ctor)
  (assert-eq expected-ctor (cdr (assoc sym cl-cc/compile::*builtin-unary-entries*))))

(deftest fr-183-known-function-property-db-classifies-representatives
  "FR-183: known function property DB exposes pure, foldable, nonnegative, and effect facts."
  :tags '(:fr-183)
  (assert-true (cl-cc/optimize:known-function-property-p '+ :pure))
  (assert-true (cl-cc/optimize:known-function-property-p '+ :foldable))
  (assert-true (cl-cc/optimize:known-function-property-p 'length :read-only))
  (assert-true (cl-cc/optimize:known-function-property-p 'length :nonneg-result))
  (assert-true (cl-cc/optimize:known-function-property-p 'eq :always-returns))
  (assert-true (cl-cc/optimize:known-function-property-p 'eq :no-escape))
  (assert-eq :pure (cl-cc/optimize:known-function-effect-kind '+))
  (assert-eq :read-only (cl-cc/optimize:known-function-effect-kind 'length))
  (assert-eq :alloc (cl-cc/optimize:known-function-effect-kind 'cons))
  (assert-eq :io (cl-cc/optimize:known-function-effect-kind 'princ))
  (assert-eq :write-global (cl-cc/optimize:known-function-effect-kind 'rplaca))
  (assert-eq :control (cl-cc/optimize:known-function-effect-kind 'error)))

(deftest fr-183-known-function-property-db-stays-conservative
  "FR-183: unsafe properties are absent for mutable reads, capture/allocation, may-signal, and unknown functions."
  :tags '(:fr-183)
  (assert-false (cl-cc/optimize:known-function-property-p 'car :pure))
  (assert-false (cl-cc/optimize:known-function-property-p 'car :no-escape))
  (assert-false (cl-cc/optimize:known-function-property-p 'cons :no-escape))
  (assert-false (cl-cc/optimize:known-function-property-p 'append :no-escape))
  (assert-false (cl-cc/optimize:known-function-property-p 'complement :pure))
  (assert-false (cl-cc/optimize:known-function-property-p 'complement :no-escape))
  (assert-eq :alloc (cl-cc/optimize:known-function-effect-kind 'complement))
  (assert-false (cl-cc/optimize:known-function-property-p 'mod :always-returns))
  (assert-false (cl-cc/optimize:known-function-property-p '/ :always-returns))
  (assert-null (cl-cc/optimize:known-function-properties 'not-a-known-function))
  (assert-eq :unknown (cl-cc/optimize:known-function-effect-kind 'not-a-known-function)))

(deftest fr-183-builtin-registry-stores-properties-on-every-entry
  "FR-183: every compile builtin registry entry carries property metadata."
  :tags '(:fr-183)
  (let ((missing '()))
    (maphash (lambda (name entry)
               (unless (and (listp (cl-cc/compile::be-properties entry))
                            (member :registered-builtin
                                    (cl-cc/compile::be-properties entry)
                                    :test #'eq))
                 (push name missing)))
             cl-cc/compile::*builtin-registry*)
    (assert-null (nreverse missing))))

(deftest fr-183-representative-registry-properties
  "FR-183: representative builtins retain specific function attributes."
  :tags '(:fr-183)
  (dolist (case '( ("LENGTH" (:read-only :nonneg-result))
                  ("CAR" (:read-only))
                  ("ABS" (:pure :foldable :no-escape))
                  ("PRINC" (:io))
                  ("RPLACA" (:write-global :always-returns))
                  ("ERROR" (:control))))
    (destructuring-bind (name-str expected-properties) case
      (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
        (assert-true entry)
        (dolist (property expected-properties)
          (assert-true (member property (cl-cc/compile::be-properties entry) :test #'eq)))))))

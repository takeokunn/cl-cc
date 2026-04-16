;;;; tests/integration/standalone-load-tests.lisp
;;;;
;;;; Verifies each extracted sibling ASDF system from the package-by-feature
;;;; migration is loaded and its feature package is populated. This codifies
;;;; the in-process guarantee that downstream consumers can load the sibling
;;;; systems (e.g. :cl-cc-type) without depending on the umbrella :cl-cc
;;;; system's eval-when asdf:load-asd bootstrap pulling them in.
;;;;
;;;; For each extracted system we check:
;;;;   1. (asdf:find-system :cl-cc-XXX) returns non-nil
;;;;   2. The corresponding feature package exists
;;;;   3. A representative exported symbol is present and :external
;;;;
;;;; A subprocess-based true-standalone load test is a future enhancement.

(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

(defun %standalone-find-system (system-name)
  "Return the ASDF system object for SYSTEM-NAME, or NIL on failure."
  (handler-case (asdf:find-system system-name nil)
    (error () nil)))

(defun %standalone-symbol-external-p (symbol-name package-name)
  "Return non-NIL when SYMBOL-NAME is interned and :external in PACKAGE-NAME."
  (let ((pkg (find-package package-name)))
    (when pkg
      (multiple-value-bind (sym status) (find-symbol symbol-name pkg)
        (and sym (eq status :external))))))

(deftest-each standalone-sibling-systems-loaded
  "Each extracted sibling ASDF system is registered, its feature package
exists, and a representative exported symbol resolves as :external."
  :cases
  (("cl-cc-ast"      :cl-cc-ast      :cl-cc/ast      "AST-CHILDREN")
   ("cl-cc-prolog"   :cl-cc-prolog   :cl-cc/prolog   "UNIFY")
   ("cl-cc-parse"   :cl-cc-parse   :cl-cc/parse   "PARSE-CL-SOURCE")
   ("cl-cc-binary"   :cl-cc-binary   :cl-cc/binary   "MACH-O-BUILDER")
   ("cl-cc-runtime"  :cl-cc-runtime  :cl-cc/runtime  "RT-CONS")
   ("cl-cc-bytecode" :cl-cc-bytecode :cl-cc/bytecode "ENCODE-ADD")
   ("cl-cc-ir"       :cl-cc-ir       :cl-cc/ir       "IR-MAKE-FUNCTION")
   ("cl-cc-mir"      :cl-cc-mir      :cl-cc/mir      "MIR-MAKE-FUNCTION")
   ("cl-cc-type"     :cl-cc-type     :cl-cc/type     "TYPE-TO-STRING")
   ("cl-cc-optimize" :cl-cc-optimize :cl-cc/optimize "OPTIMIZE-INSTRUCTIONS")
   ("cl-cc-emit"     :cl-cc-emit     :cl-cc/emit     "ALLOCATE-REGISTERS")
   ("cl-cc-expand"   :cl-cc-expand   :cl-cc/expand   "OUR-MACROEXPAND-1")
   ("cl-cc-compile"  :cl-cc-compile  :cl-cc/compile  "COMPILE-EXPRESSION")
   ("cl-cc-vm"       :cl-cc-vm       :cl-cc/vm       "VM-STATE"))
  (system-name package-name representative-symbol)
  (assert-true (%standalone-find-system system-name))
  (assert-true (find-package package-name))
  (assert-true (%standalone-symbol-external-p representative-symbol package-name)))

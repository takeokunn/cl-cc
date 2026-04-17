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

(defun %bridge-registered-p (symbol-name)
  "Return T when SYMBOL-NAME resolved via :cl-cc (as the VM does) is in the bridge table.
The VM looks up function names via (find-symbol name :cl-cc), so this mirrors that path."
  (let ((sym (find-symbol symbol-name :cl-cc)))
    (and sym (gethash sym cl-cc/vm::*vm-host-bridge-functions*))))

(deftest bridge-cross-package-symbols-registered
  "All cross-package VM host bridge symbols are registered after full system load.
Guards against load-order race where vm-bridge.lisp silently skips registration
because :cl-cc-compile/:cl-cc-parse/:cl-cc-expand packages don't exist yet.
The pipeline.lisp eval-when block re-registers these once all packages are present."
  (assert-true (%bridge-registered-p "RUN-STRING"))
  (assert-true (%bridge-registered-p "RUN-STRING-REPL"))
  (assert-true (%bridge-registered-p "OUR-LOAD"))
  (assert-true (%bridge-registered-p "COMPILE-EXPRESSION"))
  (assert-true (%bridge-registered-p "COMPILE-STRING"))
  (assert-true (%bridge-registered-p "OUR-EVAL"))
  (assert-true (%bridge-registered-p "PARSE-ALL-FORMS"))
  (assert-true (%bridge-registered-p "GENERATE-LAMBDA-BINDINGS"))
  ;; REGISTER-MACRO is intentionally absent: stores VM closures in macro-env,
  ;; causing TYPE-ERROR when host CL funcalls them during macroexpansion.
  (assert-false (%bridge-registered-p "REGISTER-MACRO")))

(deftest vm-eval-hooks-wired-after-load
  "*vm-eval-hook* and *vm-compile-string-hook* are non-nil after pipeline.lisp loads."
  (assert-true cl-cc/vm::*vm-eval-hook*)
  (assert-true cl-cc/vm::*vm-compile-string-hook*))

;;;; tests/unit/emit/wasm-ir-tests.lisp — WASM IR Unit Tests
;;;
;;; Tests for wasm-module-ir construction, function/global addition,
;;; register mapping, and WAT name conversion.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Struct Construction ──────────────────────────────────────────────────

(deftest wasm-ir-field-construction
  "wasm-field: default is type nil / :mutable; explicit type and :immutable store correctly."
  (let ((fd (cl-cc/emit::make-wasm-field))
        (fi (cl-cc/emit::make-wasm-field :type :i32 :mutability :immutable)))
    (assert-null (cl-cc/emit::wasm-field-type fd))
    (assert-eq :mutable (cl-cc/emit::wasm-field-mutability fd))
    (assert-eq :i32 (cl-cc/emit::wasm-field-type fi))
    (assert-eq :immutable (cl-cc/emit::wasm-field-mutability fi))))

(deftest wasm-ir-func-type
  "wasm-func-type stores params and results."
  (let ((ft (cl-cc/emit::make-wasm-func-type :params '(:i32 :i32) :results '(:i32))))
    (assert-equal '(:i32 :i32) (cl-cc/emit::wasm-func-type-params ft))
    (assert-equal '(:i32) (cl-cc/emit::wasm-func-type-results ft))))

(deftest wasm-ir-struct-type
  "wasm-struct-type stores fields and optional supertype."
  (let* ((f1 (cl-cc/emit::make-wasm-field :type :i32))
         (f2 (cl-cc/emit::make-wasm-field :type :eqref))
         (st (cl-cc/emit::make-wasm-struct-type :fields (list f1 f2) :supertype 0)))
    (assert-= 2 (length (cl-cc/emit::wasm-struct-type-fields st)))
    (assert-= 0 (cl-cc/emit::wasm-struct-type-supertype st))))

(deftest wasm-ir-array-type
  "wasm-array-type stores element type and mutability."
  (let ((at (cl-cc/emit::make-wasm-array-type :element-type :i32 :mutability :immutable)))
    (assert-eq :i32 (cl-cc/emit::wasm-array-type-element-type at))
    (assert-eq :immutable (cl-cc/emit::wasm-array-type-mutability at))))

(deftest wasm-ir-type-entry
  "wasm-type-entry wraps a definition with index and wat-name."
  (let* ((ft (cl-cc/emit::make-wasm-func-type :params '(:i32) :results '(:i32)))
         (te (cl-cc/emit::make-wasm-type-entry :index 0 :definition ft :wat-name "$add")))
    (assert-= 0 (cl-cc/emit::wasm-type-entry-index te))
    (assert-equal "$add" (cl-cc/emit::wasm-type-entry-wat-name te))
    (assert-true (cl-cc/emit::wasm-func-type-p (cl-cc/emit::wasm-type-entry-definition te)))))

;;; ─── Import/Export ────────────────────────────────────────────────────────

(deftest wasm-ir-import-export-construction
  "wasm-import and wasm-export store all their fields correctly."
  (let ((imp (cl-cc/emit::make-wasm-import :module "cl-io" :name "write_char"
                                       :kind :func :type-index 3))
        (exp (cl-cc/emit::make-wasm-export :name "main" :kind :func :index 5)))
    (assert-equal "cl-io" (cl-cc/emit::wasm-import-module imp))
    (assert-equal "write_char" (cl-cc/emit::wasm-import-name imp))
    (assert-eq :func (cl-cc/emit::wasm-import-kind imp))
    (assert-= 3 (cl-cc/emit::wasm-import-type-index imp))
    (assert-equal "main" (cl-cc/emit::wasm-export-name exp))
    (assert-eq :func (cl-cc/emit::wasm-export-kind exp))
    (assert-= 5 (cl-cc/emit::wasm-export-index exp))))

;;; ─── Global Definition ───────────────────────────────────────────────────

(deftest wasm-ir-global-def
  "wasm-global-def stores all fields."
  (let ((gd (cl-cc/emit::make-wasm-global-def :index 0 :wat-name "$g_x"
                                          :value-type :i32 :mutability :mutable
                                          :init-value 0)))
    (assert-= 0 (cl-cc/emit::wasm-global-def-index gd))
    (assert-equal "$g_x" (cl-cc/emit::wasm-global-def-wat-name gd))
    (assert-eq :i32 (cl-cc/emit::wasm-global-def-value-type gd))
    (assert-= 0 (cl-cc/emit::wasm-global-def-init-value gd))))

;;; ─── Local ────────────────────────────────────────────────────────────────

(deftest wasm-ir-local
  "wasm-local stores index, wat-name, value-type."
  (let ((loc (cl-cc/emit::make-wasm-local :index 2 :wat-name "$R0" :value-type :eqref)))
    (assert-= 2 (cl-cc/emit::wasm-local-index loc))
    (assert-equal "$R0" (cl-cc/emit::wasm-local-wat-name loc))
    (assert-eq :eqref (cl-cc/emit::wasm-local-value-type loc))))

;;; ─── Module Construction ──────────────────────────────────────────────────

(deftest wasm-ir-empty-module
  "make-empty-wasm-module creates a valid empty module."
  (let ((mod (cl-cc/emit::make-empty-wasm-module)))
    (assert-true (cl-cc/emit::wasm-module-ir-p mod))
    (assert-null (cl-cc/emit::wasm-module-types mod))
    (assert-null (cl-cc/emit::wasm-module-functions mod))
    (assert-null (cl-cc/emit::wasm-module-globals mod))
    (assert-= 0 (cl-cc/emit::wasm-module-next-func-index mod))
    (assert-= 0 (cl-cc/emit::wasm-module-next-global-index mod))
    (assert-true (hash-table-p (cl-cc/emit::wasm-module-global-name-table mod)))))

;;; ─── wasm-module-add-function ─────────────────────────────────────────────

(deftest wasm-ir-add-function-behavior
  "wasm-module-add-function assigns monotone indices and accumulates in function list."
  (let ((mod (cl-cc/emit::make-empty-wasm-module))
        (fn1 (cl-cc/emit::make-wasm-function-def :wat-name "$fn_test")))
    (cl-cc/emit::wasm-module-add-function mod fn1)
    (assert-= 0 (cl-cc/emit::wasm-func-index fn1))
    (assert-= 1 (cl-cc/emit::wasm-module-next-func-index mod))
    (let ((fn2 (cl-cc/emit::make-wasm-function-def :wat-name "$fn2")))
      (cl-cc/emit::wasm-module-add-function mod fn2)
      (assert-= 1 (cl-cc/emit::wasm-func-index fn2))
      (assert-= 2 (cl-cc/emit::wasm-module-next-func-index mod))
      (assert-= 2 (length (cl-cc/emit::wasm-module-functions mod))))))

;;; ─── wasm-module-add-global ───────────────────────────────────────────────

(deftest wasm-ir-add-global-behavior
  "wasm-module-add-global: assigns index, registers name in table, works with unnamed."
  (let ((mod1 (cl-cc/emit::make-empty-wasm-module))
        (mod2 (cl-cc/emit::make-empty-wasm-module))
        (mod3 (cl-cc/emit::make-empty-wasm-module)))
    (let ((gd1 (cl-cc/emit::make-wasm-global-def :wat-name "$g_x" :value-type :i32)))
      (cl-cc/emit::wasm-module-add-global mod1 gd1)
      (assert-= 0 (cl-cc/emit::wasm-global-def-index gd1))
      (assert-= 1 (cl-cc/emit::wasm-module-next-global-index mod1)))
    (let ((gd2 (cl-cc/emit::make-wasm-global-def :wat-name "$g_y" :value-type :i32)))
      (cl-cc/emit::wasm-module-add-global mod2 gd2)
      (assert-eq gd2 (gethash "$g_y" (cl-cc/emit::wasm-module-global-name-table mod2))))
    (let ((gd3 (cl-cc/emit::make-wasm-global-def :value-type :i32)))
      (cl-cc/emit::wasm-module-add-global mod3 gd3)
      (assert-= 0 (cl-cc/emit::wasm-global-def-index gd3)))))

;;; ─── wasm-lisp-name-to-wat-id ────────────────────────────────────────────

(deftest-each wasm-ir-name-conversion
  "wasm-lisp-name-to-wat-id converts names correctly."
  :cases (("simple-symbol"  'foo         "foo")
          ("with-hyphens"   'my-var      "my_var")
          ("with-asterisks" '*features*  "_features_")
          ("uppercase"      'ABC         "abc")
          ("string-input"   "Hello"      "hello"))
  (input expected)
  (assert-equal expected (cl-cc/emit::wasm-lisp-name-to-wat-id input)))

(deftest wasm-ir-vm-global-wat-name
  "vm-global-wat-name prefixes with $g_."
  (assert-equal "$g_my_var" (cl-cc/emit::vm-global-wat-name 'my-var))
  (assert-equal "$g__features_" (cl-cc/emit::vm-global-wat-name '*features*)))

;;; ─── wasm-reg-map ─────────────────────────────────────────────────────────

(deftest-each wasm-ir-reg-map-param-counts
  "make-wasm-reg-map-for-function: $pc=N, $tmp=N+1, next-index=N+2."
  :cases (("3-params" 3 3 4 5)
          ("0-params" 0 0 1 2))
  (n-params expected-pc expected-tmp expected-next)
  (let ((rm (cl-cc/emit::make-wasm-reg-map-for-function n-params)))
    (assert-= expected-pc  (cl-cc/emit::wasm-reg-map-pc-index rm))
    (assert-= expected-tmp (cl-cc/emit::wasm-reg-map-tmp-index rm))
    (assert-= expected-next (cl-cc/emit::wasm-reg-map-next-index rm))))

(deftest wasm-ir-reg-to-local-behavior
  "wasm-reg-to-local: allocates a new local for unseen register (idempotent on re-lookup); sequential registers get sequential indices."
  ;; Idempotency: same register returns same index
  (let ((rm (cl-cc/emit::make-wasm-reg-map-for-function 2)))
    (let ((idx (cl-cc/emit::wasm-reg-to-local rm :R0)))
      (assert-= 4 idx)
      (assert-= 4 (cl-cc/emit::wasm-reg-to-local rm :R0))))
  ;; Sequential allocation
  (let ((rm (cl-cc/emit::make-wasm-reg-map-for-function 0)))
    (assert-= 2 (cl-cc/emit::wasm-reg-to-local rm :R0))
    (assert-= 3 (cl-cc/emit::wasm-reg-to-local rm :R1))
    (assert-= 4 (cl-cc/emit::wasm-reg-to-local rm :R2))
    (assert-= 2 (cl-cc/emit::wasm-reg-to-local rm :R0))))

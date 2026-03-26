;;;; tests/unit/emit/wasm-ir-tests.lisp — WASM IR Unit Tests
;;;
;;; Tests for wasm-module-ir construction, function/global addition,
;;; register mapping, and WAT name conversion.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Struct Construction ──────────────────────────────────────────────────

(deftest wasm-ir-field-defaults
  "wasm-field defaults: type nil, mutability :mutable."
  (let ((f (cl-cc::make-wasm-field)))
    (assert-null (cl-cc::wasm-field-type f))
    (assert-eq :mutable (cl-cc::wasm-field-mutability f))))

(deftest wasm-ir-field-immutable
  "wasm-field with :immutable mutability."
  (let ((f (cl-cc::make-wasm-field :type :i32 :mutability :immutable)))
    (assert-eq :i32 (cl-cc::wasm-field-type f))
    (assert-eq :immutable (cl-cc::wasm-field-mutability f))))

(deftest wasm-ir-func-type
  "wasm-func-type stores params and results."
  (let ((ft (cl-cc::make-wasm-func-type :params '(:i32 :i32) :results '(:i32))))
    (assert-equal '(:i32 :i32) (cl-cc::wasm-func-type-params ft))
    (assert-equal '(:i32) (cl-cc::wasm-func-type-results ft))))

(deftest wasm-ir-struct-type
  "wasm-struct-type stores fields and optional supertype."
  (let* ((f1 (cl-cc::make-wasm-field :type :i32))
         (f2 (cl-cc::make-wasm-field :type :eqref))
         (st (cl-cc::make-wasm-struct-type :fields (list f1 f2) :supertype 0)))
    (assert-= 2 (length (cl-cc::wasm-struct-type-fields st)))
    (assert-= 0 (cl-cc::wasm-struct-type-supertype st))))

(deftest wasm-ir-array-type
  "wasm-array-type stores element type and mutability."
  (let ((at (cl-cc::make-wasm-array-type :element-type :i32 :mutability :immutable)))
    (assert-eq :i32 (cl-cc::wasm-array-type-element-type at))
    (assert-eq :immutable (cl-cc::wasm-array-type-mutability at))))

(deftest wasm-ir-type-entry
  "wasm-type-entry wraps a definition with index and wat-name."
  (let* ((ft (cl-cc::make-wasm-func-type :params '(:i32) :results '(:i32)))
         (te (cl-cc::make-wasm-type-entry :index 0 :definition ft :wat-name "$add")))
    (assert-= 0 (cl-cc::wasm-type-entry-index te))
    (assert-equal "$add" (cl-cc::wasm-type-entry-wat-name te))
    (assert-true (cl-cc::wasm-func-type-p (cl-cc::wasm-type-entry-definition te)))))

;;; ─── Import/Export ────────────────────────────────────────────────────────

(deftest wasm-ir-import-construction
  "wasm-import stores module, name, kind, type-index."
  (let ((imp (cl-cc::make-wasm-import :module "cl-io" :name "write_char"
                                       :kind :func :type-index 3)))
    (assert-equal "cl-io" (cl-cc::wasm-import-module imp))
    (assert-equal "write_char" (cl-cc::wasm-import-name imp))
    (assert-eq :func (cl-cc::wasm-import-kind imp))
    (assert-= 3 (cl-cc::wasm-import-type-index imp))))

(deftest wasm-ir-export-construction
  "wasm-export stores name, kind, index."
  (let ((exp (cl-cc::make-wasm-export :name "main" :kind :func :index 5)))
    (assert-equal "main" (cl-cc::wasm-export-name exp))
    (assert-eq :func (cl-cc::wasm-export-kind exp))
    (assert-= 5 (cl-cc::wasm-export-index exp))))

;;; ─── Global Definition ───────────────────────────────────────────────────

(deftest wasm-ir-global-def
  "wasm-global-def stores all fields."
  (let ((gd (cl-cc::make-wasm-global-def :index 0 :wat-name "$g_x"
                                          :value-type :i32 :mutability :mutable
                                          :init-value 0)))
    (assert-= 0 (cl-cc::wasm-global-def-index gd))
    (assert-equal "$g_x" (cl-cc::wasm-global-def-wat-name gd))
    (assert-eq :i32 (cl-cc::wasm-global-def-value-type gd))
    (assert-= 0 (cl-cc::wasm-global-def-init-value gd))))

;;; ─── Local ────────────────────────────────────────────────────────────────

(deftest wasm-ir-local
  "wasm-local stores index, wat-name, value-type."
  (let ((loc (cl-cc::make-wasm-local :index 2 :wat-name "$R0" :value-type :eqref)))
    (assert-= 2 (cl-cc::wasm-local-index loc))
    (assert-equal "$R0" (cl-cc::wasm-local-wat-name loc))
    (assert-eq :eqref (cl-cc::wasm-local-value-type loc))))

;;; ─── Module Construction ──────────────────────────────────────────────────

(deftest wasm-ir-empty-module
  "make-empty-wasm-module creates a valid empty module."
  (let ((mod (cl-cc::make-empty-wasm-module)))
    (assert-true (cl-cc::wasm-module-ir-p mod))
    (assert-null (cl-cc::wasm-module-types mod))
    (assert-null (cl-cc::wasm-module-functions mod))
    (assert-null (cl-cc::wasm-module-globals mod))
    (assert-= 0 (cl-cc::wasm-module-next-func-index mod))
    (assert-= 0 (cl-cc::wasm-module-next-global-index mod))
    (assert-true (hash-table-p (cl-cc::wasm-module-global-name-table mod)))))

;;; ─── wasm-module-add-function ─────────────────────────────────────────────

(deftest wasm-ir-add-function-assigns-index
  "Adding a function assigns next-func-index."
  (let* ((mod (cl-cc::make-empty-wasm-module))
         (fn (cl-cc::make-wasm-function-def :wat-name "$fn_test")))
    (cl-cc::wasm-module-add-function mod fn)
    (assert-= 0 (cl-cc::wasm-func-index fn))
    (assert-= 1 (cl-cc::wasm-module-next-func-index mod))))

(deftest wasm-ir-add-function-increments
  "Adding multiple functions increments the index."
  (let ((mod (cl-cc::make-empty-wasm-module)))
    (cl-cc::wasm-module-add-function mod (cl-cc::make-wasm-function-def :wat-name "$f1"))
    (let ((f2 (cl-cc::make-wasm-function-def :wat-name "$f2")))
      (cl-cc::wasm-module-add-function mod f2)
      (assert-= 1 (cl-cc::wasm-func-index f2))
      (assert-= 2 (cl-cc::wasm-module-next-func-index mod)))))

(deftest wasm-ir-add-function-accumulates
  "Functions accumulate in wasm-module-functions."
  (let ((mod (cl-cc::make-empty-wasm-module)))
    (cl-cc::wasm-module-add-function mod (cl-cc::make-wasm-function-def))
    (cl-cc::wasm-module-add-function mod (cl-cc::make-wasm-function-def))
    (assert-= 2 (length (cl-cc::wasm-module-functions mod)))))

;;; ─── wasm-module-add-global ───────────────────────────────────────────────

(deftest wasm-ir-add-global-assigns-index
  "Adding a global assigns next-global-index."
  (let* ((mod (cl-cc::make-empty-wasm-module))
         (gd (cl-cc::make-wasm-global-def :wat-name "$g_x" :value-type :i32)))
    (cl-cc::wasm-module-add-global mod gd)
    (assert-= 0 (cl-cc::wasm-global-def-index gd))
    (assert-= 1 (cl-cc::wasm-module-next-global-index mod))))

(deftest wasm-ir-add-global-registers-name
  "Adding a named global registers it in the name table."
  (let* ((mod (cl-cc::make-empty-wasm-module))
         (gd (cl-cc::make-wasm-global-def :wat-name "$g_y" :value-type :i32)))
    (cl-cc::wasm-module-add-global mod gd)
    (assert-eq gd (gethash "$g_y" (cl-cc::wasm-module-global-name-table mod)))))

(deftest wasm-ir-add-global-unnamed-no-table
  "Adding a global with no wat-name doesn't crash (nil key)."
  (let* ((mod (cl-cc::make-empty-wasm-module))
         (gd (cl-cc::make-wasm-global-def :value-type :i32)))
    (cl-cc::wasm-module-add-global mod gd)
    (assert-= 0 (cl-cc::wasm-global-def-index gd))))

;;; ─── wasm-lisp-name-to-wat-id ────────────────────────────────────────────

(deftest-each wasm-ir-name-conversion
  "wasm-lisp-name-to-wat-id converts names correctly."
  :cases (("simple-symbol"  'foo         "foo")
          ("with-hyphens"   'my-var      "my_var")
          ("with-asterisks" '*features*  "_features_")
          ("uppercase"      'ABC         "abc")
          ("string-input"   "Hello"      "hello"))
  (input expected)
  (assert-equal expected (cl-cc::wasm-lisp-name-to-wat-id input)))

(deftest wasm-ir-vm-global-wat-name
  "vm-global-wat-name prefixes with $g_."
  (assert-equal "$g_my_var" (cl-cc::vm-global-wat-name 'my-var))
  (assert-equal "$g__features_" (cl-cc::vm-global-wat-name '*features*)))

;;; ─── wasm-reg-map ─────────────────────────────────────────────────────────

(deftest wasm-ir-reg-map-creation
  "make-wasm-reg-map-for-function sets up $pc and $tmp indices."
  (let ((rm (cl-cc::make-wasm-reg-map-for-function 3)))
    (assert-= 3 (cl-cc::wasm-reg-map-pc-index rm))
    (assert-= 4 (cl-cc::wasm-reg-map-tmp-index rm))
    ;; next-index starts after params + $pc + $tmp
    (assert-= 5 (cl-cc::wasm-reg-map-next-index rm))))

(deftest wasm-ir-reg-to-local-allocates
  "wasm-reg-to-local allocates a new local for an unseen register."
  (let ((rm (cl-cc::make-wasm-reg-map-for-function 2)))
    ;; next-index is 4 (2 params + $pc + $tmp)
    (let ((idx (cl-cc::wasm-reg-to-local rm :R0)))
      (assert-= 4 idx)
      ;; Second call returns the same index
      (assert-= 4 (cl-cc::wasm-reg-to-local rm :R0)))))

(deftest wasm-ir-reg-to-local-increments
  "wasm-reg-to-local increments for different registers."
  (let ((rm (cl-cc::make-wasm-reg-map-for-function 0)))
    ;; next-index is 2 (0 params + $pc + $tmp)
    (assert-= 2 (cl-cc::wasm-reg-to-local rm :R0))
    (assert-= 3 (cl-cc::wasm-reg-to-local rm :R1))
    (assert-= 4 (cl-cc::wasm-reg-to-local rm :R2))
    ;; Re-lookup returns same
    (assert-= 2 (cl-cc::wasm-reg-to-local rm :R0))))

(deftest wasm-ir-reg-map-zero-params
  "wasm-reg-map with 0 params: $pc=0, $tmp=1, locals start at 2."
  (let ((rm (cl-cc::make-wasm-reg-map-for-function 0)))
    (assert-= 0 (cl-cc::wasm-reg-map-pc-index rm))
    (assert-= 1 (cl-cc::wasm-reg-map-tmp-index rm))
    (assert-= 2 (cl-cc::wasm-reg-map-next-index rm))))

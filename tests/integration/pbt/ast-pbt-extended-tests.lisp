;;;; tests/pbt/ast-pbt-extended-tests.lisp — PBT: assignment, multiple-values, dynamic control, calls, types, nested
(in-package :cl-cc/pbt)

(in-suite cl-cc-pbt-suite)
;;; ── Assignment ──────────────────────────────────────────────────────────────

(deftest ast-setq-roundtrip
  "Setq variable and value are preserved through sexp roundtrip."
  (for-all ((var   (gen-fn (gen-symbol :package nil :prefix "VAR")))
            (value (gen-fn (gen-integer :min -1000 :max 1000))))
    (let ((ast2 (%ast-roundtrip (make-ast-setq :var   var
                                               :value (make-ast-int :value value)))))
      (assert-type ast-setq ast2)
      (assert-eq   var   (ast-setq-var ast2))
      (assert-type ast-int (ast-setq-value ast2))
      (assert-=    value  (ast-int-value (ast-setq-value ast2))))))

;;; ── Multiple Values ─────────────────────────────────────────────────────────

(deftest ast-multiple-value-call-roundtrip
  "Multiple-value-call (func + one arg) is preserved through sexp roundtrip."
  (for-all ((fn-name (gen-fn (gen-symbol :package nil :prefix "FN")))
            (arg-val (gen-fn (gen-integer :min -100 :max 100))))
    (let ((ast2 (%ast-roundtrip (make-ast-multiple-value-call
                                 :func (make-ast-var :name fn-name)
                                 :args (list (make-ast-int :value arg-val))))))
      (assert-type ast-multiple-value-call ast2)
      (assert-type ast-var (ast-mv-call-func ast2))
      (assert-eq   fn-name (ast-var-name (ast-mv-call-func ast2)))
      (assert-=    1       (length (ast-mv-call-args ast2))))))

(deftest ast-multiple-value-prog1-roundtrip
  "Multiple-value-prog1 (first + one form) is preserved through sexp roundtrip."
  (for-all ((first-val (gen-fn (gen-integer :min -100 :max 100)))
            (form-val  (gen-fn (gen-integer :min -100 :max 100))))
    (let ((ast2 (%ast-roundtrip (make-ast-multiple-value-prog1
                                 :first (make-ast-int :value first-val)
                                 :forms (list (make-ast-int :value form-val))))))
      (assert-type ast-multiple-value-prog1 ast2)
      (assert-type ast-int   (ast-mv-prog1-first ast2))
      (assert-=    first-val (ast-int-value (ast-mv-prog1-first ast2)))
      (assert-=    1         (length (ast-mv-prog1-forms ast2))))))

;;; ── Dynamic Control ─────────────────────────────────────────────────────────

(deftest ast-catch-roundtrip
  "Catch (keyword tag + one body form) is preserved through sexp roundtrip."
  (for-all ((tag      (gen-fn (gen-symbol :package :keyword :prefix "TAG")))
            (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let ((ast2 (%ast-roundtrip (make-ast-catch :tag  (make-ast-var :name tag)
                                                :body (list (make-ast-int :value body-val))))))
      (assert-type ast-catch ast2)
      (assert-type ast-var   (ast-catch-tag ast2))
      (assert-=    1         (length (ast-catch-body ast2))))))

(deftest ast-throw-roundtrip
  "Throw (tag and value) is preserved through sexp roundtrip."
  (for-all ((tag   (gen-fn (gen-symbol :package :keyword :prefix "TAG")))
            (value (gen-fn (gen-integer :min -1000 :max 1000))))
    (let ((ast2 (%ast-roundtrip (make-ast-throw :tag   (make-ast-var :name tag)
                                                :value (make-ast-int :value value)))))
      (assert-type ast-throw ast2)
      (assert-type ast-var   (ast-throw-tag ast2))
      (assert-type ast-int   (ast-throw-value ast2))
      (assert-=    value     (ast-int-value (ast-throw-value ast2))))))

(deftest ast-unwind-protect-roundtrip
  "Unwind-protect (protected form + one cleanup form) is preserved through sexp roundtrip."
  (for-all ((protected-val (gen-fn (gen-integer :min -100 :max 100)))
            (cleanup-val   (gen-fn (gen-integer :min -100 :max 100))))
    (let ((ast2 (%ast-roundtrip (make-ast-unwind-protect
                                 :protected (make-ast-int :value protected-val)
                                 :cleanup   (list (make-ast-int :value cleanup-val))))))
      (assert-type ast-unwind-protect ast2)
      (assert-type ast-int       (ast-unwind-protected ast2))
      (assert-=    protected-val (ast-int-value (ast-unwind-protected ast2)))
      (assert-=    1             (length (ast-unwind-cleanup ast2))))))

;;; ── Function Calls and References ───────────────────────────────────────────

(deftest ast-call-roundtrip
  "Function call (name + N integer args) is preserved through sexp roundtrip."
  (for-all ((fn-name (gen-fn (gen-symbol :prefix "FN")))
            (args    (gen-fn (gen-list-of (gen-integer :min -100 :max 100)
                                          :min-length 1 :max-length 5))))
    (let ((ast2 (%ast-roundtrip (make-ast-call :func fn-name
                                               :args (mapcar (lambda (v) (make-ast-int :value v))
                                                             args)))))
      (assert-type  ast-call ast2)
      (assert-=     (length args) (length (ast-call-args ast2)))
      (assert-equal args (mapcar #'ast-int-value (ast-call-args ast2))))))

(deftest ast-function-roundtrip
  "Function reference with symbol name is preserved through sexp roundtrip."
  (for-all ((name (gen-fn (gen-symbol :package nil :prefix "FN"))))
    (let ((ast2 (%ast-roundtrip (make-ast-function :name name))))
      (assert-type ast-function ast2)
      (assert-eq   name (ast-function-name ast2)))))

(deftest ast-function-setf-roundtrip
  "Function reference with setf name is preserved through sexp roundtrip."
  (let* ((name '(setf accessor))
         (ast2 (%ast-roundtrip (make-ast-function :name name))))
    (assert-type  ast-function ast2)
    (assert-equal name (ast-function-name ast2))))

;;; ── Type Declarations ───────────────────────────────────────────────────────

(deftest ast-the-roundtrip
  "The type declaration (type tag + integer value) is preserved through sexp roundtrip."
  (for-all ((value (gen-fn (gen-integer :min -1000 :max 1000)))
            (type  (gen-fn (gen-one-of '(integer fixnum number)))))
    (let ((ast2 (%ast-roundtrip (make-ast-the :type  type
                                              :value (make-ast-int :value value)))))
      (assert-type ast-the ast2)
      (assert-eq   type    (ast-the-type ast2))
      (assert-type ast-int (ast-the-value ast2))
      (assert-=    value   (ast-int-value (ast-the-value ast2))))))

;;; ── Nested Structure ────────────────────────────────────────────────────────

(deftest nested-if-roundtrip
  "Nested if (outer/inner) preserves all five integer values through sexp roundtrip."
  (for-all ((v1 (gen-fn (gen-integer :min 0 :max 1)))
            (v2 (gen-fn (gen-integer :min 0 :max 1)))
            (v3 (gen-fn (gen-integer :min -10 :max 10)))
            (v4 (gen-fn (gen-integer :min -10 :max 10)))
            (v5 (gen-fn (gen-integer :min -10 :max 10))))
    (let* ((inner-if (make-ast-if :cond (make-ast-int :value v2)
                                  :then (make-ast-int :value v3)
                                  :else (make-ast-int :value v4)))
           (ast2     (%ast-roundtrip
                      (make-ast-if :cond (make-ast-int :value v1)
                                   :then inner-if
                                   :else (make-ast-int :value v5)))))
      (assert-type ast-if (ast-if-then ast2))
      (assert-=    v1     (ast-int-value (ast-if-cond ast2)))
      (assert-=    v2     (ast-int-value (ast-if-cond (ast-if-then ast2))))
      (assert-=    v3     (ast-int-value (ast-if-then (ast-if-then ast2))))
      (assert-=    v4     (ast-int-value (ast-if-else (ast-if-then ast2))))
      (assert-=    v5     (ast-int-value (ast-if-else ast2))))))

(deftest nested-binop-roundtrip
  "Nested binop (* (+ v1 v2) v3) preserves op, nesting, and values through roundtrip."
  (for-all ((v1 (gen-fn (gen-integer :min -10 :max 10)))
            (v2 (gen-fn (gen-integer :min -10 :max 10)))
            (v3 (gen-fn (gen-integer :min -10 :max 10))))
    (let* ((inner (make-ast-binop :op  '+
                                  :lhs (make-ast-int :value v1)
                                  :rhs (make-ast-int :value v2)))
           (ast2  (%ast-roundtrip (make-ast-binop :op  '*
                                                  :lhs inner
                                                  :rhs (make-ast-int :value v3)))))
      (assert-type ast-binop ast2)
      (assert-eq   '*       (ast-binop-op ast2))
      (assert-type ast-binop (ast-binop-lhs ast2))
      (assert-eq   '+       (ast-binop-op (ast-binop-lhs ast2)))
      (assert-=    v1       (ast-int-value (ast-binop-lhs (ast-binop-lhs ast2))))
      (assert-=    v2       (ast-int-value (ast-binop-rhs (ast-binop-lhs ast2))))
      (assert-=    v3       (ast-int-value (ast-binop-rhs ast2))))))

(deftest nested-let-roundtrip
  "Nested let (outer binds var1, inner binds var2) preserves both through sexp roundtrip."
  (for-all ((var1 (gen-fn (gen-symbol :package nil :prefix "X")))
            (var2 (gen-fn (gen-symbol :package nil :prefix "Y")))
            (v1   (gen-fn (gen-integer :min -10 :max 10)))
            (v2   (gen-fn (gen-integer :min -10 :max 10))))
    (let* ((inner-let (make-ast-let
                       :bindings (list (cons var2 (make-ast-int :value v2)))
                       :body     (list (make-ast-var :name var2))))
           (ast2      (%ast-roundtrip
                       (make-ast-let
                        :bindings (list (cons var1 (make-ast-int :value v1)))
                        :body     (list inner-let)))))
      (assert-type ast-let ast2)
      (assert-eq   var1 (car (first (ast-let-bindings ast2))))
      (assert-=    v1   (ast-int-value (cdr (first (ast-let-bindings ast2)))))
      (assert-=    1    (length (ast-let-body ast2)))
      (assert-type ast-let (first (ast-let-body ast2)))
      (assert-eq   var2 (car (first (ast-let-bindings (first (ast-let-body ast2)))))))))

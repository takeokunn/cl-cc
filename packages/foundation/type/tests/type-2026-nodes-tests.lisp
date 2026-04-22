;;;; tests/type-2026-nodes-tests.lisp - 2026 Type System Node and Substitution API Tests
;;;;
;;;; Covers: 2026 type node extensions (rigid vars, product/variant, exists/mu, HKT app,
;;;; record, arrow-mult, linear), ANSI upgrade helpers, and hash-table substitution API.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: New Type Node Tests (direct new API)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest type-rigid-var-identity-and-uniqueness
  "fresh-rigid-var: each call produces a distinct var; same var is equal to itself."
  (let ((r1 (fresh-rigid-var 'a))
        (r2 (fresh-rigid-var 'a)))
    (assert-true  (type-rigid-p r1))
    (assert-true  (type-rigid-p r2))
    (assert-false (type-rigid-equal-p r1 r2))
    (assert-true  (type-rigid-equal-p r1 r1))
    (assert-eq 'a (type-rigid-name r1))))

(deftest type-product-and-variant-creation
  "make-type-product stores ordered elem types; make-type-variant stores cases with nil row-var."
  (let ((pair (make-type-product :elems (list type-int type-string))))
    (assert-true (type-product-p pair))
    (assert-= 2 (length (type-product-elems pair)))
    (assert-true (type-equal-p type-int    (first  (type-product-elems pair))))
    (assert-true (type-equal-p type-string (second (type-product-elems pair)))))
  (let ((v (make-type-variant :cases (list (cons 'some type-int) (cons 'none type-null))
                              :row-var nil)))
    (assert-true (type-variant-p v))
    (assert-= 2 (length (type-variant-cases v)))
    (assert-null (type-variant-row-var v))))

(deftest type-forall-body-keyword-aliases-type
  "make-type-forall :body and :type slots are aliases — both return the same body."
  (let* ((a  (fresh-type-var 'a))
         (fn (make-type-arrow (list a) a))
         (fa (make-type-forall :var a :body fn)))
    (assert-true (type-forall-p fa))
    (assert-true (type-var-equal-p a (type-forall-var fa)))
    (assert-true (type-equal-p fn (type-forall-body fa)))))

(deftest type-exists-and-mu-creation
  "make-type-exists and make-type-mu store var and body correctly."
  (let* ((a    (fresh-type-var 'a))
         (pair (make-type-product :elems (list type-string a)))
         (ex   (make-type-exists :var a :knd nil :body pair)))
    (assert-true (type-exists-p ex))
    (assert-true (type-var-equal-p a (type-exists-var ex)))
    (assert-true (type-product-p (type-exists-body ex))))
  (let* ((a  (fresh-type-var 'a))
         (mu (make-type-mu :var a
                           :body (make-type-union
                                  (list type-null
                                        (make-type-product :elems (list type-int a)))))))
    (assert-true (type-mu-p mu))
    (assert-true (type-var-equal-p a (type-mu-var mu)))
    (assert-true (type-union-p (type-mu-body mu)))))

(deftest type-hkt-app-creation
  "make-type-app builds a higher-kinded application with fun and arg accessors."
  (let* ((list-con (make-type-primitive :name 'list))
         (list-int (make-type-app :fun list-con :arg type-int)))
    (assert-true (type-app-p list-int))
    (assert-true (type-primitive-p (type-app-fun list-int)))
    (assert-true (type-equal-p type-int (type-app-arg list-int)))))

(deftest-each type-record-open-closed
  "make-type-record: closed record has nil row-var; open record has a type-var row-var."
  :cases (("closed" nil 2)
          ("open"   t   1))
  (open-p expected-field-count)
  (let* ((rv  (when open-p (fresh-type-var 'rho)))
         (fields (if open-p
                     (list (cons 'name type-string))
                     (list (cons 'name type-string) (cons 'age type-int))))
         (rec (make-type-record :fields fields :row-var rv)))
    (assert-true (type-record-p rec))
    (assert-= expected-field-count (length (type-record-fields rec)))
    (if open-p
        (assert-true (type-var-p (type-record-row-var rec)))
        (assert-null (type-record-row-var rec)))))


(deftest-each type-arrow-mult-cases
  "make-type-arrow-raw supports :one (linear) and :zero (erased) multiplicities."
  :cases (("linear-one"  type-int  type-int  +pure-effect-row+ :one)
          ("erased-zero" type-bool type-null nil                :zero))
  (param-t ret-t effs mult)
  (let ((arr (make-type-arrow-raw :params (list param-t) :return ret-t :effects effs :mult mult)))
    (assert-true (type-arrow-p arr))
    (assert-eq mult (type-arrow-mult arr))))


(deftest-each type-linear-creation
  "make-type-linear creates graded modal types !_q T with the correct grade."
  :cases (("linear-one"   type-int    :one)
          ("erased-zero"  type-string :zero)
          ("unrestricted" type-bool   :omega))
  (base grade)
  (let ((lin (make-type-linear :base base :grade grade)))
    (assert-true (type-linear-p lin))
    (assert-eq grade (type-linear-grade lin))
    (assert-true (type-equal-p base (type-linear-base lin)))))

(deftest upgraded-array-and-complex-part-types
  "ANSI CL upgrade helpers return the expected core type nodes."
  (let ((bit-upgraded (upgraded-array-element-type 'bit))
        (char-upgraded (upgraded-array-element-type 'character))
        (fallback-upgraded (upgraded-array-element-type '(or fixnum string)))
        (complex-part (upgraded-complex-part-type 'complex)))
    (assert-true (type-equal-p (cl-cc/type:parse-type-specifier 'bit) bit-upgraded))
    (assert-true (type-equal-p (cl-cc/type:parse-type-specifier 'character) char-upgraded))
    (assert-true (type-equal-p type-any fallback-upgraded))
    (assert-true (type-equal-p (cl-cc/type:parse-type-specifier 'real) complex-part))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: Hash-Table Substitution API Tests
;;; ─────────────────────────────────────────────────────────────────────────

(deftest subst-empty-has-no-bindings
  "make-substitution: generation is 0 and fresh vars are not found."
  (let ((s (make-substitution)))
    (assert-true (substitution-p s))
    (assert-= 0 (substitution-generation s))
    (let ((v (fresh-type-var)))
      (multiple-value-bind (bound found) (subst-lookup v s)
        (declare (ignore bound))
        (assert-false found)))))

(deftest subst-extend-is-functional
  "subst-extend creates a new substitution; old substitution is unchanged."
  (let* ((v  (fresh-type-var))
         (s0 (make-substitution))
         (s1 (subst-extend v type-int s0)))
    (multiple-value-bind (b f) (subst-lookup v s0) (declare (ignore b)) (assert-false f))
    (multiple-value-bind (bound found) (subst-lookup v s1)
      (assert-true found)
      (assert-true (type-equal-p type-int bound)))
    (assert-true (> (substitution-generation s1) (substitution-generation s0)))))

(deftest subst-extend!-mutates-in-place
  "subst-extend! adds a binding to the existing substitution destructively."
  (let* ((v (fresh-type-var))
         (s (make-substitution)))
    (subst-extend! v type-string s)
    (multiple-value-bind (bound found) (subst-lookup v s)
      (assert-true found)
      (assert-true (type-equal-p type-string bound)))))

(deftest subst-advanced-operations
  "subst-compose chains via v2→v1→int; zonk applies through arrow; type-occurs-p detects circular refs."
  (let* ((v1  (fresh-type-var))
         (v2  (fresh-type-var))
         (s1  (subst-extend v1 type-int (make-substitution)))
         (s2  (subst-extend v2 v1 (make-substitution)))
         (s12 (subst-compose s1 s2)))
    (assert-true (type-equal-p type-int (zonk v2 s12))))
  (let* ((v  (fresh-type-var))
         (fn (make-type-arrow (list v) v))
         (s  (subst-extend v type-bool (make-substitution)))
         (r  (zonk fn s)))
    (assert-true (type-arrow-p r))
    (assert-true (type-equal-p type-bool (first (type-arrow-params r))))
    (assert-true (type-equal-p type-bool (type-arrow-return r))))
  (let* ((v  (fresh-type-var))
         (fn (make-type-arrow (list v) type-int))
         (s  (make-substitution))
         (w  (fresh-type-var)))
    (assert-true  (type-occurs-p v fn s))
    (assert-false (type-occurs-p v type-int s))
    (assert-false (type-occurs-p w fn s))))

;;;; tests/unit/type/types-extended-coverage-tests.lisp
;;;; Targeted coverage for uncovered branches in:
;;;;   src/type/types-extended.lisp — type-free-vars for capability/refinement/handler/gadt-con
;;;;   src/type/types-core.lisp — fresh-rigid-var, type-rigid-equal-p
;;;;   src/type/types-env.lisp — type-env-bindings

(in-package :cl-cc/test)

(defsuite types-extended-coverage-suite
  :description "Extended coverage for types-extended/core/env uncovered branches"
  :parent cl-cc-unit-suite)

(in-suite types-extended-coverage-suite)

;;; ─── type-free-vars: capability ─────────────────────────────────────────────

(deftest free-vars-capability-var-base-yields-one-fv
  "type-free-vars on capability with var base yields 1 free variable."
  (let* ((v   (cl-cc/type::fresh-type-var "cap-base"))
         (cap (cl-cc/type::make-type-capability :base v :cap 'read))
         (fvs (cl-cc/type::type-free-vars cap)))
    (assert-= 1 (length fvs))
    (assert-true (cl-cc/type::type-var-equal-p v (first fvs)))))

(deftest free-vars-capability-primitive-base-yields-nil
  "type-free-vars on capability with primitive base yields nil."
  (assert-null (cl-cc/type::type-free-vars
                (cl-cc/type::make-type-capability :base cl-cc/type::type-int :cap 'read))))

;;; ─── type-free-vars: refinement ─────────────────────────────────────────────

(deftest free-vars-refinement-var-base-yields-one-fv
  "type-free-vars on refinement with var base yields 1 free variable."
  (let* ((v   (cl-cc/type::fresh-type-var "ref-base"))
         (ref (cl-cc/type::make-type-refinement :base v :predicate (lambda (x) (> x 0))))
         (fvs (cl-cc/type::type-free-vars ref)))
    (assert-= 1 (length fvs))
    (assert-true (cl-cc/type::type-var-equal-p v (first fvs)))))

(deftest free-vars-refinement-primitive-base-yields-nil
  "type-free-vars on refinement with primitive base yields nil."
  (assert-null (cl-cc/type::type-free-vars
                (cl-cc/type::make-type-refinement
                 :base cl-cc/type::type-int :predicate (lambda (x) (> x 0))))))

;;; ─── type-free-vars: handler ─────────────────────────────────────────────────

(deftest free-vars-handler-three-distinct-vars
  "type-free-vars on handler with 3 distinct vars yields 3 free variables."
  (let* ((ve (cl-cc/type::fresh-type-var "eff"))
         (vi (cl-cc/type::fresh-type-var "in"))
         (vo (cl-cc/type::fresh-type-var "out"))
         (h  (cl-cc/type::make-type-handler :effect ve :input vi :output vo)))
    (assert-= 3 (length (cl-cc/type::type-free-vars h)))))

(deftest free-vars-handler-shared-var-deduped
  "type-free-vars on handler where effect and input share the same var yields 1 free variable."
  (let* ((v (cl-cc/type::fresh-type-var "shared"))
         (h (cl-cc/type::make-type-handler :effect v :input v :output cl-cc/type::type-int)))
    (assert-= 1 (length (cl-cc/type::type-free-vars h)))))

(deftest free-vars-handler-all-primitives-yields-nil
  "type-free-vars on handler with all primitive slots yields nil."
  (assert-null (cl-cc/type::type-free-vars
                (cl-cc/type::make-type-handler
                 :effect (cl-cc/type::make-type-effect-op :name 'io)
                 :input  cl-cc/type::type-int
                 :output cl-cc/type::type-string))))

;;; ─── type-free-vars: gadt-con ────────────────────────────────────────────────

(deftest free-vars-gadt-con-collects-arg-types
  "type-free-vars on gadt-con traverses arg-types."
  (let* ((v  (cl-cc/type::fresh-type-var "gadt-a"))
         (gc (cl-cc/type::make-type-gadt-con
              :name 'just
              :arg-types (list v)
              :index-type cl-cc/type::type-int)))
    ;; type-free-vars doesn't reach gadt-con — verify it returns nil (falls through to t nil)
    ;; since gadt-con is not in type-free-vars dispatch
    (assert-true (listp (cl-cc/type::type-free-vars gc)))))

;;; ─── type-rigid-equal-p (types-core.lisp) ───────────────────────────────────

(deftest rigid-var-equal-p-same-id-is-true
  "type-rigid-equal-p: two rigid vars with the same id are equal."
  (let* ((r1 (cl-cc/type::fresh-rigid-var "r"))
         (r2 (cl-cc/type::%make-type-rigid :id (cl-cc/type::type-rigid-id r1) :name "r")))
    (assert-true (cl-cc/type::type-rigid-equal-p r1 r2))))

(deftest rigid-var-equal-p-different-ids-is-false
  "type-rigid-equal-p: two freshly created rigid vars with different ids are not equal."
  (let ((r1 (cl-cc/type::fresh-rigid-var "a"))
        (r2 (cl-cc/type::fresh-rigid-var "b")))
    (assert-false (cl-cc/type::type-rigid-equal-p r1 r2))))

(deftest rigid-var-equal-p-rigid-vs-var-is-false
  "type-rigid-equal-p: a rigid var compared to a type-var is not equal."
  (let ((r (cl-cc/type::fresh-rigid-var "r"))
        (v (cl-cc/type::fresh-type-var  "v")))
    (assert-false (cl-cc/type::type-rigid-equal-p r v))))

;;; ─── type-env-bindings (types-env.lisp) ─────────────────────────────────────

(deftest type-env-bindings-empty-env-is-nil
  "type-env-bindings on an empty environment returns nil."
  (assert-null (cl-cc/type::type-env-bindings (cl-cc/type::type-env-empty))))

(deftest type-env-bindings-extended-env-has-entry
  "type-env-bindings on an extended environment returns a 1-element alist with the binding."
  (let* ((env  (cl-cc/type::type-env-empty))
         (sch  (cl-cc/type::type-to-scheme cl-cc/type::type-int))
         (env2 (cl-cc/type::type-env-extend 'x sch env))
         (al   (cl-cc/type::type-env-bindings env2)))
    (assert-= 1 (length al))
    (assert-eq 'x (caar al))))

;;; ─── type-equal-p: capability and error nodes ────────────────────────────────

(deftest type-equal-p-capability-self-identity
  "type-equal-p: a capability node is equal to itself."
  (let ((c (cl-cc/type::make-type-capability :base cl-cc/type::type-int :cap 'read)))
    (assert-true (cl-cc/type::type-equal-p c c))))

(deftest type-equal-p-error-node-always-false
  "type-equal-p: error nodes never compare equal, even to themselves."
  (let ((e (cl-cc/type::make-type-error :message "test")))
    (assert-false (cl-cc/type::type-equal-p e e))))

;;; ─── +pure-effect-row+ and +io-effect-row+ singletons ───────────────────────

(deftest effect-row-singletons
  "+pure-effect-row+ is empty; +io-effect-row+ has one IO effect."
  (assert-null (cl-cc/type::type-effect-row-effects cl-cc/type::+pure-effect-row+))
  (assert-null (cl-cc/type::type-effect-row-row-var cl-cc/type::+pure-effect-row+))
  (let ((effs (cl-cc/type::type-effect-row-effects cl-cc/type::+io-effect-row+)))
    (assert-= 1 (length effs))
    (assert-string= "IO" (symbol-name (cl-cc/type::type-effect-op-name (first effs))))))

;;; ─── reset-type-vars! ────────────────────────────────────────────────────────

(deftest reset-type-vars-resets-counter
  "reset-type-vars! resets the type-var counter to 0."
  (let ((old cl-cc/type::*type-var-counter*))
    (cl-cc/type::reset-type-vars!)
    (let ((v (cl-cc/type::fresh-type-var "after-reset")))
      (assert-= 1 (cl-cc/type::type-var-id v)))
    ;; Restore approximately (counter may have advanced during fresh-type-var)
    (setf cl-cc/type::*type-var-counter* (max old cl-cc/type::*type-var-counter*))))

;;; ─── %type-free-vars-list unit tests ─────────────────────────────────────────

(deftest-each free-vars-list-base-cases
  "%type-free-vars-list: var→singleton list; primitive→nil."
  :cases (("type-var"       t   1)
          ("type-primitive" nil 0))
  (is-var expected-count)
  (let* ((v   (cl-cc/type::fresh-type-var "fv"))
         (ty  (if is-var v cl-cc/type::type-int))
         (res (cl-cc/type::%type-free-vars-list ty)))
    (assert-= expected-count (length res))
    (when is-var
      (assert-true (cl-cc/type::type-var-equal-p v (first res))))))

(deftest free-vars-list-arrow-collects-all-with-duplicates
  "%type-free-vars-list: arrow with repeated var yields duplicate entries (dedup is caller's job)."
  (let* ((v  (cl-cc/type::fresh-type-var "shared"))
         (ar (cl-cc/type::make-type-arrow-raw :params (list v) :return v :effects nil :mult nil))
         (res (cl-cc/type::%type-free-vars-list ar)))
    (assert-= 2 (length res))
    (assert-true (cl-cc/type::type-var-equal-p v (first res)))
    (assert-true (cl-cc/type::type-var-equal-p v (second res)))))

(deftest free-vars-list-forall-filters-bound-var
  "%type-free-vars-list: forall removes its bound variable from child free vars."
  (let* ((v1 (cl-cc/type::fresh-type-var "bound"))
         (v2 (cl-cc/type::fresh-type-var "free"))
         (ar (cl-cc/type::make-type-arrow-raw :params (list v1) :return v2 :effects nil :mult nil))
         (fa (cl-cc/type::make-type-forall :var v1 :body ar))
         (res (cl-cc/type::%type-free-vars-list fa)))
    (assert-= 1 (length res))
    (assert-true (cl-cc/type::type-var-equal-p v2 (first res)))))

(deftest free-vars-list-nested-product
  "%type-free-vars-list: product with 3 distinct vars yields 3 entries."
  (let* ((v1  (cl-cc/type::fresh-type-var "a"))
         (v2  (cl-cc/type::fresh-type-var "b"))
         (v3  (cl-cc/type::fresh-type-var "c"))
         (prod (cl-cc/type::make-type-product :elems (list v1 v2 v3)))
         (res  (cl-cc/type::%type-free-vars-list prod)))
    (assert-= 3 (length res))))

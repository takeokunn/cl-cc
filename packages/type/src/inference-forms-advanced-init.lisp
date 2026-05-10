;;;; inference-forms-advanced-init.lisp — Advanced call policy specs, registry init, call inference
(in-package :cl-cc/type)

(defparameter +advanced-call-policy-specs+
  '((spawn :min-args 1 :validator %validate-advanced-spawn-call
           :return-type %advanced-call-future-return
           :summary "FR-1502/2201 Send-checked spawn boundary")
    (shared-ref :exact-args 1 :validator %validate-advanced-shared-ref-call
                :return-type type-any
                :summary "FR-1502 Sync-checked shared reference boundary")
    (foreign-call :min-args 1 :validator %validate-advanced-ffi-call
                  :return-type type-any
                  :summary "FR-2103 typed FFI descriptor boundary")
    (load-type-interface :exact-args 3 :validator %validate-advanced-interface-call
                         :return-type type-symbol
                         :summary "FR-2405 interface-file validation boundary")
    (smt-assert :exact-args 3 :validator %validate-advanced-smt-call
                :return-type type-bool
                :summary "FR-2406 SMT assertion validation boundary")
    (run-type-plugin :exact-args 2 :validator %validate-advanced-plugin-call
                     :return-type type-any
                     :summary "FR-3002 type-checker plugin hook boundary")
    (synthesize-program :exact-args 3 :validator %validate-advanced-synthesis-call
                        :return-type type-any
                        :summary "FR-3003 type-directed synthesis boundary")
    (apply-mapped-type :exact-args 2 :validator %validate-advanced-mapped-type-call
                       :return-type type-any
                       :summary "FR-3301 mapped type evaluation boundary")
    (apply-conditional-type :exact-args 5 :validator %validate-advanced-conditional-type-call
                            :return-type type-any
                            :summary "FR-3302 conditional type evaluation boundary")
    (make-typed-channel :min-args 1
                        :return-type %advanced-call-channel-return
                        :summary "FR-2202 typed channel constructor boundary")
    (make-buffered-channel :exact-args 2
                           :return-type %advanced-call-channel-return
                           :summary "FR-2202 buffered channel constructor boundary")
    (make-actor-ref :min-args 1
                    :return-type %advanced-call-actor-return
                    :summary "FR-2203 actor reference constructor boundary")
    (make-tvar :exact-args 2
               :return-type %advanced-call-stm-return
               :summary "FR-2204 TVar constructor boundary")
    (make-generator-type :exact-args 2
                         :return-type %advanced-call-generator-return
                         :summary "FR-2205 generator type constructor boundary")
    (make-coroutine-type :exact-args 3
                         :return-type %advanced-call-coroutine-return
                         :summary "FR-2205 coroutine type constructor boundary")
    (make-simd-type :exact-args 2
                    :return-type %advanced-call-simd-return
                    :summary "FR-2206 SIMD type constructor boundary")
    (make-api-type :min-args 4
                   :return-type %advanced-call-api-return
                   :summary "FR-3305 type-safe routing constructor boundary"))
  "Compiler-facing call policies that execute advanced FR validators during inference.")

(defun %initialize-advanced-call-policy-registry ()
  "Populate inference-time advanced call policy registry."
  (clrhash *advanced-call-policy-registry*)
  (dolist (spec +advanced-call-policy-specs+)
    (register-advanced-call-policy (%make-advanced-call-policy-from-spec spec)))
  t)

(defun validate-advanced-call (policy ast arg-types env)
  "Validate AST/ARG-TYPES against POLICY and return the inferred result type."
  (handler-case
      (or
        (%advanced-call-require-arity policy (length arg-types))
        (let ((validator (advanced-call-policy-validator policy)))
          (when validator
            (funcall (symbol-function validator) ast arg-types env)))
        (%advanced-call-return-value policy ast arg-types))
    (type-inference-error (condition)
      (error condition))
    (error (condition)
      (%advanced-call-error (advanced-call-policy-function-name policy)
                            "~A"
                            condition))))

(defun infer-advanced-call (ast env)
  "Infer a compiler-facing advanced semantic call.
Returns (values type substitution handled-p)."
  (let* ((function-name (%advanced-call-function-name (cl-cc/ast:ast-call-func ast)))
         (policy (lookup-advanced-call-policy function-name)))
    (if policy
        (let ((arg-types (infer-args (cl-cc/ast:ast-call-args ast) env)))
          (values (validate-advanced-call policy ast arg-types env) nil t))
        (values nil nil nil))))

(%initialize-advanced-call-policy-registry)


(defun %resolve-call-type (func-type arg-types result-type subst1 env)
  "Unify FUNC-TYPE with the expected arrow type for ARG-TYPES → RESULT-TYPE.
On success, checks typeclass constraints and returns (values type subst).
On failure, signals type-mismatch-error."
  (let ((expected-fn (make-type-arrow arg-types result-type)))
    (multiple-value-bind (subst ok) (type-unify func-type expected-fn subst1)
      (if ok
          (progn
            (check-qualified-constraints func-type subst env)
            (values (zonk result-type subst) subst))
          (error 'type-mismatch-error :expected expected-fn :actual func-type)))))

(defun infer-call (ast env)
  "Infer type for function call with typeclass constraint checking."
  (multiple-value-bind (advanced-type advanced-subst handled-p)
      (infer-advanced-call ast env)
    (if handled-p
        (values advanced-type advanced-subst)
        (multiple-value-bind (func-type subst1) (infer (cl-cc/ast:ast-call-func ast) env)
          (let* ((result-type (fresh-type-var))
                 (arg-types   (infer-args (cl-cc/ast:ast-call-args ast) (zonk-env env subst1))))
            (%resolve-call-type func-type arg-types result-type subst1 env))))))

(defun infer-args (asts env)
  "Infer types for list of ASTs (function arguments), threading substitution."
  (loop with subst = nil
        with current-env = env
        for ast in asts
        collect (multiple-value-bind (type new-subst) (infer ast current-env)
                  (setf subst (subst-compose new-subst subst)
                        current-env (zonk-env current-env new-subst))
                  (zonk type subst))))

(defun infer-body (asts env)
  "Infer type of sequence (return last type); single-pass over ASTS."
  (if (null asts)
      (values type-null nil)
      (loop with subst = nil
            with current-env = env
            with last-type = type-null
            for ast in asts
            do (multiple-value-bind (type new-subst) (infer ast current-env)
                 (setf subst       (subst-compose new-subst subst)
                       last-type   (zonk type subst)
                       current-env (zonk-env current-env new-subst)))
            finally (return (values last-type subst)))))


(defun infer-with-env (ast)
  "Infer type of AST in empty environment (convenience function)."
  (infer ast (type-env-empty)))

(defun infer-with-constraints (ast env)
  "Infer type by generating constraints and then solving them.
   Returns (values type substitution residual-constraints)."
  (multiple-value-bind (ty constraints)
      (collect-constraints ast env)
    (multiple-value-bind (subst residual)
        (solve-constraints constraints nil)
      (values (zonk ty subst) subst residual))))

;;; Type Annotation

(defun annotate-type (ast env)
  "Annotate AST with inferred types (for debugging).
   Returns (values type annotated-ast)."
  (multiple-value-bind (type subst) (infer ast env)
    (declare (ignore subst))
    (values type ast)))

;;; NOTE: Effect inference, bidirectional checking, constraint solving,
;;;       typeclass registries are in their respective files (loaded after).
;;;
;;; Condition classes (type-inference-error, typed-hole-error, unbound-variable-error,
;;; type-mismatch-error) and the export block are in inference-conditions.lisp (loaded next).

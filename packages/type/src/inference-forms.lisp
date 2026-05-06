(in-package :cl-cc/type)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Type Inference — Predicate Registry + Complex Form Handlers
;;;
;;; Contains: *type-predicate-table* (data), register-type-predicate,
;;; type-predicate-to-type, extract-type-guard, narrow-union-type,
;;; infer-if (type-guard narrowing), syntactic-value-p (FR-1604),
;;; infer-let, infer-lambda, infer-progn, infer-print,
;;; check-qualified-constraints, infer-call, infer-args, infer-body,
;;; infer-with-env, infer-with-constraints, annotate-type,
;;; and condition class definitions (type-error/type-mismatch-error/etc.).
;;;
;;; Basic infer-* handlers (infer-var through infer-binop), the `infer`
;;; dispatcher, and the class/registry setup are in inference.lisp (before).
;;;
;;; Load order: after inference.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Type Predicate Registry (data-driven, extensible)

(defvar *type-predicate-table* (make-hash-table :test #'eq)
  "Maps predicate name symbols to the type-node they test for.
   E.g., 'integerp → type-int.  Extensible via register-type-predicate.")

(defun register-type-predicate (predicate-name type-node)
  "Register PREDICATE-NAME as testing for TYPE-NODE."
  (setf (gethash predicate-name *type-predicate-table*) type-node))

(defun type-predicate-to-type (predicate-name)
  "Map a type predicate name to the type it tests for.
   Looks up *type-predicate-table*; returns nil if not registered."
  (gethash predicate-name *type-predicate-table*))

;;; Built-in predicate→type pairs. This is the data; registration is the logic.
(defparameter *builtin-type-predicates*
  `((numberp    . ,type-int)
    (integerp   . ,type-int)
    (stringp    . ,type-string)
    (symbolp    . ,type-symbol)
    (consp      . ,type-cons)
    (null       . ,type-null)
    (characterp . ,(make-type-primitive :name 'character))
    (functionp  . ,(make-type-primitive :name 'function)))
  "Association list of (predicate-name . type-node) for built-in type predicates.")

(dolist (pair *builtin-type-predicates*)
  (register-type-predicate (car pair) (cdr pair)))

(defun extract-type-guard (cond-ast)
  "Extract type guard info from a condition AST.
   If the condition is (predicate var) where predicate is a type predicate,
   returns (values var-name narrowed-type) or (values nil nil).
   Also handles (typep var 'classname) pattern."
  (when (typep cond-ast 'cl-cc/ast:ast-call)
    (let ((func (cl-cc/ast:ast-call-func cond-ast))
          (args (cl-cc/ast:ast-call-args cond-ast)))
      (cond
        ;; (predicate var) pattern
        ((and (typep func 'cl-cc/ast:ast-var)
              (= (length args) 1)
              (typep (first args) 'cl-cc/ast:ast-var))
         (let ((pred-type (type-predicate-to-type (cl-cc/ast:ast-var-name func))))
           (when pred-type
             (values (cl-cc/ast:ast-var-name (first args)) pred-type))))
        ;; (typep var 'classname) pattern
        ((and (typep func 'cl-cc/ast:ast-var)
              (eq (cl-cc/ast:ast-var-name func) 'typep)
              (= (length args) 2)
              (typep (first args) 'cl-cc/ast:ast-var)
              (typep (second args) 'cl-cc/ast:ast-quote)
              (symbolp (cl-cc/ast:ast-quote-value (second args))))
         (let* ((class-name (cl-cc/ast:ast-quote-value (second args)))
                (prim-type (make-type-primitive :name class-name)))
           (values (cl-cc/ast:ast-var-name (first args)) prim-type)))
        (t (values nil nil))))))

(defun narrow-union-type (union-type keep-type)
  "Remove KEEP-TYPE from UNION-TYPE, returning the remaining types.
   If UNION-TYPE is (or A B C) and KEEP-TYPE is A, returns (or B C)."
  (if (typep union-type 'type-union)
      (let ((remaining (remove-if (lambda (t1) (type-equal-p t1 keep-type))
                                  (type-union-types union-type))))
        (cond
          ((null remaining) +type-unknown+)
          ((= (length remaining) 1) (first remaining))
          (t (make-type-union remaining))))
      union-type))

(defun %infer-if-condition (ast env)
  "Infer the condition type of AST in ENV, returning the substitution.
Returns NIL when the condition check fails (gradual typing: allow failure)."
  (handler-case
      (multiple-value-bind (ct s1) (infer (cl-cc/ast:ast-if-cond ast) env)
        (declare (ignore ct))
        s1)
    (error () nil)))

(defun %narrow-else-env (guard-var guard-type base-env)
  "Return BASE-ENV with GUARD-VAR's union type narrowed to exclude GUARD-TYPE."
  (multiple-value-bind (scheme found-p) (type-env-lookup guard-var base-env)
    (let ((var-type (if found-p (instantiate scheme) nil)))
      (if (and var-type (typep var-type 'type-union))
          (type-env-extend guard-var
                           (make-type-scheme nil (narrow-union-type var-type guard-type))
                           base-env)
          base-env))))

(defun %build-if-branch-envs (ast env cond-subst)
  "Return (values then-env else-env) with narrowed types for each branch of AST."
  (multiple-value-bind (guard-var guard-type)
      (extract-type-guard (cl-cc/ast:ast-if-cond ast))
    (let ((base-env (zonk-env env cond-subst)))
      (if guard-var
          (values (type-env-extend guard-var (make-type-scheme nil guard-type) base-env)
                  (%narrow-else-env guard-var guard-type base-env))
          (values base-env base-env)))))

(defun %unify-if-branches (ast then-env else-env)
  "Infer both branches of AST and unify their types. Returns (values type subst)."
  (multiple-value-bind (then-type subst2) (infer (cl-cc/ast:ast-if-then ast) then-env)
    (multiple-value-bind (else-type subst3)
        (infer (cl-cc/ast:ast-if-else ast) (zonk-env else-env subst2))
      (multiple-value-bind (final-subst _) (type-unify then-type else-type subst3)
        (values (zonk then-type final-subst) final-subst)))))

(defun infer-if (ast env)
  "Infer type for if expression with type-guard narrowing."
  (let ((cond-subst (%infer-if-condition ast env)))
    (multiple-value-bind (then-env else-env)
        (%build-if-branch-envs ast env cond-subst)
      (%unify-if-branches ast then-env else-env))))

;;; FR-1604: Value Restriction
;;; Only syntactic values (lambda, literal, variable, quote, #'fn) may be
;;; generalized.  Applications and other impure expressions are kept monomorphic
;;; to prevent unsound polymorphism with mutable state.

;;; Value restriction (FR-1604): Only syntactic values may be generalized.
;;; Applications and impure expressions stay monomorphic to prevent unsound
;;; polymorphism with mutable state.
(defparameter *syntactic-value-ast-types*
  '(cl-cc/ast:ast-int
    cl-cc/ast:ast-var
    cl-cc/ast:ast-lambda
    cl-cc/ast:ast-quote
    cl-cc/ast:ast-function
    cl-cc/ast:ast-hole)
  "AST node types that qualify as syntactic values under the value restriction.")

(defun syntactic-value-p (ast)
  "Return T if AST is a syntactic value safe to generalize (value restriction)."
  (some (lambda (type) (typep ast type)) *syntactic-value-ast-types*))

(defun infer-let (ast env)
  "Infer type for let with polymorphism (value restriction applied).
   Only syntactic values are generalized; applications stay monomorphic."
  (let ((new-env env))
    (dolist (binding (cl-cc/ast:ast-let-bindings ast))
      (let* ((name (car binding))
             (expr (cdr binding))
             (type (multiple-value-bind (result-type s) (infer expr new-env)
                     (zonk result-type s)))
             ;; Value restriction: generalize only syntactic values.
             (scheme (if (syntactic-value-p expr)
                         (generalize new-env type)
                         (make-type-scheme nil type))))
        (setf new-env (type-env-extend name scheme new-env))))
    (infer-body (cl-cc/ast:ast-let-body ast) new-env)))

(defun %make-lambda-param-env (params env)
  "Assign fresh type vars to PARAMS, extend ENV, return (values types body-env)."
  (when (null params)
    (return-from %make-lambda-param-env (values nil env)))
  (loop for p in params
        for tv = (fresh-type-var)
        collect tv into types
        collect (cons p (make-type-scheme nil tv)) into bindings
        finally (return (values types (type-env-extend* bindings env)))))

(defun infer-lambda (ast env)
  "Infer type for lambda."
  (multiple-value-bind (param-types body-env)
      (%make-lambda-param-env (cl-cc/ast:ast-lambda-params ast) env)
    (multiple-value-bind (body-type subst)
        (infer-body (cl-cc/ast:ast-lambda-body ast) body-env)
      (values (make-type-arrow (mapcar (lambda (p) (zonk p subst)) param-types) body-type)
              subst))))

(defun infer-progn (ast env)
  "Infer type for progn (sequence of expressions)."
  (infer-body (cl-cc/ast:ast-progn-forms ast) env))

(defun infer-print (ast env)
  "Infer type for print (returns type of printed expression)."
  (infer (cl-cc/ast:ast-print-expr ast) env))

(defun check-qualified-constraints (func-type subst env)
  "If FUNC-TYPE is a qualified type (Eq a) => ..., verify each constraint
   is satisfied for the instantiated type argument."
  (when (typep func-type 'type-qualified)
    (dolist (constraint (type-qualified-constraints func-type))
      (let* ((class-name (type-constraint-class-name constraint))
             (type-arg (zonk
                        (type-constraint-type-arg constraint)
                        subst)))
        (unless (or (type-unknown-p type-arg)
                    (type-var-p type-arg)
                    (and (type-env-p env)
                         (dict-env-lookup class-name type-arg env))
                    (has-typeclass-instance-p class-name type-arg))
           (error 'type-inference-error
                  :message (format nil "No instance of ~A for ~A"
                                   class-name
                                   (type-to-string type-arg))))))))

;;; FR-1502/2103/2405/2406/3002/3003/3301/3302: compiler-facing
;;; advanced semantic call policies.  These hooks make advanced features reject
;;; invalid typed program shapes during inference instead of only parsing
;;; metadata nodes.

(defstruct (advanced-call-policy (:constructor %make-advanced-call-policy))
  "Inference-time policy for an advanced compiler/type-checker-facing call."
  (function-name nil :type symbol)
  (exact-args nil)
  (min-args nil)
  (validator nil)
  (return-type nil)
  (summary "" :type string))

(defvar *advanced-call-policy-registry* (make-hash-table :test #'equal)
  "Maps known compiler-facing advanced call names to inference validators.")

(defun %advanced-call-name-key (function-name)
  "Return a package-independent key for FUNCTION-NAME."
  (and (symbolp function-name)
       (string-upcase (symbol-name function-name))))

(defun register-advanced-call-policy (policy)
  "Register POLICY and return it."
  (setf (gethash (%advanced-call-name-key (advanced-call-policy-function-name policy))
                 *advanced-call-policy-registry*)
        policy)
  policy)

(defun lookup-advanced-call-policy (function-name)
  "Return a compiler-facing advanced call policy for FUNCTION-NAME, if any."
  (gethash (%advanced-call-name-key function-name) *advanced-call-policy-registry*))

(defun %make-advanced-call-policy-from-spec (spec)
  "Build an advanced call policy from a plist-like SPEC."
  (destructuring-bind (name &key exact-args min-args validator return-type summary) spec
    (%make-advanced-call-policy :function-name name
                                :exact-args exact-args
                                :min-args min-args
                                :validator validator
                                :return-type return-type
                                :summary (or summary ""))))

(defun %advanced-call-function-name (func)
  "Return FUNC's symbolic name when AST-CALL names a direct function."
  (cond
    ((symbolp func) func)
    ((typep func 'cl-cc/ast:ast-var) (cl-cc/ast:ast-var-name func))
    (t nil)))

(defun %advanced-call-error (function-name format-control &rest args)
  "Signal a type inference error for an invalid advanced call."
  (error 'type-inference-error
         :message (format nil "Advanced call ~S failed semantic validation: ~?"
                          function-name
                          format-control
                          args)))

(defun %advanced-call-require-arity (policy arg-count)
  "Validate ARG-COUNT against POLICY's arity contract."
  (let ((name (advanced-call-policy-function-name policy))
        (exact (advanced-call-policy-exact-args policy))
        (minimum (advanced-call-policy-min-args policy)))
    (when (and exact (/= arg-count exact))
      (%advanced-call-error name "expected exactly ~D argument(s), got ~D" exact arg-count))
    (when (and minimum (null exact) (< arg-count minimum))
      (%advanced-call-error name "expected at least ~D argument(s), got ~D" minimum arg-count))))

(defun %advanced-call-type-designator (type)
  "Conservatively map TYPE to a host designator for Send/Sync checks."
  (cond
    ((type-primitive-p type) (type-primitive-name type))
    ((type-arrow-p type) 'function)
    ((type-product-p type) 'cons)
    ((type-record-p type) 'cons)
    ((type-variant-p type) 'cons)
    ((type-union-p type) 'union)
    ((type-constructor-p type) (type-constructor-name type))
    ((type-advanced-p type) (type-advanced-name type))
    ((type-unknown-p type) nil)
    (t nil)))

(defun %advanced-call-quoted-arg (ast index function-name)
  "Return AST's quoted argument at INDEX, or signal a semantic validation error."
  (let ((arg (nth index (cl-cc/ast:ast-call-args ast))))
    (cond
      ((typep arg 'cl-cc/ast:ast-quote)
       (cl-cc/ast:ast-quote-value arg))
      ((typep arg 'cl-cc/ast:ast-int)
       (cl-cc/ast:ast-int-value arg))
      (t
       (%advanced-call-error function-name
                             "argument ~D must be a quoted/static descriptor, got ~S"
                              index
                              arg)))))

(defun %advanced-call-symbol-keyword (value)
  "Normalize a symbolic VALUE into a keyword for policy dispatch."
  (cond
    ((keywordp value) value)
    ((symbolp value) (intern (string-upcase (symbol-name value)) :keyword))
    ((stringp value) (intern (string-upcase value) :keyword))
    (t value)))

(defun %advanced-call-parse-type-form (value function-name)
  "Parse VALUE as a type form for FUNCTION-NAME, preserving existing type nodes."
  (handler-case
      (if (typep value 'type-node)
          value
          (parse-type-specifier value))
    (error (condition)
      (%advanced-call-error function-name
                            "could not parse type form ~S: ~A"
                            value
                            condition))))

(defun %advanced-call-optional-type (type)
  "Return TYPE widened with NULL unless it already allows NULL."
  (if (and (type-union-p type)
           (some (lambda (member) (type-equal-p member type-null))
                 (type-union-types type)))
      type
      (make-type-union (list type-null type) :constructor-name 'option)))

(defun %advanced-call-non-nullable-type (type)
  "Return TYPE with NULL removed from unions, or TYPE unchanged otherwise."
  (if (type-union-p type)
      (let ((members (remove-if (lambda (member) (type-equal-p member type-null))
                                (type-union-types type))))
        (cond
          ((null members) type-null)
          ((= (length members) 1) (first members))
          (t (make-type-union members :constructor-name (type-union-constructor-name type)))))
      type))

(defun %advanced-call-map-record-fields (type field-fn)
  "Map FIELD-FN across record field types, preserving row variables."
  (if (type-record-p type)
      (make-type-record :fields (mapcar (lambda (field)
                                          (cons (car field) (funcall field-fn (cdr field))))
                                        (type-record-fields type))
                        :row-var (type-record-row-var type))
      (funcall field-fn type)))

(defun %advanced-call-apply-mapped-transform (base transform function-name)
  "Evaluate a FR-3301 mapped type transform into an actual type-node."
  (let ((kind (%advanced-call-symbol-keyword transform)))
    (case kind
      ((:optional :option)
       (%advanced-call-optional-type base))
      ((:non-nullable :required)
       (%advanced-call-map-record-fields base #'%advanced-call-non-nullable-type))
      (:partial
       (%advanced-call-map-record-fields base #'%advanced-call-optional-type))
      (:readonly
       (make-type-capability :base base :cap 'readonly))
      (:record
       (if (type-record-p base)
           base
           (make-type-record :fields (list (cons 'value base)) :row-var nil)))
      (otherwise
       (%advanced-call-error function-name
                             "mapped transform ~S is validated but not executable"
                             transform)))))

(defun %advanced-call-type-head-name (type)
  "Return TYPE's primitive or constructor head name."
  (cond
    ((type-primitive-p type) (type-primitive-name type))
    ((type-constructor-p type) (type-constructor-name type))
    (t nil)))

(defun %advanced-call-same-symbol-name-p (left right)
  "Compare two symbolic designators package-independently."
  (and (symbolp left)
       (symbolp right)
       (string= (symbol-name left) (symbol-name right))))

(defun %advanced-call-type-extends-p (base extends)
  "Return T when BASE satisfies conditional type EXTENDS."
  (or (is-subtype-p base extends)
      (let ((base-head (%advanced-call-type-head-name base))
            (extends-head (%advanced-call-type-head-name extends)))
        (and base-head extends-head
             (string= (symbol-name base-head) (symbol-name extends-head))))))

(defun %advanced-call-inferred-branch-type (branch infer-var base function-name)
  "Resolve a conditional type branch, replacing INFER-VAR with BASE's first argument."
  (if (and infer-var (%advanced-call-same-symbol-name-p branch infer-var))
      (or (first (and (type-constructor-p base) (type-constructor-args base)))
          base)
      (%advanced-call-parse-type-form branch function-name)))

(defun %advanced-call-return-value (policy ast arg-types)
  "Return POLICY's inference result type."
  (let ((return-type (advanced-call-policy-return-type policy)))
    (cond
      ((functionp return-type) (funcall return-type ast arg-types))
      ((and (symbolp return-type) (fboundp return-type))
       (funcall (symbol-function return-type) ast arg-types))
      ((and (symbolp return-type) (boundp return-type))
       (symbol-value return-type))
      (return-type return-type)
       (t type-any))))

(defun %advanced-call-type-arg (ast arg-types index function-name)
  "Resolve argument INDEX as a type node for FUNCTION-NAME.
Prefers quoted/static type forms; falls back to inferred ARG-TYPES when needed."
  (let ((arg (nth index (cl-cc/ast:ast-call-args ast))))
    (cond
      ((typep arg 'cl-cc/ast:ast-quote)
       (%advanced-call-parse-type-form (cl-cc/ast:ast-quote-value arg) function-name))
      (t
       (or (nth index arg-types)
           (%advanced-call-error function-name
                                "missing argument ~D"
                                index))))))

(defun %advanced-call-future-return (_ast arg-types)
  "Return a concrete Future advanced type for SPAWN."
  (declare (ignore _ast))
  (make-type-advanced :feature-id "FR-2201"
                      :name 'future
                      :args (list (or (first arg-types) +type-unknown+))
                       :properties '((:mode . eager))))

(defun %advanced-call-channel-return (ast arg-types)
  "Return FR-2202 channel type for typed channel constructor calls."
  (make-channel-type (%advanced-call-type-arg ast arg-types 0 'make-typed-channel)))

(defun %advanced-call-actor-return (ast arg-types)
  "Return FR-2203 actor-ref type for actor constructor calls."
  (actor-ref-type (%advanced-call-type-arg ast arg-types 0 'make-actor-ref)))

(defun %advanced-call-stm-return (ast arg-types)
  "Return FR-2204 STM type for TVar constructor calls."
  (declare (ignore arg-types))
  (make-stm-type (%advanced-call-quoted-arg ast 0 'make-tvar)))

(defun %advanced-call-generator-return (ast arg-types)
  "Return FR-2205 generator type for generator constructor calls."
  (declare (ignore arg-types))
  (make-generator-type (%advanced-call-quoted-arg ast 0 'make-generator-type)
                       (%advanced-call-quoted-arg ast 1 'make-generator-type)))

(defun %advanced-call-coroutine-return (ast arg-types)
  "Return FR-2205 coroutine type for coroutine constructor calls."
  (declare (ignore arg-types))
  (make-coroutine-type (%advanced-call-quoted-arg ast 0 'make-coroutine-type)
                       (%advanced-call-quoted-arg ast 1 'make-coroutine-type)
                       (%advanced-call-quoted-arg ast 2 'make-coroutine-type)))

(defun %advanced-call-simd-return (ast arg-types)
  "Return FR-2206 SIMD type for SIMD constructor calls."
  (declare (ignore arg-types))
  (make-simd-type (%advanced-call-quoted-arg ast 0 'make-simd-type)
                  (%advanced-call-quoted-arg ast 1 'make-simd-type)))

(defun %advanced-call-api-return (ast arg-types)
  "Return FR-3305 API type for make-api-type calls."
  (declare (ignore arg-types))
  (let* ((method (%advanced-call-quoted-arg ast 0 'make-api-type))
         (path (%advanced-call-quoted-arg ast 1 'make-api-type))
         (parameter-specs (%advanced-call-quoted-arg ast 2 'make-api-type))
         (response-type (%advanced-call-quoted-arg ast 3 'make-api-type))
         (request-type (ignore-errors (%advanced-call-quoted-arg ast 4 'make-api-type)))
         (descriptor (list method path parameter-specs response-type))
         (descriptor* (if request-type
                          (append descriptor (list :request request-type))
                          descriptor)))
    (parse-type-specifier (list 'api-type descriptor*))))

(defun %validate-advanced-spawn-call (ast arg-types _env)
  "Enforce Send for values crossing a spawn boundary."
  (declare (ignore ast _env))
  (loop for ty in arg-types
        for designator = (%advanced-call-type-designator ty)
        unless (and designator (sendable-type-p designator))
          do (%advanced-call-error 'spawn
                                   "argument type ~A is not Send"
                                   (type-to-string ty))))

(defun %validate-advanced-shared-ref-call (ast arg-types _env)
  "Enforce Sync for values shared by reference."
  (declare (ignore ast _env))
  (loop for ty in arg-types
        for designator = (%advanced-call-type-designator ty)
        unless (and designator (shareable-type-p designator))
          do (%advanced-call-error 'shared-ref
                                   "argument type ~A is not Sync"
                                   (type-to-string ty))))

(defun %validate-advanced-ffi-call (ast arg-types _env)
  "Validate typed FFI descriptors at inference time."
  (declare (ignore _env))
  (let* ((descriptor-form (%advanced-call-quoted-arg ast 0 'foreign-call))
         (descriptor (ffi-descriptor-from-form descriptor-form))
         (argument-descriptors (if (ffi-function-descriptor-p descriptor)
                                    (ffi-function-descriptor-argument-types descriptor)
                                    (list descriptor)))
         (runtime-arg-types (rest arg-types)))
    (unless (ffi-descriptor-form-valid-p descriptor)
      (%advanced-call-error 'foreign-call "malformed FFI descriptor ~S" descriptor-form))
    (unless (= (length argument-descriptors) (length runtime-arg-types))
      (%advanced-call-error 'foreign-call
                            "descriptor expects ~D argument(s), got ~D"
                            (length argument-descriptors)
                            (length runtime-arg-types)))
    (loop for arg-type in runtime-arg-types
          for descriptor-type in argument-descriptors
          for designator = (%advanced-call-type-designator arg-type)
          unless (and designator
                      (ffi-lisp-type-compatible-p designator descriptor-type))
            do (%advanced-call-error 'foreign-call
                                     "argument type ~A is incompatible with FFI descriptor ~S"
                                     (type-to-string arg-type)
                                     descriptor-type))
    (if (ffi-function-descriptor-p descriptor)
        (ffi-descriptor-lisp-type (ffi-function-descriptor-return-type descriptor))
        type-any)))

(defun %validate-advanced-interface-call (ast _arg-types _env)
  "Validate interface-file metadata before accepting an interface load."
  (declare (ignore _arg-types _env))
  (let ((module (%advanced-call-quoted-arg ast 0 'load-type-interface))
        (exports (%advanced-call-quoted-arg ast 1 'load-type-interface))
        (fingerprint (%advanced-call-quoted-arg ast 2 'load-type-interface)))
    (register-type-interface module exports fingerprint)
    (make-type-advanced :feature-id "FR-2405"
                        :name 'interface-file
                        :args (list module)
                        :properties (list (cons :exports exports)
                                          (cons :fingerprint fingerprint)))
    type-symbol))

(defun %validate-advanced-smt-call (ast _arg-types _env)
  "Validate SMT solver/theory metadata before accepting an SMT assertion."
  (declare (ignore _arg-types _env))
  (let ((constraint (%advanced-call-quoted-arg ast 0 'smt-assert))
        (solver (%advanced-call-quoted-arg ast 1 'smt-assert))
        (theory (%advanced-call-quoted-arg ast 2 'smt-assert)))
    (solve-smt-constraint constraint solver theory)
    (make-type-advanced :feature-id "FR-2406"
                        :name 'smt
                        :args (list constraint)
                        :properties (list (cons :solver solver)
                                          (cons :theory theory)
                                          (cons :counterexample :none)))
    type-bool))

(defun %validate-advanced-plugin-call (ast _arg-types _env)
  "Validate type-checker plugin hook metadata at inference time."
  (let ((plugin (%advanced-call-quoted-arg ast 0 'run-type-plugin))
        (phase (%advanced-call-quoted-arg ast 1 'run-type-plugin)))
    (let ((result (run-type-checker-plugin plugin phase ast _arg-types _env)))
      (make-type-advanced :feature-id "FR-3002"
                          :name 'type-checker-plugin
                          :args (list plugin)
                          :properties (list (cons :hook plugin)
                                            (cons :phase phase)
                                            (cons :result result)))
      (or (getf result :type) type-any))))

(defun %validate-advanced-synthesis-call (ast _arg-types _env)
  "Validate type-directed synthesis metadata at inference time."
  (declare (ignore _arg-types _env))
  (let ((signature (%advanced-call-quoted-arg ast 0 'synthesize-program))
        (strategy (%advanced-call-quoted-arg ast 1 'synthesize-program))
        (fuel (%advanced-call-quoted-arg ast 2 'synthesize-program)))
    (run-type-synthesis signature strategy fuel)
    (make-type-advanced :feature-id "FR-3003"
                        :name 'program-synthesis
                        :args (list signature)
                        :properties (list (cons :search strategy)
                                          (cons :fuel fuel)))
    (%advanced-call-parse-type-form signature 'synthesize-program)))

(defun %validate-advanced-mapped-type-call (ast _arg-types _env)
  "Validate mapped type evaluation metadata at inference time."
  (declare (ignore _arg-types _env))
  (let* ((base-form (%advanced-call-quoted-arg ast 0 'apply-mapped-type))
         (transform (%advanced-call-quoted-arg ast 1 'apply-mapped-type))
         (base (%advanced-call-parse-type-form base-form 'apply-mapped-type)))
    (make-type-advanced :feature-id "FR-3301"
                        :name 'mapped-type
                        :args (list base-form)
                        :properties (list (cons :transform transform)))
    (%advanced-call-apply-mapped-transform base transform 'apply-mapped-type)))

(defun %validate-advanced-conditional-type-call (ast _arg-types _env)
  "Validate conditional type evaluation metadata at inference time."
  (declare (ignore _arg-types _env))
  (let* ((base-form (%advanced-call-quoted-arg ast 0 'apply-conditional-type))
         (extends-form (%advanced-call-quoted-arg ast 1 'apply-conditional-type))
         (infer-var (%advanced-call-quoted-arg ast 2 'apply-conditional-type))
         (then-branch (%advanced-call-quoted-arg ast 3 'apply-conditional-type))
         (else-branch (%advanced-call-quoted-arg ast 4 'apply-conditional-type))
         (base (%advanced-call-parse-type-form base-form 'apply-conditional-type))
         (extends (%advanced-call-parse-type-form extends-form 'apply-conditional-type)))
    (make-type-advanced :feature-id "FR-3302"
                        :name 'conditional-type
                        :args (list base-form)
                        :properties (list (cons :extends extends)
                                          (cons :infer infer-var)
                                          (cons :then then-branch)
                                          (cons :else else-branch)))
    (if (%advanced-call-type-extends-p base extends)
        (%advanced-call-inferred-branch-type then-branch infer-var base 'apply-conditional-type)
        (%advanced-call-inferred-branch-type else-branch infer-var base 'apply-conditional-type))))

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

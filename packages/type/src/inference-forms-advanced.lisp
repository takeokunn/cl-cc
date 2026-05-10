;;;; inference-forms-advanced.lisp — Advanced call policy helpers
(in-package :cl-cc/type)

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


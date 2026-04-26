;;;; tests/pbt/macro-pbt-advanced-tests.lisp — Advanced PBT: Nested/Idempotency/Hygiene/Validity
(in-package :cl-cc/pbt)

(in-suite macro-pbt-suite)
;;; Property: Nested Macro Expansion

(defproperty nested-when-in-let-star-advanced-pbt
    (var (gen-symbol :prefix "X" :package nil)
     val (gen-integer :min -10 :max 10)
     test (gen-test-form)
     body (gen-body-form))
  "Nested WHEN in LET* fully expands both macros."
  (let ((expanded (cl-cc:our-macroexpand-all `(let* ((,var ,val)) (when ,test ,body)))))
    (and (consp expanded)
         (eq (car expanded) 'let)
         ;; Inner should have no WHEN or LET*
         (not (form-contains-symbol-p 'when expanded))
         (not (form-contains-symbol-p 'let* expanded)))))

(defproperty nested-cond-in-and-advanced-pbt
    (test1 (gen-test-form)
     test2 (gen-test-form)
     body (gen-body-form))
  "Nested COND in AND fully expands both macros."
  (let ((expanded (cl-cc:our-macroexpand-all `(and ,test1 (cond ((,test2 ,body)))))))
    (and (not (form-contains-symbol-p 'cond expanded))
         (not (form-contains-symbol-p 'and expanded)))))

(defproperty nested-let-star-in-let-star-advanced-pbt
    (bindings1 (gen-binding-list :min-length 1 :max-length 2)
     bindings2 (gen-binding-list :min-length 1 :max-length 2)
     body (gen-body-form))
  "Nested LET* fully expands to nested LETs."
  (let ((expanded (cl-cc:our-macroexpand-all `(let* ,bindings1 (let* ,bindings2 ,body)))))
    (and (not (form-contains-symbol-p 'let* expanded))
         ;; Should have nested LETs
         (eq (car expanded) 'let))))

(defproperty nested-or-in-prog1-advanced-pbt
    (args1 (gen-list-of (gen-body-form) :min-length 2 :max-length 3)
     args2 (gen-list-of (gen-body-form) :min-length 2 :max-length 3))
  "Nested OR in PROG1 fully expands."
  (let ((expanded (cl-cc:our-macroexpand-all `(prog1 (or ,@args1) (or ,@args2)))))
    (and (not (form-contains-symbol-p 'or expanded))
         (consp expanded)
         (eq (car expanded) 'let))))

;;; Property: Macro Expansion Idempotency

(deftest macroexpand-idempotent-when
  "Fully expanding representative WHEN forms twice gives the same result."
  (dolist (form '((when t body)
                  (when flag body1 body2)
                  (when (= x 0) (print 1))))
    (let* ((exp1 (cl-cc:our-macroexpand form))
           (exp2 (cl-cc:our-macroexpand exp1)))
      (assert-equal exp1 exp2))))

(deftest macroexpand-idempotent-unless
  "Fully expanding representative UNLESS forms twice gives the same result."
  (dolist (form '((unless t body)
                  (unless flag body1 body2)
                  (unless (= x 0) (print 1))))
    (let* ((exp1 (cl-cc:our-macroexpand form))
           (exp2 (cl-cc:our-macroexpand exp1)))
      (assert-equal exp1 exp2))))

(deftest macroexpand-idempotent-and
  "Fully expanding representative AND forms twice gives the same result."
  (dolist (form '((and a b)
                  (and a b c)
                  (and (= x 0) flag (print 1))))
    (let* ((exp1 (cl-cc:our-macroexpand form))
           (exp2 (cl-cc:our-macroexpand exp1)))
      (assert-equal exp1 exp2))))

(defproperty macroexpand-idempotent-or
    (args (gen-list-of (gen-body-form) :min-length 2 :max-length 4))
  "Fully expanding OR twice gives same result."
  (let* ((form `(or ,@args))
         (exp1 (cl-cc:our-macroexpand form))
         (exp2 (cl-cc:our-macroexpand exp1)))
    (equal exp1 exp2)))

(defproperty macroexpand-idempotent-let-star
    (bindings (gen-binding-list :min-length 1 :max-length 3)
     body (gen-list-of (gen-body-form) :min-length 1 :max-length 2))
  "Fully expanding LET* twice gives same result."
  (let* ((form `(let* ,bindings ,@body))
         (exp1 (cl-cc:our-macroexpand form))
         (exp2 (cl-cc:our-macroexpand exp1)))
    (equal exp1 exp2)))

;;; Property: Macro Hygiene (Gensym Usage)

(defproperty prog1-hygiene
    (first-form (gen-body-form)
     body (gen-list-of (gen-body-form) :min-length 1 :max-length 3))
  "PROG1 uses gensym for result variable (not user-accessible name)."
  (let ((expanded (cl-cc:our-macroexpand-1 `(prog1 ,first-form ,@body))))
    (let ((result-var (caar (second expanded))))
      ;; Should be a gensym (starts with G or contains digits)
      (form-contains-gensym-p expanded))))

(defproperty prog2-hygiene
    (first-form (gen-body-form)
     second-form (gen-body-form)
     body (gen-list-of (gen-body-form) :min-length 0 :max-length 2))
  "PROG2 uses gensym for result variable."
  (let ((expanded (cl-cc:our-macroexpand-1 `(prog2 ,first-form ,second-form ,@body))))
    (form-contains-gensym-p expanded)))

(defproperty or-hygiene
    (args (gen-list-of (gen-body-form) :min-length 2 :max-length 4))
  "OR uses gensym for temporary variable."
  (let ((expanded (cl-cc:our-macroexpand-1 `(or ,@args))))
    (form-contains-gensym-p expanded)))

(defproperty psetq-hygiene
    (pairs (gen-list-of (gen-binding-pair) :min-length 1 :max-length 3))
  "PSETQ uses gensyms for temporary variables."
  (let* ((flat-pairs (apply #'append pairs))
         (expanded (cl-cc:our-macroexpand-1 `(psetq ,@flat-pairs))))
    (form-contains-gensym-p expanded)))

(defproperty mvsq-hygiene
    (vars (gen-variable-list :min-length 1 :max-length 3)
     form (gen-body-form))
  "MULTIPLE-VALUE-SETQ uses gensym for temporary."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-setq ,vars ,form))))
    (form-contains-gensym-p expanded)))

(defproperty mvl-hygiene
    (form (gen-body-form))
  "MULTIPLE-VALUE-LIST uses gensyms for accumulation."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-list ,form))))
    (form-contains-gensym-p expanded)))

;;; Property: Unique Gensym per Expansion

(defproperty prog1-unique-gensym-per-expansion
    (first-form (gen-body-form)
     body (gen-body-form))
  "Each PROG1 expansion gets a unique gensym."
  (let* ((form `(prog1 ,first-form ,body))
         (exp1 (cl-cc:our-macroexpand-1 form))
         (exp2 (cl-cc:our-macroexpand-1 form))
         (gensym1 (caar (second exp1)))
         (gensym2 (caar (second exp2))))
    (not (eq gensym1 gensym2))))

(defproperty or-unique-gensym-per-expansion
    (a (gen-body-form)
     b (gen-body-form))
  "Each OR expansion gets a unique gensym."
  (let* ((form `(or ,a ,b))
         (exp1 (cl-cc:our-macroexpand-1 form))
         (exp2 (cl-cc:our-macroexpand-1 form))
         (gensym1 (caar (second exp1)))
         (gensym2 (caar (second exp2))))
    (not (eq gensym1 gensym2))))

;;; Property: Expansion Structure Validity

(defproperty when-valid-lisp-form
    (test (gen-test-form)
     body (gen-list-of (gen-body-form) :min-length 0 :max-length 3))
  "WHEN expansion is always a valid Lisp form."
  (let ((expanded (cl-cc:our-macroexpand-1 `(when ,test ,@body))))
    (and (consp expanded)
         (symbolp (car expanded))
         (listp (cdr expanded)))))

(defproperty and-valid-lisp-form
    (args (gen-list-of (gen-body-form) :min-length 0 :max-length 5))
  "AND expansion is always a valid Lisp form."
  ;; (and) => t (symbol), (and x) => x (any atom), (and x y) => (if x (and y) nil) (cons)
  (let ((expanded (cl-cc:our-macroexpand-1 `(and ,@args))))
    (or (atom expanded)
        (and (consp expanded)
             (symbolp (car expanded))
             (listp (cdr expanded))))))

(defproperty let-star-valid-lisp-form
    (bindings (gen-binding-list :min-length 0 :max-length 4)
     body (gen-list-of (gen-body-form) :min-length 1 :max-length 3))
  "LET* expansion is always a valid Lisp form."
  (let ((expanded (cl-cc:our-macroexpand-1 `(let* ,bindings ,@body))))
    (and (consp expanded)
         (symbolp (car expanded))
         (listp (cdr expanded)))))

;;; Property: Macroexpand-All Recursiveness

(defproperty macroexpand-all-reaches-all-subforms
    (test (gen-test-form)
     body1 (gen-body-form)
     body2 (gen-body-form))
  "our-macroexpand-all expands macros in all subforms."
  (let ((form `(when (when ,test ,body1) ,body2))
        (expanded (cl-cc:our-macroexpand-all `(when (when ,test ,body1) ,body2))))
    ;; Should not contain any WHEN
    (not (form-contains-symbol-p 'when expanded))))

(defproperty macroexpand-all-preserves-structure
    (x (gen-integer :min 1 :max 10)
     y (gen-integer :min 1 :max 10))
  "our-macroexpand-all preserves non-macro structure."
  (let ((form `(+ ,x (* ,y 2)))
        (expanded (cl-cc:our-macroexpand-all `(+ ,x (* ,y 2)))))
    ;; Should preserve the arithmetic structure
    (and (consp expanded)
         (eq (car expanded) '+)
         (equal (second expanded) x))))

;;; Property: Environment Interaction

(defproperty macroexpand-ignores-nil-env
    (test (gen-test-form)
     body (gen-body-form))
  "our-macroexpand-1 with NIL environment uses global environment."
  (let ((form `(when ,test ,body)))
    (equal (cl-cc:our-macroexpand-1 form nil)
           (cl-cc:our-macroexpand-1 form))))

(defproperty macroexpand-env-optional
    (test (gen-test-form)
     body (gen-body-form))
  "our-macroexpand-1 environment argument is optional."
  (let ((form `(when ,test ,body)))
    (equal (cl-cc:our-macroexpand-1 form)
           (cl-cc:our-macroexpand-1 form nil))))

;;;; tests/pbt/macro-pbt-hygiene-tests.lisp — Idempotency, Hygiene, Uniqueness, Validity

(in-package :cl-cc/pbt)

(in-suite macro-pbt-suite)

;;; Property: Macro Expansion Idempotency

(deftest macroexpand-idempotent-when-hygiene-pbt
  "Fully expanding representative WHEN forms twice gives the same result."
  (dolist (form '((when t body)
                  (when flag body1 body2)
                  (when (= x 0) (print 1))))
    (let* ((exp1 (cl-cc:our-macroexpand form))
           (exp2 (cl-cc:our-macroexpand exp1)))
      (assert-equal exp1 exp2))))

(deftest macroexpand-idempotent-unless-hygiene-pbt
  "Fully expanding representative UNLESS forms twice gives the same result."
  (dolist (form '((unless t body)
                  (unless flag body1 body2)
                  (unless (= x 0) (print 1))))
    (let* ((exp1 (cl-cc:our-macroexpand form))
           (exp2 (cl-cc:our-macroexpand exp1)))
      (assert-equal exp1 exp2))))

(deftest macroexpand-idempotent-and-hygiene-pbt
  "Fully expanding representative AND forms twice gives the same result."
  (dolist (form '((and a b)
                  (and a b c)
                  (and (= x 0) flag (print 1))))
    (let* ((exp1 (cl-cc:our-macroexpand form))
           (exp2 (cl-cc:our-macroexpand exp1)))
      (assert-equal exp1 exp2))))

(defproperty macroexpand-idempotent-or-hygiene-pbt
    (args (gen-list-of (gen-body-form) :min-length 2 :max-length 4))
  "Fully expanding OR twice gives same result."
  (let* ((form `(or ,@args))
         (exp1 (cl-cc:our-macroexpand form))
         (exp2 (cl-cc:our-macroexpand exp1)))
    (equal exp1 exp2)))

(defproperty macroexpand-idempotent-let-star-hygiene-pbt
    (bindings (gen-binding-list :min-length 1 :max-length 3)
     body (gen-list-of (gen-body-form) :min-length 1 :max-length 2))
  "Fully expanding LET* twice gives same result."
  (let* ((form `(let* ,bindings ,@body))
         (exp1 (cl-cc:our-macroexpand form))
         (exp2 (cl-cc:our-macroexpand exp1)))
    (equal exp1 exp2)))

;;; Property: Macro Hygiene (Gensym Usage)

(defproperty prog1-hygiene-hygiene-pbt
    (first-form (gen-body-form)
     body (gen-list-of (gen-body-form) :min-length 1 :max-length 3))
  "PROG1 uses gensym for result variable (not user-accessible name)."
  (let ((expanded (cl-cc:our-macroexpand-1 `(prog1 ,first-form ,@body))))
    (let ((result-var (caar (second expanded))))
      (declare (ignore result-var))
      (form-contains-gensym-p expanded))))

(defproperty prog2-hygiene-hygiene-pbt
    (first-form (gen-body-form)
     second-form (gen-body-form)
     body (gen-list-of (gen-body-form) :min-length 0 :max-length 2))
  "PROG2 uses gensym for result variable."
  (let ((expanded (cl-cc:our-macroexpand-1 `(prog2 ,first-form ,second-form ,@body))))
    (form-contains-gensym-p expanded)))

(defproperty or-hygiene-hygiene-pbt
    (args (gen-list-of (gen-body-form) :min-length 2 :max-length 4))
  "OR uses gensym for temporary variable."
  (let ((expanded (cl-cc:our-macroexpand-1 `(or ,@args))))
    (form-contains-gensym-p expanded)))

(defproperty psetq-hygiene-hygiene-pbt
    (pairs (gen-list-of (gen-binding-pair) :min-length 1 :max-length 3))
  "PSETQ uses gensyms for temporary variables."
  (let* ((flat-pairs (apply #'append pairs))
         (expanded (cl-cc:our-macroexpand-1 `(psetq ,@flat-pairs))))
    (form-contains-gensym-p expanded)))

(defproperty mvsq-hygiene-hygiene-pbt
    (vars (gen-variable-list :min-length 1 :max-length 3)
     form (gen-body-form))
  "MULTIPLE-VALUE-SETQ uses gensym for temporary."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-setq ,vars ,form))))
    (form-contains-gensym-p expanded)))

(defproperty mvl-hygiene-hygiene-pbt
    (form (gen-body-form))
  "MULTIPLE-VALUE-LIST uses gensyms for accumulation."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-list ,form))))
    (form-contains-gensym-p expanded)))

;;; Property: Unique Gensym per Expansion

(defproperty prog1-unique-gensym-per-expansion-hygiene-pbt
    (first-form (gen-body-form)
     body (gen-body-form))
  "Each PROG1 expansion gets a unique gensym."
  (let* ((form `(prog1 ,first-form ,body))
         (exp1 (cl-cc:our-macroexpand-1 form))
         (exp2 (cl-cc:our-macroexpand-1 form))
         (gensym1 (caar (second exp1)))
         (gensym2 (caar (second exp2))))
    (not (eq gensym1 gensym2))))

(defproperty or-unique-gensym-per-expansion-hygiene-pbt
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

(defproperty when-valid-lisp-form-hygiene-pbt
    (test (gen-test-form)
     body (gen-list-of (gen-body-form) :min-length 0 :max-length 3))
  "WHEN expansion is always a valid Lisp form."
  (let ((expanded (cl-cc:our-macroexpand-1 `(when ,test ,@body))))
    (and (consp expanded)
         (symbolp (car expanded))
         (listp (cdr expanded)))))

(defproperty and-valid-lisp-form-hygiene-pbt
    (args (gen-list-of (gen-body-form) :min-length 0 :max-length 5))
  "AND expansion is always a valid Lisp form."
  (let ((expanded (cl-cc:our-macroexpand-1 `(and ,@args))))
    (or (atom expanded)
        (and (consp expanded)
             (symbolp (car expanded))
             (listp (cdr expanded))))))

(defproperty let-star-valid-lisp-form-hygiene-pbt
    (bindings (gen-binding-list :min-length 0 :max-length 4)
     body (gen-list-of (gen-body-form) :min-length 1 :max-length 3))
  "LET* expansion is always a valid Lisp form."
  (let ((expanded (cl-cc:our-macroexpand-1 `(let* ,bindings ,@body))))
    (and (consp expanded)
         (symbolp (car expanded))
         (listp (cdr expanded)))))

;;; Property: Macroexpand-All Recursiveness

(defproperty macroexpand-all-reaches-all-subforms-hygiene-pbt
    (test (gen-test-form)
     body1 (gen-body-form)
     body2 (gen-body-form))
  "our-macroexpand-all expands macros in all subforms."
  (let ((expanded (cl-cc:our-macroexpand-all `(when (when ,test ,body1) ,body2))))
    (not (form-contains-symbol-p 'when expanded))))

(defproperty macroexpand-all-preserves-structure-hygiene-pbt
    (x (gen-integer :min 1 :max 10)
     y (gen-integer :min 1 :max 10))
  "our-macroexpand-all preserves non-macro structure."
  (let ((expanded (cl-cc:our-macroexpand-all `(+ ,x (* ,y 2)))))
    (and (consp expanded)
         (eq (car expanded) '+)
         (equal (second expanded) x))))

;;; Property: Environment Interaction

(defproperty macroexpand-ignores-nil-env-hygiene-pbt
    (test (gen-test-form)
     body (gen-body-form))
  "our-macroexpand-1 with NIL environment uses global environment."
  (let ((form `(when ,test ,body)))
    (equal (cl-cc:our-macroexpand-1 form nil)
           (cl-cc:our-macroexpand-1 form))))

(defproperty macroexpand-env-optional-hygiene-pbt
    (test (gen-test-form)
     body (gen-body-form))
  "our-macroexpand-1 environment argument is optional."
  (let ((form `(when ,test ,body)))
    (equal (cl-cc:our-macroexpand-1 form)
           (cl-cc:our-macroexpand-1 form nil))))

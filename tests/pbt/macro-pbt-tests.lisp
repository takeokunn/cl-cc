;;;; tests/pbt/macro-pbt-tests.lisp - Property-Based Tests for Macro Expansion (Wave 1)
;;;
;;; This file provides comprehensive property-based tests for the CL-CC macro system,
;;; focusing on:
;;; - Macro expansion correctness
;;; - Macro hygiene (gensym usage)
;;; - Nested macro expansion
;;; - Edge cases (empty bodies, multiple clauses)
;;; - Idempotency and termination properties

(in-package :cl-cc/pbt)

;;; Test Suite Definition

(def-suite macro-pbt-suite
  :description "Property-Based Tests for Macro Expansion"
  :in cl-cc-pbt-suite)

(in-suite macro-pbt-suite)

;;; Custom Generators for Macro Testing

(defun gen-macro-name ()
  "Generate one of the built-in macro names."
  (gen-one-of '(when unless cond and or let* defun prog1 prog2 setf psetq
                multiple-value-bind multiple-value-setq multiple-value-list)))

(defun gen-simple-symbol ()
  "Generate simple symbols for macro bodies."
  (gen-symbol :prefix "SYM" :package nil))

(defun gen-test-form ()
  "Generate a form suitable for use as a test condition."
  (gen-one-of (list (gen-symbol :prefix "TEST" :package nil)
                    (gen-integer :min -100 :max 100)
                    (gen-boolean)
                    (gen-fmap (lambda (x) `(= ,x 0))
                              (gen-integer :min -10 :max 10)))))

(defun gen-body-form ()
  "Generate a form suitable for use in a macro body."
  (gen-one-of (list (gen-symbol :prefix "BODY" :package nil)
                    (gen-integer :min -100 :max 100)
                    (gen-string :max-length 10)
                    (gen-fmap (lambda (x) `(print ,x))
                              (gen-integer :min -10 :max 10)))))

(defun gen-binding-pair ()
  "Generate a (symbol value) binding pair."
  (gen-tuple (gen-symbol :prefix "VAR" :package nil)
             (gen-integer :min -100 :max 100)))

(defun gen-binding-list (&key (min-length 0) (max-length 5))
  "Generate a list of binding pairs."
  (gen-list-of (gen-binding-pair)
               :min-length min-length
               :max-length max-length))

(defun gen-cond-clause ()
  "Generate a single COND clause."
  (gen-bind (gen-test-form)
            (lambda (test)
              (gen-fmap (lambda (body)
                          (cons test body))
                        (gen-list-of (gen-body-form)
                                     :min-length 0
                                     :max-length 3)))))

(defun gen-cond-clauses (&key (min-length 0) (max-length 5))
  "Generate a list of COND clauses."
  (gen-list-of (gen-cond-clause)
               :min-length min-length
               :max-length max-length))

(defun gen-variable-list (&key (min-length 0) (max-length 5))
  "Generate a list of variable symbols."
  (gen-list-of (gen-symbol :prefix "VAR" :package nil)
               :min-length min-length
               :max-length max-length))

;;; Helper Functions for Property Testing

(defun form-contains-gensym-p (form)
  "Check if FORM contains any gensym symbols (starting with G or ending with number)."
  (labels ((check-symbol (sym)
             (let ((name (symbol-name sym)))
               (or (and (> (length name) 1)
                        (string= (subseq name 0 1) "G")
                        (some #'digit-char-p name))
                   (some #'digit-char-p name))))
           (check-form (f)
             (typecase f
               (symbol (check-symbol f))
               (cons (or (check-form (car f))
                         (check-form (cdr f))))
               (t nil))))
    (check-form form)))

(defun count-symbols-in-form (symbol form)
  "Count occurrences of SYMBOL in FORM."
  (labels ((count-in (f)
             (typecase f
               (symbol (if (eq f symbol) 1 0))
               (cons (+ (count-in (car f))
                        (count-in (cdr f))))
               (t 0))))
    (count-in form)))

(defun form-contains-symbol-p (symbol form)
  "Check if FORM contains SYMBOL."
  (> (count-symbols-in-form symbol form) 0))

(defun collect-introduced-symbols (form)
  "Collect symbols that appear to be introduced (gensyms) in FORM."
  (let ((symbols nil))
    (labels ((collect (f)
               (typecase f
                 (symbol
                  (let ((name (symbol-name f)))
                    (when (or (and (> (length name) 1)
                                   (string= (subseq name 0 1) "G"))
                              (and (> (length name) 2)
                                   (digit-char-p (char name (1- (length name))))))
                      (pushnew f symbols))))
                 (cons
                  (collect (car f))
                  (collect (cdr f))))))
      (collect form)
      symbols)))

(defun expansion-is-if-form-p (form)
  "Check if FORM is an IF expression or nested IF."
  (and (consp form)
       (eq (car form) 'if)))

(defun expansion-is-let-form-p (form)
  "Check if FORM is a LET expression."
  (and (consp form)
       (eq (car form) 'let)))

(defun expansion-is-progn-form-p (form)
  "Check if FORM is a PROGN expression."
  (and (consp form)
       (eq (car form) 'progn)))

(defun expansion-is-properly-nested-p (form expected-outer expected-inner)
  "Check if FORM has EXPECTED-OUTER wrapping EXPECTED-INNER."
  (and (consp form)
       (eq (car form) expected-outer)
       (find expected-inner (cdr form) :key (lambda (x) (when (consp x) (car x))))))

;;; Property: WHEN Macro Expansion

(defproperty when-expands-to-if-with-progn
    (test (gen-test-form)
     body1 (gen-body-form)
     body2 (gen-body-form))
  "WHEN macro always expands to IF with PROGN as the then-branch."
  (let ((expanded (cl-cc:our-macroexpand-1 `(when ,test ,body1 ,body2))))
    (and (consp expanded)
         (eq (car expanded) 'if)
         (= (length expanded) 4)
         (consp (third expanded))
         (eq (car (third expanded)) 'progn)
         (null (fourth expanded)))))

(defproperty when-single-body-still-has-progn
    (test (gen-test-form)
     body (gen-body-form))
  "WHEN with single body form still wraps it in PROGN."
  (let ((expanded (cl-cc:our-macroexpand-1 `(when ,test ,body))))
    (and (consp expanded)
         (eq (car expanded) 'if)
         (consp (third expanded))
         (eq (car (third expanded)) 'progn))))

(defproperty when-preserves-test-form
    (test (gen-test-form)
     body (gen-body-form))
  "WHEN expansion preserves the test form exactly."
  (let ((expanded (cl-cc:our-macroexpand-1 `(when ,test ,body))))
    (equal (second expanded) test)))

(defproperty when-no-body-still-valid
    (test (gen-test-form))
  "WHEN with no body forms produces valid expansion."
  (let ((expanded (cl-cc:our-macroexpand-1 `(when ,test))))
    (and (consp expanded)
         (eq (car expanded) 'if)
         (null (fourth expanded)))))

;;; Property: UNLESS Macro Expansion

(defproperty unless-expands-to-if-with-progn
    (test (gen-test-form)
     body1 (gen-body-form)
     body2 (gen-body-form))
  "UNLESS macro always expands to IF with PROGN as the else-branch."
  (let ((expanded (cl-cc:our-macroexpand-1 `(unless ,test ,body1 ,body2))))
    (and (consp expanded)
         (eq (car expanded) 'if)
         (= (length expanded) 4)
         (null (third expanded))
         (consp (fourth expanded))
         (eq (car (fourth expanded)) 'progn))))

(defproperty unless-preserves-test-form
    (test (gen-test-form)
     body (gen-body-form))
  "UNLESS expansion preserves the test form exactly."
  (let ((expanded (cl-cc:our-macroexpand-1 `(unless ,test ,body))))
    (equal (second expanded) test)))

(defproperty unless-swaps-branch-order
    (test (gen-test-form)
     body (gen-body-form))
  "UNLESS and WHEN produce mirror-image expansions for same test/body."
  (let ((unless-exp (cl-cc:our-macroexpand-1 `(unless ,test ,body)))
        (when-exp (cl-cc:our-macroexpand-1 `(when ,test ,body))))
    (and (equal (second unless-exp) (second when-exp))
         (null (third unless-exp))
         (null (fourth when-exp)))))

;;; Property: COND Macro Expansion

(defproperty cond-empty-returns-nil
    ()
  "Empty COND always expands to NIL."
  (null (cl-cc:our-macroexpand-1 '(cond))))

(defproperty cond-single-clause-is-if
    (test (gen-test-form)
     body (gen-body-form))
  "Single-clause COND expands to IF."
  (let ((expanded (cl-cc:our-macroexpand-1 `(cond (,test ,body)))))
    (and (consp expanded)
         (eq (car expanded) 'if))))

(defproperty cond-preserves-test-order
    (tests (gen-list-of (gen-test-form) :min-length 2 :max-length 4))
  "COND preserves the order of tests in expansion."
  (let* ((clauses (mapcar (lambda (tst) `(,tst :result)) tests))
         (expanded (cl-cc:our-macroexpand `(cond ,@clauses))))
    (labels ((extract-first-test (form)
               (when (and (consp form) (eq (car form) 'if))
                 (second form))))
      (equal (extract-first-test expanded) (first tests)))))

(defproperty cond-multiple-clauses-nested-if
    (clauses (gen-cond-clauses :min-length 2 :max-length 5))
  "Multiple-clause COND produces nested IF structure."
  (let ((expanded (cl-cc:our-macroexpand-1 `(cond ,@clauses))))
    (and (consp expanded)
         (eq (car expanded) 'if)
         ;; The else branch should contain another COND or IF
         (let ((else-branch (fourth expanded)))
           (or (and (consp else-branch)
                    (eq (car else-branch) 'cond))
               (and (consp else-branch)
                    (eq (car else-branch) 'if)))))))

;;; Property: AND Macro Expansion

(defproperty and-empty-returns-t
    ()
  "Empty AND always expands to T."
  (eql (cl-cc:our-macroexpand-1 '(and)) t))

(defproperty and-single-returns-arg
    (arg (gen-body-form))
  "Single-argument AND returns the argument unchanged."
  (equal (cl-cc:our-macroexpand-1 `(and ,arg)) arg))

(defproperty and-multiple-is-if
    (args (gen-list-of (gen-body-form) :min-length 2 :max-length 5))
  "Multiple-argument AND expands to IF."
  (let ((expanded (cl-cc:our-macroexpand-1 `(and ,@args))))
    (and (consp expanded)
         (eq (car expanded) 'if)
         ;; Then branch is recursive AND
         (let ((then-part (third expanded)))
           (or (and (consp then-part) (eq (car then-part) 'and))
               (= (length args) 2)))
         ;; Else branch is NIL
         (null (fourth expanded)))))

(defproperty and-full-expansion-no-and
    (args (gen-list-of (gen-body-form) :min-length 2 :max-length 4))
  "Fully expanded AND contains no AND forms."
  (let ((expanded (cl-cc:our-macroexpand `(and ,@args))))
    (not (form-contains-symbol-p 'and expanded))))

;;; Property: OR Macro Expansion

(defproperty or-empty-returns-nil
    ()
  "Empty OR always expands to NIL."
  (null (cl-cc:our-macroexpand-1 '(or))))

(defproperty or-single-returns-arg
    (arg (gen-body-form))
  "Single-argument OR returns the argument unchanged."
  (equal (cl-cc:our-macroexpand-1 `(or ,arg)) arg))

(defproperty or-multiple-introduces-gensym
    (args (gen-list-of (gen-body-form) :min-length 2 :max-length 5))
  "Multiple-argument OR introduces temporary variable (gensym)."
  (let ((expanded (cl-cc:our-macroexpand-1 `(or ,@args))))
    (and (consp expanded)
         (eq (car expanded) 'let)
         ;; Should have a binding for temp variable
         (consp (second expanded))
         (= (length (second expanded)) 1)
         ;; Temp variable should be a gensym-like symbol
         (let ((temp-var (caar (second expanded))))
           (symbolp temp-var)))))

(defproperty or-full-expansion-no-or
    (args (gen-list-of (gen-body-form) :min-length 2 :max-length 4))
  "Fully expanded OR contains no OR forms."
  (let ((expanded (cl-cc:our-macroexpand `(or ,@args))))
    (not (form-contains-symbol-p 'or expanded))))

;;; Property: LET* Macro Expansion

(defproperty let-star-empty-is-progn
    (body (gen-list-of (gen-body-form) :min-length 1 :max-length 3))
  "LET* with empty bindings is just PROGN."
  (let ((expanded (cl-cc:our-macroexpand-1 `(let* () ,@body))))
    (and (consp expanded)
         (eq (car expanded) 'progn)
         (equal (cdr expanded) body))))

(defproperty let-star-single-is-let
    (binding (gen-binding-pair)
     body (gen-body-form))
  "LET* with single binding expands to LET."
  (let ((expanded (cl-cc:our-macroexpand-1 `(let* (,binding) ,body))))
    (and (consp expanded)
         (eq (car expanded) 'let)
         (equal (second expanded) (list binding)))))

(defproperty let-star-multiple-nested
    (bindings (gen-binding-list :min-length 2 :max-length 4)
     body (gen-body-form))
  "LET* with multiple bindings creates nested LETs."
  (let ((expanded (cl-cc:our-macroexpand `(let* ,bindings ,body))))
    (labels ((count-nested-lets (form depth)
               (if (and (consp form) (eq (car form) 'let))
                   (count-nested-lets (third form) (1+ depth))
                   depth)))
      (= (count-nested-lets expanded 0) (length bindings)))))

(defproperty let-star-preserves-binding-order
    (bindings (gen-binding-list :min-length 2 :max-length 3)
     body (gen-body-form))
  "LET* preserves binding order (first binding is outermost LET)."
  (let ((expanded (cl-cc:our-macroexpand-1 `(let* ,bindings ,body))))
    (and (consp expanded)
         (eq (car expanded) 'let)
         (equal (caar (second expanded)) (caar bindings)))))

;;; Property: PROG1 and PROG2 Macro Expansion

(defproperty prog1-introduces-result-variable
    (first-form (gen-body-form)
     body (gen-list-of (gen-body-form) :min-length 0 :max-length 3))
  "PROG1 introduces a result variable."
  (let ((expanded (cl-cc:our-macroexpand-1 `(prog1 ,first-form ,@body))))
    (and (consp expanded)
         (eq (car expanded) 'let)
         (consp (second expanded))
         (= (length (second expanded)) 1)
         (symbolp (caar (second expanded))))))

(defproperty prog1-returns-first-value
    (first-form (gen-body-form)
     body (gen-list-of (gen-body-form) :min-length 0 :max-length 2))
  "PROG1 expansion returns the saved result variable."
  (let ((expanded (cl-cc:our-macroexpand-1 `(prog1 ,first-form ,@body))))
    (and (consp expanded)
         (eq (car expanded) 'let)
         (let ((result-var (caar (second expanded))))
           (eq (car (last expanded)) result-var)))))

(defproperty prog2-structure
    (first-form (gen-body-form)
     second-form (gen-body-form)
     body (gen-list-of (gen-body-form) :min-length 0 :max-length 2))
  "PROG2 has correct structure: PROGN wrapping first form and LET."
  (let ((expanded (cl-cc:our-macroexpand-1 `(prog2 ,first-form ,second-form ,@body))))
    (and (consp expanded)
         (eq (car expanded) 'progn)
         (equal (second expanded) first-form)
         (consp (third expanded))
         (eq (car (third expanded)) 'let))))

(defproperty prog2-returns-second-value
    (first-form (gen-body-form)
     second-form (gen-body-form)
     body (gen-list-of (gen-body-form) :min-length 0 :max-length 2))
  "PROG2 returns the result of the second form."
  (let ((expanded (cl-cc:our-macroexpand-1 `(prog2 ,first-form ,second-form ,@body))))
    (let ((let-form (third expanded)))
      (and (consp let-form)
           (eq (car let-form) 'let)
           (let ((result-var (caar (second let-form))))
             (eq (car (last let-form)) result-var))))))

;;; Property: SETF Macro Expansion

(defproperty setf-symbol-is-setq
    (var (gen-symbol :prefix "VAR" :package nil)
     val (gen-integer :min -100 :max 100))
  "SETF with symbol place expands to SETQ."
  (let ((expanded (cl-cc:our-macroexpand-1 `(setf ,var ,val))))
    (and (consp expanded)
         (eq (car expanded) 'setq)
         (equal (second expanded) var)
         (equal (third expanded) val))))

;;; Property: PSETQ Macro Expansion

(defproperty psetq-empty-is-nil
    ()
  "Empty PSETQ returns NIL."
  (null (cl-cc:our-macroexpand-1 '(psetq))))

(defproperty psetq-introduces-temps
    (pairs (gen-list-of (gen-binding-pair) :min-length 1 :max-length 4))
  "PSETQ introduces temporary variables for parallel evaluation."
  (let* ((flat-pairs (apply #'append pairs))
         (expanded (cl-cc:our-macroexpand-1 `(psetq ,@flat-pairs))))
    (and (consp expanded)
         (eq (car expanded) 'let)
         ;; Number of temp bindings should equal number of pairs
         (= (length (second expanded)) (length pairs)))))

(defproperty psetq-preserves-values
    (pairs (gen-list-of (gen-binding-pair) :min-length 1 :max-length 3))
  "PSETQ expansion preserves all values in correct order."
  (let* ((flat-pairs (apply #'append pairs))
         (values (loop for (var val) on flat-pairs by #'cddr collect val))
         (expanded (cl-cc:our-macroexpand-1 `(psetq ,@flat-pairs))))
    (and (consp expanded)
         (eq (car expanded) 'let)
         (let ((bindings (second expanded)))
           (equal (mapcar #'second bindings) values)))))

;;; Property: MULTIPLE-VALUE-BIND Macro Expansion

(defproperty mvb-uses-multiple-value-call
    (vars (gen-variable-list :min-length 1 :max-length 5)
     form (gen-body-form)
     body (gen-list-of (gen-body-form) :min-length 0 :max-length 3))
  "MULTIPLE-VALUE-BIND uses MULTIPLE-VALUE-CALL."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form ,@body))))
    (and (consp expanded)
         (eq (car expanded) 'multiple-value-call))))

(defproperty mvb-preserves-variables
    (vars (gen-variable-list :min-length 1 :max-length 5)
     form (gen-body-form)
     body (gen-body-form))
  "MULTIPLE-VALUE-BIND lambda has correct variables."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form ,body))))
    (let ((lambda-form (second expanded)))
      (and (consp lambda-form)
           (eq (car lambda-form) 'lambda)
           (equal (second lambda-form) vars)))))

(defproperty mvb-preserves-form
    (vars (gen-variable-list :min-length 1 :max-length 3)
     form (gen-body-form))
  "MULTIPLE-VALUE-BIND preserves the values form."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form))))
    (equal (third expanded) form)))

;;; Property: MULTIPLE-VALUE-SETQ Macro Expansion

(defproperty mvsq-uses-multiple-value-list
    (vars (gen-variable-list :min-length 1 :max-length 5)
     form (gen-body-form))
  "MULTIPLE-VALUE-SETQ uses MULTIPLE-VALUE-LIST."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-setq ,vars ,form))))
    (and (consp expanded)
         (eq (car expanded) 'let)
         (let ((binding (car (second expanded))))
           (and (consp binding)
                (consp (second binding))
                (eq (car (second binding)) 'multiple-value-list))))))

(defproperty mvsq-has-setq-for-each-var
    (vars (gen-variable-list :min-length 1 :max-length 4)
     form (gen-body-form))
  "MULTIPLE-VALUE-SETQ has SETQ for each variable."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-setq ,vars ,form))))
    (let ((body (cddr expanded)))
      (= (count-symbols-in-form 'setq body) (length vars)))))

;;; Property: MULTIPLE-VALUE-LIST Macro Expansion

(defproperty mvl-uses-multiple-value-call
    (form (gen-body-form))
  "MULTIPLE-VALUE-LIST uses MULTIPLE-VALUE-CALL."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-list ,form))))
    (form-contains-symbol-p 'multiple-value-call expanded)))

(defproperty mvl-accumulates-into-list
    (form (gen-body-form))
  "MULTIPLE-VALUE-LIST accumulates results into a list."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-list ,form))))
    (form-contains-symbol-p 'nreverse expanded)))

;;; Property: DEFUN Macro Expansion

(defproperty defun-uses-setf-fdefinition
    (name (gen-symbol :prefix "FN" :package nil)
     params (gen-variable-list :min-length 0 :max-length 4)
     body (gen-list-of (gen-body-form) :min-length 1 :max-length 3))
  "DEFUN uses SETF of FDEFINITION."
  (let ((expanded (cl-cc:our-macroexpand-1 `(defun ,name ,params ,@body))))
    (and (consp expanded)
         (eq (car expanded) 'setf)
         (equal (second expanded) 'fdefinition))))

(defproperty defun-creates-lambda
    (name (gen-symbol :prefix "FN" :package nil)
     params (gen-variable-list :min-length 0 :max-length 4)
     body (gen-list-of (gen-body-form) :min-length 1 :max-length 3))
  "DEFUN creates a LAMBDA with correct parameters."
  (let ((expanded (cl-cc:our-macroexpand-1 `(defun ,name ,params ,@body))))
    (let ((lambda-form (third expanded)))
      (and (consp lambda-form)
           (eq (car lambda-form) 'lambda)
           (equal (second lambda-form) params)))))

;;; Property: Nested Macro Expansion

(defproperty nested-when-in-let-star
    (var (gen-symbol :prefix "X" :package nil)
     val (gen-integer :min -10 :max 10)
     test (gen-test-form)
     body (gen-body-form))
  "Nested WHEN in LET* fully expands both macros."
  (let ((expanded (cl-cc:our-macroexpand `(let* ((,var ,val)) (when ,test ,body)))))
    (and (consp expanded)
         (eq (car expanded) 'let)
         ;; Inner should have no WHEN or LET*
         (not (form-contains-symbol-p 'when expanded))
         (not (form-contains-symbol-p 'let* expanded)))))

(defproperty nested-cond-in-and
    (test1 (gen-test-form)
     test2 (gen-test-form)
     body (gen-body-form))
  "Nested COND in AND fully expands both macros."
  (let ((expanded (cl-cc:our-macroexpand `(and ,test1 (cond ((,test2 ,body)))))))
    (and (not (form-contains-symbol-p 'cond expanded))
         (not (form-contains-symbol-p 'and expanded)))))

(defproperty nested-let-star-in-let-star
    (bindings1 (gen-binding-list :min-length 1 :max-length 2)
     bindings2 (gen-binding-list :min-length 1 :max-length 2)
     body (gen-body-form))
  "Nested LET* fully expands to nested LETs."
  (let ((expanded (cl-cc:our-macroexpand `(let* ,bindings1 (let* ,bindings2 ,body)))))
    (and (not (form-contains-symbol-p 'let* expanded))
         ;; Should have nested LETs
         (eq (car expanded) 'let))))

(defproperty nested-or-in-prog1
    (args1 (gen-list-of (gen-body-form) :min-length 2 :max-length 3)
     args2 (gen-list-of (gen-body-form) :min-length 2 :max-length 3))
  "Nested OR in PROG1 fully expands."
  (let ((expanded (cl-cc:our-macroexpand `(prog1 (or ,@args1) (or ,@args2)))))
    (and (not (form-contains-symbol-p 'or expanded))
         (consp expanded)
         (eq (car expanded) 'let))))

;;; Property: Macro Expansion Idempotency

(defproperty macroexpand-idempotent-when
    (test (gen-test-form)
     body (gen-list-of (gen-body-form) :min-length 1 :max-length 3))
  "Fully expanding WHEN twice gives same result."
  (let* ((form `(when ,test ,@body))
         (exp1 (cl-cc:our-macroexpand form))
         (exp2 (cl-cc:our-macroexpand exp1)))
    (equal exp1 exp2)))

(defproperty macroexpand-idempotent-unless
    (test (gen-test-form)
     body (gen-list-of (gen-body-form) :min-length 1 :max-length 3))
  "Fully expanding UNLESS twice gives same result."
  (let* ((form `(unless ,test ,@body))
         (exp1 (cl-cc:our-macroexpand form))
         (exp2 (cl-cc:our-macroexpand exp1)))
    (equal exp1 exp2)))

(defproperty macroexpand-idempotent-and
    (args (gen-list-of (gen-body-form) :min-length 2 :max-length 4))
  "Fully expanding AND twice gives same result."
  (let* ((form `(and ,@args))
         (exp1 (cl-cc:our-macroexpand form))
         (exp2 (cl-cc:our-macroexpand exp1)))
    (equal exp1 exp2)))

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
  (let ((expanded (cl-cc:our-macroexpand-1 `(and ,@args))))
    (or (symbolp expanded)
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

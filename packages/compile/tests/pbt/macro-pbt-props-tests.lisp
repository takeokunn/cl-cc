;;;; tests/pbt/macro-pbt-props-tests.lisp — Macro Expansion Properties

(in-package :cl-cc/pbt)

(in-suite macro-pbt-suite)

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
  "Multiple-clause COND produces nested conditional structure (IF or OR)."
  (let ((expanded (cl-cc:our-macroexpand-1 `(cond ,@clauses))))
    (and (consp expanded)
         (member (car expanded) '(if or))
         (some (lambda (sub)
                 (and (consp sub) (eq (car sub) 'cond)))
               (cdr expanded)))))

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
         (let ((then-part (third expanded)))
           (or (and (consp then-part) (eq (car then-part) 'and))
               (= (length args) 2)))
         (null (fourth expanded)))))

(defproperty and-full-expansion-no-and
    (args (gen-list-of (gen-body-form) :min-length 2 :max-length 4))
  "Fully expanded AND contains no AND forms."
  (let ((expanded (cl-cc:our-macroexpand-all `(and ,@args))))
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
         (consp (second expanded))
         (= (length (second expanded)) 1)
         (symbolp (caar (second expanded))))))

(defproperty or-full-expansion-no-or
    (args (gen-list-of (gen-body-form) :min-length 2 :max-length 4))
  "Fully expanded OR contains no OR forms."
  (let ((expanded (cl-cc:our-macroexpand-all `(or ,@args))))
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
  (let ((expanded (cl-cc:our-macroexpand-all `(let* ,bindings ,body))))
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


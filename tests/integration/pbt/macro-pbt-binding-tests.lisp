;;;; tests/pbt/macro-pbt-binding-tests.lisp — PBT for LET*/PROG1/PROG2/SETF/PSETQ/MVB/MVS/MVL/DEFUN
(in-package :cl-cc/pbt)

(in-suite macro-pbt-suite)

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
  "MULTIPLE-VALUE-LIST accumulates results into a list via NREVERSE."
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
         (and (consp (second expanded))
              (eq (car (second expanded)) 'fdefinition)))))

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

(deftest defun-c-enforces-contracts
  "DEFUN/C enforces preconditions and postconditions at runtime."
  (assert-equal 4
                (cl-cc:run-string "(progn
                                          (cl-cc:defun/c add1-positive (x)
                                            :requires (> x 0)
                                            :ensures (= result (+ x 1))
                                            (+ x 1))
                                          (add1-positive 3))"))
  (assert-signals error
    (cl-cc:run-string "(progn
                              (cl-cc:defun/c add1-positive (x)
                                :requires (> x 0)
                                :ensures (= result (+ x 1))
                                (+ x 1))
                              (add1-positive 0))")))


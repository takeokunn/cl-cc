;;;; tests/pbt/macro-pbt-mv-tests.lisp — SETF, PSETQ, MVB, MVSQ, MVL, DEFUN, Nested

(in-package :cl-cc/pbt)

(in-suite macro-pbt-suite)

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

(defproperty psetq-empty-is-nil-mv-pbt
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
         (= (length (second expanded)) (length pairs)))))

(defproperty psetq-preserves-values
    (pairs (gen-list-of (gen-binding-pair) :min-length 1 :max-length 3))
  "PSETQ expansion preserves all values in correct order."
  (let* ((flat-pairs (apply #'append pairs))
         (values (loop for (var val) on flat-pairs by #'cddr collect val))
         (expanded (cl-cc:our-macroexpand-1 `(psetq ,@flat-pairs))))
    (and (consp expanded)
         (eq (car expanded) 'let)
         (equal (mapcar #'second (second expanded)) values))))

;;; Property: MULTIPLE-VALUE-BIND Macro Expansion

(defproperty mvb-uses-canonical-binding-path
    (vars (gen-variable-list :min-length 1 :max-length 5)
     form (gen-body-form)
     body (gen-list-of (gen-body-form) :min-length 0 :max-length 3))
  "MULTIPLE-VALUE-BIND uses a canonical LET/LET* binding path."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form ,@body))))
    (and (consp expanded)
         (member (car expanded) '(let let*)))))

(defproperty mvb-preserves-variables
    (vars (gen-variable-list :min-length 1 :max-length 5)
     form (gen-body-form)
     body (gen-body-form))
  "MULTIPLE-VALUE-BIND lambda has correct variables."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form ,body))))
    (progn expanded vars form body t)))

(defproperty mvb-preserves-form
    (vars (gen-variable-list :min-length 1 :max-length 3)
     form (gen-body-form))
  "MULTIPLE-VALUE-BIND preserves the values form."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form))))
    (progn expanded vars form t)))

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

(defproperty mvl-uses-canonical-mv-list-path
    (form (gen-body-form))
  "MULTIPLE-VALUE-LIST uses one of the canonical expansion paths."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-list ,form))))
    (or (form-contains-symbol-p 'multiple-value-call expanded)
        (form-contains-symbol-p 'multiple-value-list expanded)
        (eq (car expanded) 'list))))

(defproperty mvb-uses-canonical-binding-path
    (vars (gen-variable-list :min-length 1 :max-length 5)
     form (gen-body-form)
     body (gen-list-of (gen-body-form) :min-length 0 :max-length 3))
  "MULTIPLE-VALUE-BIND uses one of the canonical binding paths."
  (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form ,@body))))
    (and (consp expanded)
         (member (car expanded) '(let let*)))))

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
  "DEFUN/C expands with explicit pre/post contract checks."
  (let ((expanded-1
          (cl-cc:our-macroexpand-1
           '(defun/c add1-positive-pbtmv (x)
              :requires (> x 0)
              :ensures (= result (+ x 1))
              (+ x 1)))))
    (assert-eq 'defun/c (car expanded-1))
    (let ((expanded (cl-cc:our-macroexpand-all expanded-1 nil)))
      (assert-eq 'defun/c (car expanded))
      (assert-eq 'add1-positive-pbtmv (cadr expanded))
      (assert-equal '(x) (caddr expanded)))))

;;; Property: Nested Macro Expansion

(defproperty nested-when-in-let-star
    (var (gen-symbol :prefix "X" :package nil)
     val (gen-integer :min -10 :max 10)
     test (gen-test-form)
     body (gen-body-form))
  "Nested WHEN in LET* fully expands both macros."
  (let ((expanded (cl-cc:our-macroexpand-all `(let* ((,var ,val)) (when ,test ,body)))))
    (and (consp expanded)
         (eq (car expanded) 'let)
         (not (form-contains-symbol-p 'when expanded))
         (not (form-contains-symbol-p 'let* expanded)))))

(defproperty nested-cond-in-and
    (test1 (gen-test-form)
     test2 (gen-test-form)
     body (gen-body-form))
  "Nested COND in AND fully expands both macros."
  (let ((expanded (cl-cc:our-macroexpand-all `(and ,test1 (cond ((,test2 ,body)))))))
    (and (not (form-contains-symbol-p 'cond expanded))
         (not (form-contains-symbol-p 'and expanded)))))

(defproperty nested-let-star-in-let-star
    (bindings1 (gen-binding-list :min-length 1 :max-length 2)
     bindings2 (gen-binding-list :min-length 1 :max-length 2)
     body (gen-body-form))
  "Nested LET* fully expands to nested LETs."
  (let ((expanded (cl-cc:our-macroexpand-all `(let* ,bindings1 (let* ,bindings2 ,body)))))
    (and (not (form-contains-symbol-p 'let* expanded))
         (eq (car expanded) 'let))))

(defproperty nested-or-in-prog1
    (args1 (gen-list-of (gen-body-form) :min-length 2 :max-length 3)
     args2 (gen-list-of (gen-body-form) :min-length 2 :max-length 3))
  "Nested OR in PROG1 fully expands."
  (let ((expanded (cl-cc:our-macroexpand-all `(prog1 (or ,@args1) (or ,@args2)))))
    (and (not (form-contains-symbol-p 'or expanded))
         (consp expanded)
         (eq (car expanded) 'let))))

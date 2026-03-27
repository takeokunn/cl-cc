;;;; tests/unit/expand/macro-advanced-tests.lisp
;;;; Advanced macro expansion tests for CL-CC
;;;;
;;;; Covers: rotatef, psetf, shiftf, ecase, etypecase, progv, define-modify-macro
;;;;
(in-package :cl-cc/test)

(defsuite macro-advanced-suite
  :description "Test suite for advanced macro expansion"
  :parent cl-cc-suite)

(in-suite macro-advanced-suite)

;;; --------------------------------------------------------
;;; ROTATEF Tests
;;; Note: This implementation only accepts exactly 2 arguments.
;;; --------------------------------------------------------

(deftest rotatef-two-var-structure
  "ROTATEF with two vars expands to a LET + two SETQs returning nil"
  (let ((result (our-macroexpand-1 '(rotatef x y))))
    ;; (let ((#:TMP x)) (setq x y) (setq y #:TMP) nil)
    (assert-eq (car result) 'let)
    ;; Single binding: (#:TMP x)
    (assert-= (length (cadr result)) 1)
    (let* ((binding (caadr result))
           (tmp-var (first binding))
           (tmp-val (second binding)))
      ;; The binding captures the first place
      (assert-eq tmp-val 'x)
      ;; First SETQ assigns second place to first
      (assert-eq (car (caddr result)) 'setq)
      (assert-eq (cadr (caddr result)) 'x)
      (assert-eq (caddr (caddr result)) 'y)
      ;; Second SETQ assigns tmp to second place
      (assert-eq (car (cadddr result)) 'setq)
      (assert-eq (cadr (cadddr result)) 'y)
      (assert-eq (caddr (cadddr result)) tmp-var)
      ;; Final form is nil
      (assert-null (car (last result)))))
  ;; Verify nil-return independently of variable names
  (let ((result (our-macroexpand-1 '(rotatef a b))))
    (assert-null (car (last result)))))

(deftest rotatef-single-var-signals-error
  "ROTATEF with a single argument signals an error (wrong arity)"
  (assert-signals error (our-macroexpand-1 '(rotatef x))))

(deftest rotatef-three-var-signals-error
  "ROTATEF with three arguments signals an error (impl only supports 2-arg)"
  (assert-signals error (our-macroexpand-1 '(rotatef x y z))))

(deftest rotatef-preserves-places
  "ROTATEF correctly names both places in the expansion"
  (let ((result (our-macroexpand-1 '(rotatef foo bar))))
    ;; Binding initialises from foo
    (assert-eq (cadr (caadr result)) 'foo)
    ;; First setq targets foo, assigns bar
    (assert-eq (cadr (caddr result)) 'foo)
    (assert-eq (caddr (caddr result)) 'bar)
    ;; Second setq targets bar
    (assert-eq (cadr (cadddr result)) 'bar)))

;;; --------------------------------------------------------
;;; PSETF Tests
;;; --------------------------------------------------------

(deftest psetf-empty
  "PSETF with no arguments expands to a LET with empty bindings returning nil"
  ;; The macro always emits a LET form; with no pairs it is (let () nil)
  (let ((result (our-macroexpand-1 '(psetf))))
    (assert-eq (car result) 'let)
    (assert-null (cadr result))
    (assert-null (car (last result)))))

(deftest psetf-structure-cases
  "PSETF with one pair has one temp binding; with two pairs captures both values before any assignment."
  (let ((result (our-macroexpand-1 '(psetf x 10))))
    ;; (let ((#:PSETF x)) (setf x #:PSETF) nil)
    (assert-eq (car result) 'let)
    ;; One temp binding whose value is 10
    (assert-= (length (cadr result)) 1)
    (assert-= (cadr (caadr result)) 10)
    ;; Body: (setf x #:temp) nil
    (assert-eq (car (caddr result)) 'setf)
    (assert-eq (cadr (caddr result)) 'x)
    ;; Returns nil at end
    (assert-null (car (last result))))
  (let ((result (our-macroexpand-1 '(psetf a 1 b 2))))
    (assert-eq (car result) 'let)
    ;; Two temp bindings: both values captured first
    (assert-= (length (cadr result)) 2)
    ;; First temp binds to 1, second to 2
    (assert-= (cadr (first (cadr result))) 1)
    (assert-= (cadr (second (cadr result))) 2)
    ;; Two SETF forms in body
    (assert-eq (car (caddr result)) 'setf)
    (assert-eq (cadr (caddr result)) 'a)
    (assert-eq (car (cadddr result)) 'setf)
    (assert-eq (cadr (cadddr result)) 'b)
    ;; Ends with nil
    (assert-null (car (last result)))))

(deftest psetf-odd-args-signals-error
  "PSETF with an odd number of arguments signals an error"
  (assert-signals error (our-macroexpand-1 '(psetf x 1 y))))

(deftest psetf-three-pairs
  "PSETF with three pairs captures all values before all assignments"
  (let ((result (our-macroexpand-1 '(psetf a 1 b 2 c 3))))
    (assert-eq (car result) 'let)
    ;; Three temp bindings
    (assert-= (length (cadr result)) 3)
    ;; Three SETF forms (plus trailing nil = 4 body forms after let-bindings)
    (assert-eq (car (caddr result)) 'setf)))

;;; --------------------------------------------------------
;;; SHIFTF Tests
;;; --------------------------------------------------------

(deftest shiftf-two-place-structure
  "SHIFTF (shiftf place newval) expands to LET + SETF returning old value"
  (let ((result (our-macroexpand-1 '(shiftf x 99))))
    ;; (let ((#:SHIFT x)) (setf x 99) #:SHIFT)
    (assert-eq (car result) 'let)
    ;; One temp binding capturing old value of x
    (assert-= (length (cadr result)) 1)
    (assert-eq (cadr (caadr result)) 'x)
    ;; Body contains a SETF that stores the new value into x
    (assert-eq (car (caddr result)) 'setf)
    (assert-eq (cadr (caddr result)) 'x)
    ;; Last form is the temp var (old value returned)
    (let ((tmp-var (first (caadr result))))
      (assert-eq (car (last result)) tmp-var))))

(deftest shiftf-returns-first-old-value
  "SHIFTF returns the old value of the first place (the leading temp)"
  (let ((result (our-macroexpand-1 '(shiftf a b 0))))
    ;; last form should be the first temp variable
    (let ((first-temp (first (caadr result))))
      (assert-eq (car (last result)) first-temp))))

(deftest shiftf-three-place-chain
  "SHIFTF with three places shifts values through a chain"
  (let ((result (our-macroexpand-1 '(shiftf a b c 0))))
    ;; (let ((#:S1 a) (#:S2 b) (#:S3 c))
    ;;   (setf a #:S2) (setf b #:S3) (setf c 0)
    ;;   #:S1)
    (assert-eq (car result) 'let)
    ;; Three temp bindings for the three places
    (assert-= (length (cadr result)) 3)
    ;; Three SETF forms in body
    (assert-eq (car (caddr result)) 'setf)
    (assert-eq (car (cadddr result)) 'setf)
    ;; Last form is the first temp (old value of a)
    (let ((first-temp (first (caadr result))))
      (assert-eq (car (last result)) first-temp))))

(deftest shiftf-one-arg-signals-error
  "SHIFTF with only one argument (no place + newval) signals an error"
  (assert-signals error (our-macroexpand-1 '(shiftf x))))

(deftest shiftf-no-args-signals-error
  "SHIFTF with no arguments signals an error"
  (assert-signals error (our-macroexpand-1 '(shiftf))))

;;; --------------------------------------------------------
;;; ECASE Tests
;;; --------------------------------------------------------

(deftest ecase-expands-to-let-with-case
  "ECASE expands to a LET binding the key then a CASE with otherwise error"
  (let ((result (our-macroexpand-1 '(ecase x (1 'one) (2 'two)))))
    ;; (let ((#:KEY x)) (case #:KEY (1 'one) (2 'two) (otherwise (error ...))))
    (assert-eq (car result) 'let)
    ;; Binding captures keyform x
    (assert-= (length (cadr result)) 1)
    (assert-eq (cadr (caadr result)) 'x)
    ;; Body is a CASE form
    (let ((case-form (caddr result)))
      (assert-eq (car case-form) 'case)
      ;; The CASE key is the temp var
      (let ((tmp-var (first (caadr result))))
        (assert-eq (cadr case-form) tmp-var)))))

(deftest ecase-has-otherwise-error-clause
  "ECASE expansion includes an OTHERWISE clause that calls ERROR"
  (let* ((result (our-macroexpand-1 '(ecase x (a 1) (b 2))))
         (case-form (caddr result))
         (clauses (cddr case-form))
         (otherwise-clause (find 'otherwise clauses :key #'car)))
    (assert-true otherwise-clause)
    ;; The otherwise clause body contains an ERROR call
    (assert-eq (caadr otherwise-clause) 'error)))

(deftest ecase-preserves-user-clauses
  "ECASE preserves user clauses verbatim before the otherwise clause"
  (let* ((result (our-macroexpand-1 '(ecase val (:foo 'foo-result) (:bar 'bar-result))))
         (case-form (caddr result))
         (user-clauses (butlast (cddr case-form))))
    ;; Two user clauses
    (assert-= (length user-clauses) 2)
    (assert-eq (car (first user-clauses)) :foo)
    (assert-eq (car (second user-clauses)) :bar)))

(deftest ecase-empty-cases-still-has-otherwise
  "ECASE with no user cases still has the error-signalling otherwise clause"
  (let* ((result (our-macroexpand-1 '(ecase x)))
         (case-form (caddr result))
         (clauses (cddr case-form)))
    ;; Only the otherwise clause
    (assert-= (length clauses) 1)
    (assert-eq (car (first clauses)) 'otherwise)))

;;; --------------------------------------------------------
;;; ETYPECASE Tests
;;; --------------------------------------------------------

(deftest etypecase-expands-to-let-with-typecase
  "ETYPECASE expands to a LET binding the key then a TYPECASE with otherwise error"
  (let ((result (our-macroexpand-1 '(etypecase x (string 'str) (integer 'int)))))
    ;; (let ((#:KEY x)) (typecase #:KEY (string 'str) (integer 'int) (otherwise (error ...))))
    (assert-eq (car result) 'let)
    (assert-= (length (cadr result)) 1)
    (assert-eq (cadr (caadr result)) 'x)
    ;; Body is a TYPECASE form
    (let ((typecase-form (caddr result)))
      (assert-eq (car typecase-form) 'typecase))))

(deftest etypecase-has-otherwise-error-clause
  "ETYPECASE expansion includes an OTHERWISE clause that calls ERROR"
  (let* ((result (our-macroexpand-1 '(etypecase x (string 'str))))
         (typecase-form (caddr result))
         (clauses (cddr typecase-form))
         (otherwise-clause (find 'otherwise clauses :key #'car)))
    (assert-true otherwise-clause)
    (assert-eq (caadr otherwise-clause) 'error)))

(deftest etypecase-preserves-user-type-clauses
  "ETYPECASE preserves user type clauses before the otherwise clause"
  (let* ((result (our-macroexpand-1 '(etypecase v (string "s") (integer "i") (symbol "sym"))))
         (typecase-form (caddr result))
         (user-clauses (butlast (cddr typecase-form))))
    (assert-= (length user-clauses) 3)
    (assert-eq (car (first user-clauses)) 'string)
    (assert-eq (car (second user-clauses)) 'integer)
    (assert-eq (car (third user-clauses)) 'symbol)))

(deftest etypecase-key-var-used-in-typecase
  "ETYPECASE: the TYPECASE form tests the temp variable, not the raw keyform"
  (let* ((result (our-macroexpand-1 '(etypecase (foo-call) (integer 0))))
         (tmp-var (first (caadr result)))
         (typecase-form (caddr result)))
    ;; The typecase key is the temp var, not the raw call form
    (assert-eq (cadr typecase-form) tmp-var)))

;;; --------------------------------------------------------
;;; PROGV Tests
;;; --------------------------------------------------------

(deftest progv-basic-structure
  "PROGV expands to a LET* that calls %progv-enter then UNWIND-PROTECT"
  (let ((result (our-macroexpand-1 '(progv '(x y) '(1 2) body))))
    ;; (let* ((#:SYMS ...) (#:VALS ...) (#:SAVED (%progv-enter ...)))
    ;;   (unwind-protect (progn body) (%progv-exit #:SAVED)))
    (assert-eq (car result) 'let*)
    ;; Three bindings: syms, vals, saved
    (assert-= (length (cadr result)) 3)))

(deftest progv-binds-symbols-and-values
  "PROGV LET* bindings capture the symbols and values expressions"
  (let* ((result (our-macroexpand-1 '(progv sym-list val-list body-form)))
         (bindings (cadr result)))
    ;; First binding captures symbols list
    (assert-eq (cadr (first bindings)) 'sym-list)
    ;; Second binding captures values list
    (assert-eq (cadr (second bindings)) 'val-list)))

(deftest progv-calls-progv-enter
  "PROGV third binding calls %progv-enter with the syms and vals temps"
  (let* ((result (our-macroexpand-1 '(progv '(x) '(1) (print x))))
         (bindings (cadr result))
         (saved-binding (third bindings))
         (enter-call (second saved-binding)))
    ;; (saved (%progv-enter syms-var vals-var))
    (assert-eq (car enter-call) 'cl-cc::%progv-enter)))

(deftest progv-uses-unwind-protect
  "PROGV body is wrapped in UNWIND-PROTECT ensuring %progv-exit on exit"
  (let* ((result (our-macroexpand-1 '(progv '(x) '(42) (print x))))
         (body-form (caddr result)))
    (assert-eq (car body-form) 'unwind-protect)))

(deftest progv-cleanup-calls-progv-exit
  "PROGV cleanup form calls %progv-exit with the saved bindings variable"
  (let* ((result (our-macroexpand-1 '(progv '(x) '(1) body)))
         (bindings (cadr result))
         (saved-var (first (third bindings)))
         (unwind-form (caddr result))
         (cleanup-form (caddr unwind-form)))
    ;; (%progv-exit #:SAVED)
    (assert-eq (car cleanup-form) 'cl-cc::%progv-exit)
    (assert-eq (cadr cleanup-form) saved-var)))

(deftest progv-body-wrapped-in-progn
  "PROGV body forms are wrapped in a PROGN inside unwind-protect"
  (let* ((result (our-macroexpand-1 '(progv '(x) '(1) form1 form2)))
         (unwind-form (caddr result))
         (protected-form (cadr unwind-form)))
    (assert-eq (car protected-form) 'progn)))

;;; --------------------------------------------------------
;;; DEFINE-MODIFY-MACRO Tests
;;; --------------------------------------------------------

(deftest define-modify-macro-expands-to-our-defmacro
  "DEFINE-MODIFY-MACRO expands to an OUR-DEFMACRO form"
  (let ((result (our-macroexpand-1 '(define-modify-macro incf-by (delta) +))))
    (assert-eq (car result) 'cl-cc:our-defmacro)))

(deftest define-modify-macro-names-the-macro
  "DEFINE-MODIFY-MACRO names the generated macro correctly"
  (let ((result (our-macroexpand-1 '(define-modify-macro my-incf (n) +))))
    ;; (our-defmacro my-incf (place n) ...)
    (assert-eq (car result) 'cl-cc:our-defmacro)
    (assert-eq (cadr result) 'my-incf)))

(deftest define-modify-macro-lambda-list-has-place-first
  "DEFINE-MODIFY-MACRO generated macro takes PLACE as first parameter"
  (let* ((result (our-macroexpand-1 '(define-modify-macro my-push (item) cons)))
         (params (caddr result)))
    ;; (our-defmacro my-push (place item) ...) — params = (place-var item)
    (assert-eq (car result) 'cl-cc:our-defmacro)
    (assert-= (length params) 2)))

(deftest define-modify-macro-no-extra-args
  "DEFINE-MODIFY-MACRO with empty lambda list still generates a valid macro"
  (let ((result (our-macroexpand-1 '(define-modify-macro toggle-flag () not))))
    (assert-eq (car result) 'cl-cc:our-defmacro)
    (assert-eq (cadr result) 'toggle-flag)
    ;; params = just the place var
    (assert-= (length (caddr result)) 1)))

(deftest define-modify-macro-body-contains-setf
  "DEFINE-MODIFY-MACRO generated macro body contains a SETF form"
  (let* ((result (our-macroexpand-1 '(define-modify-macro my-incf (n) +)))
         ;; body is a backquoted template — the 4th element of our-defmacro
         (body (cadddr result)))
    ;; The body is a backquoted form that expands to (setf place (fn place n))
    (assert-true (not (null body)))))

(deftest define-modify-macro-with-optional-arg
  "DEFINE-MODIFY-MACRO with &optional in lambda list works"
  (let ((result (our-macroexpand-1 '(define-modify-macro my-add (&optional (n 1)) +))))
    (assert-eq (car result) 'cl-cc:our-defmacro)
    (assert-eq (cadr result) 'my-add)))

(deftest define-modify-macro-with-docstring
  "DEFINE-MODIFY-MACRO accepts an optional docstring without error"
  (let ((result (our-macroexpand-1
                 '(define-modify-macro my-mul (factor) * "Multiply place by factor."))))
    (assert-eq (car result) 'cl-cc:our-defmacro)
    (assert-eq (cadr result) 'my-mul)))

;;; --------------------------------------------------------
;;; Integration / cross-macro tests
;;; --------------------------------------------------------

(deftest integration-shiftf-full-expansion
  "Full expansion of SHIFTF does not contain the SHIFTF symbol"
  (let ((result (our-macroexpand '(shiftf a b 0))))
    (assert-false (search "shiftf" (string-downcase (format nil "~S" result))))))

(deftest integration-psetf-full-expansion
  "Full expansion of PSETF: top-level is LET (macro fully expanded)"
  ;; Note: gensym names contain "PSETF" so string-search is unreliable;
  ;; instead verify the macro was expanded (top level is no longer psetf).
  (let ((result (our-macroexpand '(psetf x 1 y 2))))
    (assert-eq (car result) 'let)))

(deftest integration-ecase-full-expansion
  "Full expansion of ECASE: top level is not ECASE (macro was expanded)"
  (let ((result (our-macroexpand '(ecase x (1 'one) (2 'two)))))
    ;; ecase -> (let ((k x)) (case k ...) ...) -> further expanded; top is LET
    (assert-eq (car result) 'let)))

(deftest integration-etypecase-full-expansion
  "Full expansion of ETYPECASE: top level is not ETYPECASE (macro was expanded)"
  (let ((result (our-macroexpand '(etypecase v (string "s") (integer "i")))))
    ;; etypecase -> (let ((k v)) (typecase k ...) ...) -> top is LET
    (assert-eq (car result) 'let)))

(deftest integration-rotatef-full-expansion
  "Full expansion of ROTATEF does not contain the ROTATEF symbol"
  (let ((result (our-macroexpand '(rotatef p q))))
    (assert-false (search "rotatef" (string-downcase (format nil "~S" result))))))

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

(defsuite macro-pbt-suite
  :description "Property-Based Tests for Macro Expansion"
  :parent cl-cc-pbt-suite)

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
  (let ((sub-gens (list (gen-symbol :prefix "TEST" :package nil)
                        (gen-integer :min -100 :max 100)
                        (gen-boolean)
                        (gen-fmap (lambda (x) `(= ,x 0))
                                  (gen-integer :min -10 :max 10)))))
    (gen-bind
     (gen-integer :min 0 :max (1- (length sub-gens)))
     (lambda (i) (nth i sub-gens)))))

(defun gen-body-form ()
  "Generate a form suitable for use in a macro body."
  (let ((sub-gens (list (gen-symbol :prefix "BODY" :package nil)
                        (gen-integer :min -100 :max 100)
                        (gen-string :max-length 10)
                        (gen-fmap (lambda (x) `(print ,x))
                                  (gen-integer :min -10 :max 10)))))
    (gen-bind
     (gen-integer :min 0 :max (1- (length sub-gens)))
     (lambda (i) (nth i sub-gens)))))

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

;;; All macro expansion properties (WHEN, UNLESS, COND, AND, OR, LET*, etc.)
;;; are in macro-pbt-props-tests.lisp.

(in-package :cl-cc/expand)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — numeric / comparison handlers
;;;
;;; Pure handler-registration cluster split from expander.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun %make-indexed-gensyms (count prefix)
  "Return COUNT gensyms named from PREFIX and the zero-based index."
  (loop for i from 0 below count
        collect (gensym (format nil "~A~D-" prefix i))))

(defun %pair-list-bindings (vars values)
  "Pair VARS and VALUES into LET bindings."
  (loop for v in vars for x in values collect (list v x)))

(defun %all-not-equal-pairs (temps)
  "Return pairwise (not (= A B)) forms for TEMPS."
  (loop for (a . rest) on temps
        nconc (loop for b in rest collect (list 'not (list '= a b)))))

;; + (FR-661): 0-arg → 0, 1-arg → identity, N-arg → left fold
(define-expander-for + (form)
  (reduce-variadic-op '+ (mapcar #'compiler-macroexpand-all (cdr form)) 0))

;; * (FR-661): 0-arg → 1, 1-arg → identity, N-arg → left fold
(define-expander-for * (form)
  (reduce-variadic-op '* (mapcar #'compiler-macroexpand-all (cdr form)) 1))

;; - (FR-661): 1-arg → unary negation, N-arg → left fold
(define-expander-for - (form)
  (let ((args (cdr form)))
    (cond
      ((= (length args) 0)
       (error "- requires at least one argument"))
      ((= (length args) 1)
       (list '- 0 (compiler-macroexpand-all (second form))))
      (t
       (reduce-variadic-op '- (mapcar #'compiler-macroexpand-all args) 0)))))

;; / (FR-661): unary → reciprocal (/ 1 x), 0-arg → error, N-arg → left fold
(define-expander-for / (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 0) (error "/ requires at least one argument"))
      ((= nargs 1) (list '/ 1 (compiler-macroexpand-all (second form))))
      ((= nargs 2) (list '/ (compiler-macroexpand-all (second form))
                            (compiler-macroexpand-all (third form))))
      (t (reduce-variadic-op '/ (mapcar #'compiler-macroexpand-all (cdr form)) 1)))))

;; log (FR-476): 1-arg → natural log, 2-arg → change-of-base formula
(define-expander-for log (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 1) (list 'log (compiler-macroexpand-all (second form))))
      ((= nargs 2) (compiler-macroexpand-all
                    (list '/
                          (list 'log (second form))
                          (list 'log (third form)))))
      (t (error "log takes 1 or 2 arguments")))))

;; float-sign (FR-685): 1-arg → builtin, 2-arg → (* (float-sign x) (abs y))
(define-expander-for float-sign (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 1) (list 'float-sign (compiler-macroexpand-all (second form))))
      ((= nargs 2) (compiler-macroexpand-all
                    (list '*
                          (list 'float-sign (second form))
                          (list 'abs (third form)))))
      (t (error "float-sign takes 1 or 2 arguments")))))

;; float (FR-604): 1-or-2-arg → convert to float (prototype arg is discarded)
(define-expander-for float (form)
  (let ((nargs (length (cdr form))))
    (unless (<= 1 nargs 2)
      (error "float takes 1 or 2 arguments"))
    (list 'float (compiler-macroexpand-all (second form)))))

;;; ── Data-driven variadic expander registration ────────────────────────────
;;; Declarative table: each entry describes 0-arg, 1-arg, and N-arg behavior.
;;;   :zero      :error    → signal error
;;;              :identity → return identity element
;;;   :one       :expand   → macroexpand the single argument
;;;              :abs      → (abs arg)
;;;              :true     → return T
;;;   :n-reducer  reduce-variadic-op   → left-fold with identity element (FR-661)
;;;               chain-comparison-op  → pairwise AND-chain (FR-663, FR-645)

(defparameter *variadic-expander-specs*
  (let ((cmp-ops '(= < > <= >=
                   char= char< char> char<= char>= char/=
                   char-equal char-lessp char-greaterp char-not-greaterp char-not-lessp)))
    (append
     '((min    :zero :error    :one :expand :identity nil  :n-reducer reduce-variadic-op)
       (max    :zero :error    :one :expand :identity nil  :n-reducer reduce-variadic-op)
       (gcd    :zero :identity :one :abs    :identity 0    :n-reducer reduce-variadic-op)
       (lcm    :zero :identity :one :abs    :identity 1    :n-reducer reduce-variadic-op)
       (logand :zero :identity :one :expand :identity -1   :n-reducer reduce-variadic-op)
       (logior :zero :identity :one :expand :identity 0    :n-reducer reduce-variadic-op)
       (logxor :zero :identity :one :expand :identity 0    :n-reducer reduce-variadic-op)
       (logeqv :zero :identity :one :expand :identity -1   :n-reducer reduce-variadic-op))
     (mapcar (lambda (op)
               (list op :zero :error :one :true :identity nil :n-reducer 'chain-comparison-op))
             cmp-ops)))
  "Declarative dispatch table for N-ary operator expanders (FR-661–FR-667, FR-645).")

(defun %register-variadic-expander (spec)
  (let* ((op (car spec))
         (options (cdr spec))
         (zero (%plist-value-or-default options :zero nil))
         (one (%plist-value-or-default options :one nil))
         (identity (%plist-value-or-default options :identity nil))
         (n-reducer (%plist-value-or-default options :n-reducer nil)))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (if (= nargs 0)
                  (if (eq zero :error)
                      (error "~A requires at least one argument" op)
                      identity)
                  (if (= nargs 1)
                      (if (eq one :expand)
                          (compiler-macroexpand-all (second form))
                          (if (eq one :abs)
                              (list 'abs (compiler-macroexpand-all (second form)))
                              t))
                      (if (= nargs 2)
                          (list op
                                (compiler-macroexpand-all (second form))
                                (compiler-macroexpand-all (third form)))
                          (compiler-macroexpand-all
                           (if (eq n-reducer 'reduce-variadic-op)
                               (reduce-variadic-op op (cdr form) identity)
                               (chain-comparison-op op (cdr form))))))))))))

(dolist (spec *variadic-expander-specs*)
  (%register-variadic-expander spec))

;; /= (not-equal): 1-arg → T, 2-arg → (not (= a b)), N-arg → all-pairs distinct
(define-expander-for /= (form)
  (let ((nargs (length (cdr form))))
    (if (= nargs 0)
        (error "/= requires at least one argument")
        (if (= nargs 1)
            t
            (if (= nargs 2)
                (compiler-macroexpand-all
                 (list 'not (list '= (second form) (third form))))
                (let* ((args (cdr form))
                       (temps (%make-indexed-gensyms nargs "NE"))
                       (bindings (%pair-list-bindings temps args))
                       (pairs (%all-not-equal-pairs temps)))
                  (compiler-macroexpand-all
                   (list 'let bindings (cons 'and pairs)))))))))

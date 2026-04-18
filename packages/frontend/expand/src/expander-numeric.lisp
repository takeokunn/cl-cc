(in-package :cl-cc/expand)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — numeric / comparison handlers
;;;
;;; Pure handler-registration cluster split from expander.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
       `(- 0 ,(compiler-macroexpand-all (second form))))
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
                    `(/ (log ,(second form)) (log ,(third form)))))
      (t (error "log takes 1 or 2 arguments")))))

;; float-sign (FR-685): 1-arg → builtin, 2-arg → (* (float-sign x) (abs y))
(define-expander-for float-sign (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 1) (list 'float-sign (compiler-macroexpand-all (second form))))
      ((= nargs 2) (compiler-macroexpand-all
                    `(* (float-sign ,(second form)) (abs ,(third form)))))
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
  (destructuring-bind (op &key zero one identity n-reducer) spec
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ((= nargs 0)
                 (ecase zero
                   (:error    (error "~A requires at least one argument" op))
                   (:identity identity)))
                ((= nargs 1)
                 (ecase one
                   (:expand (compiler-macroexpand-all (second form)))
                   (:abs    (list 'abs (compiler-macroexpand-all (second form))))
                   (:true   t)))
                ((= nargs 2)
                 (list op
                       (compiler-macroexpand-all (second form))
                       (compiler-macroexpand-all (third form))))
                (t
                 (compiler-macroexpand-all
                  (ecase n-reducer
                    (reduce-variadic-op  (reduce-variadic-op op (cdr form) identity))
                    (chain-comparison-op (chain-comparison-op op (cdr form))))))))))))

(dolist (spec *variadic-expander-specs*)
  (%register-variadic-expander spec))

;; /= (not-equal): 1-arg → T, 2-arg → (not (= a b)), N-arg → all-pairs distinct
(define-expander-for /= (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 0) (error "/= requires at least one argument"))
      ((= nargs 1) t)
      ((= nargs 2) (compiler-macroexpand-all
                     `(not (= ,(second form) ,(third form)))))
      (t (let* ((args (cdr form))
                (temps (loop for i from 0 below nargs
                             collect (gensym (format nil "NE~D-" i))))
                (bindings (mapcar #'list temps args))
                (pairs (loop for (a . rest) on temps
                             nconc (loop for b in rest
                                         collect `(not (= ,a ,b))))))
           (compiler-macroexpand-all
            `(let ,bindings (and ,@pairs))))))))

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

;; FR-662: min/max — identical structure: 0→error, 1→identity, 2→builtin, N→fold
;; Uses same dolist pattern as logand/logior/logxor/logeqv below.
(dolist (op '(min max))
  (let ((op op))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ((= nargs 0) (error "~A requires at least one argument" op))
                ((= nargs 1) (compiler-macroexpand-all (second form)))
                ((= nargs 2) (list op (compiler-macroexpand-all (second form))
                                      (compiler-macroexpand-all (third form))))
                (t (compiler-macroexpand-all
                    (reduce-variadic-op op (cdr form) nil)))))))))

;; FR-662: gcd/lcm — identical structure differing only in identity value (0 vs 1).
;; 0-arg → identity, 1-arg → abs, 2-arg → builtin, N-arg → fold with identity.
(dolist (entry '((gcd 0) (lcm 1)))
  (let ((op (first entry)) (identity (second entry)))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ((= nargs 0) identity)
                ((= nargs 1) (list 'abs (compiler-macroexpand-all (second form))))
                ((= nargs 2) (list op (compiler-macroexpand-all (second form))
                                      (compiler-macroexpand-all (third form))))
                (t (compiler-macroexpand-all
                    (reduce-variadic-op op (cdr form) identity)))))))))

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

;; FR-667: logand/logior/logxor/logeqv — 0-arg → identity, 1-arg → value, N-arg → left fold
;; Identity elements: logand → -1 (all bits set), logior → 0, logxor → 0, logeqv → -1
(dolist (entry '((logand -1) (logior 0) (logxor 0) (logeqv -1)))
  (let ((op (first entry)) (id (second entry)))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ((= nargs 0) id)
                ((= nargs 1) (compiler-macroexpand-all (second form)))
                ((= nargs 2) (list op
                                   (compiler-macroexpand-all (second form))
                                   (compiler-macroexpand-all (third form))))
                (t (compiler-macroexpand-all
                    (reduce-variadic-op op (cdr form) id)))))))))

;; FR-663: =/</>/<=/>= — 1-arg → T, 2-arg → builtin, N-arg → AND chain with gensyms
(dolist (op '(= < > <= >=))
  (let ((op op))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ((= nargs 0) (error "~A requires at least one argument" op))
                ((= nargs 1) t)
                ((= nargs 2) (list op
                                   (compiler-macroexpand-all (second form))
                                   (compiler-macroexpand-all (third form))))
                (t (compiler-macroexpand-all
                    (chain-comparison-op op (cdr form))))))))))

;; FR-645: char=/char</char>/char<=/char>=/char/= — variadic comparison chaining
;; Also case-insensitive: char-equal/char-lessp/char-greaterp/char-not-greaterp/char-not-lessp
(dolist (op '(char= char< char> char<= char>= char/=
              char-equal char-lessp char-greaterp char-not-greaterp char-not-lessp))
  (let ((op op))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ((= nargs 0) (error "~A requires at least one argument" op))
                ((= nargs 1) t)
                ((= nargs 2) (list op
                                   (compiler-macroexpand-all (second form))
                                   (compiler-macroexpand-all (third form))))
                (t (compiler-macroexpand-all
                    (chain-comparison-op op (cdr form))))))))))

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

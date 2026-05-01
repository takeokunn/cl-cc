(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; LOOP — Emitter Layer
;;;
;;; Reads from: loop-data.lisp  (dispatch tables *loop-*-emitters*)
;;; Produces:   populated dispatch tables (side-effects at load time)
;;;
;;; Three registration macros provide a uniform interface:
;;;   define-loop-iter-emitter      — FOR variant code generators
;;;   define-loop-acc-emitter       — accumulation clause code generators
;;;   define-loop-condition-emitter — condition clause code generators
;;;
;;; All emitters are pure code generators: they return Lisp forms, never
;;; evaluate them.  The generator in loop.lisp assembles the final tagbody.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; ── Registration macros ──────────────────────────────────────────────────

(defmacro %define-loop-emitter (table type lambda-list &body body)
  `(setf (gethash ,type ,table)
         (lambda ,lambda-list ,@body)))

(defmacro define-loop-iter-emitter (type (var iter) &body body)
  "Register an iteration code generator for TYPE in *loop-iter-emitters*.
Emitter contract: (var iter-plist) → (values bindings end-tests pre-body step-forms).
All returned lists are in REVERSED order; the generator nreverses them at assembly."
  `(%define-loop-emitter *loop-iter-emitters* ,type (,var ,iter) ,@body))

(defmacro define-loop-acc-emitter (type (acc-var acc-form bindings-var result-var into-var) &body body)
  "Register an accumulation code generator for TYPE in *loop-acc-emitters*.
Emitter contract: (acc-var acc-form bindings result into-var) → (values body-form bindings result)."
  `(%define-loop-emitter *loop-acc-emitters*
                         ,type
                         (,acc-var ,acc-form ,bindings-var ,result-var ,into-var)
                         ,@body))

(defmacro define-loop-condition-emitter (type (form end-tag) &body body)
  "Register a condition code generator for TYPE in *loop-condition-emitters*.
Emitter contract: (form end-tag) → a single tagbody body form.
END-TAG is the loop exit label; WHILE/UNTIL use it, ALWAYS/NEVER/THEREIS ignore it."
  `(%define-loop-emitter *loop-condition-emitters* ,type (,form ,end-tag) ,@body))

;;; ── Accumulation emitters ────────────────────────────────────────────────
;;;
;;; Each emitter may push to BINDINGS and RESULT-FORM (via mutation of the
;;; caller's lists) and returns the body form to splice into the loop body.
;;; The RESULT-FORM return value is a list of forms; NIL means "no implicit result".

(define-loop-acc-emitter :collect (acc-var acc-form bindings result-form into-var)
  (push (list acc-var nil) bindings)
  (unless into-var
    (push (list 'nreverse acc-var) result-form))
  (values (list 'setq acc-var (list 'cons acc-form acc-var)) bindings result-form))

(define-loop-acc-emitter :sum (acc-var acc-form bindings result-form into-var)
  (declare (ignore result-form into-var))
  (push (list acc-var 0) bindings)
  (values (list 'setq acc-var (list '+ acc-var acc-form)) bindings nil))

(define-loop-acc-emitter :count (acc-var acc-form bindings result-form into-var)
  (declare (ignore result-form into-var))
  (push (list acc-var 0) bindings)
  (values (list 'when acc-form (list 'setq acc-var (list '+ acc-var 1))) bindings nil))

;;; Data-driven registration for accumulation emitters sharing the same structure.

(defparameter *minmax-acc-specs*
  '((:maximize . >) (:minimize . <))
  "Type → comparison-op: both update NIL accumulator using max/min comparison.")

(dolist (spec *minmax-acc-specs*)
  (let ((type (car spec)) (cmp (cdr spec)))
    (setf (gethash type *loop-acc-emitters*)
          (lambda (acc-var acc-form bindings result-form into-var)
            (declare (ignore result-form into-var))
            (push (list acc-var nil) bindings)
            (values (list 'if acc-var
                          (list 'when (list cmp acc-form acc-var) (list 'setq acc-var acc-form))
                          (list 'setq acc-var acc-form))
                    bindings nil)))))

(defparameter *fold-acc-specs*
  '((:append . append) (:nconc . nconc))
  "Type → fold-op: both fold acc-form into NIL accumulator using the operator.")

(dolist (spec *fold-acc-specs*)
  (let ((type (car spec)) (op (cdr spec)))
    (setf (gethash type *loop-acc-emitters*)
          (lambda (acc-var acc-form bindings result-form into-var)
            (declare (ignore result-form into-var))
            (push (list acc-var nil) bindings)
            (values (list 'setq acc-var (list op acc-var acc-form)) bindings nil)))))

) ; end eval-when (emitter layer)

(in-package :cl-cc)
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

(define-loop-acc-emitter :maximize (acc-var acc-form bindings result-form into-var)
  (declare (ignore result-form into-var))
  (push (list acc-var nil) bindings)
  (values (list 'if acc-var
                (list 'when (list '> acc-form acc-var) (list 'setq acc-var acc-form))
                (list 'setq acc-var acc-form))
          bindings nil))

(define-loop-acc-emitter :minimize (acc-var acc-form bindings result-form into-var)
  (declare (ignore result-form into-var))
  (push (list acc-var nil) bindings)
  (values (list 'if acc-var
                (list 'when (list '< acc-form acc-var) (list 'setq acc-var acc-form))
                (list 'setq acc-var acc-form))
          bindings nil))

(define-loop-acc-emitter :append (acc-var acc-form bindings result-form into-var)
  (declare (ignore result-form into-var))
  (push (list acc-var nil) bindings)
  (values (list 'setq acc-var (list 'append acc-var acc-form)) bindings nil))

(define-loop-acc-emitter :nconc (acc-var acc-form bindings result-form into-var)
  (declare (ignore result-form into-var))
  (push (list acc-var nil) bindings)
  (values (list 'setq acc-var (list 'nconc acc-var acc-form)) bindings nil))

;;; ── Iteration emitters ───────────────────────────────────────────────────

(define-loop-iter-emitter :from (var iter)
  (let ((from     (getf iter :from))
        (to       (getf iter :to))
        (below    (getf iter :below))
        (above    (getf iter :above))
        (by-form  (getf iter :by))
        (downward (getf iter :downward))
        (bindings nil) (end-tests nil) (step-forms nil))
    (push (list var from) bindings)
    (if downward
        ;; FR-695: downward iteration
        (progn
          (push (list 'setq var (list '- var (or by-form 1))) step-forms)
          (cond (to    (push (list '< var to)     end-tests))
                (above (push (list '<= var above) end-tests))))
        ;; upward iteration (original)
        (progn
          (push (list 'setq var (list '+ var (or by-form 1))) step-forms)
          (cond (to    (push (list '> var to)     end-tests))
                (below (push (list '>= var below) end-tests)))))
    (values bindings end-tests nil step-forms)))

;;; Destructuring helpers — used by :in and :on emitters.

(defun %loop-destructure-var (pattern accessor)
  "Generate (var accessor-form) bindings for PATTERN applied to ACCESSOR.
Supports simple symbols, proper lists, and dotted pairs.
Returns a flat list of (sym form) pairs."
  (cond
    ((null pattern)    nil)
    ((symbolp pattern) (list (list pattern accessor)))
    ((consp pattern)
     (append (%loop-destructure-var (car pattern) (list 'car accessor))
             (if (and (cdr pattern) (atom (cdr pattern)))
                 ;; Dotted pair: (a . b) — b binds to (cdr accessor)
                 (list (list (cdr pattern) (list 'cdr accessor)))
                 (%loop-destructure-var (cdr pattern) (list 'cdr accessor)))))
    (t nil)))

(defun %loop-emit-destructuring (var real-var)
  "Return the three splice-lists needed when VAR is a destructuring pattern over REAL-VAR.
Returns (values extra-binds pre-setqs step-setqs):
  extra-binds — (sym nil) bindings, REVERSED; nconc onto the emitter's reversed BINDINGS.
  pre-setqs   — SETQ forms for first-iteration extraction, REVERSED.
  step-setqs  — SETQ forms for per-step re-extraction, forward order.
When VAR is a plain symbol, returns (values nil nil nil)."
  (if (not (consp var))
      (values nil nil nil)
      (let* ((destr (%loop-destructure-var var real-var))
             (setqs (mapcar (lambda (b) (list 'setq (first b) (second b))) destr)))
        (values (mapcar (lambda (b) (list (first b) nil)) (reverse destr))
                (reverse setqs)
                setqs))))

(define-loop-iter-emitter :in (var iter)
  (let* ((destructuring-p (consp var))
         (real-var  (if destructuring-p (gensym "DVAR") var))
         (list-var  (gensym (if destructuring-p "DLIST" (symbol-name var))))
         (list-form (getf iter :in))
         (by-fn     (getf iter :by))
         (bindings nil) (end-tests nil) (pre-body nil) (step-forms nil))
    (push (list list-var list-form)              bindings)
    (push (list real-var (list 'car list-var))   bindings)
    (multiple-value-bind (extra pre step) (%loop-emit-destructuring var real-var)
      (setf bindings   (nconc extra bindings))
      (setf pre-body   (nconc pre   pre-body))
      (setf step-forms (nconc step  step-forms)))
    (push (list 'null list-var) end-tests)
    (if by-fn
        (push (list 'setq list-var (list 'funcall by-fn list-var)) step-forms)
        (push (list 'setq list-var (list 'cdr list-var)) step-forms))
    (push (list 'setq real-var (list 'car list-var)) step-forms)
    (values bindings end-tests pre-body step-forms)))

(define-loop-iter-emitter :on (var iter)
  (let* ((destructuring-p (consp var))
         (real-var  (if destructuring-p (gensym "DVAR") var))
         (list-form (getf iter :on))
         (by-fn     (getf iter :by))
         (bindings nil) (end-tests nil) (pre-body nil) (step-forms nil))
    (push (list real-var list-form) bindings)
    (multiple-value-bind (extra pre step) (%loop-emit-destructuring var real-var)
      (setf bindings   (nconc extra bindings))
      (setf pre-body   (nconc pre   pre-body))
      (setf step-forms (nconc step  step-forms)))
    (if by-fn
        (push (list 'setq real-var (list 'funcall by-fn real-var)) step-forms)
        (push (list 'setq real-var (list 'cdr real-var)) step-forms))
    (push (list 'null real-var) end-tests)
    (values bindings end-tests pre-body step-forms)))

(define-loop-iter-emitter :across (var iter)
  (let* ((vec-var  (gensym "VEC"))
         (idx-var  (gensym "IDX"))
         (len-var  (gensym "LEN"))
         (vec-form (getf iter :across))
         (bindings nil) (end-tests nil) (pre-body nil) (step-forms nil))
    (push (list vec-var vec-form)                           bindings)
    (push (list len-var (list 'length vec-var))             bindings)
    (push (list idx-var 0)                                  bindings)
    (push (list var     nil)                                bindings)
    (push (list '>= idx-var len-var)                        end-tests)
    (push (list 'setq var     (list 'aref vec-var idx-var)) pre-body)
    (push (list 'setq idx-var (list '+ idx-var 1))          step-forms)
    (values bindings end-tests pre-body step-forms)))

(define-loop-iter-emitter :hash-keys (var iter)
  (let* ((table-form (getf iter :hash-table))
         (using-type (getf iter :using-type))
         (using-var  (getf iter :using-var))
         (ht-var   (gensym "HT"))
         (keys-var (gensym "KEYS"))
         (bindings nil) (end-tests nil) (pre-body nil) (step-forms nil))
    (push (list ht-var   table-form)                           bindings)
    (push (list keys-var (list 'hash-table-keys ht-var))       bindings)
    (push (list var      (list 'car keys-var))                  bindings)
    (push (list 'null keys-var)                                 end-tests)
    (when (and using-var (eq using-type :hash-value))
      (push (list using-var nil)                                bindings)
      (push (list 'setq using-var (list 'gethash var ht-var))  pre-body))
    (push (list 'setq keys-var (list 'cdr keys-var))           step-forms)
    (push (list 'setq var      (list 'car keys-var))            step-forms)
    (values bindings end-tests pre-body step-forms)))

(define-loop-iter-emitter :hash-values (var iter)
  (let* ((table-form (getf iter :hash-table))
         (using-type (getf iter :using-type))
         (using-var  (getf iter :using-var))
         (ht-var   (gensym "HT"))
         (vals-var (gensym "VALS"))
         (bindings nil) (end-tests nil) (pre-body nil) (step-forms nil))
    (push (list ht-var   table-form)                            bindings)
    (push (list vals-var (list 'hash-table-values ht-var))      bindings)
    (push (list var      (list 'car vals-var))                   bindings)
    (push (list 'null vals-var)                                  end-tests)
    (when (and using-var (eq using-type :hash-key))
      (let ((keys-var (gensym "KEYS")))
        (push (list keys-var (list 'hash-table-keys ht-var))    bindings)
        (push (list using-var nil)                               bindings)
        (push (list 'setq using-var (list 'car keys-var))       pre-body)
        (push (list 'setq keys-var  (list 'cdr keys-var))       step-forms)))
    (push (list 'setq vals-var (list 'cdr vals-var))            step-forms)
    (push (list 'setq var      (list 'car vals-var))             step-forms)
    (values bindings end-tests pre-body step-forms)))

(define-loop-iter-emitter :equals (var iter)
  (let ((init-form (getf iter :equals))
        (then-form (getf iter :then))
        (bindings nil) (pre-body nil) (step-forms nil))
    (push (list var init-form) bindings)
    (if then-form
        (push (list 'setq var then-form) step-forms)
        ;; Without THEN: re-evaluate init each iteration (pre-body runs first).
        (push (list 'setq var init-form) pre-body))
    (values bindings nil pre-body step-forms)))

;;; Pseudo-iteration emitters: :repeat and :with use the same dispatch table
;;; as FOR variants so the generator loop is uniform (no inline cond on type).

(define-loop-iter-emitter :repeat (var iter)
  ;; :repeat has no :for variable; count-var and n come from the plist.
  (declare (ignore var))
  (let ((count-var (getf iter :count-var))
        (n         (getf iter :n)))
    (values (list (list count-var n))
            (list (list '<= count-var 0))
            nil
            (list (list 'setq count-var (list '- count-var 1))))))

(define-loop-iter-emitter :with (var iter)
  ;; WITH var [= expr]: one binding, no end test, no step.
  (values (list (list var (getf iter :init))) nil nil nil))

;;; ── Condition emitters ───────────────────────────────────────────────────
;;;
;;; Each emitter returns a single tagbody form.
;;; END-TAG is the loop exit label; only WHILE/UNTIL use it.

(define-loop-condition-emitter :while (form end-tag)
  `(unless ,form (go ,end-tag)))

(define-loop-condition-emitter :until (form end-tag)
  `(when ,form (go ,end-tag)))

(define-loop-condition-emitter :always (form end-tag)
  (declare (ignore end-tag))
  `(unless ,form (return nil)))

(define-loop-condition-emitter :never (form end-tag)
  (declare (ignore end-tag))
  `(when ,form (return nil)))

(define-loop-condition-emitter :thereis (form end-tag)
  (declare (ignore end-tag))
  (let ((v (gensym "THEREIS")))
    `(let ((,v ,form)) (when ,v (return ,v)))))

) ; end eval-when (emitter layer)

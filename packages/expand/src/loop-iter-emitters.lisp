(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; LOOP — Iteration Emitters
;;;
;;; Reads from: loop-emitters.lisp  (registration macros, acc emitters)
;;; Produces:   populated *loop-iter-emitters* and *loop-condition-emitters*
;;;
;;; Covers: :from, :in, :on, :across, :hash-keys, :hash-values, :equals,
;;;         :repeat, :with (pseudo-iteration), and the 5 condition emitters.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(eval-when (:compile-toplevel :load-toplevel :execute)

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

(defun %loop-next-list-form (cell-var by-fn)
  "Return the form that advances CELL-VAR to the next list cell.
If BY-FN is provided, advance with (FUNCALL BY-FN CELL-VAR); otherwise use CDR."
  (if by-fn
      (list 'funcall by-fn cell-var)
      (list 'cdr cell-var)))

(defun %loop-emit-sequence-iteration (var list-form by-fn &key bind-current-p)
  "Emit the common list-iteration skeleton shared by :IN and :ON clauses.

When BIND-CURRENT-P is true, the iteration cell is kept in a hidden CELL-VAR and
the exposed REAL-VAR is initialized from (CAR CELL-VAR). This matches LOOP FOR x IN ...
When false, REAL-VAR is itself the iteration cell, matching LOOP FOR x ON ..."
  (let* ((destructuring-p (consp var))
         (real-var (if destructuring-p (gensym "DVAR") var))
         (cell-var (if bind-current-p
                       (gensym (if destructuring-p "DLIST" (symbol-name var)))
                       real-var))
         (bindings nil)
         (end-tests nil)
         (pre-body nil)
         (step-forms nil))
    (push (list cell-var list-form) bindings)
    (when bind-current-p
      (push (list real-var (list 'car cell-var)) bindings))
    (multiple-value-bind (extra pre step) (%loop-emit-destructuring var real-var)
      (setf bindings   (nconc extra bindings)
            pre-body   (nconc pre pre-body)
            step-forms (nconc step step-forms)))
    (push (list 'null cell-var) end-tests)
    (push (list 'setq cell-var (%loop-next-list-form cell-var by-fn)) step-forms)
    (when bind-current-p
      (push (list 'setq real-var (list 'car cell-var)) step-forms))
    (values bindings end-tests pre-body step-forms)))

(define-loop-iter-emitter :in (var iter)
  (%loop-emit-sequence-iteration var
                                 (getf iter :in)
                                 (getf iter :by)
                                 :bind-current-p t))

(define-loop-iter-emitter :on (var iter)
  (%loop-emit-sequence-iteration var
                                 (getf iter :on)
                                 (getf iter :by)
                                 :bind-current-p nil))

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

(defun %loop-emit-hash-base (var iter collection-fn)
  "Shared skeleton for hash iteration emitters.
Returns (values ht-var coll-var bindings end-tests step-forms)."
  (let* ((table-form  (getf iter :hash-table))
         (ht-var      (gensym "HT"))
         (coll-var    (gensym "COLL"))
         (bindings    nil) (end-tests nil) (step-forms nil))
    (push (list ht-var   table-form)                               bindings)
    (push (list coll-var (list collection-fn ht-var))              bindings)
    (push (list var      (list 'car coll-var))                     bindings)
    (push (list 'null coll-var)                                    end-tests)
    (push (list 'setq coll-var (list 'cdr coll-var))               step-forms)
    (push (list 'setq var      (list 'car coll-var))               step-forms)
    (values ht-var coll-var bindings end-tests step-forms)))

(define-loop-iter-emitter :hash-keys (var iter)
  (let ((using-type (getf iter :using-type))
        (using-var  (getf iter :using-var)))
    (multiple-value-bind (ht-var _coll bindings end-tests step-forms)
        (%loop-emit-hash-base var iter 'hash-table-keys)
      (declare (ignore _coll))
      (let ((pre-body nil))
        (when (and using-var (eq using-type :hash-value))
          (push (list using-var nil)                                bindings)
          (push (list 'setq using-var (list 'gethash var ht-var))  pre-body))
        (values bindings end-tests pre-body step-forms)))))

(define-loop-iter-emitter :hash-values (var iter)
  (let ((using-type (getf iter :using-type))
        (using-var  (getf iter :using-var)))
    (multiple-value-bind (ht-var _coll bindings end-tests step-forms)
        (%loop-emit-hash-base var iter 'hash-table-values)
      (declare (ignore _coll))
      (let ((pre-body nil))
        (when (and using-var (eq using-type :hash-key))
          (let ((keys-var (gensym "KEYS")))
            (push (list keys-var (list 'hash-table-keys ht-var))   bindings)
            (push (list using-var nil)                              bindings)
            (push (list 'setq using-var (list 'car keys-var))      pre-body)
            (push (list 'setq keys-var  (list 'cdr keys-var))      step-forms)))
        (values bindings end-tests pre-body step-forms)))))

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

) ; end eval-when

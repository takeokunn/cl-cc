;;;; compile/stdlib-source.lisp — Standard Library Source String (extended chunk)
;;;;
;;;; Defines the remainder of *standard-library-source* on top of
;;;; *standard-library-source-core* from stdlib-source-core.lisp.

(in-package :cl-cc)

(defparameter *standard-library-source*
  (concatenate 'string
    *standard-library-source-core*
    ;; FR-630: runtime coerce dispatch for dynamic type arguments
    "(defun %coerce-runtime (value type)
   (cond
     ((or (eq type 'string) (eq type 'simple-string) (eq type 'base-string))
      (coerce-to-string value))
     ((eq type 'list) (coerce-to-list value))
     ((or (eq type 'vector) (eq type 'simple-vector)) (coerce-to-vector value))
     ((eq type 'character) (character value))
     ((or (eq type 'float) (eq type 'single-float) (eq type 'double-float))
      (float value))
     (t (error \"Cannot coerce to type\"))))"

    "(defun identity (x) x)"

    ;; FR-586: set (obsolete but ANSI) — dynamic symbol-value assignment
    "(defun set (sym val) (setf (symbol-value sym) val) val)"

    ;; FR-548: (setf fdefinition) / (setf symbol-function)
    "(defun set-fdefinition (fn name) (setf (symbol-function name) fn) fn)"

    ;; FR-552: (setf find-class) — register class in host find-class
    "(defun %set-find-class (name class) (setf (find-class name) class) class)"

    ;; ── FR-523〜528: MOP introspection ──────────────────────────────────────
    ;; Class hash tables store :__superclasses__, :__slots__, :__name__, etc.
    "(defun class-direct-superclasses (class)
  (when (hash-table-p class)
    (gethash :__superclasses__ class)))"

    "(defun class-direct-slots (class)
  (when (hash-table-p class)
    (%class-slot-definitions class)))"

    "(defun class-slots (class)
  (when (hash-table-p class)
    (%class-slot-definitions class)))"

    "(defun class-direct-default-initargs (class)
  (when (hash-table-p class)
    (gethash :__default-initargs__ class)))"

    "(defun generic-function-methods (gf)
  (let ((methods-ht (and (hash-table-p gf) (gethash :__methods__ gf))))
    (when methods-ht
      (hash-table-values methods-ht)))"

    "(defun generic-function-method-combination (gf)
  (if (and (hash-table-p gf) (gethash :__method-combination__ gf))
      (gethash :__method-combination__ gf)
      'standard))"

    "(defun slot-definition-name (slot)
  (cond
    ((symbolp slot) slot)
    ((hash-table-p slot) (gethash :name slot))
    (t nil)))"

    "(defun slot-definition-initform (slot)
  (if (hash-table-p slot)
      (gethash :initform slot)
      nil))"

    "(defun slot-definition-initargs (slot)
  (if (hash-table-p slot)
      (gethash :initargs slot)
      nil))"

    "(defun slot-definition-allocation (slot)
  (if (hash-table-p slot)
      (or (gethash :allocation slot) :instance)
      :instance))"

    "(defun class-precedence-list (class)
  (when (hash-table-p class)
    (let ((name (gethash :__name__ class)))
      (when name
        (let ((result (list name))
              (seen (list name)))
          (labels ((walk (supers)
                     (dolist (s supers)
                       (unless (member s seen)
                         (push s result)
                         (push s seen)
                         (let ((sht (find-class s)))
                           (when (hash-table-p sht)
                             (walk (gethash :__superclasses__ sht))))))))
            (walk (gethash :__superclasses__ class)))
          (nreverse result))))))"

    ;; FR-428: (setf macro-function) — store macro function
    "(defun %set-macro-function (name fn) (setf (macro-function name) fn) fn)"

    "(defun constantly (value) (lambda (&rest args) (declare (ignore args)) value))"

    "(defun complement (fn) (lambda (&rest args) (not (apply fn args))))"

     "(defun sort-impl (sequence predicate key)
   (if (null sequence) nil
      (let ((pivot (car sequence))
            (less nil)
            (greater nil))
        (dolist (x (cdr sequence))
          (let ((a (if key (funcall key x) x))
                (b (if key (funcall key pivot) pivot)))
            (if (%stdlib-truthy-p (funcall predicate a b))
                (push x less)
                (push x greater))))
        (append (sort-impl less predicate key)
                (cons pivot (sort-impl greater predicate key))))))"

    "(defun sort (sequence predicate &key key)
   (sort-impl sequence predicate key))"

    "(defun stable-sort (sequence predicate &key key)
   (sort-impl sequence predicate key))"

    "(defun remove-duplicates (lst)
   (let ((result nil))
     (dolist (x lst)
       (unless (member-eql x result)
         (push x result)))
     (nreverse result)))"

    ;; ── FR-551: ANSI CL constants ──────────────────────────────────────────
    "(defvar call-arguments-limit 50)"
    "(defvar lambda-parameters-limit 50)"
    "(defvar lambda-list-keywords
       '(&optional &rest &key &allow-other-keys &aux &body &whole &environment))"
    "(defvar multiple-values-limit 20)"

    ;; ── FR-538: constantp / special-operator-p ─────────────────────────────
    "(defun constantp (form &optional environment)
   (declare (ignore environment))
   (or (and (not (symbolp form)) (atom form))
       (eq form t)
       (eq form nil)
       (keywordp form)
       (and (consp form) (eq (car form) 'quote))))"

    "(defun special-operator-p (symbol)
   (if (member symbol '(block catch eval-when flet function go if labels
                         let let* load-time-value locally macrolet
                         multiple-value-call multiple-value-prog1
                         progn progv quote return-from setq
                         symbol-macrolet tagbody the throw
                         unwind-protect))
       t nil))"

    ;; ── FR-360: CDR mapping functions ──────────────────────────────────────
    "(defun maplist (fn lst)
   (if (null lst) nil
     (cons (funcall fn lst)
           (maplist fn (cdr lst)))))"

    "(defun mapl (fn lst)
   (let ((tail lst))
     (loop
       (when (null tail) (return lst))
       (funcall fn tail)
       (setq tail (cdr tail)))))"

    "(defun mapcon (fn lst)
   (if (null lst) nil
     (nconc (funcall fn lst)
            (mapcon fn (cdr lst)))))"

    ;; ── FR-548: fdefinition ────────────────────────────────────────────────
    "(defun fdefinition (name)
   (if (and (consp name) (eq (car name) 'setf))
       (symbol-function (car (cdr name)))
       (symbol-function name)))"

    ;; ── FR-582: equalp ────────────────────────────────────────────────────
    "(defun equalp (x y)
   (cond
     ((eql x y) t)
     ((and (characterp x) (characterp y))
      (char-equal x y))
     ((and (numberp x) (numberp y))
      (= x y))
     ((and (stringp x) (stringp y))
      (string-equal x y))
     ((and (consp x) (consp y))
      (and (equalp (car x) (car y))
           (equalp (cdr x) (cdr y))))
     ((and (vectorp x) (vectorp y) (not (stringp x)) (not (stringp y)))
      (and (= (length x) (length y))
           (let ((i 0) (len (length x)) (result t))
             (loop while (and result (< i len))
                   do (unless (equalp (aref x i) (aref y i))
                        (setq result nil))
                      (setq i (+ i 1)))
             result)))
     ((and (hash-table-p x) (hash-table-p y))
      (and (= (hash-table-count x) (hash-table-count y))
           (let ((result t))
             (maphash (lambda (k v)
                        (multiple-value-bind (v2 found) (gethash k y)
                          (unless (and found (equalp v v2))
                            (setq result nil))))
                      x)
             result)))
     (t nil)))"

    ;; ── FR-664: /= (not equal) ─────────────────────────────────────────────
    "(defun /= (a b) (not (= a b)))"

    ;; ── FR-529: Derived logical operations ─────────────────────────────────
    "(defun lognand (a b) (lognot (logand a b)))"
    "(defun lognor (a b) (lognot (logior a b)))"
    "(defun logandc1 (a b) (logand (lognot a) b))"
    "(defun logandc2 (a b) (logand a (lognot b)))"
    "(defun logorc1 (a b) (logior (lognot a) b))"
    "(defun logorc2 (a b) (logior a (lognot b)))"

    ;; ── FR-508: cis ────────────────────────────────────────────────────────
    "(defun cis (theta) (complex (cos theta) (sin theta)))"

    ;; ── FR-679: get-decoded-time ────────────────────────────────────────────
    "(defun get-decoded-time () (decode-universal-time (get-universal-time)))"

    ;; ── FR-560/FR-561: Numeric and float constants ─────────────────────────
    "(defvar most-positive-fixnum 4611686018427387903)"
    "(defvar most-negative-fixnum -4611686018427387904)"
    "(defvar internal-time-units-per-second 1000000)"
    "(defvar pi 3.141592653589793d0)"
    "(defvar double-float-epsilon 1.1102230246251568d-16)"
    "(defvar single-float-epsilon 1.1920929e-7)"
    "(defvar short-float-epsilon 1.1920929e-7)"
    "(defvar long-float-epsilon 1.1102230246251568d-16)"
    "(defvar most-positive-double-float 1.7976931348623157d+308)"
    "(defvar most-negative-double-float -1.7976931348623157d+308)"
    "(defvar most-positive-single-float 3.4028235e+38)"
    "(defvar most-negative-single-float -3.4028235e+38)"
    "(defvar most-positive-short-float 3.4028235e+38)"
    "(defvar most-negative-short-float -3.4028235e+38)"
    "(defvar most-positive-long-float 1.7976931348623157d+308)"
    "(defvar most-negative-long-float -1.7976931348623157d+308)"
    "(defvar least-positive-double-float 5.0d-324)"
    "(defvar least-negative-double-float -5.0d-324)"
    "(defvar least-positive-single-float 1.4e-45)"
    "(defvar least-negative-single-float -1.4e-45)"
    "(defvar least-positive-short-float 1.4e-45)"
    "(defvar least-negative-short-float -1.4e-45)"
    "(defvar least-positive-long-float 5.0d-324)"
    "(defvar least-negative-long-float -5.0d-324)"
    "(defvar least-positive-normalized-double-float 2.2250738585072014d-308)"
    "(defvar least-positive-normalized-single-float 1.17549435e-38)"
    "(defvar least-negative-normalized-double-float -2.2250738585072014d-308)"
    "(defvar least-negative-normalized-single-float -1.17549435e-38)"
    "(defvar array-dimension-limit 1073741823)"
    "(defvar array-total-size-limit 1073741823)"
    "(defvar array-rank-limit 8)"
    "(defvar char-code-limit 1114112)"


)
  "Standard library source — core functions (mapcar through numeric constants).
   Extended by stdlib-source-ext.lisp which loads after this file.")

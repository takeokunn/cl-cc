(in-package :cl-cc/expand)

;;; Mutation macros split from stdlib

(defun %compound-place-binding (place)
  "Return PLACE and temporary bindings that evaluate its subforms once."
  (let* ((args     (rest place))
         (arg-syms (mapcar (lambda (arg) (declare (ignore arg)) (gensym "PA")) args))
         (bindings (mapcar (lambda (sym arg) `(,sym ,arg)) arg-syms args))
         (saved    (cons (first place) arg-syms)))
    (values saved bindings)))

;; PUSH macro — FR-693: evaluate compound place subforms exactly once
(our-defmacro push (value place)
  "Push VALUE onto the front of list PLACE."
  (if (symbolp place)
      (list (quote setf) place (list (quote cons) value place))
      (multiple-value-bind (saved bindings)
          (%compound-place-binding place)
        (let ((v (gensym "V")))
          (list (quote let*)
                (cons (list v value) bindings)
                (list (quote setf) saved (list (quote cons) v saved)))))))

;; POP macro — FR-693: read compound place once; evaluate subforms once
(our-defmacro pop (place)
  "Remove and return the first element of list PLACE."
  (if (symbolp place)
      (let ((tmp (gensym "TMP")))
        (list (quote let)
              (list (list tmp place))
              (list (quote setf) place (list (quote cdr) tmp))
              (list (quote car) tmp)))
      (multiple-value-bind (saved bindings)
          (%compound-place-binding place)
        (let ((tmp (gensym "TMP")))
          (list (quote let*)
                (append bindings (list (list tmp saved)))
                (list (quote setf) saved (list (quote cdr) tmp))
                (list (quote car) tmp))))))

;; INCF macro — FR-693: gensym-protect compound place subforms
(our-defmacro incf (place &optional (delta 1))
  "Increment PLACE by DELTA (default 1)."
  (if (symbolp place)
      (list (quote setq) place (list (quote +) place delta))
      (multiple-value-bind (saved bindings)
          (%compound-place-binding place)
        (let ((d (gensym "D")))
          (list (quote let*)
                (cons (list d delta) bindings)
                (list (quote setf) saved (list (quote +) saved d)))))))

;; DECF macro — FR-693: gensym-protect compound place subforms
(our-defmacro decf (place &optional (delta 1))
  "Decrement PLACE by DELTA (default 1)."
  (if (symbolp place)
      (list (quote setq) place (list (quote -) place delta))
      (multiple-value-bind (saved bindings)
          (%compound-place-binding place)
        (let ((d (gensym "D")))
          (list (quote let*)
                (cons (list d delta) bindings)
                (list (quote setf) saved (list (quote -) saved d)))))))

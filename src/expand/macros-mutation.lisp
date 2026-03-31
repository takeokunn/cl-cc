(in-package :cl-cc)

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
      ;; Simple variable: no subform evaluation issue
      `(setf ,place (cons ,value ,place))
      ;; Compound place: save each subform arg to avoid double-eval
      (multiple-value-bind (saved bindings)
          (%compound-place-binding place)
        (let ((v (gensym "V")))
          `(let* ((,v ,value) ,@bindings)
             (setf ,saved (cons ,v ,saved)))))))

;; POP macro — FR-693: read compound place once; evaluate subforms once
(our-defmacro pop (place)
  "Remove and return the first element of list PLACE."
  (if (symbolp place)
      ;; Simple variable: read into tmp once, write cdr back
      (let ((tmp (gensym "TMP")))
        `(let ((,tmp ,place))
           (setf ,place (cdr ,tmp))
           (car ,tmp)))
      ;; Compound place: save subform args, read place into tmp once
      (multiple-value-bind (saved bindings)
          (%compound-place-binding place)
        (let ((tmp (gensym "TMP")))
          `(let* (,@bindings (,tmp ,saved))
             (setf ,saved (cdr ,tmp))
             (car ,tmp))))))

;; INCF macro — FR-693: gensym-protect compound place subforms
(our-defmacro incf (place &optional (delta 1))
  "Increment PLACE by DELTA (default 1)."
  (if (symbolp place)
      `(setq ,place (+ ,place ,delta))
      (multiple-value-bind (saved bindings)
          (%compound-place-binding place)
        (let ((d (gensym "D")))
          `(let* ((,d ,delta) ,@bindings)
             (setf ,saved (+ ,saved ,d)))))))

;; DECF macro — FR-693: gensym-protect compound place subforms
(our-defmacro decf (place &optional (delta 1))
  "Decrement PLACE by DELTA (default 1)."
  (if (symbolp place)
      `(setq ,place (- ,place ,delta))
      (multiple-value-bind (saved bindings)
          (%compound-place-binding place)
        (let ((d (gensym "D")))
          `(let* ((,d ,delta) ,@bindings)
             (setf ,saved (- ,saved ,d)))))))

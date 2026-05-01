;;;; lower-setf.lisp — Setf Compound Place Lowering
;;;;
;;;; %lower-setf-place: dispatches slot-value/gethash/getf/fdefinition/simple-rewrite
;;;; places; falls back to error for unsupported compound places.
;;;; define-list-lowerer for (setf) integrates with the main lowerer table.
;;;;
;;;; Load order: after lower.lisp (needs lower-sexp-to-ast, *setf-place-simple-rewrites*).

(in-package :cl-cc/parse)

;;; ── Setf ─────────────────────────────────────────────────────────────────────

(defun %lower-setf-place (place value-form sf sl sc)
  "Lower a (setf (PLACE ...) VALUE-FORM) compound place.
Simple places dispatch via *setf-place-simple-rewrites*; complex places
(slot-value, gethash, getf) are handled specially."
  (let* ((head (car place))
         (place-args (cdr place))
         (rule (assoc head *setf-place-simple-rewrites*)))
    (if rule
        (let* ((fn-sym  (second rule))
               (n-args  (third rule))
               (used-args (subseq place-args 0 n-args)))
          (lower-sexp-to-ast (append (list fn-sym) used-args (list value-form))))
        (case head
          (slot-value
           (let ((obj-form (second place)) (slot-form (third place)))
             (if (and (consp slot-form) (eq (car slot-form) 'quote))
                 (make-ast-set-slot-value
                  :object (lower-sexp-to-ast obj-form)
                  :slot   (second slot-form)
                  :value  (lower-sexp-to-ast value-form)
                  :source-file sf :source-line sl :source-column sc)
                 (lower-sexp-to-ast (list 'rt-slot-set obj-form slot-form value-form)))))
          (gethash
           (make-ast-set-gethash
            :key   (lower-sexp-to-ast (second place))
            :table (lower-sexp-to-ast (third place))
            :value (lower-sexp-to-ast value-form)
            :source-file sf :source-line sl :source-column sc))
          (getf
           (let ((plist-var (second place)) (indicator (third place)))
             (if (symbolp plist-var)
                 (let ((val-ast   (lower-sexp-to-ast value-form))
                       (plist-ast (lower-sexp-to-ast plist-var))
                       (ind-ast   (lower-sexp-to-ast indicator)))
                   (make-ast-progn
                    :forms (list (make-ast-setq
                                  :var plist-var
                                  :value (make-ast-call
                                          :func (lower-sexp-to-ast 'rt-plist-put)
                                          :args (list plist-ast ind-ast val-ast)))
                                 val-ast)))
                 (lower-sexp-to-ast (list 'rt-plist-put (second place) (third place) value-form)))))
          (fdefinition
           (lower-sexp-to-ast (list 'set-fdefinition value-form (second place))))
          (otherwise
            (error "setf only supports symbol, slot-value, gethash, getf, aref, svref, car, cdr, bit, char, elt, nth places"))))))

(define-list-lowerer (setf) (node sf sl sc)
  (unless (= (length node) 3)
    (error "setf requires a place and a value"))
  (let ((place (second node)) (value-form (third node)))
    (if (symbolp place)
        (make-ast-setq :var place :value (lower-sexp-to-ast value-form)
                       :source-file sf :source-line sl :source-column sc)
        (%lower-setf-place place value-form sf sl sc))))

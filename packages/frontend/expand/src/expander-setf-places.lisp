(in-package :cl-cc/expand)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — setf compound-place handlers
;;;
;;; Fact table for place-head symbols. Each entry is a simple registration.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %register-setf-place-handler (head handler)
    "Register HANDLER under HEAD in *SETF-COMPOUND-PLACE-HANDLERS*."
    (setf (gethash head *setf-compound-place-handlers*) handler)
    head)

  (defun %register-shared-setf-place-handler (heads handler)
    "Register HANDLER for each symbol in HEADS."
    (dolist (sym heads)
      (setf (gethash sym *setf-compound-place-handlers*) handler))
    heads))

(%register-setf-place-handler
 'aref
 (lambda (place value)
   (compiler-macroexpand-all
    (list 'aset (second place) (third place) value))))

(%register-setf-place-handler
 'getf
 (lambda (place value)
   (let ((v (gensym "V")))
     (compiler-macroexpand-all
      (list 'let (list (list v value))
            (list 'setq (second place)
                  (list 'rt-plist-put (second place) (third place) v))
            v)))))

(%register-setf-place-handler
 'get
 (lambda (place value)
   (let ((sym (gensym "SYM"))
         (ind (gensym "IND"))
         (v (gensym "V")))
     (compiler-macroexpand-all
      (list 'let* (list (list sym (second place))
                        (list ind (third place))
                        (list v value))
            (list '%set-symbol-plist sym
                  (list 'rt-plist-put (list 'symbol-plist sym) ind v))
            v)))))

;; All cons-cell accessors share the same expansion logic — hoist the closure once
(%register-shared-setf-place-handler
 '(car first cdr rest nth cadr cddr)
 (lambda (place value)
   (compiler-macroexpand-all (expand-setf-cons-place place value))))

;;; FR-593: (setf (subseq seq start end) new-seq) → (replace seq new-seq :start1 start :end1 end)
(%register-setf-place-handler
 'subseq
 (lambda (place value)
   (compiler-macroexpand-all
    (append (list 'replace (second place) value :start1 (third place))
            (when (fourth place)
              (list :end1 (fourth place)))))))

;;; FR-614: (setf (char s i) v) and (setf (schar s i) v)
(%register-shared-setf-place-handler
 '(char schar)
 (lambda (place value)
   (compiler-macroexpand-all
    (list 'rt-string-set (second place) (third place) value))))

;;; FR-636: (setf (bit bv i) x) and (setf (sbit bv i) x)
(%register-shared-setf-place-handler
 '(bit sbit)
 (lambda (place value)
   (compiler-macroexpand-all
    (list 'rt-bit-set (second place) (third place) value))))

;;; FR-620: (setf (svref v i) x) and (setf (row-major-aref a i) x)
(%register-setf-place-handler
 'svref
 (lambda (place value)
   (compiler-macroexpand-all
    (list '%svset (second place) (third place) value))))

(%register-setf-place-handler
 'row-major-aref
 (lambda (place value)
   (compiler-macroexpand-all
    (list 'aset (second place) (third place) value))))

;;; FR-552: (setf (find-class name) class) → register in class registry
(%register-setf-place-handler
 'find-class
 (lambda (place value)
   (compiler-macroexpand-all
    (list '%set-find-class (second place) value))))

;;; FR-548: (setf (symbol-function name) fn) → host bridge
(%register-setf-place-handler
 'symbol-function
 (lambda (place value)
   (compiler-macroexpand-all
    (list 'set-fdefinition value (second place)))))

;;; (setf (fdefinition name) fn) → host bridge, same lowering as symbol-function
(%register-setf-place-handler
 'fdefinition
 (lambda (place value)
   (compiler-macroexpand-all
    (list 'set-fdefinition value (second place)))))

;;; FR-586: (setf (symbol-value name) value) → host bridge
(%register-setf-place-handler
 'symbol-value
 (lambda (place value)
   (let ((sym (gensym "SYM"))
         (v (gensym "V")))
     (compiler-macroexpand-all
      (list 'let (list (list sym (second place))
                       (list v value))
            (list 'funcall
                  'cl-cc/bootstrap::*runtime-set-symbol-value-fn*
                  sym v)
            v)))))

;;; FR-428: (setf (macro-function name) fn) → host bridge
(%register-setf-place-handler
 'macro-function
 (lambda (place value)
   (compiler-macroexpand-all
    (list '%set-macro-function (second place) value))))

;;; FR-603: (setf (values a b ...) expr) → multiple-value-bind + setq chain
(%register-setf-place-handler
 'values
 (lambda (place value)
   (let* ((vars (cdr place))
          (temps (mapcar (lambda (var)
                           (declare (ignore var))
                           (gensym "V"))
                         vars)))
     (compiler-macroexpand-all
      (cons 'multiple-value-bind
            (cons temps
                  (cons value
                        (append (mapcar (lambda (var temp) (list 'setq var temp))
                                        vars temps)
                                (list (cons 'values vars))))))))))

(in-package :cl-cc)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — setf compound-place handlers
;;;
;;; Fact table for place-head symbols. Each entry is a simple registration.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(setf (gethash 'aref *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(aset ,(second place) ,(third place) ,value))))

(setf (gethash 'getf *setf-compound-place-handlers*)
      (lambda (place value)
        (let ((v (gensym "V")))
          (compiler-macroexpand-all
           `(let ((,v ,value))
              (setq ,(second place)
                    (rt-plist-put ,(second place) ,(third place) ,v))
              ,v)))))

;; All cons-cell accessors share the same expansion logic — hoist the closure once
(let ((cons-place-handler (lambda (place value)
                            (compiler-macroexpand-all (expand-setf-cons-place place value)))))
  (dolist (sym '(car first cdr rest nth cadr cddr))
    (setf (gethash sym *setf-compound-place-handlers*) cons-place-handler)))

;;; FR-593: (setf (subseq seq start end) new-seq) → (replace seq new-seq :start1 start :end1 end)
(setf (gethash 'subseq *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(replace ,(second place) ,value
                   :start1 ,(third place)
                   ,@(when (fourth place) `(:end1 ,(fourth place)))))))

;;; FR-614: (setf (char s i) v) and (setf (schar s i) v)
(let ((char-place-handler (lambda (place value)
                            (compiler-macroexpand-all
                             `(rt-string-set ,(second place) ,(third place) ,value)))))
  (dolist (sym '(char schar))
    (setf (gethash sym *setf-compound-place-handlers*) char-place-handler)))

;;; FR-636: (setf (bit bv i) x) and (setf (sbit bv i) x)
(let ((bit-place-handler (lambda (place value)
                           (compiler-macroexpand-all
                            `(rt-bit-set ,(second place) ,(third place) ,value)))))
  (dolist (sym '(bit sbit))
    (setf (gethash sym *setf-compound-place-handlers*) bit-place-handler)))

;;; FR-620: (setf (svref v i) x) and (setf (row-major-aref a i) x)
(setf (gethash 'svref *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(%svset ,(second place) ,(third place) ,value))))

(setf (gethash 'row-major-aref *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(aset ,(second place) ,(third place) ,value))))

;;; FR-552: (setf (find-class name) class) → register in class registry
(setf (gethash 'find-class *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(%set-find-class ,(second place) ,value))))

;;; FR-548: (setf (symbol-function name) fn) → host bridge
(setf (gethash 'symbol-function *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(set-fdefinition ,value ,(second place)))))

;;; FR-428: (setf (macro-function name) fn) → host bridge
(setf (gethash 'macro-function *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(%set-macro-function ,(second place) ,value))))

;;; FR-603: (setf (values a b ...) expr) → multiple-value-bind + setq chain
(setf (gethash 'values *setf-compound-place-handlers*)
      (lambda (place value)
        (let* ((vars (cdr place))
               (temps (mapcar (lambda (v) (declare (ignore v)) (gensym "V")) vars)))
          (compiler-macroexpand-all
           `(multiple-value-bind ,temps ,value
              ,@(mapcar (lambda (var temp) `(setq ,var ,temp)) vars temps)
              (values ,@vars))))))

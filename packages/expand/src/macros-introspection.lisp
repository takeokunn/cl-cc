(in-package :cl-cc/expand)

;;; ─── Equality / implementation / introspection helpers split from stdlib ────

(our-defmacro equalp (x y)
  "Test for structural equality with type coercion (ANSI 5.3)."
  (let ((xv (gensym "X"))
        (yv (gensym "Y"))
        (iv (gensym "I"))
        (kv (gensym "K"))
        (fn (gensym "EQP")))
    `(labels ((,fn (,xv ,yv)
                (cond
                  ((eq ,xv ,yv) t)
                  ((and (numberp ,xv) (numberp ,yv)) (= ,xv ,yv))
                  ((and (characterp ,xv) (characterp ,yv)) (char-equal ,xv ,yv))
                  ((and (stringp ,xv) (stringp ,yv)) (string-equal ,xv ,yv))
                  ((and (consp ,xv) (consp ,yv))
                   (and (,fn (car ,xv) (car ,yv)) (,fn (cdr ,xv) (cdr ,yv))))
                  ((and (vectorp ,xv) (vectorp ,yv))
                   (and (= (length ,xv) (length ,yv))
                        (block vec-eq
                          (dotimes (,iv (length ,xv) t)
                            (unless (,fn (aref ,xv ,iv) (aref ,yv ,iv))
                              (return-from vec-eq nil))))))
                  ((and (hash-table-p ,xv) (hash-table-p ,yv))
                   (and (= (hash-table-count ,xv) (hash-table-count ,yv))
                        (block ht-eq
                          (maphash (lambda (,kv v)
                                     (multiple-value-bind (yval found)
                                         (gethash ,kv ,yv)
                                       (unless (and found (,fn v yval))
                                         (return-from ht-eq nil))))
                                   ,xv)
                          t)))
                  (t nil))))
       (,fn ,x ,y))))

;;; Introspection queries are provided as runtime builtins in the VM/native pipeline.

(in-package :cl-cc/expand)

;;; ─── Ordering and list utility helpers split from stdlib ─────────────────────

;; SORT: stable sequence sort. Vectors are sorted in-place and returned; lists
;; are copied to a temporary vector, sorted, then returned as a fresh list.
(our-defmacro sort (sequence predicate &key key)
  (let ((seq       (gensym "SEQ"))
        (pred      (gensym "PRED"))
        (keyfn     (gensym "KEY"))
        (len       (gensym "LEN"))
        (vec       (gensym "VEC"))
        (i         (gensym "I"))
        (j         (gensym "J"))
        (src       (gensym "SRC"))
        (a         (gensym "A"))
        (b         (gensym "B"))
        (result    (gensym "RESULT")))
    (if key
        `(let* ((,seq ,sequence)
                (,pred ,predicate)
                (,keyfn ,key)
                (,len (length ,seq))
                (,vec (if (vectorp ,seq) ,seq (make-array ,len))))
           (unless (vectorp ,seq)
             (let ((,i 0))
               (dolist (,src ,seq)
                 (aset ,vec ,i ,src)
                 (setq ,i (+ ,i 1)))))
           (loop for ,i from 0 below ,len
                 do (loop for ,j from 0 below (- ,len 1)
                          do (let ((,a (aref ,vec ,j))
                                   (,b (aref ,vec (+ ,j 1))))
                               (when (funcall ,pred (funcall ,keyfn ,b) (funcall ,keyfn ,a))
                                 (aset ,vec ,j ,b)
                                 (aset ,vec (+ ,j 1) ,a)))))
           (if (vectorp ,seq)
               ,seq
               (let ((,result nil))
                 (loop for ,i from 0 below ,len
                       do (setq ,result (cons (aref ,vec ,i) ,result)))
                 (reverse ,result))))
        `(let* ((,seq ,sequence)
                (,pred ,predicate)
                (,len (length ,seq))
                (,vec (if (vectorp ,seq) ,seq (make-array ,len))))
           (unless (vectorp ,seq)
             (let ((,i 0))
               (dolist (,src ,seq)
                 (aset ,vec ,i ,src)
                 (setq ,i (+ ,i 1)))))
           (loop for ,i from 0 below ,len
                 do (loop for ,j from 0 below (- ,len 1)
                          do (let ((,a (aref ,vec ,j))
                                   (,b (aref ,vec (+ ,j 1))))
                               (when (funcall ,pred ,b ,a)
                                 (aset ,vec ,j ,b)
                                 (aset ,vec (+ ,j 1) ,a)))))
           (if (vectorp ,seq)
               ,seq
               (let ((,result nil))
                 (loop for ,i from 0 below ,len
                       do (setq ,result (cons (aref ,vec ,i) ,result)))
                 (reverse ,result)))))))

;; STABLE-SORT: same as sort (merge sort is inherently stable)
(our-defmacro stable-sort (list predicate &key key)
  (if key
      (list (quote sort) list predicate :key key)
      (list (quote sort) list predicate)))

;; MAP: map fn over seq, coerce result to result-type
(our-defmacro map (result-type fn seq)
  (list (quote coerce)
        (list (quote mapcar) fn (list (quote coerce) seq (list (quote quote) (quote list))))
        result-type))

;; IGNORE-ERRORS: catch all errors, return nil on error
(our-defmacro ignore-errors (&body forms)
  (let ((e-var (gensym "E")))
    (list (quote handler-case)
          (cons (quote progn) forms)
          (list (quote error) (list e-var) (list (quote values) nil e-var)))))

;; CONCATENATE: type-dispatching sequence concatenation (FR-615)
(our-defmacro concatenate (result-type &rest sequences)
  (let ((rtype (if (and (consp result-type) (eq (car result-type) (quote quote)))
                   (cadr result-type)
                   result-type)))
    (cond
      ((null sequences)
       (if (eq rtype (quote string)) "" nil))
      ((eq rtype (quote list))
       (cons (quote append)
             (mapcar (lambda (seq) (list (quote coerce) seq (list (quote quote) (quote list))))
                     sequences)))
      ((or (eq rtype (quote vector)) (eq rtype (quote simple-vector)))
       (list (quote coerce-to-vector)
             (cons (quote append)
                   (mapcar (lambda (seq) (list (quote coerce) seq (list (quote quote) (quote list))))
                           sequences))))
      (t
       (labels ((build (remaining acc)
                  (if (null remaining)
                      acc
                      (build (cdr remaining)
                             (list (quote string-concat) acc (car remaining))))))
         (if (null (cdr sequences))
             (car sequences)
             (build (cdr sequences) (car sequences))))))))

;;; ─── LIST* (FR-609) ─────────────────────────────────────────────────────────
;;; (list* a) = a
;;; (list* a b) = (cons a b)
;;; (list* a b c) = (cons a (cons b c))

(our-defmacro list* (&rest args)
  (cond
    ((null args) (error "list* requires at least one argument"))
    ((null (cdr args)) (car args))
    (t (labels ((build (items)
                  (if (null (cdr items))
                      (car items)
                      (list (quote cons) (car items) (build (cdr items))))))
         (build args)))))

;;; ─── List accessors sixth–tenth (FR-563) ───────────────────────────────────

(our-defmacro sixth   (list) (list (quote nth) 5 list))
(our-defmacro seventh (list) (list (quote nth) 6 list))
(our-defmacro eighth  (list) (list (quote nth) 7 list))
(our-defmacro ninth   (list) (list (quote nth) 8 list))
(our-defmacro tenth   (list) (list (quote nth) 9 list))

;;; ─── PUSHNEW (FR-587) ───────────────────────────────────────────────────────

(our-defmacro pushnew (item place &key (test (function eql)))
  (let ((item-var (gensym "ITEM")))
    (let ((member-form
            (if (or (equal test (function eql))
                    (equal test (list (quote quote) (function eql))))
                (list (quote member) item-var place)
                (list (quote member) item-var place :test test))))
      (list (quote let)
            (list (list item-var item))
            (list (quote unless)
                  member-form
                  (list (quote setf) place (list (quote cons) item-var place)))))))

;;; ─── NRECONC (FR-640) ───────────────────────────────────────────────────────

(our-defmacro nreconc (list tail)
  (list (quote nconc) (list (quote nreverse) list) tail))

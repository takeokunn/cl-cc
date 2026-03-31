(in-package :cl-cc)

;;; ─── Ordering and list utility helpers split from stdlib ─────────────────────

;; SORT: stable merge sort, with optional :key argument (FR-611)
(our-defmacro sort (list predicate &key key)
  (let ((pred    (gensym "PRED"))
        (keyfn   (gensym "KEY"))
        (len     (gensym "LEN"))
        (mid     (gensym "MID"))
        (left    (gensym "LEFT"))
        (right   (gensym "RIGHT"))
        (msort   (gensym "MSORT"))
        (mmerge  (gensym "MMERGE"))
        (take-n  (gensym "TAKEN")))
    (if key
        `(let ((,pred ,predicate)
               (,keyfn ,key))
           (labels ((,take-n (lst n)
                      (if (= n 0) nil
                          (cons (car lst) (,take-n (cdr lst) (- n 1)))))
                    (,mmerge (a b)
                      (cond ((null a) b)
                            ((null b) a)
                            ((funcall ,pred (funcall ,keyfn (car a)) (funcall ,keyfn (car b)))
                             (cons (car a) (,mmerge (cdr a) b)))
                            (t (cons (car b) (,mmerge a (cdr b))))))
                    (,msort (lst)
                      (let ((,len (length lst)))
                        (if (<= ,len 1) lst
                            (let* ((,mid (truncate ,len 2))
                                   (,left (,take-n lst ,mid))
                                   (,right (nthcdr ,mid lst)))
                              (,mmerge (,msort ,left) (,msort ,right)))))))
             (,msort ,list)))
        `(let ((,pred ,predicate))
           (labels ((,take-n (lst n)
                      (if (= n 0) nil
                          (cons (car lst) (,take-n (cdr lst) (- n 1)))))
                    (,mmerge (a b)
                      (cond ((null a) b)
                            ((null b) a)
                            ((funcall ,pred (car a) (car b))
                             (cons (car a) (,mmerge (cdr a) b)))
                            (t (cons (car b) (,mmerge a (cdr b))))))
                    (,msort (lst)
                      (let ((,len (length lst)))
                        (if (<= ,len 1) lst
                            (let* ((,mid (truncate ,len 2))
                                   (,left (,take-n lst ,mid))
                                   (,right (nthcdr ,mid lst)))
                              (,mmerge (,msort ,left) (,msort ,right)))))))
             (,msort ,list))))))

;; STABLE-SORT: same as sort (merge sort is inherently stable)
(our-defmacro stable-sort (list predicate &key key)
  (if key
      `(sort ,list ,predicate :key ,key)
      `(sort ,list ,predicate)))

;; MAP: map fn over seq, coerce result to result-type
(our-defmacro map (result-type fn seq)
  `(coerce (mapcar ,fn (coerce ,seq 'list)) ,result-type))

;; IGNORE-ERRORS: catch all errors, return nil on error
(our-defmacro ignore-errors (&body forms)
  (let ((e-var (gensym "E")))
    `(handler-case (progn ,@forms)
       (error (,e-var) (values nil ,e-var)))))

;; CONCATENATE: type-dispatching sequence concatenation (FR-615)
(our-defmacro concatenate (result-type &rest sequences)
  (let ((rtype (if (and (consp result-type) (eq (car result-type) 'quote))
                   (cadr result-type)
                   result-type)))
    (cond
      ((null sequences)
       (if (eq rtype 'string) "" nil))
      ((eq rtype 'list)
       `(append ,@sequences))
      ((member rtype '(vector simple-vector))
       `(coerce-to-vector (append ,@sequences)))
      (t
       (if (null (cdr sequences))
           (car sequences)
           (reduce (lambda (acc s) `(string-concat ,acc ,s))
                   (cdr sequences)
                   :initial-value (car sequences)))))))

;;; ─── LIST* (FR-609) ─────────────────────────────────────────────────────────
;;; (list* a) = a
;;; (list* a b) = (cons a b)
;;; (list* a b c) = (cons a (cons b c))

(our-defmacro list* (&rest args)
  (cond
    ((null args) (error "list* requires at least one argument"))
    ((null (cdr args)) (car args))
    (t (reduce (lambda (x acc) `(cons ,x ,acc))
               args
               :from-end t))))

;;; ─── List accessors sixth–tenth (FR-563) ───────────────────────────────────

(our-defmacro sixth   (list) `(nth 5 ,list))
(our-defmacro seventh (list) `(nth 6 ,list))
(our-defmacro eighth  (list) `(nth 7 ,list))
(our-defmacro ninth   (list) `(nth 8 ,list))
(our-defmacro tenth   (list) `(nth 9 ,list))

;;; ─── PUSHNEW (FR-587) ───────────────────────────────────────────────────────

(our-defmacro pushnew (item place &key (test '#'eql))
  (let ((item-var (gensym "ITEM")))
    (if (equal test '#'eql)
        `(let ((,item-var ,item))
           (unless (member ,item-var ,place)
             (setf ,place (cons ,item-var ,place))))
        `(let ((,item-var ,item))
           (unless (member ,item-var ,place :test ,test)
             (setf ,place (cons ,item-var ,place)))))))

;;; ─── NRECONC (FR-640) ───────────────────────────────────────────────────────

(our-defmacro nreconc (list tail)
  `(nconc (nreverse ,list) ,tail))

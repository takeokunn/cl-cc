;;;; macros-sequence.lisp — Sequence operations (FR-500)
(in-package :cl-cc)
;;; Phase 3: Sequence Operations (FR-500)
;;; ------------------------------------------------------------

;; COPY-SEQ (FR-507): shallow copy of a sequence (list or vector)
(our-defmacro copy-seq (seq)
  "Return a fresh copy of SEQ (works for lists and vectors)."
  (let ((s (gensym "SEQ")))
    `(let ((,s ,seq))
       (if (listp ,s)
           (copy-list ,s)
           (subseq ,s 0)))))

;; FILL (FR-502): fill a sequence (list or vector) with item; supports :start/:end
(our-defmacro fill (seq item &key start end)
  "Fill SEQ[start..end) with ITEM. Works for lists and vectors."
  (let ((s (gensym "SEQ")) (it (gensym "ITEM")) (i (gensym "I"))
        (lim (gensym "LIM")) (ptr (gensym "PTR")) (cnt (gensym "CNT")) (lbl (gensym "FILL")))
    `(let* ((,s ,seq) (,it ,item)
            (,i ,(or start 0))
            (,lim (or ,(or end nil) (length ,s))))
       (if (vectorp ,s)
           ;; vector path: indexed fill with aref
           (tagbody ,lbl
             (when (< ,i ,lim)
               (setf (aref ,s ,i) ,it)
               (setq ,i (+ ,i 1))
               (go ,lbl)))
           ;; list path: advance to start, then fill (lim-i) elements
           (let ((,ptr (nthcdr ,i ,s)) (,cnt 0) (,lim (- ,lim ,i)))
             (tagbody ,lbl
               (when (and ,ptr (< ,cnt ,lim))
                 (setf (car ,ptr) ,it)
                 (setq ,ptr (cdr ,ptr))
                 (setq ,cnt (+ ,cnt 1))
                 (go ,lbl)))))
       ,s)))

;; REPLACE (FR-502): copy elements from source into dest; supports :start/:end keywords
(our-defmacro replace (dest source &key start1 end1 start2 end2)
  "Copy elements from SOURCE[start2..end2) into DEST[start1..end1). Works for lists and vectors."
  (let ((d (gensym "DEST")) (s (gensym "SRC"))
        (di (gensym "DI")) (si (gensym "SI"))
        (dlim (gensym "DLIM")) (slim (gensym "SLIM"))
        (dp (gensym "DP")) (sp (gensym "SP")) (dcnt (gensym "DC")) (scnt (gensym "SC"))
        (lbl (gensym "REPLACE")))
    `(let* ((,d ,dest) (,s ,source))
       (if (and (vectorp ,d) (vectorp ,s))
           ;; vector path: indexed copy with aref
           (let ((,di ,(or start1 0)) (,si ,(or start2 0))
                 (,dlim (or ,(or end1 nil) (length ,d)))
                 (,slim (or ,(or end2 nil) (length ,s))))
             (tagbody ,lbl
               (when (and (< ,di ,dlim) (< ,si ,slim))
                 (setf (aref ,d ,di) (aref ,s ,si))
                 (setq ,di (+ ,di 1))
                 (setq ,si (+ ,si 1))
                 (go ,lbl))))
           ;; list path: pointer walk from start positions
           (let ((,dp (nthcdr ,(or start1 0) ,d))
                 (,sp (nthcdr ,(or start2 0) ,s))
                 (,dcnt (- (or ,(or end1 nil) (length ,d)) ,(or start1 0)))
                 (,scnt (- (or ,(or end2 nil) (length ,s)) ,(or start2 0)))
                 (,di 0) (,si 0))
             (tagbody ,lbl
               (when (and ,dp ,sp (< ,di ,dcnt) (< ,si ,scnt))
                 (setf (car ,dp) (car ,sp))
                 (setq ,dp (cdr ,dp))
                 (setq ,sp (cdr ,sp))
                 (setq ,di (+ ,di 1))
                 (setq ,si (+ ,si 1))
                 (go ,lbl)))))
       ,d)))

;; MISMATCH (FR-506): first position where sequences differ
(our-defmacro mismatch (seq1 seq2 &key test key from-end)
  "Return index of first mismatch between SEQ1 and SEQ2, or NIL if equal.
Supports :test (default #'eql), :key, :from-end."
  (let ((s1 (gensym "S1"))
        (s2 (gensym "S2"))
        (idx (gensym "IDX"))
        (tst-var (gensym "TST"))
        (kfn-var (gensym "KEY"))
        (lbl (gensym "MISMATCH-LOOP")))
    (let* ((e1 (if key `(funcall ,kfn-var (car ,s1)) `(car ,s1)))
           (e2 (if key `(funcall ,kfn-var (car ,s2)) `(car ,s2)))
           (test-expr (if test `(funcall ,tst-var ,e1 ,e2) `(eql ,e1 ,e2)))
           (bindings `(,@(when test `((,tst-var ,test)))
                       ,@(when key  `((,kfn-var ,key)))
                       (,s1 ,(if from-end `(reverse ,seq1) seq1))
                       (,s2 ,(if from-end `(reverse ,seq2) seq2))
                       (,idx 0)))
           (forward-loop
             `(block nil
                (let (,@bindings)
                  (tagbody
                    ,lbl
                    (cond
                      ((and (null ,s1) (null ,s2)) (return nil))
                      ((or  (null ,s1) (null ,s2)) (return ,idx))
                      ((not ,test-expr) (return ,idx))
                      (t (setq ,s1 (cdr ,s1))
                         (setq ,s2 (cdr ,s2))
                         (setq ,idx (+ ,idx 1))
                         (go ,lbl))))))))
      (if from-end
          ;; from-end: run forward mismatch on reversed seqs, map index back
          (let ((len1 (gensym "LEN1")) (len2 (gensym "LEN2")) (mi (gensym "MI")))
            `(let ((,len1 (length ,seq1)) (,len2 (length ,seq2))
                   (,mi ,forward-loop))
               (if (null ,mi) nil (- (min ,len1 ,len2) ,mi))))
          forward-loop))))

;; DELETE (FR-504): like REMOVE but destructive — delegates to remove (with keyword support)
(our-defmacro delete (item seq &key test key test-not)
  "Remove all elements matching ITEM from SEQ (delegates to remove)."
  (if (or test key test-not)
      `(remove ,item ,seq
               ,@(when test `(:test ,test))
               ,@(when key `(:key ,key))
               ,@(when test-not `(:test-not ,test-not)))
      `(remove ,item ,seq)))

(defun %delete-if-key-expand (pred seq key keep-when-true-p)
  (let ((fn-var (gensym "FN"))
        (kfn (gensym "KEY"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,pred) (,kfn ,key) (,acc nil))
       (dolist (,x ,seq (nreverse ,acc))
         (when ,(if keep-when-true-p
                    `(funcall ,fn-var (funcall ,kfn ,x))
                    `(not (funcall ,fn-var (funcall ,kfn ,x))))
           (setq ,acc (cons ,x ,acc)))))))

;; DELETE-IF / DELETE-IF-NOT (FR-504) — same as remove-if/-not (with :key support)
(our-defmacro delete-if (pred seq &key key)
  "Remove all elements for which PRED is true (same as remove-if)."
  (if key
      (%delete-if-key-expand pred seq key nil)
      (%filter-list-expand 'unless pred seq)))

(our-defmacro delete-if-not (pred seq &key key)
  "Remove all elements for which PRED is false (same as remove-if-not)."
  (if key
      (%delete-if-key-expand pred seq key t)
      (%filter-list-expand 'when pred seq)))

;; DELETE-DUPLICATES (FR-504) — delegates to remove-duplicates with keyword support
(our-defmacro delete-duplicates (seq &key test key test-not)
  "Remove duplicate elements (keeps first occurrence)."
  (if (or test key test-not)
      `(remove-duplicates ,seq
                          ,@(when test `(:test ,test))
                          ,@(when key `(:key ,key))
                          ,@(when test-not `(:test-not ,test-not)))
      `(remove-duplicates ,seq)))

;; SUBSTITUTE (FR-505): replace occurrences of old with new (with optional :test/:key)
(our-defmacro substitute (new old seq &key test key test-not)
  "Return new sequence with each matching OLD replaced by NEW."
  (if (or test key test-not)
      (let ((new-var (gensym "NEW")) (old-var (gensym "OLD")) (x (gensym "X"))
            (tst-var (gensym "TST")) (kfn-var (gensym "KEY")) (acc (gensym "ACC")))
        `(let ((,new-var ,new) (,old-var ,old) (,acc nil)
               (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
               (,kfn-var ,(or key '#'identity)))
           (dolist (,x ,seq (nreverse ,acc))
             (setq ,acc (cons (if (funcall ,tst-var ,old-var (funcall ,kfn-var ,x)) ,new-var ,x)
                              ,acc)))))
      (let ((new-var (gensym "NEW")) (old-var (gensym "OLD")) (x (gensym "X")) (acc (gensym "ACC")))
        `(let ((,new-var ,new) (,old-var ,old) (,acc nil))
           (dolist (,x ,seq (nreverse ,acc))
             (setq ,acc (cons (if (eql ,x ,old-var) ,new-var ,x) ,acc)))))))

;; Shared expansion for substitute-if / substitute-if-not.
;; MATCH-FORM: the value chosen when PRED is true (new-var or x).
;; ELSE-FORM:  the value chosen when PRED is false (x or new-var).
(defun %substitute-if-expand (new pred seq match-form else-form)
  (let ((new-var (gensym "NEW"))
        (fn-var  (gensym "FN"))
        (x       (gensym "X"))
        (acc     (gensym "ACC")))
    (let ((match (subst new-var 'new-var (subst x 'x match-form)))
          (else  (subst new-var 'new-var (subst x 'x else-form))))
      `(let ((,new-var ,new)
             (,fn-var  ,pred)
             (,acc nil))
         (dolist (,x ,seq (nreverse ,acc))
           (setq ,acc (cons (if (funcall ,fn-var ,x) ,match ,else) ,acc)))))))

(defun %substitute-if-key-expand (new pred seq key match-form else-form)
  (let ((new-var (gensym "NEW"))
        (fn-var (gensym "FN"))
        (kfn-var (gensym "KEY"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,new-var ,new)
           (,fn-var ,pred)
           (,kfn-var ,key)
           (,acc nil))
       (dolist (,x ,seq (nreverse ,acc))
         (setq ,acc (cons (if (funcall ,fn-var (funcall ,kfn-var ,x))
                              ,match-form
                              ,else-form)
                          ,acc))))))

;; SUBSTITUTE-IF (FR-505) — replace where pred is true
(our-defmacro substitute-if (new pred seq &key key)
  "Replace elements for which PRED is true with NEW."
  (if key
      (%substitute-if-key-expand new pred seq key 'new-var 'x)
      (%substitute-if-expand new pred seq 'new-var 'x)))

;; SUBSTITUTE-IF-NOT (FR-505) — replace where pred is false
(our-defmacro substitute-if-not (new pred seq &key key)
  "Replace elements for which PRED is false with NEW."
  (if key
      (%substitute-if-key-expand new pred seq key 'x 'new-var)
      (%substitute-if-expand new pred seq 'x 'new-var)))


;;; (reduce, nsubstitute, map-into, merge, last/butlast/search
;;;  are in macros-sequence-fold.lisp which loads after this file.)


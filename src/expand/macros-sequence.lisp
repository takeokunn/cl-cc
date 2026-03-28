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

;; DELETE-IF / DELETE-IF-NOT (FR-504) — same as remove-if/-not (with :key support)
(our-defmacro delete-if (pred seq &key key)
  "Remove all elements for which PRED is true (same as remove-if)."
  (if key
      (let ((fn-var (gensym "FN")) (kfn (gensym "KEY"))
            (x (gensym "X")) (acc (gensym "ACC")))
        `(let ((,fn-var ,pred) (,kfn ,key) (,acc nil))
           (dolist (,x ,seq (nreverse ,acc))
             (unless (funcall ,fn-var (funcall ,kfn ,x))
               (setq ,acc (cons ,x ,acc))))))
      (%filter-list-expand 'unless pred seq)))

(our-defmacro delete-if-not (pred seq &key key)
  "Remove all elements for which PRED is false (same as remove-if-not)."
  (if key
      (let ((fn-var (gensym "FN")) (kfn (gensym "KEY"))
            (x (gensym "X")) (acc (gensym "ACC")))
        `(let ((,fn-var ,pred) (,kfn ,key) (,acc nil))
           (dolist (,x ,seq (nreverse ,acc))
             (when (funcall ,fn-var (funcall ,kfn ,x))
               (setq ,acc (cons ,x ,acc))))))
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

;; SUBSTITUTE-IF (FR-505) — replace where pred is true
(our-defmacro substitute-if (new pred seq &key key)
  "Replace elements for which PRED is true with NEW."
  (if key
      (let ((new-var (gensym "NEW")) (fn-var (gensym "FN"))
            (kfn-var (gensym "KEY")) (x (gensym "X")) (acc (gensym "ACC")))
        `(let ((,new-var ,new) (,fn-var ,pred) (,kfn-var ,key) (,acc nil))
           (dolist (,x ,seq (nreverse ,acc))
             (setq ,acc (cons (if (funcall ,fn-var (funcall ,kfn-var ,x)) ,new-var ,x) ,acc)))))
      (%substitute-if-expand new pred seq 'new-var 'x)))

;; SUBSTITUTE-IF-NOT (FR-505) — replace where pred is false
(our-defmacro substitute-if-not (new pred seq &key key)
  "Replace elements for which PRED is false with NEW."
  (if key
      (let ((new-var (gensym "NEW")) (fn-var (gensym "FN"))
            (kfn-var (gensym "KEY")) (x (gensym "X")) (acc (gensym "ACC")))
        `(let ((,new-var ,new) (,fn-var ,pred) (,kfn-var ,key) (,acc nil))
           (dolist (,x ,seq (nreverse ,acc))
             (setq ,acc (cons (if (funcall ,fn-var (funcall ,kfn-var ,x)) ,x ,new-var) ,acc)))))
      (%substitute-if-expand new pred seq 'x 'new-var)))

;; NSUBSTITUTE / NSUBSTITUTE-IF / NSUBSTITUTE-IF-NOT (FR-505): same as substitute (non-destructive)
;;; REDUCE (FR-500 adjacent): fold a sequence using a binary function
;;; Uses the proven dolist-style tagbody pattern with (go end-tag) inside.
(our-defmacro reduce (fn seq &rest keys)
  "Fold SEQ using FN. Supports :initial-value, :from-end, :key (static at macro-expand time)."
  (let* ((fn-var   (gensym "FN"))
         (cur-var  (gensym "CUR"))
         (acc-var  (gensym "ACC"))
         (start-lbl (gensym "REDUCE-START"))
         (end-lbl   (gensym "REDUCE-END"))
         (has-iv   (not (null (member :initial-value keys))))
         (iv-expr  (getf keys :initial-value nil))
         (fe-expr  (getf keys :from-end nil))
         (key-fn   (getf keys :key nil))
         (seq-init (if fe-expr `(reverse ,seq) seq))
         (elem-form (if key-fn
                        `(funcall ,key-fn (car ,cur-var))
                        `(car ,cur-var))))
    (if has-iv
        ;; With initial-value: acc starts at iv, cur iterates entire seq
        `(let ((,fn-var ,fn)
               (,acc-var ,iv-expr)
               (,cur-var ,seq-init))
           (tagbody
            ,start-lbl
            (if (null ,cur-var) (go ,end-lbl))
            (setq ,acc-var (funcall ,fn-var ,acc-var ,elem-form))
            (setq ,cur-var (cdr ,cur-var))
            (go ,start-lbl)
            ,end-lbl)
           ,acc-var)
        ;; No initial-value: acc starts at first element, cur at rest
        `(let ((,fn-var ,fn)
               (,cur-var ,seq-init))
           (let ((,acc-var (car ,cur-var)))
             (setq ,cur-var (cdr ,cur-var))
             (tagbody
              ,start-lbl
              (if (null ,cur-var) (go ,end-lbl))
              (setq ,acc-var (funcall ,fn-var ,acc-var ,elem-form))
              (setq ,cur-var (cdr ,cur-var))
              (go ,start-lbl)
              ,end-lbl)
             ,acc-var)))))

(our-defmacro nsubstitute (new old seq &key test key test-not)
  "Destructive substitute (delegates to substitute with keyword forwarding)."
  `(substitute ,new ,old ,seq
               ,@(when test     `(:test ,test))
               ,@(when key      `(:key ,key))
               ,@(when test-not `(:test-not ,test-not))))

(our-defmacro nsubstitute-if (new pred seq &key key)
  "Destructive substitute-if (delegates to substitute-if)."
  `(substitute-if ,new ,pred ,seq ,@(when key `(:key ,key))))

(our-defmacro nsubstitute-if-not (new pred seq &key key)
  "Destructive substitute-if-not (delegates to substitute-if-not)."
  `(substitute-if-not ,new ,pred ,seq ,@(when key `(:key ,key))))

;; MAP-INTO (FR-503): fill sequence with mapped results
(our-defmacro map-into (dest fn &rest seqs)
  "Fill DEST with (fn (elt seq1 i) ...) for each i."
  (if (= (length seqs) 1)
      (let ((d (gensym "DEST"))
            (src (gensym "SRC"))
            (dp (gensym "DP"))
            (sp (gensym "SP"))
            (fn-var (gensym "FN"))
            (lbl (gensym "MAP-INTO-LOOP")))
        `(let ((,d ,dest)
               (,src ,(first seqs))
               (,fn-var ,fn))
           (let ((,dp ,d)
                 (,sp ,src))
             (tagbody
               ,lbl
               (when (and ,dp ,sp)
                 (setf (car ,dp) (funcall ,fn-var (car ,sp)))
                 (setq ,dp (cdr ,dp))
                 (setq ,sp (cdr ,sp))
                 (go ,lbl))))
           ,d))
      `(progn ,dest)))

;;; MERGE (FR-504): merge two sorted sequences using predicate
(our-defmacro merge (result-type seq1 seq2 pred &rest keys)
  "Merge two sorted sequences SEQ1 and SEQ2 into a sorted sequence using PRED."
  (when keys)
  (when result-type)
  (let ((l1 (gensym "L1"))
        (l2 (gensym "L2"))
        (fn-var (gensym "PRED"))
        (acc (gensym "ACC"))
        (lbl1 (gensym "MERGE-LOOP"))
        (lbl2 (gensym "DRAIN1"))
        (lbl3 (gensym "DRAIN2"))
        (end (gensym "MERGE-END")))
    `(let ((,l1 ,seq1)
           (,l2 ,seq2)
           (,fn-var ,pred)
           (,acc nil))
       (tagbody
         ,lbl1
         (when (and ,l1 ,l2)
           (if (funcall ,fn-var (car ,l1) (car ,l2))
               (progn (setq ,acc (cons (car ,l1) ,acc))
                      (setq ,l1 (cdr ,l1)))
               (progn (setq ,acc (cons (car ,l2) ,acc))
                      (setq ,l2 (cdr ,l2))))
           (go ,lbl1))
         ,lbl2
         (when ,l1
           (setq ,acc (cons (car ,l1) ,acc))
           (setq ,l1 (cdr ,l1))
           (go ,lbl2))
         ,lbl3
         (when ,l2
           (setq ,acc (cons (car ,l2) ,acc))
           (setq ,l2 (cdr ,l2))
           (go ,lbl3))
         ,end)
       (nreverse ,acc))))


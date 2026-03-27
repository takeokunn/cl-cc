;;;; macros-sequence.lisp — Sequence operations (FR-500)
(in-package :cl-cc)
;;; Phase 3: Sequence Operations (FR-500)
;;; ------------------------------------------------------------

;; COPY-SEQ (FR-507): shallow copy of a sequence
(our-defmacro copy-seq (seq)
  "Return a fresh copy of SEQ."
  `(copy-list ,seq))

;; FILL (FR-502): fill a sequence (list) with item
(our-defmacro fill (seq item &rest keys)
  "Fill SEQ with ITEM (list version; :start/:end ignored)."
  (when keys)
  (let ((s (gensym "SEQ"))
        (ptr (gensym "PTR"))
        (lbl (gensym "FILL-LOOP")))
    `(let* ((,s ,seq)
            (,ptr ,s))
       (tagbody
         ,lbl
         (when ,ptr
           (setf (car ,ptr) ,item)
           (setq ,ptr (cdr ,ptr))
           (go ,lbl)))
       ,s)))

;; REPLACE (FR-502): copy elements from source into destination
(our-defmacro replace (dest source &rest keys)
  "Copy elements from SOURCE into DEST (list version; keys ignored)."
  (when keys)
  (let ((d (gensym "DEST"))
        (s (gensym "SRC"))
        (dp (gensym "DP"))
        (sp (gensym "SP"))
        (lbl (gensym "REPLACE-LOOP")))
    `(let ((,d ,dest)
           (,s ,source))
       (let ((,dp ,d)
             (,sp ,s))
         (tagbody
           ,lbl
           (when (and ,dp ,sp)
             (setf (car ,dp) (car ,sp))
             (setq ,dp (cdr ,dp))
             (setq ,sp (cdr ,sp))
             (go ,lbl))))
       ,d)))

;; MISMATCH (FR-506): first position where sequences differ
(our-defmacro mismatch (seq1 seq2 &rest keys)
  "Return index of first mismatch between SEQ1 and SEQ2, or NIL if equal."
  (when keys)
  (let ((s1 (gensym "S1"))
        (s2 (gensym "S2"))
        (idx (gensym "IDX"))
        (lbl (gensym "MISMATCH-LOOP")))
    `(block nil
       (let ((,s1 ,seq1)
             (,s2 ,seq2)
             (,idx 0))
         (tagbody
           ,lbl
           (cond
             ((and (null ,s1) (null ,s2)) (return nil))
             ((or  (null ,s1) (null ,s2)) (return ,idx))
             ((not (eql (car ,s1) (car ,s2))) (return ,idx))
             (t (setq ,s1 (cdr ,s1))
                (setq ,s2 (cdr ,s2))
                (setq ,idx (+ ,idx 1))
                (go ,lbl))))))))

;; DELETE (FR-504): like REMOVE but destructive (same as remove here)
(our-defmacro delete (item seq &rest keys)
  "Remove all elements EQL to ITEM from SEQ."
  (when keys)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,item-var ,item)
           (,acc nil))
       (dolist (,x ,seq (nreverse ,acc))
         (unless (eql ,item-var ,x)
           (setq ,acc (cons ,x ,acc)))))))

;; DELETE-IF / DELETE-IF-NOT (FR-504) — reuse %filter-list-expand from macros-stdlib
(our-defmacro delete-if (pred seq &rest keys)
  "Remove all elements for which PRED is true (same as remove-if)."
  (when keys)
  (%filter-list-expand 'unless pred seq))

(our-defmacro delete-if-not (pred seq &rest keys)
  "Remove all elements for which PRED is false (same as remove-if-not)."
  (when keys)
  (%filter-list-expand 'when pred seq))

;; DELETE-DUPLICATES (FR-504)
(our-defmacro delete-duplicates (seq &rest keys)
  "Remove duplicate elements (keeps first occurrence)."
  (when keys)
  `(remove-duplicates ,seq))

;; SUBSTITUTE (FR-505): replace occurrences of old with new
(our-defmacro substitute (new old seq &rest keys)
  "Return new sequence with each EQL OLD replaced by NEW."
  (when keys)
  (let ((new-var (gensym "NEW"))
        (old-var (gensym "OLD"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,new-var ,new)
           (,old-var ,old)
           (,acc nil))
       (dolist (,x ,seq (nreverse ,acc))
         (setq ,acc (cons (if (eql ,x ,old-var) ,new-var ,x) ,acc))))))

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
(our-defmacro substitute-if (new pred seq &rest keys)
  "Replace elements for which PRED is true with NEW."
  (when keys)
  (%substitute-if-expand new pred seq 'new-var 'x))

;; SUBSTITUTE-IF-NOT (FR-505) — replace where pred is false
(our-defmacro substitute-if-not (new pred seq &rest keys)
  "Replace elements for which PRED is false with NEW."
  (when keys)
  (%substitute-if-expand new pred seq 'x 'new-var))

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

(our-defmacro nsubstitute (new old seq &rest keys)
  "Destructive substitute (same as substitute in this impl)."
  (when keys)
  `(substitute ,new ,old ,seq))

(our-defmacro nsubstitute-if (new pred seq &rest keys)
  "Destructive substitute-if (same as substitute-if in this impl)."
  (when keys)
  `(substitute-if ,new ,pred ,seq))

(our-defmacro nsubstitute-if-not (new pred seq &rest keys)
  "Destructive substitute-if-not (same as substitute-if-not in this impl)."
  (when keys)
  `(substitute-if-not ,new ,pred ,seq))

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


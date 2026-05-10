;;;; macros-sequence-fold.lisp — Sequence fold/transform/tail macros (FR-500)
;;;
;;; Contains:
;;;   - reduce (fold with :initial-value, :from-end, :key)
;;;   - nsubstitute, nsubstitute-if, nsubstitute-if-not (destructive delegates)
;;;   - map-into (fill sequence with mapped results)
;;;   - merge (merge two sorted sequences)
;;;   - last, butlast, nbutlast (tail operations)
;;;   - search (subsequence matching with :key/:test/:start/:end)
;;;
;;; Element query/modification macros (copy-seq, fill, replace, mismatch,
;;; delete, substitute) are in macros-sequence.lisp (loads before).
;;;
;;; Load order: after macros-sequence.lisp.
(in-package :cl-cc/expand)

;;; REDUCE (FR-500 adjacent): fold a sequence using a binary function
(register-macro 'reduce
  (lambda (form env)
    (declare (ignore env))
    (let* ((fn (second form))
           (seq (third form))
           (keys (cdddr form))
           (fn-var (gensym "FN"))
           (cur-var (gensym "CUR"))
           (acc-var (gensym "ACC"))
           (has-iv (not (null (member :initial-value keys))))
           (iv-expr (getf keys :initial-value nil))
           (fe-expr (getf keys :from-end nil))
           (key-fn (getf keys :key nil))
           (seq-init (if fe-expr (list 'reverse seq) seq))
           (elem-form (if key-fn
                          (list 'funcall key-fn (list 'car cur-var))
                          (list 'car cur-var))))
      (if has-iv
          (list 'let (list (list fn-var fn)
                           (list acc-var iv-expr)
                           (list cur-var seq-init))
                (list 'loop 'while cur-var
                      'do
                      (list 'setq acc-var (list 'funcall fn-var acc-var elem-form))
                      (list 'setq cur-var (list 'cdr cur-var)))
                acc-var)
          (list 'let (list (list fn-var fn)
                           (list cur-var seq-init))
                (list 'let (list (list acc-var (if key-fn
                                                   (list 'funcall key-fn (list 'car cur-var))
                                                   (list 'car cur-var))))
                      (list 'setq cur-var (list 'cdr cur-var))
                      (list 'loop 'while cur-var
                            'do
                            (list 'setq acc-var (list 'funcall fn-var acc-var elem-form))
                            (list 'setq cur-var (list 'cdr cur-var)))
                      acc-var))))))

;;; NSUBSTITUTE / NSUBSTITUTE-IF / NSUBSTITUTE-IF-NOT (FR-505): destructive delegates

(register-macro 'nsubstitute
  (lambda (form env)
    (declare (ignore env))
    (let* ((new (second form))
           (old (third form))
           (seq (fourth form))
           (rest (cddddr form))
           (test (getf rest :test))
           (key (getf rest :key))
           (test-not (getf rest :test-not)))
      (append (list 'substitute new old seq)
              (when test (list :test test))
              (when key (list :key key))
              (when test-not (list :test-not test-not))))))

;;; nsubstitute-if / nsubstitute-if-not differ only in the target delegate name.
(register-macro 'nsubstitute-if
  (lambda (form env)
    (declare (ignore env))
    (let ((new (second form))
          (pred (third form))
          (seq (fourth form))
          (key (getf (cddddr form) :key)))
      (append (list 'substitute-if new pred seq)
              (when key (list :key key))))))

(register-macro 'nsubstitute-if-not
  (lambda (form env)
    (declare (ignore env))
    (let ((new (second form))
          (pred (third form))
          (seq (fourth form))
          (key (getf (cddddr form) :key)))
      (append (list 'substitute-if-not new pred seq)
              (when key (list :key key))))))

;;; MAP-INTO (FR-503): fill sequence with mapped results

(register-macro 'map-into
  (lambda (form env)
    (declare (ignore env))
    (let* ((dest (second form))
           (fn (third form))
           (seqs (cdddr form)))
      (if (= (length seqs) 1)
          (let ((d (gensym "DEST"))
                (src (gensym "SRC"))
                (dp (gensym "DP"))
                (sp (gensym "SP"))
                (fn-var (gensym "FN"))
                (loop-fn (gensym "MAP-INTO-LOOP")))
            (list 'let (list (list d dest)
                             (list src (first seqs))
                             (list fn-var fn))
                  (list 'labels
                        (list (list loop-fn (list dp sp)
                                    (list 'when (list 'and dp sp)
                                           (list 'rplaca dp (list 'funcall fn-var (list 'car sp)))
                                          (list loop-fn (list 'cdr dp) (list 'cdr sp)))))
                        (list loop-fn d src))
                  d))
          (list 'progn dest)))))

;;; MERGE (FR-504): merge two sorted sequences using predicate

(register-macro 'merge
  (lambda (form env)
    (declare (ignore env))
    (let ((result-type (second form))
          (seq1 (third form))
          (seq2 (fourth form))
          (pred (fifth form))
          (keys (cddddr form))
          (l1 (gensym "L1"))
          (l2 (gensym "L2"))
          (fn-var (gensym "PRED"))
          (loop-fn (gensym "MERGE-LOOP")))
      (declare (ignore result-type keys))
      (list 'let (list (list l1 seq1)
                       (list l2 seq2)
                       (list fn-var pred))
            (list 'labels
                  (list (list loop-fn '(a b)
                              (list 'cond
                                    '((null a) b)
                                    '((null b) a)
                                    (list (list 'funcall fn-var '(car a) '(car b))
                                          (list 'cons '(car a) (list loop-fn '(cdr a) 'b)))
                                    (list 't
                                          (list 'cons '(car b) (list loop-fn 'a '(cdr b)))))))
                  (list loop-fn l1 l2))))))

;;; LAST/BUTLAST/SEARCH (FR-500 adjacent): sequence tail and subsequence helpers

(defun %build-list-tail-setup (list-expr n-expr)
  "Return (values let*-bindings lst-sym nv-sym len-sym) for last/butlast expansion."
  (let ((lst (gensym "LST"))
        (nv  (gensym "N"))
        (len (gensym "LEN")))
    (values (list (list lst list-expr)
                  (list nv (or n-expr 1))
                  (list len (list 'length lst)))
            lst nv len)))

(register-macro 'last
  (lambda (form env)
    (declare (ignore env))
    (multiple-value-bind (bindings lst nv len)
        (%build-list-tail-setup (second form) (third form))
      (list 'let* bindings
            (list 'nthcdr (list 'max 0 (list '- len nv)) lst)))))

(register-macro 'butlast
  (lambda (form env)
    (declare (ignore env))
    (multiple-value-bind (bindings lst nv len)
        (%build-list-tail-setup (second form) (third form))
      (list 'let* bindings
            (list 'when (list '> len nv)
                  (list 'subseq lst 0 (list '- len nv)))))))

(register-macro 'nbutlast
  (lambda (form env)
    (declare (ignore env))
  "Destructively trim the last N conses from LIST (delegates to butlast in cl-cc)."
    (list 'butlast (second form) (or (third form) 1))))

(register-macro 'search
  (lambda (form env)
    (declare (ignore env))
    (let ((pattern (second form))
          (sequence (third form))
          (test (or (getf (cdddr form) :test) '#'eql))
          (start1 (or (getf (cdddr form) :start1) 0))
          (end1 (getf (cdddr form) :end1))
          (start2 (or (getf (cdddr form) :start2) 0))
          (end2 (getf (cdddr form) :end2))
          (key (getf (cdddr form) :key))
          (pat   (gensym "PAT"))
          (seq   (gensym "SEQ"))
          (fn    (gensym "FN"))
          (kfn   (gensym "KFN"))
          (s1    (gensym "S1"))
          (e1    (gensym "E1"))
          (s2    (gensym "S2"))
          (e2    (gensym "E2"))
          (plen  (gensym "PLEN"))
          (i     (gensym "I"))
          (j     (gensym "J"))
          (match (gensym "MATCH")))
      (list 'let* (list (list pat pattern)
                        (list seq sequence)
                        (list fn test)
                        (list kfn (if key key '(function identity)))
                        (list s1 start1)
                        (list e1 (or end1 (list 'length pat)))
                        (list s2 start2)
                        (list e2 (or end2 (list 'length seq)))
                        (list plen (list '- e1 s1)))
            (list 'when (list '<= plen (list '- e2 s2))
                  (list 'block 'found
                        (list 'do (list (list i s2 (list '+ i 1)))
                              (list (list '> (list '+ i plen) e2) nil)
                              (list 'let (list (list match t))
                                    (list 'do (list (list j 0 (list '+ j 1)))
                                          (list (list 'or (list 'not match) (list '= j plen)))
                                          (list 'unless
                                                (list 'funcall fn
                                                      (list 'funcall kfn (list 'elt pat (list '+ s1 j)))
                                                      (list 'funcall kfn (list 'elt seq (list '+ i j))))
                                                (list 'setq match nil)))
                                    (list 'when match (list 'return-from 'found i))))))))))

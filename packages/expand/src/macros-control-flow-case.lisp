;;;; macros-control-flow-case.lisp — CASE and TYPECASE macro expansion
;;;
;;; Extracted from macros-control-flow.lisp.
;;; Depends on macros-control-flow.lisp (our-defmacro, gensym-based environment).
;;; Load order: immediately after macros-control-flow.lisp.

(in-package :cl-cc/expand)

;;; CASE macro helpers

(defun %case-clause->pairs (clause)
  (let ((keys (car clause))
        (body (cdr clause)))
    (cond
      ((or (eq keys 'otherwise) (eq keys 't))
       :default)
      ((listp keys)
       (mapcar (lambda (key) (cons key body)) keys))
      (t
       (list (cons keys body))))))

(defun %case-expand-integer-tree (key-var pairs default-form)
  "Build a binary-search tree dispatch for sorted integer CASE PAIRS."
  (if (null pairs)
      default-form
      (if (null (cdr pairs))
          (destructuring-bind (key . body) (car pairs)
            `(if (eql ,key-var ',key)
                 (progn ,@body)
                 ,default-form))
          (let* ((mid   (floor (length pairs) 2))
                 (pivot (nth mid pairs))
                 (left  (subseq pairs 0 mid))
                 (right (subseq pairs (1+ mid))))
            `(if (eql ,key-var ',(car pivot))
                 (progn ,@(cdr pivot))
                 (if (< ,key-var ',(car pivot))
                     ,(%case-expand-integer-tree key-var left default-form)
                     ,(%case-expand-integer-tree key-var right default-form)))))))

(defun %case-expand-integer-table (key-var pairs default-form)
  "Expand dense integer CASE clauses into a table dispatch.

   The table stores one thunk per integer slot in the covered range, so the
   runtime path is a bounds check plus indexed FUNCALL. Missing slots fall back
   to DEFAULT-FORM."
  (let* ((min-key (caar pairs))
         (max-key (car (car (last pairs))))
         (span    (1+ (- max-key min-key))))
    `(let ((dispatch (vector
                      ,@(loop for key from min-key to max-key
                              for pair = (assoc key pairs)
                              collect (if pair
                                          `(lambda () (progn ,@(cdr pair)))
                                          `(lambda () ,default-form))))))
       (if (integerp ,key-var)
           (let ((idx (- ,key-var ',min-key)))
             (if (and (<= 0 idx)
                      (< idx ,span))
                 (funcall (svref dispatch idx))
                 ,default-form))
           ,default-form))))

(defun %case-build-eql-chain (cases key-var default-form)
  "Recursively build a nested if-eql chain from raw CASES for non-integer CASE dispatch."
  (if (null cases)
      default-form
      (destructuring-bind ((keys . body) &rest rest) cases
        (cond
          ((or (eq keys 'otherwise) (eq keys 't))
           `(progn ,@body))
          ((listp keys)
           `(if (or ,@(mapcar (lambda (k) `(eql ,key-var ',k)) keys))
                (progn ,@body)
                ,(%case-build-eql-chain rest key-var default-form)))
          (t
           `(if (eql ,key-var ',keys)
                (progn ,@body)
                ,(%case-build-eql-chain rest key-var default-form)))))))

(defun %case-collect-integer-pairs (cases)
  "Collect (key . body) pairs and default-form from raw CASES.
Returns (values default-form pairs integer-only-p)."
  (let* ((classified   (loop for clause in cases
                             collect (list (%case-clause->pairs clause) clause)))
         (default-ent  (find :default classified :key #'first))
         (default-form (when default-ent `(progn ,@(cdr (second default-ent)))))
         (pairs        (loop for (norm) in classified
                             unless (eq norm :default) append norm))
         (integer-only-p (every (lambda (p) (integerp (car p))) pairs)))
    (when integer-only-p
      (setf pairs (sort pairs #'< :key #'car)))
    (values default-form pairs integer-only-p)))

(our-defmacro case (keyform &body cases)
  "Match KEYFORM against CASES.
   Each case is (key body...) or (otherwise body...) or (t body...).
   Keys are compared with EQL (not evaluated)."
  (let ((key-var (gensym "KEY")))
    `(let ((,key-var ,keyform))
       ,(multiple-value-bind (default-form pairs integer-only-p)
            (%case-collect-integer-pairs cases)
          (if (and integer-only-p (>= (length pairs) 4))
              (let* ((min-key (caar pairs))
                     (max-key (car (car (last pairs))))
                     (span    (1+ (- max-key min-key))))
                (if (<= span (* 2 (length pairs)))
                    (%case-expand-integer-table key-var pairs default-form)
                    `(if (integerp ,key-var)
                         ,(%case-expand-integer-tree key-var pairs default-form)
                         ,default-form)))
              (%case-build-eql-chain cases key-var default-form))))))

;;; TYPECASE macro helpers

(defun %prune-typecase-clauses (cases)
  "Remove TYPECASE clauses that are already covered by an earlier clause.

This is a conservative decision-tree simplification: if a later clause is a
subtype of any earlier clause, it can never be reached and can be dropped."
  (let ((seen nil)
        (result nil))
    (dolist (clause cases (nreverse result))
      (let ((type (car clause)))
        (cond
          ((or (eq type 'otherwise) (eq type 't))
           (push clause result)
           (return (nreverse result)))
          ((not (some (lambda (prev)
                        (multiple-value-bind (subp surep)
                            (%typecase-subtypep type prev)
                          (and surep subp)))
                      seen))
           (push type seen)
           (push clause result)))))))

(defun %typecase-subtypep (type1 type2)
  "Call the type system subtype predicate if it is available."
  (multiple-value-bind (subp surep)
      (subtypep type1 type2)
    (values subp surep)))

(defun %typecase-build-typep-chain (cases key-var)
  "Recursively build a nested if-typep chain from CASES for TYPECASE dispatch."
  (if (null cases)
      nil
      (destructuring-bind ((type . body) &rest rest) cases
        (if (or (eq type 'otherwise) (eq type 't))
            `(progn ,@body)
             `(if (typep ,key-var ',type)
                  (progn ,@body)
                  ,(%typecase-build-typep-chain rest key-var))))))

(defun %typecase-types-disjoint-p (type1 type2)
  "Return T when TYPE1 and TYPE2 are known-disjoint.

We consider two types disjoint when both subtype checks are provably false.
Unknown relationships are treated as non-disjoint for safety."
  (multiple-value-bind (s12 sure12) (%typecase-subtypep type1 type2)
    (multiple-value-bind (s21 sure21) (%typecase-subtypep type2 type1)
      (and sure12 sure21 (not s12) (not s21)))))

(defun %typecase-all-disjoint-p (cases)
  "Return T when all non-default CASES are pairwise disjoint."
  (let* ((typed (loop for (type . body) in cases
                      unless (or (eq type 'otherwise) (eq type 't))
                        collect (cons type body)))
         (n (length typed)))
    (loop for i from 0 below n
          always (loop for j from (1+ i) below n
                       always (%typecase-types-disjoint-p
                               (car (nth i typed))
                               (car (nth j typed)))))))

(defun %typecase-build-left-guard (cases key-var)
  "Build an OR guard that checks whether KEY-VAR matches any CASE type."
  (if (null cases)
      nil
      `(or ,@(mapcar (lambda (cl)
                       `(typep ,key-var ',(car cl)))
                     cases))))

(defun %typecase-should-use-decision-tree-p (typed-clauses)
  "Return T when TYPECASE dispatch should use a balanced decision tree.

The tree is semantics-preserving even for overlapping type sets because each
internal guard routes to the earliest half first. We enable the tree for
larger arm counts where linear TYPEP chains become expensive."
  (>= (length typed-clauses) 3))

(defun %typecase-related-types-p (type1 type2)
  "Return T when TYPE1/TYPE2 have a known subtype relationship."
  (multiple-value-bind (s12 sure12) (%typecase-subtypep type1 type2)
    (multiple-value-bind (s21 sure21) (%typecase-subtypep type2 type1)
      (or (and sure12 s12)
          (and sure21 s21)))))

(defun %typecase-split-cross-score (cases split-index)
  "Compute overlap score across a candidate split boundary.

Lower is better: we prefer boundaries where left/right halves have fewer
subtype/supertype relations, because those tend to reduce redundant TYPEP
checks in upper tree levels."
  (let ((left (subseq cases 0 split-index))
        (right (subseq cases split-index))
        (score 0))
    (dolist (l left score)
      (dolist (r right)
        (when (%typecase-related-types-p (car l) (car r))
          (incf score))))))

(defun %typecase-choose-split-index (cases)
  "Choose a hierarchy-aware split index while preserving arm order."
  (let* ((n (length cases))
         (best-index 1)
         (best-score most-positive-fixnum)
         (best-balance most-positive-fixnum))
    (loop for idx from 1 below n do
      (let* ((score (%typecase-split-cross-score cases idx))
             (left-size idx)
             (right-size (- n idx))
             (balance (abs (- left-size right-size))))
        (when (or (< score best-score)
                  (and (= score best-score) (< balance best-balance)))
          (setf best-score score
                best-balance balance
                best-index idx))))
    best-index))

(defun %typecase-build-decision-tree (cases key-var default-form)
  "Build a balanced decision tree for ordered TYPECASE clauses.

The generated tree preserves left-to-right TYPECASE semantics: each internal
node guards the left half and only falls through to the right half when no
left-half type matches."
  (if (null cases)
      default-form
      (if (null (cdr cases))
          (destructuring-bind (type . body) (car cases)
            `(if (typep ,key-var ',type)
                 (progn ,@body)
                 ,default-form))
          (let* ((mid (%typecase-choose-split-index cases))
                 (left (subseq cases 0 mid))
                 (right (subseq cases mid))
                 (left-guard (%typecase-build-left-guard left key-var)))
             `(if ,left-guard
                  ,(%typecase-build-decision-tree left key-var default-form)
                  ,(%typecase-build-decision-tree right key-var default-form))))))

(our-defmacro typecase (keyform &body cases)
  "Match KEYFORM against TYPE-CASES.
   Each case is (type body...) or (otherwise body...) or (t body...).
   Types are checked with TYPEP."
  (let* ((key-var (gensym "KEY"))
         (pruned-cases (%prune-typecase-clauses cases))
         (default-clause (find-if (lambda (cl)
                                    (let ((type (car cl)))
                                      (or (eq type 'otherwise) (eq type 't))))
                                  pruned-cases))
         (typed-clauses (remove-if (lambda (cl)
                                     (let ((type (car cl)))
                                       (or (eq type 'otherwise) (eq type 't))))
                                   pruned-cases))
         (default-form (if default-clause
                           `(progn ,@(cdr default-clause))
                           nil))
         (dispatch-form (if (%typecase-should-use-decision-tree-p typed-clauses)
                             (%typecase-build-decision-tree typed-clauses key-var default-form)
                             (%typecase-build-typep-chain pruned-cases key-var))))
    `(let ((,key-var ,keyform))
       ,dispatch-form)))

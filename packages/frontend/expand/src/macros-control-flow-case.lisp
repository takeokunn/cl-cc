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
  (labels ((expand (pairs)
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
                                 ,(expand left)
                                 ,(expand right))))))))
    (expand pairs)))

(defun %case-expand-integer-table (key-var pairs default-form)
  "Expand dense integer CASE clauses into a table dispatch.

   The table stores one thunk per integer slot in the covered range, so the
   runtime path is a bounds check plus indexed FUNCALL. Missing slots fall back
   to DEFAULT-FORM."
  (let* ((min-key (caar pairs))
         (max-key (car (car (last pairs))))
         (span    (1+ (- max-key min-key))))
    `(let* ((idx (- ,key-var ',min-key))
            (dispatch (vector
                       ,@(loop for key from min-key to max-key
                               for pair = (assoc key pairs)
                               collect (if pair
                                           `(lambda () (progn ,@(cdr pair)))
                                           `(lambda () ,default-form))))))
       (if (and (integerp ,key-var)
                (<= 0 idx)
                (< idx ,span))
           (funcall (svref dispatch idx))
           ,default-form))))

(our-defmacro case (keyform &body cases)
  "Match KEYFORM against CASES.
   Each case is (key body...) or (otherwise body...) or (t body...).
   Keys are compared with EQL (not evaluated)."
  (let ((key-var (gensym "KEY")))
    `(let ((,key-var ,keyform))
       ,(let ((default-form nil)
              (pairs nil)
              (integer-only-p t))
          (dolist (clause cases)
            (let ((normalized (%case-clause->pairs clause)))
              (cond
                ((eq normalized :default)
                 (setf default-form `(progn ,@(cdr clause))))
                (t
                 (dolist (pair normalized)
                   (unless (integerp (car pair))
                     (setf integer-only-p nil))
                   (push pair pairs))))))
           (when integer-only-p
             (setf pairs (sort pairs #'< :key #'car)))
           (cond
            ((and integer-only-p (>= (length pairs) 4))
              (let* ((min-key (caar pairs))
                     (max-key (car (car (last pairs))))
                     (span    (1+ (- max-key min-key))))
                (if (<= span (* 2 (length pairs)))
                    (%case-expand-integer-table key-var pairs default-form)
                    `(if (integerp ,key-var)
                         ,(%case-expand-integer-tree key-var pairs default-form)
                         ,default-form))))
            (t
              (labels ((build-case (cases)
                         (if (null cases)
                             default-form
                            (let ((case (car cases))
                                  (rest (cdr cases)))
                              (let ((keys (car case))
                                    (body (cdr case)))
                                (cond
                                  ((or (eq keys 'otherwise) (eq keys 't))
                                   `(progn ,@body))
                                  ((listp keys)
                                   `(if (or ,@(mapcar #'(lambda (k) `(eql ,key-var ',k)) keys))
                                        (progn ,@body)
                                        ,(build-case rest)))
                                  (t
                                   `(if (eql ,key-var ',keys)
                                        (progn ,@body)
                                        ,(build-case rest)))))))))
               (build-case cases))))))))

;;; TYPECASE macro helpers

(defun %prune-typecase-clauses (cases)
  "Remove TYPECASE clauses that are already covered by an earlier clause.

This is a conservative decision-tree simplification: if a later clause is a
subtype of any earlier clause, it can never be reached and can be dropped."
  (let ((seen nil)
        (result nil))
    (dolist (case cases (nreverse result))
      (let ((type (car case)))
        (cond
          ((or (eq type 'otherwise) (eq type 't))
           (push case result)
           (return (nreverse result)))
          ((some (lambda (prev)
                   (multiple-value-bind (subp surep)
                       (%typecase-subtypep type prev)
                     (and surep subp)))
                 seen)
           nil)
          (t
           (push type seen)
           (push case result)))))))

(defun %typecase-subtypep (type1 type2)
  "Call the type system subtype predicate if it is available."
  (multiple-value-bind (subp surep)
      (subtypep type1 type2)
    (values subp surep)))

(our-defmacro typecase (keyform &body cases)
  "Match KEYFORM against TYPE-CASES.
   Each case is (type body...) or (otherwise body...) or (t body...).
   Types are checked with TYPEP."
  (let* ((key-var (gensym "KEY"))
         (pruned-cases (%prune-typecase-clauses cases)))
    `(let ((,key-var ,keyform))
       ,(labels ((build-typecase (cases)
                   (if (null cases)
                       nil
                       (let ((case (car cases))
                             (rest (cdr cases)))
                         (let ((type (car case))
                               (body (cdr case)))
                           (cond
                             ((or (eq type 'otherwise) (eq type 't))
                              `(progn ,@body))
                             (t
                              `(if (typep ,key-var ',type)
                                   (progn ,@body)
                                   ,(build-typecase rest)))))))))
            (build-typecase pruned-cases)))))

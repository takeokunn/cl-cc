;;;; tests/pbt/framework-ast-generators.lisp - AST Generators and Shrinking

(in-package :cl-cc/pbt)

;;; AST Generators

(defvar *ast-terminal-generators* nil
  "List of generators for terminal AST nodes.")

(defvar *ast-recursive-generators* nil
  "List of generators for recursive AST nodes.")

(defun init-ast-generators ()
  "Initialize AST generator lists based on current SIZE."
  (setf *ast-terminal-generators*
        (list
         (make-generator
          (lambda () (make-ast-int :value (generate (gen-integer :min -100 :max 100)))))
         (make-generator
          (lambda () (make-ast-var :name (generate (gen-symbol :prefix "VAR")))))))

  (setf *ast-recursive-generators*
        (list
         (make-generator
          (lambda ()
            (make-ast-binop :op (generate (gen-one-of '(+ - *)))
                            :lhs (generate (gen-ast-node))
                            :rhs (generate (gen-ast-node)))))
         (make-generator
          (lambda ()
            (make-ast-if :cond (generate (gen-ast-node))
                         :then (generate (gen-ast-node))
                         :else (generate (gen-ast-node)))))
         (make-generator
          (lambda ()
            (make-ast-progn :forms (generate (gen-list-of (gen-ast-node)
                                                           :min-length 1 :max-length 3)))))
         (make-generator
          (lambda ()
            (let ((var-name (generate (gen-symbol :prefix "VAR"))))
              (make-ast-let :bindings (list (cons var-name (generate (gen-ast-node))))
                            :body (list (make-ast-var :name var-name))))))
         (make-generator
          (lambda ()
            (make-ast-lambda :params (generate (gen-list-of (gen-symbol :prefix "ARG")
                                                             :min-length 0 :max-length 3))
                             :body (list (generate (gen-ast-node))))))
         (make-generator
          (lambda ()
            (make-ast-call :func (generate (gen-symbol :prefix "FN"))
                           :args (generate (gen-list-of (gen-ast-node)
                                                         :min-length 0 :max-length 3))))))))

(defun gen-ast-node (&key (max-depth 3))
  "Generate random AST nodes using SIZE to control complexity."
  (make-generator
   (lambda ()
     (unless *ast-terminal-generators*
       (init-ast-generators))
     (let ((effective-depth (max 0 (min max-depth (floor (- 100 *size*) 20)))))
       (if (or (zerop effective-depth)
               (and (> effective-depth 0)
                    (< (random 100 (%pbt-rng)) (- 100 (/ *size* 2)))))
           (generate (gen-one-of *ast-terminal-generators*))
           (let ((*size* (min 100 (+ *size* 20))))
             (generate (gen-one-of *ast-recursive-generators*))))))))

(defun gen-expr (&key (max-depth 3))
  "Generate random Lisp expressions (S-expressions) for CL-CC frontend testing."
  (gen-fmap #'ast-to-sexp (gen-ast-node :max-depth max-depth)))

;;; Shrinking

(defun shrink (value)
  "Generic shrink function that dispatches based on VALUE type."
  (typecase value
    (integer (shrink-integer value))
    (list (shrink-list value))
    (t nil)))

(defun shrink-integer (n)
  "Return a list of shrunk integers toward 0."
  (cond ((zerop n) nil)
        ((> (abs n) 1) (list 0 (floor n 2)))
        (t (list 0))))

(defun shrink-list (lst)
  "Return a list of shrunk versions of LST."
  (when (null lst)
    (return-from shrink-list nil))
  (let ((shrinks '()))
    (push nil shrinks)
    (when (cdr lst)
      (push (cdr lst) shrinks))
    (when (> (length lst) 2)
      (push (subseq lst 0 (floor (length lst) 2)) shrinks))
    shrinks))

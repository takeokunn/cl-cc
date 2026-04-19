;;;; tests/pbt/generators.lisp - Type Expression Generators
;;;
;;; Generators for type expressions used in type system property-based testing.
;;; Mach-O binary structure generators are in generators-macho.lisp.

(in-package :cl-cc/pbt)

;;; Configuration Variables

(defvar *max-type-depth* 3
  "Maximum depth for recursive type expressions.")

;;; Type Expression Generators

(defun gen-primitive-type ()
  "Generate a primitive type specifier."
  (gen-one-of '(fixnum single-float double-float string boolean symbol
                integer number character list cons null
                t)))

(defun gen-type-variable ()
  "Generate a type variable for polymorphism testing (?a, ?b, etc.)."
  (gen-fmap (lambda (c) (intern (format nil "?~A" c) :keyword))
            (gen-one-of '(a b c d e f x y z))))

(defun gen-simple-compound-type ()
  "Generate simple compound types like (or T1 T2), (and T1 T2)."
  (gen-bind
   (gen-one-of '(or and))
   (lambda (op)
     (gen-fmap
      (lambda (types)
        (cons op types))
      (gen-list-of (gen-primitive-type)
                   :min-length 2 :max-length 4)))))

(defun gen-values-type ()
  "Generate (values T1 T2 ...) type for multiple values."
  (gen-fmap
   (lambda (types)
     (cons 'values types))
   (gen-list-of (gen-primitive-type)
                :min-length 0 :max-length 5)))

(defun gen-fn-type-args ()
  "Generate argument types for function type."
  (gen-list-of (gen-type-expr :depth 0)
               :min-length 0 :max-length 4))

(defun gen-fn-type ()
  "Generate function type (arg-types -> return-type)."
  (gen-bind
   (gen-fn-type-args)
   (lambda (args)
     (gen-fmap
      (lambda (ret)
        (list 'function args ret))
      (gen-type-expr :depth 0)))))

(defun gen-array-type ()
  "Generate array type specifiers."
  (gen-bind
   (gen-one-of '(simple-array array vector simple-vector
                 bit-vector simple-bit-vector string simple-string))
   (lambda (base)
     (gen-fmap
      (lambda (dims)
        (if dims
            (list base dims)
            base))
      (gen-one-of '(nil (1) (*) (* *) ((*) (*))))))))

(defun gen-cons-type ()
  "Generate (cons car-type cdr-type) type specifiers."
  (gen-fmap
   (lambda (types)
     (cons 'cons types))
   (gen-tuple (gen-type-expr :depth 0)
              (gen-type-expr :depth 0))))

(defun gen-type-expr (&key (depth *max-type-depth*))
  "Generate random type expressions for testing.

   Generates:
   - Primitives: fixnum, single-float, string, boolean, symbol
   - Compound: (function (T1 T2) R), (or T1 T2), (values T1 T2)
   - Variables: ?a, ?b (for polymorphism testing)"
  (let ((effective-depth (max 0 (min depth *max-type-depth*))))
    (if (or (zerop effective-depth)
            (< (random 100 (%pbt-rng))
               (- 100 (* effective-depth 20))))
        ;; Generate terminal type
        (gen-bind
         (gen-one-of (list 0 1))
         (lambda (choice)
           (case choice
             (0 (gen-primitive-type))
             (1 (gen-type-variable)))))
        ;; Generate compound type
        (gen-bind
         (gen-one-of (list 0 1 2 3 4 5))
         (lambda (choice)
           (case choice
             (0 (gen-simple-compound-type))
             (1 (gen-values-type))
             (2 (gen-fn-type))
             (3 (gen-array-type))
             (4 (gen-cons-type))
             (5 (gen-fn-type))))))))

(defun gen-type-specifier ()
  "Generate a full type specifier (alias for gen-type-expr)."
  (gen-type-expr))

;;; Mach-O structure generators are in generators-macho.lisp.

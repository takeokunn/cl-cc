;;;; tests/pbt/framework.lisp - Property-Based Testing Framework Implementation
;;;
;;; This module provides a complete property-based testing framework for CL-CC,
;;; including generators, combinators, and integration with the cl-cc/test framework.

(in-package :cl-cc/pbt)

;;; Configuration Variables

(defvar *test-count*
  (or (let ((raw (uiop:getenv "CLCC_PBT_COUNT")))
        (and raw (ignore-errors
                   (let ((n (parse-integer raw :junk-allowed t)))
                     (and n (plusp n) n)))))
      10)
  "Default number of test cases to run for each property.
Honors CLCC_PBT_COUNT environment variable so CI / `nix run .#test` can
scale down (e.g. CLCC_PBT_COUNT=3) without touching test sources.")

(defvar *max-list-length* 20
  "Maximum length for generated lists.")

(defvar *max-string-length* 50
  "Maximum length for generated strings.")

(defvar *size* 0
  "Current size parameter for generators (0-100).")

(defvar *pbt-rng-override* nil
  "Optional override random state for reproducible PBT runs.
When NIL (the default) generators fall back to *RANDOM-STATE*, which SBCL
scopes per-thread. Setting this to a fixed state forces all generators to
draw from it — useful for reproduction but unsafe under parallel execution.")

(defun %pbt-rng ()
  "Return the active PBT random state (override, or the thread-local default).
Kept as a function so callers never capture a stale binding of the override."
  (or *pbt-rng-override* *random-state*))

;;; Generator Protocol

(defclass generator ()
  ((generate-fn :initarg :generate-fn :reader generator-generate-fn
                :documentation "Function that generates a random value.")
   (shrink-fn :initarg :shrink-fn :initform (lambda (x) (declare (ignore x)) nil)
              :reader generator-shrink-fn
              :documentation "Function that returns a list of shrunk values."))
  (:documentation "Base class for all generators."))

(defun generate (generator)
  "Generate a random value using GENERATOR.
   Accepts either a generator object or a plain function (as returned by gen-fn)."
  (if (functionp generator)
      (funcall generator)
      (funcall (generator-generate-fn generator))))

(defun gen-fn (generator)
  "Convert a generator object to a plain function.
   Returns a lambda that calls generate on the generator."
  (lambda () (generate generator)))

(defun shrink-value (generator value)
  "Return a list of shrunk values for VALUE using GENERATOR."
  (funcall (generator-shrink-fn generator) value))

;;; Generator Construction

(defun make-generator (generate-fn &key (shrink-fn (lambda (x) (declare (ignore x)) nil)))
  "Create a new generator with the given generate and shrink functions."
  (make-instance 'generator
                 :generate-fn generate-fn
                 :shrink-fn shrink-fn))

;;; Built-in Generators

(defun gen-integer (&key (min most-negative-fixnum) (max most-positive-fixnum))
  "Generate random integers between MIN and MAX (inclusive)."
  (make-generator
   (lambda ()
     (+ min (random (1+ (- max min)) (%pbt-rng))))
   :shrink-fn (lambda (n)
                (let ((shrinks '()))
                  (when (and (minusp n) (< min (floor n 2)))
                    (push (floor n 2) shrinks))
                  (when (and (plusp n) (> max (floor n 2)))
                    (push (floor n 2) shrinks))
                  (when (and (minusp n) (<= min 0))
                    (push 0 shrinks))
                  (when (and (plusp n) (>= max 0))
                    (push 0 shrinks))
                  shrinks))))

(defun gen-float (&key (min -1.0d6) (max 1.0d6))
  "Generate random floating-point numbers between MIN and MAX."
  (make-generator
   (lambda ()
     (+ min (random (- max min) (%pbt-rng))))))

(defun gen-boolean ()
  "Generate random boolean values (T or NIL)."
  (make-generator
   (lambda ()
     (zerop (random 2 (%pbt-rng))))))

(defun gen-character (&key (alphanumeric-only t))
  "Generate random characters."
  (make-generator
   (lambda ()
     (if alphanumeric-only
         (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
           (char chars (random (length chars) (%pbt-rng))))
         (code-char (random 256 (%pbt-rng)))))))

(defun gen-symbol (&key (package nil) (prefix "SYM"))
  "Generate random symbols.
   PACKAGE can be NIL (uninterned), :keyword, or a package object.
   PREFIX is prepended to a random number."
  (make-generator
   (lambda ()
     (let ((name (format nil "~A-~D" prefix (random 100000 (%pbt-rng)))))
       (case package
         (:keyword (intern name :keyword))
         ((nil) (gensym prefix))
         (t (intern name (if (packagep package) package (find-package package)))))))))

(defun gen-string (&key (length nil) (min-length 0) (max-length *max-string-length*)
                        (alphanumeric-only t))
  "Generate random strings."
  (make-generator
   (lambda ()
     (let* ((range (- max-length min-length))
            (len (or length (+ min-length (if (zerop range) 0 (random range (%pbt-rng))))))
            (chars (loop repeat len
                         collect (generate (gen-character :alphanumeric-only alphanumeric-only)))))
       (coerce chars 'string)))))

(defun gen-list (&key (element-gen (gen-integer))
                      (min-length 0) (max-length *max-list-length*))
  "Generate random lists with elements from ELEMENT-GEN."
  (make-generator
   (lambda ()
     (let ((range (- max-length min-length)))
       (loop repeat (+ min-length (if (zerop range) 0 (random range (%pbt-rng))))
             collect (generate element-gen))))
   :shrink-fn (lambda (lst)
                (let ((shrinks '()))
                  ;; Try shorter lists
                  (when (> (length lst) min-length)
                    (push (subseq lst 0 (1- (length lst))) shrinks))
                  ;; Try removing elements
                  (loop for i from 0 below (length lst)
                        for shorter = (append (subseq lst 0 i) (subseq lst (1+ i)))
                        when (>= (length shorter) min-length)
                          do (push shorter shrinks))
                  shrinks))))

(defun gen-cons (&key (car-gen (gen-integer)) (cdr-gen (gen-integer)))
  "Generate random cons cells."
  (make-generator
   (lambda ()
     (cons (generate car-gen) (generate cdr-gen)))))

(defun gen-vector (&key (element-gen (gen-integer))
                        (min-length 0) (max-length *max-list-length*))
  "Generate random vectors with elements from ELEMENT-GEN."
  (make-generator
   (lambda ()
     (coerce (generate (gen-list :element-gen element-gen
                                  :min-length min-length
                                  :max-length max-length))
             'vector))))

;;; Combinators

(defun gen-one-of (choices)
  "Generate one of the elements from CHOICES."
  (make-generator
   (lambda ()
     (nth (random (length choices) (%pbt-rng)) choices))))

(defun gen-tuple (&rest generators)
  "Generate a tuple with one element from each generator."
  (make-generator
   (lambda ()
     (mapcar #'generate generators))
   :shrink-fn (lambda (tuple)
                (loop for i from 0 below (length tuple)
                      for gen in generators
                      for elem = (nth i tuple)
                      append (mapcar (lambda (shrunk)
                                       (let ((copy (copy-list tuple)))
                                         (setf (nth i copy) shrunk)
                                         copy))
                                     (shrink-value gen elem))))))

(defun gen-list-of (element-gen &key (min-length 0) (max-length *max-list-length*))
  "Generate lists of elements from ELEMENT-GEN."
  (gen-list :element-gen element-gen :min-length min-length :max-length max-length))

(defun gen-alist (&key (key-gen (gen-symbol :prefix "KEY"))
                       (value-gen (gen-integer))
                       (min-length 0) (max-length *max-list-length*))
  "Generate association lists."
  (gen-list-of (gen-tuple key-gen value-gen)
               :min-length min-length
               :max-length max-length))

(defun gen-map (key-gen value-gen)
  "Generate property lists (alternating keys and values)."
  (make-generator
   (lambda ()
     (let ((alist (generate (gen-alist :key-gen key-gen
                                        :value-gen value-gen))))
       (loop for (key . value) in alist
             append (list key value))))))

(defun gen-bind (generator fn)
  "Monadic bind: apply FN to the generated value to get a new generator."
  (make-generator
   (lambda ()
     (generate (funcall fn (generate generator))))))

(defun gen-fmap (fn generator)
  "Map FN over the generated value."
  (make-generator
   (lambda ()
     (funcall fn (generate generator)))))

(defun gen-such-that (generator predicate &key (max-tries 100))
  "Generate values from GENERATOR that satisfy PREDICATE."
  (make-generator
   (lambda ()
     (loop repeat max-tries
           for value = (generate generator)
           when (funcall predicate value)
             do (return-from nil value)
           finally (error "Could not generate value satisfying predicate after ~D tries"
                          max-tries)))))

(defun gen-resize (size generator)
  "Create a generator that runs with a specific SIZE."
  (make-generator
   (lambda ()
     (let ((*size* size))
       (generate generator)))))

(defun gen-scale (scale-fn generator)
  "Create a generator that scales the size parameter."
  (make-generator
   (lambda ()
     (let ((*size* (funcall scale-fn *size*)))
       (generate generator)))))

;;; AST generators (gen-ast-node, gen-expr, init-ast-generators) and shrink
;;; functions (shrink, shrink-integer, shrink-list) are in framework-ast-generators.lisp.


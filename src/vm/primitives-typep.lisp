(in-package :cl-cc)

;;; VM Type Checking Subsystem (vm-typep)
;;;
;;; Contains the data tables and execution logic for the vm-typep instruction:
;;;   *vm-primitive-type-predicates* — symbol → single-arg predicate alist
;;;   *vm-type-of-dispatch*          — ordered dispatch for vm-type-of
;;;   *vm-compound-type-handlers*    — hash table for (or/and/not/member ...) forms
;;;   vm-typep-check                 — main dispatch function
;;;   execute-instruction for vm-typep
;;;
;;; Depends on: primitives.lisp (vm-typep struct definition)

;;; Instruction Execution - General Type Predicate

;;; Data table: maps type symbols to single-argument predicate functions.
(defparameter *vm-primitive-type-predicates*
  `((integer          . ,#'integerp)
    (fixnum           . ,#'integerp)
    (float            . ,#'floatp)
    (single-float     . ,#'floatp)
    (double-float     . ,#'floatp)
    (short-float      . ,#'floatp)
    (long-float       . ,#'floatp)
    (real             . ,#'realp)
    (rational         . ,#'rationalp)
    (complex          . ,#'complexp)
    (string           . ,#'stringp)
    (symbol           . ,#'symbolp)
    (keyword          . ,#'keywordp)
    (cons             . ,#'consp)
    (null             . ,#'null)
    (list             . ,#'listp)
    (number           . ,#'numberp)
    (character        . ,#'characterp)
    (base-char        . ,#'characterp)
    (standard-char    . ,#'characterp)
    (atom             . ,#'atom)
    (function         . ,#'functionp)
    (vector           . ,#'vectorp)
    (package          . ,#'packagep)
    (stream           . ,#'streamp)
    (input-stream     . ,#'input-stream-p)
    (output-stream    . ,#'output-stream-p)
    (bit-vector       . ,(lambda (v) (typep v 'bit-vector)))
    (simple-vector    . ,(lambda (v) (typep v 'simple-vector)))
    (simple-array     . ,(lambda (v) (typep v 'simple-array)))
    (array            . ,#'arrayp)
    (file-stream      . ,(lambda (v) (typep v 'file-stream)))
    (string-stream    . ,(lambda (v) (typep v 'string-stream)))
    (broadcast-stream . ,(lambda (v) (typep v 'broadcast-stream)))
    (two-way-stream   . ,(lambda (v) (typep v 'two-way-stream)))
    (echo-stream      . ,(lambda (v) (typep v 'echo-stream)))
    (concatenated-stream . ,(lambda (v) (typep v 'concatenated-stream)))
    (synonym-stream   . ,(lambda (v) (typep v 'synonym-stream)))
    (hash-table       . ,(lambda (v) (typep v 'vm-hash-table-object)))
    (bit              . ,(lambda (v) (or (eql v 0) (eql v 1)))))
  "Alist mapping type-symbol to single-argument predicate function for vm-typep-check.")

;;; Ordered dispatch list for vm-type-of: first matching predicate wins.
(defparameter *vm-type-of-dispatch*
  `((,#'null                                . null)
    (,(lambda (v) (typep v 'fixnum))        . fixnum)
    (,(lambda (v) (typep v 'bignum))        . bignum)
    (,(lambda (v) (typep v 'ratio))         . ratio)
    (,(lambda (v) (typep v 'double-float))  . double-float)
    (,(lambda (v) (typep v 'long-float))    . long-float)
    (,(lambda (v) (or (typep v 'single-float)
                      (typep v 'short-float)
                      (floatp v)))          . single-float)
    (,(lambda (v) (typep v 'bit-vector))    . bit-vector)
    (,#'stringp                             . string)
    (,(lambda (v) (typep v 'simple-vector)) . simple-vector)
    (,(lambda (v) (typep v 'simple-array))  . simple-array)
    (,#'characterp                          . character)
    (,#'symbolp                             . symbol)
    (,#'pathnamep                           . pathname)
    (,(lambda (v) (typep v 'random-state))  . random-state)
    (,#'readtablep                          . readtable)
    (,#'packagep                            . package)
    (,#'consp                               . cons)
    (,#'complexp                            . complex)
    (,(lambda (v) (or (functionp v) (typep v 'vm-closure-object))) . function)
    (,#'vectorp                             . vector)
    (,#'arrayp                              . array))
  "Ordered dispatch list for vm-type-of: first matching predicate wins.")

(defun %vm-typep-normalize-sym (type-sym)
  "Normalize TYPE-SYM: unwrap HM type primitives and intern into CL package."
  (let* ((type-pkg (find-package "CL-CC/TYPE"))
         (primitive-sym (and type-pkg (find-symbol "TYPE-PRIMITIVE" type-pkg)))
         (name-sym      (and type-pkg (find-symbol "NAME" type-pkg)))
         (primitive-class (and primitive-sym (find-class primitive-sym nil))))
    (when (and primitive-class name-sym (typep type-sym primitive-class))
      (setf type-sym (slot-value type-sym name-sym))))
  (when (symbolp type-sym)
    (let ((cl-sym (find-symbol (symbol-name type-sym) :cl)))
      (when cl-sym (setf type-sym cl-sym))))
  type-sym)

(defun %vm-typep-call-predicate (value predicate)
  "Invoke PREDICATE on VALUE, supporting function/symbol/lambda forms."
  (cond
    ((functionp predicate)
     (ignore-errors (funcall predicate value)))
    ((and (symbolp predicate) (fboundp predicate))
     (ignore-errors (funcall (symbol-function predicate) value)))
    ((and (consp predicate) (eq (car predicate) 'lambda))
     (let ((fn (ignore-errors (eval predicate))))
       (and fn (ignore-errors (funcall fn value)))))
    (t nil)))

;;; Data table: maps compound type-specifier head symbols to handler functions.
;;; Each handler is (value type-sym) → boolean.
;;; Note: lambdas here call vm-typep-check by symbol — resolved at call time, not load time.
(defparameter *vm-compound-type-handlers*
  (let ((ht (make-hash-table :test #'eq)))
    (flet ((reg (sym fn) (setf (gethash sym ht) fn)))
      (reg 'refine    (lambda (v ts) (and (vm-typep-check v (second ts))
                                           (%vm-typep-call-predicate v (third ts)))))
      (reg 'satisfies (lambda (v ts) (ignore-errors (funcall (second ts) v))))
      (reg 'or        (lambda (v ts) (some  (lambda (t2) (vm-typep-check v t2)) (cdr ts))))
      (reg 'and       (lambda (v ts) (every (lambda (t2) (vm-typep-check v t2)) (cdr ts))))
      (reg 'not       (lambda (v ts) (not (vm-typep-check v (second ts)))))
      (reg 'member    (lambda (v ts) (member v (cdr ts) :test #'eql)))
      (reg 'eql       (lambda (v ts) (eql v (second ts))))
      (reg 'values    (lambda (v ts) (declare (ignore v ts)) t))
      (reg 'function  (lambda (v ts) (declare (ignore ts))
                         (or (typep v 'vm-closure-object) (functionp v)))))
    ht)
  "Hash table mapping compound type-specifier head symbols to handlers (value type-sym) → boolean.
Used by vm-typep-check for compound forms: (or ...), (and ...), (not ...), etc.")

(defun vm-typep-check (value type-sym)
  "Check if VALUE is of TYPE-SYM using table dispatch for primitive and compound types."
  (setf type-sym (%vm-typep-normalize-sym type-sym))
  (let ((pred (and (symbolp type-sym)
                   (cdr (assoc type-sym *vm-primitive-type-predicates*)))))
    (cond
      ;; Fast table lookup for all primitive types
      (pred
       (funcall pred value))
      ;; Compound type specifiers: dispatch via *vm-compound-type-handlers*
      ((consp type-sym)
       (let ((handler (gethash (car type-sym) *vm-compound-type-handlers*)))
         (if handler
             (funcall handler value type-sym)
             (ignore-errors (typep value type-sym)))))
      ;; Structural refinement type objects
      ((type-refinement-p type-sym)
       (and (vm-typep-check value (type-refinement-base type-sym))
            (%vm-typep-call-predicate value (type-refinement-predicate type-sym))))
      ;; VM CLOS instances — check class name / CPL
      ((and (hash-table-p value) (gethash :__class__ value))
       (let* ((class-ht   (gethash :__class__ value))
              (class-name (and (hash-table-p class-ht) (gethash :__name__ class-ht)))
              (cpl        (and (hash-table-p class-ht) (gethash :__cpl__ class-ht))))
         (or (eq class-name type-sym) (member type-sym cpl))))
      ;; Fallback to host typep
      (t (ignore-errors (typep value type-sym))))))

(defmethod execute-instruction ((inst vm-typep) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (type-sym (vm-type-name inst))
         (result (if (vm-typep-check value type-sym) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

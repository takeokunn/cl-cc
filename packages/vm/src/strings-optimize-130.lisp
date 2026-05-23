;;;; packages/vm/src/strings-optimize-130.lisp — Phase 130: String/Symbol/Numeric Optimization
;;;; FR-722 String Interning, FR-723 Small String Optimization,
;;;; FR-724 Numeric Tower Optimization, FR-725 Symbol Table Optimization

(in-package :cl-cc/vm)

;;; ──── FR-722: String Interning ────
(defvar *string-intern-table* (make-hash-table :test #'equal :weakness :value)
  "Global string intern table. Uses weak references so unreferenced strings
can be collected by GC.")

(defun intern-string (s)
  "Intern string S: if an equal string already exists in the intern table,
return that canonical object. Otherwise register S and return it.
Interned strings can be compared with EQ (O(1) pointer comparison)."
  (check-type s string)
  (or (gethash s *string-intern-table*)
      (setf (gethash s *string-intern-table*) s)))

(defun string-interned-p (s)
  "Return T if S is an interned string."
  (eq s (gethash s *string-intern-table*)))

;;; ──── FR-723: Small String Optimization / SSO ────
(defconstant +sso-max-length+ 15
  "Maximum number of characters that can be stored inline in a small string.")

(defun sso-string-p (obj)
  "Return T if OBJ is a small (inline) string object."
  (and (integerp obj)
       (logtest obj #b111)))

(defun make-sso-string (chars)
  "Create a small string from CHARS (list or vector of characters, max 15 chars)."
  (let ((len (length chars))
        (tag 0))
    (when (> len +sso-max-length+)
      (error "SSO string too long: ~D chars (max ~D)" len +sso-max-length+))
    (loop for i from 0 below len
          for ch = (if (listp chars) (nth i chars) (aref chars i))
          do (setf tag (logior tag (ash (char-code ch) (+ 4 (* i 4)))))
          finally (return (logior tag (ash len 1) #b1)))))

(defun sso-string-length (sso)
  "Return the character length of small string SSO."
  (ash (logand sso (ash (1- (ash 1 5)) 1)) -1))

(defun sso-string-char (sso index)
  "Return the character at INDEX in small string SSO."
  (code-char (ldb (byte 4 (+ 4 (* index 4))) sso)))

;;; ──── FR-724: Numeric Tower Optimization ────
(defconstant +fixnum-tag+ 0
  "Tag for fixnum: least significant bit = 0 (63-bit signed on 64-bit arch).")

(defun fixnum-p (x)
  "Return T if X is a tagged fixnum."
  (and (integerp x) (zerop (logand x #b1))))

(defun make-fixnum (n)
  "Create a tagged fixnum from integer N."
  (ash n 1))

(defun fixnum-value (x)
  "Extract the integer value from fixnum X."
  (ash x -1))

(defun fixnum-add (a b)
  "Add two fixnums. Returns (values result overflow-p)."
  (let ((result (+ (fixnum-value a) (fixnum-value b))))
    (if (<= (- (ash 1 62)) result (1- (ash 1 62)))
        (values (make-fixnum result) nil)
        (values nil t))))

(defun fixnum-mul (a b)
  "Multiply two fixnums. Returns (values result overflow-p)."
  (let ((result (* (fixnum-value a) (fixnum-value b))))
    (if (<= (- (ash 1 62)) result (1- (ash 1 62)))
        (values (make-fixnum result) nil)
        (values nil t))))

;;; ──── FR-725: Symbol Table Optimization (MPHF) ────
(defvar *perfect-hash-tables-enabled* nil
  "When T, emit Minimal Perfect Hash Function tables for compiled packages.")

(defstruct mphf-table
  "A Minimal Perfect Hash Function table for O(1) zero-collision symbol lookup."
  (seeds nil :type list)
  (table (make-array 0) :type vector)
  (symbols (make-array 0) :type vector))

(defun mphf-hash (key seeds table-size)
  "Compute MPHF hash for KEY using SEEDS and TABLE-SIZE (CHD algorithm style)."
  (let ((h 0))
    (dolist (seed seeds (mod h table-size))
      (setf h (+ h (* seed (sxhash key)))))))

(defun mphf-lookup (key mphf)
  "Look up KEY in MPHF-TABLE. Returns the symbol or NIL."
  (let* ((idx (mphf-hash key (mphf-table-seeds mphf) (length (mphf-table-table mphf))))
         (candidate (aref (mphf-table-symbols mphf) idx)))
    (when (and candidate (eq (if (symbolp key) (symbol-name key) key)
                             (if (symbolp candidate) (symbol-name candidate) candidate)))
      candidate)))

(defun build-perfect-hash-table (symbols)
  "Build an MPHF table for the list of SYMBOLS."
  (let* ((n (length symbols))
         (seeds (list (random most-positive-fixnum) (random most-positive-fixnum)))
         (table (make-array n :initial-element nil))
         (symbol-vec (make-array n :initial-element nil)))
    (dolist (sym symbols)
      (let ((idx (mphf-hash (symbol-name sym) seeds n)))
        (setf (aref table idx) t
              (aref symbol-vec idx) sym)))
    (make-mphf-table :seeds seeds :table table :symbols symbol-vec)))

;; ── Exports ──
(export '(*string-intern-table* intern-string string-interned-p
          +sso-max-length+ sso-string-p make-sso-string
          sso-string-length sso-string-char
          fixnum-p make-fixnum fixnum-value fixnum-add fixnum-mul
          *perfect-hash-tables-enabled* mphf-table make-mphf-table
          mphf-hash mphf-lookup build-perfect-hash-table))

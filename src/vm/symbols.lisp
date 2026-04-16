(in-package :cl-cc/vm)

;;; VM Symbol and Character Instructions
;;;
;;; This file provides VM instructions for symbol manipulation
;;; (symbol-name, make-symbol, intern, gensym, keywordp) and
;;; character predicates (digit-char-p, alpha-char-p, upper/lower-case-p,
;;; char-upcase/downcase, stringp, characterp, parse-integer, alphanumericp).
;;;

;;; ─── Symbol Manipulation Instructions ───────────────────────────────────────

(define-vm-instruction vm-symbol-name (vm-instruction)
  "Get the name string of a symbol."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :symbol-name)
  (:sexp-slots dst src))

(define-vm-instruction vm-make-symbol (vm-instruction)
  "Create an uninterned symbol from a string."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :make-symbol)
  (:sexp-slots dst src))

(define-vm-instruction vm-intern-symbol (vm-instruction)
  "Intern a string as a symbol. Optional package designator."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (pkg nil :reader vm-intern-pkg)
  (:sexp-tag :intern)
  (:sexp-slots dst src pkg))

(define-vm-instruction vm-gensym-inst (vm-instruction)
  "Generate a unique uninterned symbol."
  (dst nil :reader vm-dst)
  (:sexp-tag :gensym)
  (:sexp-slots dst))

(define-vm-instruction vm-keywordp (vm-instruction)
  "Test if value is a keyword symbol. Returns 1 if true, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :keywordp)
  (:sexp-slots dst src))

(define-simple-instruction vm-symbol-name :unary symbol-name)
(define-simple-instruction vm-make-symbol :unary make-symbol)

(defmethod execute-instruction ((inst vm-intern-symbol) state pc labels)
  (declare (ignore labels))
  (let* ((name (vm-reg-get state (vm-src inst)))
         (pkg-designator (when (vm-intern-pkg inst)
                           (vm-reg-get state (vm-intern-pkg inst))))
         (result (if pkg-designator
                     (intern name (find-package pkg-designator))
                     (intern name))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-gensym-inst) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (gensym))
  (values (1+ pc) nil nil))

(define-simple-instruction vm-keywordp :pred1 keywordp)

;;; ─── Character Predicate Instructions ───────────────────────────────────────

;; define-vm-unary-instruction is defined in vm.lisp (available to all vm/ files).

(define-vm-unary-instruction vm-digit-char-p  :digit-char-p  "Test if character is a digit. Returns weight or nil.")
(define-vm-unary-instruction vm-alpha-char-p  :alpha-char-p  "Test if character is alphabetic. Returns 1/0.")
(define-vm-unary-instruction vm-upper-case-p  :upper-case-p  "Test if character is upper case. Returns 1/0.")
(define-vm-unary-instruction vm-lower-case-p  :lower-case-p  "Test if character is lower case. Returns 1/0.")
(define-vm-unary-instruction vm-char-upcase   :char-upcase   "Upcase a character. DST = uppercase of SRC.")
(define-vm-unary-instruction vm-char-downcase :char-downcase "Downcase a character. DST = lowercase of SRC.")
(define-vm-unary-instruction vm-stringp       :stringp       "Test if value is a string. Returns 1/0.")
(define-vm-unary-instruction vm-characterp    :characterp    "Test if value is a character. Returns 1/0.")
(define-vm-unary-instruction vm-parse-integer :parse-integer "Parse an integer from a string. DST = integer value.")
(define-vm-unary-instruction vm-alphanumericp :alphanumericp "Test if character is alphanumeric. Returns 1/0.")

(defmethod execute-instruction ((inst vm-digit-char-p) state pc labels)
  (declare (ignore labels))
  (let* ((ch (vm-reg-get state (vm-src inst)))
         (result (digit-char-p ch)))
    (vm-reg-set state (vm-dst inst) (or result nil))
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-alpha-char-p :pred1 alpha-char-p)
(define-simple-instruction vm-upper-case-p :pred1 upper-case-p)
(define-simple-instruction vm-lower-case-p :pred1 lower-case-p)
(define-simple-instruction vm-char-upcase :unary char-upcase)
(define-simple-instruction vm-char-downcase :unary char-downcase)
(define-simple-instruction vm-stringp :pred1 stringp)
(define-simple-instruction vm-characterp :pred1 characterp)
(define-simple-instruction vm-parse-integer :unary parse-integer)
(define-simple-instruction vm-alphanumericp :pred1 alphanumericp)

;;; ─── Sleep (FR-681) ─────────────────────────────────────────────────────────

(define-vm-unary-instruction vm-sleep-inst :sleep "Suspend execution for N seconds.")
(define-simple-instruction vm-sleep-inst :unary sleep)

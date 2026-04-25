;;;; packages/backend/runtime/src/runtime-math-io.lisp - CL-CC Runtime: Symbols, Hash Tables, Conditions, Misc
;;;
;;; Contains: rt-symbol-*, rt-intern, rt-make-hash-table, rt-gethash/sethash/remhash/clrhash/maphash,
;;; rt-signal-error, rt-signal, rt-warn-fn, rt-cerror, rt-boundp/fboundp,
;;; rt-random, rt-coerce, rt-read-from-string, rt-read-sexp, rt-write-to-string.
;;;
;;; Strings/characters are in runtime-strings.lisp; CLOS/generic dispatch in runtime-clos.lisp.
;;; Depends on runtime.lisp. Load order: after runtime-ops.lisp.

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Symbols
;;; ------------------------------------------------------------

(defun rt-symbol-name (sym) (symbol-name sym))
(defun rt-make-symbol (name) (make-symbol name))
(defvar *rt-global-var-registry* (make-hash-table :test #'eq)
  "Runtime global variable registry used instead of host symbol-value cells.")

(defun rt-symbol-value (sym)
  (multiple-value-bind (value present-p) (gethash sym *rt-global-var-registry*)
    (if present-p
        value
        (error "Unbound runtime variable: ~S" sym))))

(defun rt-set-symbol-value (sym val)
  (setf (gethash sym *rt-global-var-registry*) val)
  val)
(defun rt-symbol-plist (sym) (symbol-plist sym))
(defun rt-get-prop (sym indicator) (get sym indicator))
(defun rt-put-prop (sym indicator val) (setf (get sym indicator) val))
(defun rt-remprop (sym indicator) (remprop sym indicator))

;;; ------------------------------------------------------------
;;; Pure scalar/sequence helpers used by the VM bridge
;;; ------------------------------------------------------------

(defun rt-1+ (x) (1+ x))
(defun rt-1- (x) (1- x))
(defun rt-+ (&rest xs) (apply #'+ xs))
(defun rt-- (&rest xs) (apply #'- xs))
(defun rt-* (&rest xs) (apply #'* xs))
(defun rt-/ (&rest xs) (apply #'/ xs))
(defun rt-< (&rest xs) (apply #'< xs))
(defun rt-> (&rest xs) (apply #'> xs))
(defun rt-<= (&rest xs) (apply #'<= xs))
(defun rt->= (&rest xs) (apply #'>= xs))
(defun rt-max (&rest xs) (apply #'max xs))
(defun rt-min (&rest xs) (apply #'min xs))
(defun rt-length (x) (length x))
(defun rt-char= (&rest xs) (apply #'char= xs))
(defun rt-char-equal (&rest xs) (apply #'char-equal xs))
(defun rt-eql (a b) (eql a b))
(defun rt-equal (a b) (equal a b))
(defun rt-equalp (a b) (equalp a b))
(defun rt-elt (sequence index) (elt sequence index))
(defun rt-append (&rest lists) (apply #'append lists))

;;; ------------------------------------------------------------
;;; Hash Tables
;;; ------------------------------------------------------------

(defun rt-make-hash-table (&key (test #'eql) size)
  (if size
      (make-hash-table :test test :size size)
      (make-hash-table :test test)))
(defun rt-gethash (key ht) (gethash key ht))
(defun rt-sethash (key ht val) (setf (gethash key ht) val))
(defun rt-remhash (key ht) (remhash key ht))
(defun rt-clrhash (ht) (clrhash ht))
(defun rt-hash-count (ht) (hash-table-count ht))
(defun rt-hash-test (ht) (hash-table-test ht))
(defun rt-maphash (fn ht) (maphash fn ht))
(defun rt-hash-keys (ht)
  (let (keys) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) ht) keys))
(defun rt-hash-values (ht)
  (let (vals) (maphash (lambda (k v) (declare (ignore k)) (push v vals)) ht) vals))

;;; ------------------------------------------------------------
;;; Conditions / Error Handling
;;; ------------------------------------------------------------

(defun rt-signal-error (condition)
  (error condition))

(defun rt-signal (condition)
  (signal condition))

(defun rt-warn-fn (condition)
  (warn "~A" condition))

(defun rt-cerror (continue-string condition)
  (cerror continue-string "~A" condition))

(defun rt-invoke-restart (name &rest args)
  (apply #'invoke-restart name args))

;;; ------------------------------------------------------------
;;; Misc
;;; ------------------------------------------------------------

(defun rt-boundp (sym)
  (if (nth-value 1 (gethash sym *rt-global-var-registry*)) 1 0))

(defun rt-makunbound (sym)
  (remhash sym *rt-global-var-registry*)
  sym)
(defun rt-random (n) (random n))
(defun rt-make-random-state (&optional state)
  (if state (make-random-state state) (make-random-state)))
(defun rt-get-universal-time () (get-universal-time))
(defun rt-get-internal-real-time () (get-internal-real-time))
(defun rt-get-internal-run-time () (get-internal-run-time))
(defun rt-read-from-string (s) (read-from-string s))
(defun rt-read-sexp (stream) (read stream))
(defun rt-coerce (x type) (coerce x type))
(defun rt-write-to-string (x) (write-to-string x))

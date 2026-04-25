(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — Data Layer
;;;
;;; This file is the "Prolog database" for the compiler macro expander:
;;; all grammar tables, arity classifications, and dispatch tables.
;;; No logic lives here — only facts.  expander.lisp reads this data.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Runtime state ────────────────────────────────────────────────────────

(defvar *accessor-slot-map* (make-hash-table :test #'eq)
  "Maps accessor function names to (class-name . slot-name) for setf expansion.")

(defvar *defstruct-slot-registry* (make-hash-table :test #'eq)
  "Maps struct name to list of (slot-name default-value) for :include inheritance.")

(defvar *defstruct-type-registry* (make-hash-table :test #'eq)
  "Maps struct name to its defstruct representation type: NIL, LIST, or VECTOR.")

(defvar *macro-eval-fn* #'eval
  "Function used to evaluate macro bodies at compile time.
Initially #'eval for bootstrap.  After full pipeline loads, rebound to our-eval
so macro expansion runs through cl-cc's own compiler — the key self-hosting step.")

(defvar *symbol-macro-table* (make-hash-table :test #'eq)
  "Global symbol macro environment: maps symbol → expansion form.
Used by define-symbol-macro. Local symbol-macrolet bindings shadow these.")

(defvar *constant-table* (make-hash-table :test #'eq)
  "Global constant environment: maps defconstant names to their values.
Used by the compiler to inline constant symbol references.")

(defvar *compiler-macro-table* (make-hash-table :test #'eq)
  "Global compiler-macro environment: maps function names to compiler macro expanders.")

;;; ── Compiler special form grammar ────────────────────────────────────────
;;;
;;; Forms in this list are handled directly by the parser/compiler.
;;; They are not subject to macro expansion unless they also have an entry
;;; in *expander-head-table*, which takes precedence.

(defparameter *compiler-special-forms*
  '(if progn lambda quote setq setf
    defun defvar defparameter defmacro defclass defgeneric defmethod
    make-instance slot-value
    block return-from tagbody go
    flet labels function funcall
    the print
    catch throw unwind-protect
    handler-case eval-when defstruct
    macrolet symbol-macrolet
    multiple-value-call multiple-value-prog1
    values multiple-value-bind apply)
  "Forms handled directly by the parser/compiler.
These recurse into subforms but their head is not macro-expanded.")

;;; ── Builtin arity classification ─────────────────────────────────────────
;;;
;;; These lists drive #'name → lambda wrapping and (apply #'name list)
;;; → dolist-fold expansion.  Adding a builtin = one entry here.

(defparameter *variadic-fold-builtins*
  '(+ * append nconc)
  "Builtins that fold over args with an identity: (OP a b c) = (OP (OP a b) c).")

(defparameter *binary-builtins*
  '(cons = < > <= >= mod rem eq eql equal
    nth nthcdr member assoc acons
    string= string< string> string<= string>= string-equal
    string-lessp string-greaterp string/=
    string-not-equal string-not-greaterp string-not-lessp string-concat
    char min max floor ceiling truncate round ffloor fceiling ftruncate fround
    ash logand logior logxor logeqv logtest logbitp
    bswap
    expt scale-float gcd lcm complex
    array-dimension row-major-aref svref vector-push
    bit sbit bit-and bit-or bit-xor adjust-array nreconc)
  "Builtins that take exactly 2 arguments.")

(defparameter *unary-builtins*
  '(car cdr not null consp symbolp numberp integerp stringp
    atom listp characterp functionp
    first second third fourth fifth sixth seventh eighth ninth tenth rest last
    nreverse butlast nbutlast endp reverse length copy-list copy-tree
    symbol-name make-symbol intern gensym keywordp
    string-length string-upcase string-downcase nstring-upcase nstring-downcase nstring-capitalize
    char-code code-char
    typep hash-table-p hash-table-count simple-vector-p
    hash-table-test hash-table-keys hash-table-values
    zerop plusp minusp evenp oddp abs lognot logcount integer-length
    sqrt exp log sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh float float-sign
    list-length
    rational rationalize numerator denominator realpart imagpart conjugate phase
    boundp fboundp makunbound
    array-rank array-total-size array-dimensions
    fill-pointer array-has-fill-pointer-p array-adjustable-p vector-pop
    bit-not array-displacement char-int
    princ prin1 print write-to-string prin1-to-string princ-to-string
    type-of make-list alphanumericp eval identity constantly complement sleep
    macroexpand-1 macroexpand sxhash class-name class-of)
  "Builtins that take exactly 1 argument.")

(defparameter *cxr-builtins*
  '(caar cadr cdar cddr
    caaar cdaar cadar cddar
    caadr cdadr caddr cdddr
    caaaar cadaar caadar caddar
    cdaaar cddaar cdadar cdddar
    caaadr cadadr caaddr cadddr
    cdaadr cddadr cdaddr cddddr)
  "CXR accessor builtins — all unary.  There are exactly 28.")

(defparameter *all-builtin-names*
  (append *variadic-fold-builtins* '(- list) *binary-builtins* *unary-builtins* *cxr-builtins*)
  "Union of all known builtin names — used by #'name → lambda wrapping.")

;; Rounding operations whose 1-arg form normalises to 2-arg (divisor = 1).
(defparameter *rounding-ops* '(floor ceiling truncate round)
  "Rounding ops for which (op x) normalises to (op x 1) (FR-301).")

;; Identity elements for variadic-fold builtins — used by both
;; expand-function-builtin (#'name wrapping) and expand-apply-named-fn.
(defparameter *variadic-fold-identities*
  '((+      . 0)
    (*      . 1)
    (append . nil)
    (nconc  . nil))
  "Maps each variadic-fold builtin to its identity element for fold initialisation.")

;;; ── setf compound-place dispatch table ──────────────────────────────────
;;;
;;; Populated in expander.lisp after helper functions are defined.
;;; Maps a place head symbol to (lambda (place value) → expanded-form).
;;; Adding a new settable place requires exactly one entry here.

(defvar *setf-compound-place-handlers* (make-hash-table :test 'eq)
  "Dispatch: compound-place head symbol → (lambda (place value) → expanded-form).
Known entries: aref, getf, car, first, cdr, rest, nth, cadr, cddr.")

;;; ── Query helpers for the data layer ─────────────────────────────────────

(defun compiler-special-form-p (name)
  "Return T when NAME is handled directly by the compiler/parser layer."
  (member name *compiler-special-forms*))

(defun builtin-name-p (name)
  "Return T when NAME appears in the consolidated builtin registry." 
  (member name *all-builtin-names*))

(defun variadic-fold-identity (name)
  "Return the identity element for a variadic fold builtin, or NIL if unknown."
  (cdr (assoc name *variadic-fold-identities*)))

;;; ── Expander dispatch table ───────────────────────────────────────────────
;;;
;;; Populated by define-expander-for in expander.lisp.
;;; Forms in *compiler-special-forms* that are NOT here are handled by
;;; the default "recurse into subforms" rule.

(defvar *expander-head-table* (make-hash-table :test 'eq)
  "Dispatch: form-head symbol → handler fn (form → expanded-form).
Handler contract: form → fully-expanded form (may call compiler-macroexpand-all recursively).
Handlers take precedence over *compiler-special-forms* recurse-fallback.")

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

(defvar *defstruct-read-only-accessor-map* (make-hash-table :test #'eq)
  "Set-like table of read-only defstruct accessor names.
When accessor is present, `(setf (accessor obj) ...)` is rejected at expansion.")

(defvar *defstruct-slot-registry* (make-hash-table :test #'eq)
  "Maps struct name to list of (slot-name default-value) for :include inheritance.")

(defvar *defstruct-type-registry* (make-hash-table :test #'eq)
  "Maps struct name to its defstruct representation type: NIL, LIST, or VECTOR.")

(defvar *declaim-inline-registry* (make-hash-table :test #'eq)
  "Maps function names to global inline policy keywords (:INLINE or :NOTINLINE).
Used by `(declaim (inline ...))` / `(declaim (notinline ...))` during source compilation.")

(defvar *declaim-optimize-registry* (make-hash-table :test #'eq)
  "Maps optimize qualities to global quality levels 0..3.
Used by `(declaim (optimize ...))` during source compilation.")

(defvar *global-proclamations* (make-hash-table :test #'eq)
  "Maps proclamation kinds to proclamation metadata tables.
FR-935 stores TYPE, FTYPE, and SPECIAL proclamations here while preserving the
existing inline and optimize registries for their specialized consumers.")

(defvar *macro-eval-fn* #'%bootstrap-macro-eval
  "Function used to evaluate macro bodies at compile time.
Initially a bootstrap wrapper that prefers our-eval and only falls back to host
eval until the selfhosted compiler is fully available.")

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
  '(cons hash-cons = < > <= >= mod rem eq eql equal
    nth nthcdr member assoc acons
    string= string< string> string<= string>= string-equal
    string-lessp string-greaterp string/=
    string-not-equal string-not-greaterp string-not-lessp string-concat
     char min max floor ceiling truncate round ffloor fceiling ftruncate fround
     typep subtypep
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
     char-code code-char character
    hash-table-p hash-table-count simple-vector-p
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

;;; ── Perfect Hash Tables (FR-130) ──────────────────────────────────────────
;;;
;;; Hash-table versions of the linear lookup lists above.
;;; compiler-macroexpand-all and expander-core are the hot paths;
;;; gethash is O(1) while member/%list-contains-eq are O(n).

(defparameter *compiler-special-forms-table*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (s *compiler-special-forms*) (setf (gethash s ht) t))
    ht)
  "Hash table of *compiler-special-forms* for O(1) lookup.")

(defparameter *all-builtin-names-table*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (s *all-builtin-names*) (setf (gethash s ht) t))
    ht)
  "Hash table of *all-builtin-names* for O(1) builtin-name lookup.")

(defparameter *binary-builtins-table*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (s *binary-builtins*) (setf (gethash s ht) t))
    ht)
  "Hash table of *binary-builtins* for O(1) lookup.")

(defparameter *unary-builtins-table*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (s *unary-builtins*) (setf (gethash s ht) t))
    ht)
  "Hash table of *unary-builtins* for O(1) lookup.")

(defparameter *variadic-fold-builtins-table*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (s *variadic-fold-builtins*) (setf (gethash s ht) t))
    ht)
  "Hash table of *variadic-fold-builtins* for O(1) lookup.")

(defparameter *variadic-fold-or-list-table*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (s *variadic-fold-builtins*) (setf (gethash s ht) t))
    (setf (gethash '- ht) t)
    ;; Note: 'list is handled by a dedicated branch in expand-apply-named-fn,
    ;; not included here to avoid misclassifying it as a fold operation.
    ht)
  "Hash table of (*variadic-fold-builtins* plus '-) for O(1) lookup.")

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
 Known entries: aref, fill-pointer, getf, car, first, cdr, rest, nth, cadr, cddr.")

;;; ── Query helpers for the data layer ─────────────────────────────────────

;;; ── Expander dispatch table ───────────────────────────────────────────────
;;;
;;; Populated by define-expander-for in expander.lisp.
;;; Forms in *compiler-special-forms* that are NOT here are handled by
;;; the default "recurse into subforms" rule.

(defvar *expander-head-table* (make-hash-table :test 'eq)
  "Dispatch: form-head symbol → handler fn (form → expanded-form).
Handler contract: form → fully-expanded form (may call compiler-macroexpand-all recursively).
Handlers take precedence over *compiler-special-forms* recurse-fallback.")

;;; ── FR-153: Macro expansion memoization ────────────────────────────────────
;;;
;;; Side-effect-free macros (defun/let/cond etc.) are expanded once and the
;;; result is cached.  Uses EQUAL for the form key (not just sxhash) to avoid
;;; collisions.  Disabled by default; enable with --memoize-macros CLI flag.

(defvar *enable-macro-memoization* nil
  "FR-153: When T, macro expansion results are cached for reuse.
Disabled by default because caching side-effecting or environment-dependent
macros could produce stale results.")

(defvar *macro-expansion-cache* (make-hash-table :test #'equal)
  "FR-153: Memoization cache for macro expansion results.
Keys are (macro-name . form) conses using EQUAL; values are expanded forms.
Cleared between compilation units to avoid stale entries.")

(defun %macro-cache-key (name form)
  "Build a cache key for macro NAME applied to FORM.
Uses EQUAL on the cons so structural equality is checked."
  (cons name form))

(defun %cached-macro-expansion (name form)
  "Return two values: (cached-form, present-p).
Uses gethash second value to distinguish NIL cached expansion from cache miss."
  (when *enable-macro-memoization*
    (gethash (%macro-cache-key name form) *macro-expansion-cache*)))

(defun %cache-macro-expansion (name form expanded)
  "Store EXPANDED as the cached expansion of (NAME FORM)."
  (when *enable-macro-memoization*
    (setf (gethash (%macro-cache-key name form) *macro-expansion-cache*) expanded)))

(defun clear-macro-expansion-cache ()
  "Clear the macro expansion memoization cache.
Called between compilation units or when macros are redefined."
  (clrhash *macro-expansion-cache*))
;;; ── End of FR-153 ────────────────────────────────────────────────────────

;;; ── FR-241: Macro Expansion Tracing ────────────────────────────────────────

(defvar *trace-macros* nil
  "FR-241: When T, print each macro expansion step with indentation.
Set by --trace-macros CLI flag.")

(defvar *macro-expand-depth* 0
  "Current macro expansion recursion depth for indentation.")

;;;; compile/builtin-registry.lisp — Declarative builtin dispatch tables
;;;
;;; Maps CL function names to their VM instruction constructors and calling
;;; conventions.  Used by compile-ast (ast-call) to replace ~850 lines of
;;; macrolet-expanded dispatch with a single table lookup + generic emitter.
;;;
;;; Calling conventions (keys in *builtin-registry*):
;;;   :unary         — (fn arg)         → (:dst :src)
;;;   :binary        — (fn a b)         → (:dst :lhs :rhs)
;;;   :string-cmp    — (fn s1 s2)       → (:dst :str1 :str2)
;;;   :char-cmp      — (fn c1 c2)       → (:dst :char1 :char2)
;;;   :table-query   — (fn table)       → (:dst :table)
;;;   :handle-input  — (fn handle)      → (:dst :handle)
;;;   :side-effect   — (fn arg)         → (:src) + move dst←src
;;;   :void-side-eff — (fn)             → () + const dst←nil
;;;   :nullary       — (fn)             → (:dst)
;;;   :string-trim   — (fn bag str)     → (:dst :char-bag :string)
;;;   :handle-effect — (fn handle)      → (:handle) + const dst←nil
(in-package :cl-cc)

;;; ─── Registry Entry ────────────────────────────────────────────────────────

(defstruct (builtin-entry (:conc-name be-))
  "A single builtin dispatch entry."
  (name-str  ""     :type string)       ; uppercase CL name string
  (convention :unary :type keyword)     ; calling convention tag
  (ctor       nil    :type symbol))     ; make-vm-* constructor symbol

;;; ─── Table Definitions ─────────────────────────────────────────────────────

(defparameter *builtin-unary-entries*
  '(;; String operations
    (string-length    . make-vm-string-length)
    (string-upcase    . make-vm-string-upcase)
    (string-downcase  . make-vm-string-downcase)
    (string-capitalize . make-vm-string-capitalize)
    ;; Type predicates
    (symbolp          . make-vm-symbol-p)
    (numberp          . make-vm-number-p)
    (integerp         . make-vm-integer-p)
    (consp            . make-vm-cons-p)
    (null             . make-vm-null-p)
    (functionp        . make-vm-function-p)
    ;; List operations
    (car              . make-vm-car)
    (cdr              . make-vm-cdr)
    (first            . make-vm-first)
    (second           . make-vm-second)
    (third            . make-vm-third)
    (fourth           . make-vm-fourth)
    (fifth            . make-vm-fifth)
    (rest             . make-vm-rest)
    (last             . make-vm-last)
    (length           . make-vm-length)
    (reverse          . make-vm-reverse)
    (not              . make-vm-not)
    (nreverse         . make-vm-nreverse)
    (butlast          . make-vm-butlast)
    (endp             . make-vm-endp)
    ;; Arithmetic
    (abs              . make-vm-abs)
    (evenp            . make-vm-evenp)
    (oddp             . make-vm-oddp)
    ;; Bitwise
    (lognot           . make-vm-lognot)
    (logcount         . make-vm-logcount)
    (integer-length   . make-vm-integer-length)
    ;; Transcendentals
    (sqrt             . make-vm-sqrt)
    (exp              . make-vm-exp-inst)
    (log              . make-vm-log-inst)
    (sin              . make-vm-sin-inst)
    (cos              . make-vm-cos-inst)
    (tan              . make-vm-tan-inst)
    (asin             . make-vm-asin-inst)
    (acos             . make-vm-acos-inst)
    (atan             . make-vm-atan-inst)
    (sinh             . make-vm-sinh-inst)
    (cosh             . make-vm-cosh-inst)
    (tanh             . make-vm-tanh-inst)
    ;; Float operations
    (float            . make-vm-float-inst)
    (float-precision  . make-vm-float-precision)
    (float-radix      . make-vm-float-radix)
    (float-sign       . make-vm-float-sign)
    (float-digits     . make-vm-float-digits)
    (decode-float     . make-vm-decode-float)
    (integer-decode-float . make-vm-integer-decode-float)
    ;; Binding predicates
    (boundp           . make-vm-boundp)
    (fboundp          . make-vm-fboundp)
    (makunbound       . make-vm-makunbound)
    (fmakunbound      . make-vm-fmakunbound)
    ;; Random
    (random           . make-vm-random)
    ;; Rational
    (rational         . make-vm-rational)
    (rationalize      . make-vm-rationalize)
    (numerator        . make-vm-numerator)
    (denominator      . make-vm-denominator)
    ;; Complex
    (realpart         . make-vm-realpart)
    (imagpart         . make-vm-imagpart)
    (conjugate        . make-vm-conjugate)
    (phase            . make-vm-phase)
    ;; Hash table
    (hash-table-p     . make-vm-hash-table-p)
    ;; Stream predicates
    (streamp           . make-vm-streamp)
    (input-stream-p    . make-vm-input-stream-p)
    (output-stream-p   . make-vm-output-stream-p)
    (open-stream-p     . make-vm-open-stream-p)
    (interactive-stream-p . make-vm-interactive-stream-p)
    (stream-element-type . make-vm-stream-element-type-inst)
    ;; Array/vector
    (vectorp          . make-vm-vectorp)
    (array-length     . make-vm-array-length)
    (array-rank       . make-vm-array-rank)
    (array-total-size . make-vm-array-total-size)
    (array-dimensions . make-vm-array-dimensions)
    (fill-pointer     . make-vm-fill-pointer-inst)
    (array-has-fill-pointer-p . make-vm-array-has-fill-pointer-p)
    (array-adjustable-p . make-vm-array-adjustable-p)
    (vector-pop       . make-vm-vector-pop)
    (array-displacement . make-vm-array-displacement)
    ;; Bit array
    (bit-not          . make-vm-bit-not)
    ;; Symbol
    (symbol-name      . make-vm-symbol-name)
    (make-symbol      . make-vm-make-symbol)
    (keywordp         . make-vm-keywordp)
    (symbol-plist     . make-vm-symbol-plist)
    ;; Time
    (decode-universal-time . make-vm-decode-universal-time)
    ;; Character predicates and operations
    (both-case-p      . make-vm-both-case-p)
    (graphic-char-p   . make-vm-graphic-char-p)
    (standard-char-p  . make-vm-standard-char-p)
    (digit-char       . make-vm-digit-char)
    (char-name        . make-vm-char-name)
    (name-char        . make-vm-name-char)
    (char-int         . make-vm-char-code)
    (digit-char-p     . make-vm-digit-char-p)
    (alpha-char-p     . make-vm-alpha-char-p)
    (alphanumericp    . make-vm-alphanumericp)
    (upper-case-p     . make-vm-upper-case-p)
    (lower-case-p     . make-vm-lower-case-p)
    (char-upcase      . make-vm-char-upcase)
    (char-downcase    . make-vm-char-downcase)
    (char-code        . make-vm-char-code)
    (code-char        . make-vm-code-char)
    (stringp          . make-vm-stringp)
    (characterp       . make-vm-characterp)
    (parse-integer    . make-vm-parse-integer)
    ;; Coercion
    (coerce-to-string . make-vm-coerce-to-string)
    (coerce-to-list   . make-vm-coerce-to-list)
    (coerce-to-vector . make-vm-coerce-to-vector)
    (string           . make-vm-string-coerce)
    ;; List utilities
    (listp            . make-vm-listp)
    (atom             . make-vm-atom)
    (copy-list        . make-vm-copy-list)
    (copy-tree        . make-vm-copy-tree)
    ;; Type
    (type-of          . make-vm-type-of)
    ;; Eval
    (eval             . make-vm-eval)
    ;; String stream
    (get-output-stream-string . make-vm-get-output-stream-string-inst)
    ;; Reader
    (read-from-string . make-vm-read-from-string-inst)
    (read             . make-vm-read-sexp-inst)
    ;; Load
    (load             . make-vm-load-file))
  "Alist of (cl-symbol . vm-constructor) for unary builtins: (fn arg) → (:dst :src).")

(defparameter *builtin-binary-entries*
  '(;; Arithmetic
    (mod              . make-vm-mod)
    (rem              . make-vm-rem)
    (truncate         . make-vm-truncate)
    (floor            . make-vm-floor-inst)
    (ceiling          . make-vm-ceiling-inst)
    (min              . make-vm-min)
    (max              . make-vm-max)
    (round            . make-vm-round-inst)
    ;; Bitwise
    (ash              . make-vm-ash)
    (logand           . make-vm-logand)
    (logior           . make-vm-logior)
    (logxor           . make-vm-logxor)
    (logeqv           . make-vm-logeqv)
    (logtest          . make-vm-logtest)
    (logbitp          . make-vm-logbitp)
    ;; Transcendental (2-arg)
    (expt             . make-vm-expt)
    ;; Float (2-arg)
    (scale-float      . make-vm-scale-float)
    ;; Rational (2-arg)
    (gcd              . make-vm-gcd)
    (lcm              . make-vm-lcm)
    ;; Complex (2-arg)
    (complex          . make-vm-complex)
    ;; Array
    (array-dimension  . make-vm-array-dimension)
    (row-major-aref   . make-vm-row-major-aref)
    (svref            . make-vm-svref)
    ;; Bit array
    (bit-and          . make-vm-bit-and)
    (bit-or           . make-vm-bit-or)
    (bit-xor          . make-vm-bit-xor)
    ;; List
    (equal            . make-vm-equal)
    (nconc            . make-vm-nconc)
    ;; 2-arg atan → atan2 (only fires when args=2; 1-arg atan is unary above)
    (atan             . make-vm-atan2-inst))
  "Alist of (cl-symbol . vm-constructor) for binary builtins: (fn a b) → (:dst :lhs :rhs).
   Binary builtins require exactly 2 args (checked at dispatch time).")

(defparameter *builtin-string-cmp-entries*
  '((string=            . make-vm-string=)
    (string<            . make-vm-string<)
    (string>            . make-vm-string>)
    (string<=           . make-vm-string<=)
    (string>=           . make-vm-string>=)
    (string-equal       . make-vm-string-equal)
    (string-lessp       . make-vm-string-lessp)
    (string-greaterp    . make-vm-string-greaterp)
    (string/=           . make-vm-string-not-equal)
    (string-not-equal   . make-vm-string-not-equal)
    (string-not-greaterp . make-vm-string-not-greaterp)
    (string-not-lessp   . make-vm-string-not-lessp)
    ;; String concatenation shares string-cmp slot layout
    (string-concat      . make-vm-concatenate))
  "Alist of (cl-symbol . vm-constructor) for string comparison builtins: (fn s1 s2) → (:dst :str1 :str2).")

(defparameter *builtin-char-cmp-entries*
  '((char=             . make-vm-char=)
    (char<             . make-vm-char<)
    (char>             . make-vm-char>)
    (char<=            . make-vm-char<=)
    (char>=            . make-vm-char>=)
    (char/=            . make-vm-char/=)
    (char-equal        . make-vm-char-equal)
    (char-not-equal    . make-vm-char-not-equal)
    (char-lessp        . make-vm-char-lessp)
    (char-greaterp     . make-vm-char-greaterp)
    (char-not-greaterp . make-vm-char-not-greaterp)
    (char-not-lessp    . make-vm-char-not-lessp))
  "Alist of (cl-symbol . vm-constructor) for char comparison builtins: (fn c1 c2) → (:dst :char1 :char2).")

(defparameter *builtin-table-query-entries*
  '((hash-table-count  . make-vm-hash-table-count)
    (hash-table-keys   . make-vm-hash-table-keys)
    (hash-table-values . make-vm-hash-table-values)
    (hash-table-test   . make-vm-hash-table-test))
  "Alist of (cl-symbol . vm-constructor) for hash-table query builtins: (fn table) → (:dst :table).")

(defparameter *builtin-handle-input-entries*
  '((file-position     . make-vm-file-position)
    (file-length       . make-vm-file-length)
    (read-byte         . make-vm-read-byte)
    (listen            . make-vm-listen-inst))
  "Alist of (cl-symbol . vm-constructor) for handle-input builtins: (fn handle) → (:dst :handle).
read-char and read-line are handled by custom codegen (optional stream argument).")

(defparameter *builtin-side-effect-entries*
  '((princ             . make-vm-princ)
    (prin1             . make-vm-prin1)
    (print             . make-vm-print-inst))
  "Alist of (cl-symbol . vm-constructor) for side-effect builtins: (fn arg) → emit (:src), move dst←src.")

(defparameter *builtin-void-side-effect-entries*
  '((terpri            . make-vm-terpri-inst)
    (fresh-line        . make-vm-fresh-line-inst))
  "Alist of (cl-symbol . vm-constructor) for void side-effect builtins: (fn) → emit (), const dst←nil.")

(defparameter *builtin-nullary-entries*
  '((%values-to-list         . make-vm-values-to-list)
    (gensym                  . make-vm-gensym-inst)
    (get-universal-time      . make-vm-get-universal-time)
    (get-internal-real-time  . make-vm-get-internal-real-time)
    (get-internal-run-time   . make-vm-get-internal-run-time)
    (next-method-p           . make-vm-next-method-p)
    (make-string-output-stream . make-vm-make-string-output-stream-inst))
  "Alist of (cl-symbol . vm-constructor) for nullary builtins: (fn) → (:dst).")

(defparameter *builtin-string-trim-entries*
  '((string-trim       . make-vm-string-trim)
    (string-left-trim  . make-vm-string-left-trim)
    (string-right-trim . make-vm-string-right-trim))
  "Alist of (cl-symbol . vm-constructor) for string-trim builtins: (fn bag str) → (:dst :char-bag :string).")

(defparameter *builtin-handle-effect-entries*
  '((close             . make-vm-close-file))
  "Alist of (cl-symbol . vm-constructor) for handle-effect builtins: (fn handle) → emit (:handle), const dst←nil.")

;;; ─── Unified Registry Hash Table ───────────────────────────────────────────

(defparameter *builtin-registry* (make-hash-table :test #'equal)
  "Maps uppercase CL function name strings to builtin-entry structs.
   Populated at load time from the category alists above.")

(defun %register-builtins (alist convention)
  "Register all entries from ALIST under CONVENTION in *builtin-registry*.
   Also emits a Prolog fact (builtin-convention cl-sym vm-ctor) for each entry,
   making the builtin classification queryable by the Prolog engine."
  (dolist (pair alist)
    (let ((name-str (symbol-name (car pair)))
          (cl-sym   (car pair))
          (ctor     (cdr pair)))
      (setf (gethash name-str *builtin-registry*)
            (make-builtin-entry :name-str name-str
                                :convention convention
                                :ctor ctor))
      ;; Emit Prolog fact: (builtin-<convention> <cl-sym> <vm-ctor>)
      (let ((pred (intern (format nil "BUILTIN-~A" (symbol-name convention)) :cl-cc)))
        (add-rule pred (make-prolog-rule :head (list pred cl-sym ctor)))))))

;; Populate at load time
(%register-builtins *builtin-unary-entries*            :unary)
(%register-builtins *builtin-binary-entries*           :binary)
(%register-builtins *builtin-string-cmp-entries*       :string-cmp)
(%register-builtins *builtin-char-cmp-entries*         :char-cmp)
(%register-builtins *builtin-table-query-entries*      :table-query)
(%register-builtins *builtin-handle-input-entries*     :handle-input)
(%register-builtins *builtin-side-effect-entries*      :side-effect)
(%register-builtins *builtin-void-side-effect-entries* :void-side-eff)
(%register-builtins *builtin-nullary-entries*          :nullary)
(%register-builtins *builtin-string-trim-entries*      :string-trim)
(%register-builtins *builtin-handle-effect-entries*    :handle-effect)

;;; ─── Generic Emitters ──────────────────────────────────────────────────────
;;;
;;; Each emitter takes (entry args result-reg ctx) and emits VM instructions
;;; for the corresponding calling convention.  Returns result-reg.

(defun emit-builtin-unary (entry args result-reg ctx)
  (let ((arg-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :src arg-reg))
    result-reg))

(defun emit-builtin-binary (entry args result-reg ctx)
  (let ((lhs-reg (compile-ast (first args) ctx))
        (rhs-reg (compile-ast (second args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :lhs lhs-reg :rhs rhs-reg))
    result-reg))

(defun emit-builtin-string-cmp (entry args result-reg ctx)
  (let ((arg1-reg (compile-ast (first args) ctx))
        (arg2-reg (compile-ast (second args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :str1 arg1-reg :str2 arg2-reg))
    result-reg))

(defun emit-builtin-char-cmp (entry args result-reg ctx)
  (let ((c1-reg (compile-ast (first args) ctx))
        (c2-reg (compile-ast (second args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :char1 c1-reg :char2 c2-reg))
    result-reg))

(defun emit-builtin-table-query (entry args result-reg ctx)
  (let ((table-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :table table-reg))
    result-reg))

(defun emit-builtin-handle-input (entry args result-reg ctx)
  (let ((handle-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :handle handle-reg))
    result-reg))

(defun emit-builtin-side-effect (entry args result-reg ctx)
  (let ((arg-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :src arg-reg))
    (emit ctx (make-vm-move :dst result-reg :src arg-reg))
    result-reg))

(defun emit-builtin-void-side-eff (entry _args result-reg ctx)
  (declare (ignore _args))
  (emit ctx (funcall (be-ctor entry)))
  (emit ctx (make-vm-const :dst result-reg :value nil))
  result-reg)

(defun emit-builtin-nullary (entry _args result-reg ctx)
  (declare (ignore _args))
  (emit ctx (funcall (be-ctor entry) :dst result-reg))
  result-reg)

(defun emit-builtin-string-trim (entry args result-reg ctx)
  (let ((bag-reg (compile-ast (first args) ctx))
        (str-reg (compile-ast (second args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :char-bag bag-reg :string str-reg))
    result-reg))

(defun emit-builtin-handle-effect (entry args result-reg ctx)
  (let ((handle-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :handle handle-reg))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

;;; ─── Convention Dispatcher ─────────────────────────────────────────────────

(defparameter *builtin-emitter-table*
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :unary        ht) #'emit-builtin-unary)
    (setf (gethash :binary       ht) #'emit-builtin-binary)
    (setf (gethash :string-cmp   ht) #'emit-builtin-string-cmp)
    (setf (gethash :char-cmp     ht) #'emit-builtin-char-cmp)
    (setf (gethash :table-query  ht) #'emit-builtin-table-query)
    (setf (gethash :handle-input ht) #'emit-builtin-handle-input)
    (setf (gethash :side-effect  ht) #'emit-builtin-side-effect)
    (setf (gethash :void-side-eff ht) #'emit-builtin-void-side-eff)
    (setf (gethash :nullary      ht) #'emit-builtin-nullary)
    (setf (gethash :string-trim  ht) #'emit-builtin-string-trim)
    (setf (gethash :handle-effect ht) #'emit-builtin-handle-effect)
    ht)
  "Maps convention keywords to their emitter functions.")

(defun emit-registered-builtin (entry args result-reg ctx)
  "Dispatch to the correct emitter for ENTRY's calling convention.
   Returns result-reg on success, or NIL if convention requires arg count
   check that fails (e.g. binary needs exactly 2 args)."
  (let ((conv (be-convention entry)))
    ;; Binary convention requires exactly 2 args
    (when (and (eq conv :binary) (/= (length args) 2))
      (return-from emit-registered-builtin nil))
    (let ((emitter (gethash conv *builtin-emitter-table*)))
      (when emitter
        (funcall emitter entry args result-reg ctx)))))

;;;; tests/unit/compile/builtin-registry-tests.lisp — Builtin Registry tests
(in-package :cl-cc/test)

;;; ─── Registry Lookup Tests ─────────────────────────────────────────────────

(it-sequential "builtin-registry-lookup car-unary"
  (destructuring-bind (name-str expected-conv) (list "CAR" :unary)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup cdr-unary"
  (destructuring-bind (name-str expected-conv) (list "CDR" :unary)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup not-unary"
  (destructuring-bind (name-str expected-conv) (list "NOT" :unary)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup bswap-unary"
  (destructuring-bind (name-str expected-conv) (list "BSWAP" :unary)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup length-unary"
  (destructuring-bind (name-str expected-conv) (list "LENGTH" :unary)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup mod-binary"
  (destructuring-bind (name-str expected-conv) (list "MOD" :binary)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup ash-binary"
  (destructuring-bind (name-str expected-conv) (list "ASH" :binary)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup expt-binary"
  (destructuring-bind (name-str expected-conv) (list "EXPT" :binary)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup string=-str-cmp"
  (destructuring-bind (name-str expected-conv) (list "STRING=" :string-cmp)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup char=-char-cmp"
  (destructuring-bind (name-str expected-conv) (list "CHAR=" :char-cmp)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup ht-count-table"
  (destructuring-bind (name-str expected-conv) (list "HASH-TABLE-COUNT" :table-query)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup read-byte-handle"
  (destructuring-bind (name-str expected-conv) (list "READ-BYTE" :handle-input)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup princ-side-eff"
  (destructuring-bind (name-str expected-conv) (list "PRINC" :side-effect)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup terpri-void"
  (destructuring-bind (name-str expected-conv) (list "TERPRI" :void-side-eff)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup gensym-nullary"
  (destructuring-bind (name-str expected-conv) (list "GENSYM" :nullary)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup string-trim-trim"
  (destructuring-bind (name-str expected-conv) (list "STRING-TRIM" :string-trim)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-lookup close-handle-eff"
  (destructuring-bind (name-str expected-conv) (list "CLOSE" :handle-effect)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be expected-conv))))

(it-sequential "builtin-registry-constructor-symbols car"
  (destructuring-bind (name-str expected-ctor) (list "CAR" 'cl-cc::make-vm-car)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "builtin-registry-constructor-symbols bswap"
  (destructuring-bind (name-str expected-ctor) (list "BSWAP" 'cl-cc::make-vm-bswap)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "builtin-registry-constructor-symbols mod"
  (destructuring-bind (name-str expected-ctor) (list "MOD" 'cl-cc::make-vm-mod)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "builtin-registry-constructor-symbols princ"
  (destructuring-bind (name-str expected-ctor) (list "PRINC" 'cl-cc::make-vm-princ)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "builtin-registry-constructor-symbols float"
  (destructuring-bind (name-str expected-ctor) (list "FLOAT" 'cl-cc::make-vm-float-inst)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "builtin-registry-constructor-symbols float-precision"
  (destructuring-bind (name-str expected-ctor) (list "FLOAT-PRECISION" 'cl-cc::make-vm-float-precision)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "builtin-registry-constructor-symbols float-radix"
  (destructuring-bind (name-str expected-ctor) (list "FLOAT-RADIX" 'cl-cc::make-vm-float-radix)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "builtin-registry-constructor-symbols float-sign"
  (destructuring-bind (name-str expected-ctor) (list "FLOAT-SIGN" 'cl-cc::make-vm-float-sign)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "builtin-registry-constructor-symbols float-digits"
  (destructuring-bind (name-str expected-ctor) (list "FLOAT-DIGITS" 'cl-cc::make-vm-float-digits)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "builtin-registry-constructor-symbols decode-float"
  (destructuring-bind (name-str expected-ctor) (list "DECODE-FLOAT" 'cl-cc::make-vm-decode-float)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "builtin-registry-constructor-symbols integer-decode-float"
  (destructuring-bind (name-str expected-ctor) (list "INTEGER-DECODE-FLOAT" 'cl-cc::make-vm-integer-decode-float)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "builtin-registry-constructors-are-fbound"
  (let (missing)
    (maphash (lambda (_key entry)
               (declare (ignore _key))
               (let ((ctor (cl-cc/compile::be-ctor entry)))
                 (unless (fboundp ctor)
                   (push ctor missing))))
             cl-cc/compile::*builtin-registry*)
    (expect (nreverse missing) :to-be-null)))

(it-sequential "builtin-registry-missing-returns-nil not-a-builtin"
  (destructuring-bind (name-str) (list "NOT-A-BUILTIN")
    (expect (null (gethash name-str cl-cc/compile::*builtin-registry*)) :to-be-truthy)))

(it-sequential "builtin-registry-missing-returns-nil defun"
  (destructuring-bind (name-str) (list "DEFUN")
    (expect (null (gethash name-str cl-cc/compile::*builtin-registry*)) :to-be-truthy)))

(it-sequential "builtin-registry-missing-returns-nil let"
  (destructuring-bind (name-str) (list "LET")
    (expect (null (gethash name-str cl-cc/compile::*builtin-registry*)) :to-be-truthy)))

(it-sequential "builtin-registry-binary-needs-2-args"
  (let ((entry (gethash "MOD" cl-cc/compile::*builtin-registry*)))
    (expect (cl-cc/compile::be-convention entry) :to-be :binary)))

(it-sequential "builtin-registry-bit-array-uses-ansi-ior-name"
  (let ((entry (gethash "BIT-IOR" cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-ctor entry) :to-be 'cl-cc::make-vm-bit-or)
    (expect (gethash "BIT-OR" cl-cc/compile::*builtin-registry*) :to-be-null)))

(it-sequential "builtin-registry-known-function-properties"
  (let ((char-entry (gethash "CHAR=" cl-cc/compile::*builtin-registry*))
        (princ-entry (gethash "PRINC" cl-cc/compile::*builtin-registry*)))
    (expect (member :pure (cl-cc/compile::be-properties char-entry)) :to-be-truthy)
    (expect (member :foldable (cl-cc/compile::be-properties char-entry)) :to-be-truthy)
    (expect (member :io (cl-cc/compile::be-properties princ-entry)) :to-be-truthy)))

;;; ─── Prolog Fact Tests ─────────────────────────────────────────────────────

(it-sequential "builtin-prolog-facts-registered"
  (let ((unary-rules (cl-prolog-kit:query-prolog cl-cc/compile::*builtin-rulebase*
                                             '(cl-cc/compile::builtin-unary ?sym ?ctor)))
        (binary-rules (cl-prolog-kit:query-prolog cl-cc/compile::*builtin-rulebase*
                                              '(cl-cc/compile::builtin-binary ?sym ?ctor)))
        (string-cmp-rules (cl-prolog-kit:query-prolog cl-cc/compile::*builtin-rulebase*
                                                   '(cl-cc/compile::builtin-string-cmp ?sym ?ctor)))
        (nullary-rules (cl-prolog-kit:query-prolog cl-cc/compile::*builtin-rulebase*
                                               '(cl-cc/compile::builtin-nullary ?sym ?ctor))))
    (expect (> (length unary-rules) 50) :to-be-truthy)
    (expect (> (length binary-rules) 15) :to-be-truthy)
    (expect (> (length string-cmp-rules) 10) :to-be-truthy)
    (expect (> (length nullary-rules) 3) :to-be-truthy)))

(it-sequential "builtin-prolog-fact-structure"
  (let ((car-solution (cl-prolog-kit:query-prolog-first
                       cl-cc/compile::*builtin-rulebase*
                       '(cl-cc/compile::builtin-unary car ?ctor))))
    (expect car-solution :to-be-truthy)
    (expect (cl-prolog-kit:solution-binding '?ctor car-solution) :to-be 'cl-cc/vm:make-vm-car)))

;;; ─── Emitter Dispatch Tests ────────────────────────────────────────────────

(it-sequential "builtin-emitter-table-coverage unary"
  (destructuring-bind (conv) (list :unary)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage binary"
  (destructuring-bind (conv) (list :binary)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage binary-custom"
  (destructuring-bind (conv) (list :binary-custom)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage string-cmp"
  (destructuring-bind (conv) (list :string-cmp)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage char-cmp"
  (destructuring-bind (conv) (list :char-cmp)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage table-query"
  (destructuring-bind (conv) (list :table-query)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage handle-input"
  (destructuring-bind (conv) (list :handle-input)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage side-effect"
  (destructuring-bind (conv) (list :side-effect)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage void-side-eff"
  (destructuring-bind (conv) (list :void-side-eff)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage nullary"
  (destructuring-bind (conv) (list :nullary)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage string-trim"
  (destructuring-bind (conv) (list :string-trim)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage handle-effect"
  (destructuring-bind (conv) (list :handle-effect)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage zero-compare"
  (destructuring-bind (conv) (list :zero-compare)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage stream-input-opt"
  (destructuring-bind (conv) (list :stream-input-opt)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage stream-void-opt"
  (destructuring-bind (conv) (list :stream-void-opt)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage stream-write-val"
  (destructuring-bind (conv) (list :stream-write-val)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage unary-custom"
  (destructuring-bind (conv) (list :unary-custom)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage unary-custom-void"
  (destructuring-bind (conv) (list :unary-custom-void)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage binary-move-first"
  (destructuring-bind (conv) (list :binary-move-first)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage binary-void"
  (destructuring-bind (conv) (list :binary-void)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage binary-synth-zero"
  (destructuring-bind (conv) (list :binary-synth-zero)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage unary-opt-nil"
  (destructuring-bind (conv) (list :unary-opt-nil)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage binary-opt-one"
  (destructuring-bind (conv) (list :binary-opt-one)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage binary-opt-nil-slot"
  (destructuring-bind (conv) (list :binary-opt-nil-slot)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage ternary-custom"
  (destructuring-bind (conv) (list :ternary-custom)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

(it-sequential "builtin-emitter-table-coverage ternary-opt-nil-custom"
  (destructuring-bind (conv) (list :ternary-opt-nil-custom)
    (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy)))

;;; ─── Binary-Custom Convention Tests ─────────────────────────────────────────

(it-sequential "binary-custom-entry-count"
  (let ((count 0))
    (maphash (lambda (_key entry)
               (declare (ignore _key))
               (when (eq (cl-cc/compile::be-convention entry) :binary-custom)
                 (incf count)))
             cl-cc/compile::*builtin-registry*)
    (expect count :to-equal 18)))

(it-sequential "binary-custom-entry-validation"
  (let ((slots-ok t)
        (keywords-ok t))
    (maphash (lambda (_key entry)
               (declare (ignore _key))
               (when (eq (cl-cc/compile::be-convention entry) :binary-custom)
                 (unless (and (listp (cl-cc/compile::be-slots entry))
                              (= 2 (length (cl-cc/compile::be-slots entry))))
                   (setf slots-ok nil))
                 (unless (and (keywordp (first (cl-cc/compile::be-slots entry)))
                              (keywordp (second (cl-cc/compile::be-slots entry))))
                   (setf keywords-ok nil))))
             cl-cc/compile::*builtin-registry*)
    (expect slots-ok :to-be-truthy)
    (expect keywords-ok :to-be-truthy)))

(it-sequential "binary-custom-entry-details cons"
  (destructuring-bind (name-str expected-slots) (list "CONS" '(:car-src :cdr-src))
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-equal :binary-custom)
    (expect (cl-cc/compile::be-slots entry) :to-equal expected-slots))))

(it-sequential "binary-custom-entry-details nth"
  (destructuring-bind (name-str expected-slots) (list "NTH" '(:index :list))
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-equal :binary-custom)
    (expect (cl-cc/compile::be-slots entry) :to-equal expected-slots))))

;;; ─── Zero-Compare Convention Tests ────────────────────────────────────────

(it-sequential "zero-compare-registry-entries zerop"
  (destructuring-bind (name-str expected-ctor) (list "ZEROP" 'cl-cc::make-vm-num-eq)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be :zero-compare)
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "zero-compare-registry-entries plusp"
  (destructuring-bind (name-str expected-ctor) (list "PLUSP" 'cl-cc::make-vm-gt)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be :zero-compare)
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

(it-sequential "zero-compare-registry-entries minusp"
  (destructuring-bind (name-str expected-ctor) (list "MINUSP" 'cl-cc::make-vm-lt)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be :zero-compare)
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor))))

;;; ─── Stream I/O Convention Tests ──────────────────────────────────────────

(it-sequential "stream-input-opt-registry-entries read-char"
  (destructuring-bind (name-str expected-ctor default-handle) (list "READ-CHAR" 'cl-cc::make-vm-read-char 0)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be :stream-input-opt)
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor)
    (expect (cl-cc/compile::be-slots entry) :to-equal (list default-handle)))))

(it-sequential "stream-input-opt-registry-entries read-line"
  (destructuring-bind (name-str expected-ctor default-handle) (list "READ-LINE" 'cl-cc::make-vm-read-line 0)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be :stream-input-opt)
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor)
    (expect (cl-cc/compile::be-slots entry) :to-equal (list default-handle)))))

(it-sequential "stream-void-opt-registry-entries force-output"
  (destructuring-bind (name-str expected-ctor default-handle) (list "FORCE-OUTPUT" 'cl-cc::make-vm-force-output 1)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be :stream-void-opt)
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor)
    (expect (cl-cc/compile::be-slots entry) :to-equal (list default-handle)))))

(it-sequential "stream-void-opt-registry-entries finish-output"
  (destructuring-bind (name-str expected-ctor default-handle) (list "FINISH-OUTPUT" 'cl-cc::make-vm-finish-output 1)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be :stream-void-opt)
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor)
    (expect (cl-cc/compile::be-slots entry) :to-equal (list default-handle)))))

(it-sequential "stream-void-opt-registry-entries clear-input"
  (destructuring-bind (name-str expected-ctor default-handle) (list "CLEAR-INPUT" 'cl-cc::make-vm-clear-input 0)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be :stream-void-opt)
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor)
    (expect (cl-cc/compile::be-slots entry) :to-equal (list default-handle)))))

(it-sequential "stream-write-val-registry-entries write-char"
  (destructuring-bind (name-str expected-ctor val-slot default-handle) (list "WRITE-CHAR" 'cl-cc::make-vm-write-char :char 1)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be :stream-write-val)
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor)
    (expect (cl-cc/compile::be-slots entry) :to-equal (list val-slot default-handle)))))

(it-sequential "stream-write-val-registry-entries write-byte"
  (destructuring-bind (name-str expected-ctor val-slot default-handle) (list "WRITE-BYTE" 'cl-cc::make-vm-write-byte :byte-val 1)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be :stream-write-val)
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor)
    (expect (cl-cc/compile::be-slots entry) :to-equal (list val-slot default-handle)))))

(it-sequential "stream-write-val-registry-entries write-line"
  (destructuring-bind (name-str expected-ctor val-slot default-handle) (list "WRITE-LINE" 'cl-cc::make-vm-write-line :str 1)
    (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be :stream-write-val)
    (expect (cl-cc/compile::be-ctor entry) :to-be expected-ctor)
    (expect (cl-cc/compile::be-slots entry) :to-equal (list val-slot default-handle)))))

;;; ─── Integration: Stream I/O Compilation ──────────────────────────────────

(it-sequential "stream-io-operation-behavior"
  (let ((entry (gethash "READ-CHAR" cl-cc/compile::*builtin-registry*)))
    (expect entry :to-be-truthy)
    (expect (cl-cc/compile::be-convention entry) :to-be :stream-input-opt))
  (let ((*standard-output* (make-broadcast-stream)))
    (expect (cl-cc:run-string "(write-char #\\A)") :to-equal #\A)
    (expect (cl-cc:run-string "(force-output)") :to-be-null)
    (expect (cl-cc:run-string "(clear-input)") :to-be-null)))

;;; ─── *convention-arity* completeness ─────────────────────────────────────────

(it-sequential "convention-arity-all-conventions-in-emitter-table"
  (dolist (entry cl-cc/compile::*convention-arity*)
    (let ((conv (car entry)))
      (expect (gethash conv cl-cc/compile::*builtin-emitter-table*) :to-be-truthy))))

(it-sequential "convention-arity-arg-bounds unary"
  (destructuring-bind (conv expected-min expected-max) (list :unary 1 1)
    (let ((bounds (cdr (assoc conv cl-cc/compile::*convention-arity* :test #'eq))))
    (expect (not (null bounds)) :to-be-truthy)
    (expect (= expected-min (car bounds)) :to-be-truthy)
    (expect (= expected-max (cdr bounds)) :to-be-truthy))))

(it-sequential "convention-arity-arg-bounds binary"
  (destructuring-bind (conv expected-min expected-max) (list :binary 2 2)
    (let ((bounds (cdr (assoc conv cl-cc/compile::*convention-arity* :test #'eq))))
    (expect (not (null bounds)) :to-be-truthy)
    (expect (= expected-min (car bounds)) :to-be-truthy)
    (expect (= expected-max (cdr bounds)) :to-be-truthy))))

(it-sequential "convention-arity-arg-bounds unary-opt-nil"
  (destructuring-bind (conv expected-min expected-max) (list :unary-opt-nil 0 1)
    (let ((bounds (cdr (assoc conv cl-cc/compile::*convention-arity* :test #'eq))))
    (expect (not (null bounds)) :to-be-truthy)
    (expect (= expected-min (car bounds)) :to-be-truthy)
    (expect (= expected-max (cdr bounds)) :to-be-truthy))))

(it-sequential "convention-arity-arg-bounds binary-opt-one"
  (destructuring-bind (conv expected-min expected-max) (list :binary-opt-one 1 2)
    (let ((bounds (cdr (assoc conv cl-cc/compile::*convention-arity* :test #'eq))))
    (expect (not (null bounds)) :to-be-truthy)
    (expect (= expected-min (car bounds)) :to-be-truthy)
    (expect (= expected-max (cdr bounds)) :to-be-truthy))))

(it-sequential "convention-arity-arg-bounds nullary"
  (destructuring-bind (conv expected-min expected-max) (list :nullary 0 0)
    (let ((bounds (cdr (assoc conv cl-cc/compile::*convention-arity* :test #'eq))))
    (expect (not (null bounds)) :to-be-truthy)
    (expect (= expected-min (car bounds)) :to-be-truthy)
    (expect (= expected-max (cdr bounds)) :to-be-truthy))))

(it-sequential "convention-arity-arg-bounds ternary-custom"
  (destructuring-bind (conv expected-min expected-max) (list :ternary-custom 3 3)
    (let ((bounds (cdr (assoc conv cl-cc/compile::*convention-arity* :test #'eq))))
    (expect (not (null bounds)) :to-be-truthy)
    (expect (= expected-min (car bounds)) :to-be-truthy)
    (expect (= expected-max (cdr bounds)) :to-be-truthy))))

;;; ─── emit-registered-builtin arity validation ─────────────────────────────────

(it-sequential "emit-registered-builtin-returns-nil-for-wrong-arity"
  (let* ((entry (gethash "CAR" cl-cc/compile::*builtin-registry*))  ; :unary, needs exactly 1 arg
         (ctx   (make-codegen-ctx))
         (reg   (cl-cc/compile:make-register ctx)))
    ;; Pass 0 args for a :unary convention (min=1, max=1) → should return nil
    (expect (cl-cc/compile::emit-registered-builtin entry nil reg ctx) :to-be-null)))

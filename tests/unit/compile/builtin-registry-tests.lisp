;;;; tests/unit/compile/builtin-registry-tests.lisp — Builtin Registry tests
(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Registry Lookup Tests ─────────────────────────────────────────────────

(deftest-each builtin-registry-lookup
  "Registry contains entries for representative builtins from each convention."
  :cases (("car-unary"          "CAR"              :unary)
          ("cdr-unary"          "CDR"              :unary)
          ("not-unary"          "NOT"              :unary)
          ("bswap-unary"        "BSWAP"            :unary)
          ("length-unary"       "LENGTH"           :unary)
          ("mod-binary"         "MOD"              :binary)
          ("ash-binary"         "ASH"              :binary)
          ("expt-binary"        "EXPT"             :binary)
          ("string=-str-cmp"    "STRING="          :string-cmp)
          ("char=-char-cmp"     "CHAR="            :char-cmp)
          ("ht-count-table"     "HASH-TABLE-COUNT" :table-query)
          ("read-byte-handle"   "READ-BYTE"        :handle-input)
          ("princ-side-eff"     "PRINC"            :side-effect)
          ("terpri-void"        "TERPRI"           :void-side-eff)
          ("gensym-nullary"     "GENSYM"           :nullary)
          ("string-trim-trim"   "STRING-TRIM"      :string-trim)
          ("close-handle-eff"   "CLOSE"            :handle-effect))
  (name-str expected-conv)
  (let ((entry (gethash name-str cl-cc::*builtin-registry*)))
    (assert-true entry)
    (assert-true (eq expected-conv (cl-cc::be-convention entry)))))

(deftest-each builtin-registry-constructor-symbols
  "Registry entries have correct constructor symbols."
  :cases (("car"   "CAR"   'cl-cc::make-vm-car)
          ("bswap" "BSWAP" 'cl-cc::make-vm-bswap)
          ("mod"   "MOD"   'cl-cc::make-vm-mod)
          ("princ" "PRINC" 'cl-cc::make-vm-princ))
  (name-str expected-ctor)
  (let ((entry (gethash name-str cl-cc::*builtin-registry*)))
    (assert-true (eq expected-ctor (cl-cc::be-ctor entry)))))

(deftest-each builtin-registry-missing-returns-nil
  "Looking up a non-builtin name returns NIL."
  :cases (("not-a-builtin" "NOT-A-BUILTIN")
          ("defun"         "DEFUN")
          ("let"           "LET"))
  (name-str)
  (assert-true (null (gethash name-str cl-cc::*builtin-registry*))))

(deftest builtin-registry-binary-needs-2-args
  "Binary convention entries have :binary convention."
  (let ((entry (gethash "MOD" cl-cc::*builtin-registry*)))
    (assert-true (eq :binary (cl-cc::be-convention entry)))))

;;; ─── Prolog Fact Tests ─────────────────────────────────────────────────────

(deftest builtin-prolog-facts-registered
  "Prolog facts are registered for each builtin convention."
  (let ((unary-rules (gethash 'cl-cc::builtin-unary cl-cc::*prolog-rules*))
        (binary-rules (gethash 'cl-cc::builtin-binary cl-cc::*prolog-rules*))
        (string-cmp-rules (gethash 'cl-cc::builtin-string-cmp cl-cc::*prolog-rules*))
        (nullary-rules (gethash 'cl-cc::builtin-nullary cl-cc::*prolog-rules*)))
    (assert-true (> (length unary-rules) 50))
    (assert-true (> (length binary-rules) 15))
    (assert-true (> (length string-cmp-rules) 10))
    (assert-true (> (length nullary-rules) 3))))

(deftest builtin-prolog-fact-structure
  "Prolog facts have correct (predicate cl-sym vm-ctor) structure."
  (let* ((unary-rules (gethash 'cl-cc::builtin-unary cl-cc::*prolog-rules*))
         (car-fact (find-if (lambda (r)
                              (let ((head (cl-cc::rule-head r)))
                                (and (= (length head) 3)
                                     (eq (second head) 'cl-cc::car))))
                            unary-rules)))
    (assert-true car-fact)
    (let ((head (cl-cc::rule-head car-fact)))
      (assert-true (eq (first head) 'cl-cc::builtin-unary))
      (assert-true (eq (second head) 'cl-cc::car))
      (assert-true (eq (third head) 'cl-cc::make-vm-car)))))

;;; ─── Emitter Dispatch Tests ────────────────────────────────────────────────

(deftest-each builtin-emitter-table-coverage
  "Every convention has an emitter in the dispatch table."
  :cases (("unary"          :unary)
          ("binary"         :binary)
          ("binary-custom"  :binary-custom)
          ("string-cmp"     :string-cmp)
          ("char-cmp"       :char-cmp)
          ("table-query"    :table-query)
          ("handle-input"   :handle-input)
          ("side-effect"    :side-effect)
          ("void-side-eff"  :void-side-eff)
          ("nullary"        :nullary)
          ("string-trim"    :string-trim)
          ("handle-effect"  :handle-effect)
          ("zero-compare"   :zero-compare)
          ("stream-input-opt"  :stream-input-opt)
          ("stream-void-opt"   :stream-void-opt)
          ("stream-write-val"  :stream-write-val)
          ;; 2026 conventions
          ("unary-custom"           :unary-custom)
          ("unary-custom-void"      :unary-custom-void)
          ("binary-move-first"      :binary-move-first)
          ("binary-void"            :binary-void)
          ("binary-synth-zero"      :binary-synth-zero)
          ("unary-opt-nil"          :unary-opt-nil)
          ("binary-opt-one"         :binary-opt-one)
          ("binary-opt-nil-slot"    :binary-opt-nil-slot)
          ("ternary-custom"         :ternary-custom)
          ("ternary-opt-nil-custom" :ternary-opt-nil-custom))
  (conv)
  (assert-true (gethash conv cl-cc::*builtin-emitter-table*)))

;;; ─── Binary-Custom Convention Tests ─────────────────────────────────────────

(deftest binary-custom-entry-count
  "There are exactly 17 binary-custom entries in the registry."
  (let ((count 0))
    (maphash (lambda (_key entry)
               (declare (ignore _key))
               (when (eq (cl-cc::be-convention entry) :binary-custom)
                 (incf count)))
             cl-cc::*builtin-registry*)
    (assert-equal 17 count)))

(deftest binary-custom-entry-validation
  "Every binary-custom entry has a 2-element slots list with keyword slot names."
  (let ((slots-ok t)
        (keywords-ok t))
    (maphash (lambda (_key entry)
               (declare (ignore _key))
               (when (eq (cl-cc::be-convention entry) :binary-custom)
                 (unless (and (listp (cl-cc::be-slots entry))
                              (= 2 (length (cl-cc::be-slots entry))))
                   (setf slots-ok nil))
                 (unless (and (keywordp (first (cl-cc::be-slots entry)))
                              (keywordp (second (cl-cc::be-slots entry))))
                   (setf keywords-ok nil))))
             cl-cc::*builtin-registry*)
    (assert-true slots-ok)
    (assert-true keywords-ok)))

(deftest-each binary-custom-entry-details
  "CONS and NTH binary-custom slots are registered with the correct convention and slot names."
  :cases (("cons" "CONS" '(:car-src :cdr-src))
          ("nth"  "NTH"  '(:index :list)))
  (name-str expected-slots)
  (let ((entry (gethash name-str cl-cc::*builtin-registry*)))
    (assert-true entry)
    (assert-equal :binary-custom (cl-cc::be-convention entry))
    (assert-equal expected-slots (cl-cc::be-slots entry))))

;;; ─── Zero-Compare Convention Tests ────────────────────────────────────────

(deftest-each zero-compare-registry-entries
  "Zero-compare entries are correctly registered."
  :cases (("zerop"  "ZEROP"  'cl-cc::make-vm-num-eq)
          ("plusp"   "PLUSP"  'cl-cc::make-vm-gt)
          ("minusp"  "MINUSP" 'cl-cc::make-vm-lt))
  (name-str expected-ctor)
  (let ((entry (gethash name-str cl-cc::*builtin-registry*)))
    (assert-true entry)
    (assert-eq :zero-compare (cl-cc::be-convention entry))
    (assert-eq expected-ctor (cl-cc::be-ctor entry))))

;;; ─── Stream I/O Convention Tests ──────────────────────────────────────────

(deftest-each stream-input-opt-registry-entries
  "Stream-input-opt entries have correct convention and default handle."
  :cases (("read-char"  "READ-CHAR"  'cl-cc::make-vm-read-char  0)
          ("read-line"  "READ-LINE"  'cl-cc::make-vm-read-line  0))
  (name-str expected-ctor default-handle)
  (let ((entry (gethash name-str cl-cc::*builtin-registry*)))
    (assert-true entry)
    (assert-eq :stream-input-opt (cl-cc::be-convention entry))
    (assert-eq expected-ctor (cl-cc::be-ctor entry))
    (assert-equal (list default-handle) (cl-cc::be-slots entry))))

(deftest-each stream-void-opt-registry-entries
  "Stream-void-opt entries have correct convention and default handle."
  :cases (("force-output"  "FORCE-OUTPUT"  'cl-cc::make-vm-force-output  1)
          ("finish-output" "FINISH-OUTPUT" 'cl-cc::make-vm-finish-output 1)
          ("clear-input"   "CLEAR-INPUT"   'cl-cc::make-vm-clear-input   0))
  (name-str expected-ctor default-handle)
  (let ((entry (gethash name-str cl-cc::*builtin-registry*)))
    (assert-true entry)
    (assert-eq :stream-void-opt (cl-cc::be-convention entry))
    (assert-eq expected-ctor (cl-cc::be-ctor entry))
    (assert-equal (list default-handle) (cl-cc::be-slots entry))))

(deftest-each stream-write-val-registry-entries
  "Stream-write-val entries have correct convention, value slot, and default handle."
  :cases (("write-char" "WRITE-CHAR" 'cl-cc::make-vm-write-char :char     1)
          ("write-byte" "WRITE-BYTE" 'cl-cc::make-vm-write-byte :byte-val 1)
          ("write-line" "WRITE-LINE" 'cl-cc::make-vm-write-line :str      1))
  (name-str expected-ctor val-slot default-handle)
  (let ((entry (gethash name-str cl-cc::*builtin-registry*)))
    (assert-true entry)
    (assert-eq :stream-write-val (cl-cc::be-convention entry))
    (assert-eq expected-ctor (cl-cc::be-ctor entry))
    (assert-equal (list val-slot default-handle) (cl-cc::be-slots entry))))

;;; ─── Integration: Stream I/O Compilation ──────────────────────────────────

(deftest stream-io-operation-behavior
  "Stream I/O: read-char resolves via registry; write-char returns value; force-output and clear-input return nil."
  (let ((entry (gethash "READ-CHAR" cl-cc::*builtin-registry*)))
    (assert-true entry)
    (assert-eq :stream-input-opt (cl-cc::be-convention entry)))
  (assert-equal #\A (cl-cc:run-string "(write-char #\\A)"))
  (assert-null (cl-cc:run-string "(force-output)"))
  (assert-null (cl-cc:run-string "(clear-input)")))

;;; ─── *convention-arity* completeness ─────────────────────────────────────────

(deftest convention-arity-all-conventions-in-emitter-table
  "Every convention in *convention-arity* has an emitter registered."
  (dolist (entry cl-cc::*convention-arity*)
    (let ((conv (car entry)))
      (assert-true (gethash conv cl-cc::*builtin-emitter-table*)))))

(deftest-each convention-arity-arg-bounds
  "Selected conventions have the expected (min . max) arg counts."
  :cases (("unary"          :unary          1 1)
          ("binary"         :binary         2 2)
          ("unary-opt-nil"  :unary-opt-nil  0 1)
          ("binary-opt-one" :binary-opt-one 1 2)
          ("nullary"        :nullary        0 0)
          ("ternary-custom" :ternary-custom 3 3))
  (conv expected-min expected-max)
  (let ((bounds (cdr (assoc conv cl-cc::*convention-arity* :test #'eq))))
    (assert-true (not (null bounds)))
    (assert-= expected-min (car bounds))
    (assert-= expected-max (cdr bounds))))

;;; ─── emit-registered-builtin arity validation ─────────────────────────────────

(deftest emit-registered-builtin-returns-nil-for-wrong-arity
  "emit-registered-builtin returns nil when arg count is outside convention bounds."
  (let* ((entry (gethash "CAR" cl-cc::*builtin-registry*))  ; :unary, needs exactly 1 arg
         (ctx   (make-codegen-ctx))
         (reg   (cl-cc::make-register ctx)))
    ;; Pass 0 args for a :unary convention (min=1, max=1) → should return nil
    (assert-null (cl-cc::emit-registered-builtin entry nil reg ctx))))

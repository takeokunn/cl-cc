;;;; tests/unit/compile/builtin-registry-tests.lisp — Builtin Registry tests
(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Registry Lookup Tests ─────────────────────────────────────────────────

(deftest-each builtin-registry-lookup
  "Registry contains entries for representative builtins from each convention."
  :cases (("car-unary"          "CAR"              :unary)
          ("cdr-unary"          "CDR"              :unary)
          ("not-unary"          "NOT"              :unary)
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

(deftest builtin-registry-constructor-symbols
  "Registry entries have correct constructor symbols."
  (let ((car-entry (gethash "CAR" cl-cc::*builtin-registry*)))
    (assert-true (eq 'cl-cc::make-vm-car (cl-cc::be-ctor car-entry))))
  (let ((mod-entry (gethash "MOD" cl-cc::*builtin-registry*)))
    (assert-true (eq 'cl-cc::make-vm-mod (cl-cc::be-ctor mod-entry))))
  (let ((princ-entry (gethash "PRINC" cl-cc::*builtin-registry*)))
    (assert-true (eq 'cl-cc::make-vm-princ (cl-cc::be-ctor princ-entry)))))

(deftest builtin-registry-missing-returns-nil
  "Looking up a non-builtin returns NIL."
  (assert-true (null (gethash "NOT-A-BUILTIN" cl-cc::*builtin-registry*)))
  (assert-true (null (gethash "DEFUN" cl-cc::*builtin-registry*)))
  (assert-true (null (gethash "LET" cl-cc::*builtin-registry*))))

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
          ("stream-write-val"  :stream-write-val))
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

(deftest binary-custom-entries-have-slots
  "Every binary-custom entry in the registry has a 2-element slots list."
  (let ((all-ok t))
    (maphash (lambda (_key entry)
               (declare (ignore _key))
               (when (eq (cl-cc::be-convention entry) :binary-custom)
                 (unless (and (listp (cl-cc::be-slots entry))
                              (= 2 (length (cl-cc::be-slots entry))))
                   (setf all-ok nil))))
             cl-cc::*builtin-registry*)
    (assert-true all-ok)))

(deftest binary-custom-slot-names-are-keywords
  "All slot names in binary-custom entries are keywords."
  (let ((all-ok t))
    (maphash (lambda (_key entry)
               (declare (ignore _key))
               (when (eq (cl-cc::be-convention entry) :binary-custom)
                 (unless (and (keywordp (first (cl-cc::be-slots entry)))
                              (keywordp (second (cl-cc::be-slots entry))))
                   (setf all-ok nil))))
             cl-cc::*builtin-registry*)
    (assert-true all-ok)))

(deftest binary-custom-cons-entry
  "cons is registered as binary-custom with :car-src/:cdr-src slots."
  (let ((entry (gethash "CONS" cl-cc::*builtin-registry*)))
    (assert-true entry)
    (assert-equal :binary-custom (cl-cc::be-convention entry))
    (assert-equal '(:car-src :cdr-src) (cl-cc::be-slots entry))))

(deftest binary-custom-nth-entry
  "nth is registered as binary-custom with :index/:list slots."
  (let ((entry (gethash "NTH" cl-cc::*builtin-registry*)))
    (assert-true entry)
    (assert-equal :binary-custom (cl-cc::be-convention entry))
    (assert-equal '(:index :list) (cl-cc::be-slots entry))))

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

(deftest stream-io-read-char-compiles
  "read-char is resolved through the registry (stream-input-opt convention)."
  (let ((entry (gethash "READ-CHAR" cl-cc::*builtin-registry*)))
    (assert-true entry)
    (assert-eq :stream-input-opt (cl-cc::be-convention entry))))

(deftest stream-io-write-char-returns-value
  "write-char returns its character argument."
  (assert-equal #\A (cl-cc:run-string "(write-char #\\A)")))

(deftest stream-io-force-output-returns-nil
  "force-output returns nil."
  (assert-null (cl-cc:run-string "(force-output)")))

(deftest stream-io-clear-input-returns-nil
  "clear-input returns nil."
  (assert-null (cl-cc:run-string "(clear-input)")))

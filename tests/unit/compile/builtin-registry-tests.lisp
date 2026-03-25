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
  :cases (("unary"        :unary)
          ("binary"       :binary)
          ("string-cmp"   :string-cmp)
          ("char-cmp"     :char-cmp)
          ("table-query"  :table-query)
          ("handle-input" :handle-input)
          ("side-effect"  :side-effect)
          ("void-side-eff" :void-side-eff)
          ("nullary"      :nullary)
          ("string-trim"  :string-trim)
          ("handle-effect" :handle-effect))
  (conv)
  (assert-true (gethash conv cl-cc::*builtin-emitter-table*)))

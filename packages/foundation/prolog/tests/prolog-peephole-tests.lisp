(in-package :cl-cc/test)

(in-suite cl-cc-coverage-unstable-unit-suite)

(defmacro assert-prolog-peephole-equal (instructions expected)
  (let ((result (gensym "RESULT")))
    `(let ((,result (cl-cc:apply-prolog-peephole ,instructions)))
       (assert-equal ,expected ,result))))

(defmacro assert-prolog-peephole-length= (instructions expected-length)
  (let ((result (gensym "RESULT")))
    `(let ((,result (cl-cc:apply-prolog-peephole ,instructions)))
       (assert-= ,expected-length (length ,result)))))

(defmacro assert-prolog-peephole-not-contains (instructions pattern)
  (let ((result (gensym "RESULT")))
    `(let ((,result (cl-cc:apply-prolog-peephole ,instructions)))
       (assert-false (member ,pattern ,result :test #'equal)))))

(defmacro assert-prolog-peephole-case (input expected)
  `(assert-prolog-peephole-equal ,input ,expected))

(deftest prolog-type-of-integer-const
  "type-of/3: integer constant has type (integer-type)."
  (let ((result (cl-cc:query-one '(type-of (const 42) nil ?t))))
    (assert-true result)
    (assert-equal '(type-of (const 42) nil (integer-type)) result)))

(deftest-each prolog-type-of-operation-types
  "type-of/3 resolves operation types: binop → integer-type, cmp → boolean-type."
  :cases (("binop" '(type-of (binop + (const 1) (const 2)) nil ?t) '(integer-type))
          ("cmp"   '(type-of (cmp < (const 1) (const 2)) nil ?t)   '(boolean-type)))
  (goal expected-type)
  (let ((result (cl-cc:query-one goal)))
    (assert-true result)
    (assert-equal expected-type (nth 3 result))))

(deftest-each prolog-peephole-equality-cases
  "Peephole: const-move fusion, passthrough, single-instruction identity, and jump-chain shortening."
  :cases (("const-move-fusion"   '((:const :r0 42) (:move :r1 :r0)) '((:const :r1 42)))
          ("passthrough"         '((:add :r2 :r0 :r1))              '((:add :r2 :r0 :r1)))
          ("single-instruction"  '((:const :r0 7))                  '((:const :r0 7)))
          ("jump-chain-shortest" '((:jump "L1") (:jump "L2"))       '((:jump "L1"))))
  (input expected)
  (assert-prolog-peephole-equal input expected))

(deftest prolog-peephole-self-move-removal
  "Peephole: (:move :r0 :r0) is eliminated from the output."
  (assert-prolog-peephole-not-contains '((:move :r0 :r0) (:const :r1 1)) '(:move :r0 :r0)))

(deftest-each prolog-peephole-arithmetic-identities
  "Peephole: local arithmetic and comparison identities preserve the next instruction."
  :cases (("add-zero"      '((:add :r2 :r0 0) (:const :r3 1))    '((:move :r2 :r0) (:const :r3 1)))
          ("sub-from-zero" '((:sub :r4 0 :r1) (:const :r5 2))    '((:neg :r4 :r1) (:const :r5 2)))
          ("sub-same"      '((:sub :r4 :r1 :r1) (:const :r5 2))  '((:const :r4 0) (:const :r5 2)))
          ("mul-zero"      '((:mul :r6 :r1 0) (:const :r7 3))    '((:const :r6 0) (:const :r7 3)))
          ("div-one"       '((:div :r8 :r2 1) (:const :r9 4))    '((:move :r8 :r2) (:const :r9 4)))
          ("logand-all"    '((:logand :r10 :r3 -1) (:const :r11 5)) '((:move :r10 :r3) (:const :r11 5)))
          ("logand-zero"   '((:logand :r10 :r3 0) (:const :r11 5))  '((:const :r10 0) (:const :r11 5)))
          ("logior-all"    '((:logior :r10 :r3 -1) (:const :r11 5)) '((:const :r10 -1) (:const :r11 5)))
          ("num-eq-same"   '((:num-eq :r12 :r4 :r4) (:const :r13 6)) '((:const :r12 1) (:const :r13 6)))
          ("logxor-same"   '((:logxor :r14 :r5 :r5) (:const :r15 7)) '((:const :r14 0) (:const :r15 7))))
  (input expected)
  (assert-prolog-peephole-case input expected))

(deftest-each prolog-peephole-same-reg-identities
  "Peephole: same-register comparison/bitwise identities collapse locally."
  :cases (("eq"     '((:eq :r0 :r1 :r1) (:const :r2 1))         '((:const :r0 1) (:const :r2 1)))
          ("gt"     '((:gt :r3 :r4 :r4) (:const :r5 2))         '((:const :r3 0) (:const :r5 2)))
          ("le"     '((:le :r6 :r7 :r7) (:const :r8 3))         '((:const :r6 1) (:const :r8 3)))
          ("logand" '((:logand :r9 :r10 :r10) (:const :r11 4))  '((:move :r9 :r10) (:const :r11 4)))
          ("logior" '((:logior :r12 :r13 :r13) (:const :r14 5)) '((:move :r12 :r13) (:const :r14 5))))
  (input expected)
  (assert-prolog-peephole-case input expected))

(deftest-each prolog-peephole-negated-comparisons
  "Peephole: compare followed by logical not collapses to the inverse comparison."
  :cases (("lt->ge" '((:lt :r0 :r1 :r2) (:not :r3 :r0))      '((:ge :r3 :r1 :r2)))
          ("gt->le" '((:gt :r4 :r5 :r6) (:not :r7 :r4))      '((:le :r7 :r5 :r6)))
          ("le->gt" '((:le :r8 :r9 :r10) (:not :r11 :r8))    '((:gt :r11 :r9 :r10)))
          ("ge->lt" '((:ge :r12 :r13 :r14) (:not :r15 :r12)) '((:lt :r15 :r13 :r14))))
  (input expected)
  (assert-prolog-peephole-case input expected))

(deftest prolog-peephole-empty-input
  "Peephole: empty instruction list returns empty."
  (assert-null (cl-cc:apply-prolog-peephole nil)))

(deftest prolog-peephole-multiple-pairs
  "Peephole: matching windows are rewritten in a single left-to-right pass; non-overlapping leftovers pass through."
  (let ((result (cl-cc:apply-prolog-peephole '((:const :r0 1) (:move :r1 :r0) (:add :r2 :r1 :r1)))))
    (assert-true (member (length result) '(2 3)))
    (assert-equal '(:const :r1 1) (first result))
    (assert-equal '(:add :r2 :r1 :r1) (second result))))

(deftest-each prolog-peephole-single-pair-reduction
  "Peephole rules that collapse a 2-instruction window into a single instruction."
  :cases (("jump-to-label" '((:jump "L0") (:label "L0")) '(:label "L0"))
          ("double-const"  '((:const :r0 1) (:const :r0 99)) '(:const :r0 99)))
  (input expected-first)
  (assert-prolog-peephole-length= input 1)
  (assert-equal expected-first (first (cl-cc:apply-prolog-peephole input))))

(deftest-each prolog-peephole-terminal-sequence-rules
  "Peephole rule 5: in any terminal-instruction pair, only the first instruction is kept."
  :cases (("jump-before-ret"   '((:jump "L1") (:ret  :r0)) '((:jump "L1")))
          ("jump-before-halt"  '((:jump "L1") (:halt :r0)) '((:jump "L1")))
          ("ret-before-jump"   '((:ret  :r0) (:jump "L1")) '((:ret  :r0)))
          ("halt-before-jump"  '((:halt :r0) (:jump "L1")) '((:halt :r0)))
          ("ret-before-ret"    '((:ret  :r0) (:ret  :r1)) '((:ret  :r0)))
          ("halt-before-halt"  '((:halt :r0) (:halt :r1)) '((:halt :r0)))
          ("ret-before-halt"   '((:ret  :r0) (:halt :r1)) '((:ret  :r0)))
          ("halt-before-ret"   '((:halt :r0) (:ret  :r1)) '((:halt :r0))))
  (input expected)
  (assert-prolog-peephole-equal input expected))

(deftest prolog-peephole-move-chain-propagates-source
  "Peephole rule 4: (:move :r1 :r0)(:move :r2 :r1) — source propagated to end of chain."
  (let ((result (cl-cc:apply-prolog-peephole '((:move :r1 :r0) (:move :r2 :r1)))))
    (assert-= 2 (length result))
    (assert-equal '(:move :r1 :r0) (first result))
    (assert-equal '(:move :r2 :r0) (second result))))

(deftest-each prolog-peephole-pair-preserved
  "Peephole: instruction pairs with different registers/labels are never merged."
  :cases (("jump-label-mismatch"   '((:jump "L1") (:label "L0")))
          ("const-different-regs"  '((:const :r0 1) (:const :r1 2)))
          ("move-different-source" '((:move :r1 :r0) (:move :r3 :r2))))
  (insts)
  (assert-prolog-peephole-length= insts 2))

;;; %peephole-walk unit tests (internal helper)

(deftest-each peephole-walk-direct-cases
  "%peephole-walk: nil→nil; singleton→singleton; non-matching pair passes through."
  :cases (("nil"       nil                              nil)
          ("singleton" '((:const :r0 1))                '((:const :r0 1)))
          ("no-match"  '((:add :r1 :r0 :r0) (:sub :r2 :r1 :r1))
                       '((:add :r1 :r0 :r0) (:sub :r2 :r1 :r1))))
  (input expected)
  (assert-equal expected (cl-cc/prolog::%peephole-walk input nil)))

(deftest peephole-walk-matching-pair-fused
  "%peephole-walk: a matching const→move pair is fused into a single const."
  (let ((result (cl-cc/prolog::%peephole-walk '((:const :r0 99) (:move :r1 :r0)) nil)))
    (assert-= 1 (length result))
    (assert-equal '(:const :r1 99) (first result))))

(deftest peephole-walk-out-accumulator-is-prepended
  "%peephole-walk: instructions in OUT are prepended (reversed) to the result."
  (let ((result (cl-cc/prolog::%peephole-walk '((:const :r0 1)) '((:add :r2 :r1 :r1)))))
    (assert-= 2 (length result))
    (assert-equal '(:add :r2 :r1 :r1) (first result))
    (assert-equal '(:const :r0 1)     (second result))))

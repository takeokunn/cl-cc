;;;; packages/binary/tests/got-plt-tests.lisp — GOT/PLT section generation tests
;;;;
;;;; Tests for: add-plt-stubs, add-got-entries, add-dynamic-relocations,
;;;; bind-now-mode, setup-got-plt (got-plt.lisp).
;;;;
;;;; PLT layout (System V AMD64 ABI):
;;;;   PLT[0]  16 bytes: resolver stub  (ff 35 ... ff 25 ...)
;;;;   PLT[n]  16 bytes each: jmpq *GOT[n+3](%rip); push $n; jmpq PLT[0]
;;;;
;;;; GOT layout (.got.plt):
;;;;   GOT[0..2]  reserved (3 × 8 bytes = 24 bytes)
;;;;   GOT[3+i]   per-symbol slot (8 bytes each), filled lazily by ld.so

(in-package :cl-cc/test)

(defsuite got-plt-suite
  :description "GOT/PLT section byte generation (got-plt.lisp)"
  :parent cl-cc-unit-suite)

(in-suite got-plt-suite)

;;; ─── bind-now-mode ───────────────────────────────────────────────────────

(deftest got-plt-bind-now-mode-returns-df-bind-now
  "bind-now-mode returns the DF_BIND_NOW flag value (#x8)."
  (assert-equal cl-cc/binary::+df-bind-now+
                (cl-cc/binary::bind-now-mode)))

;;; ─── add-plt-stubs ───────────────────────────────────────────────────────

(deftest got-plt-add-plt-stubs-empty-returns-16-bytes
  "add-plt-stubs with no symbols returns only the 16-byte PLT[0] resolver."
  (let ((plt (cl-cc/binary::add-plt-stubs '())))
    (assert-equal 16 (length plt))))

(deftest got-plt-add-plt-stubs-one-symbol-returns-32-bytes
  "add-plt-stubs with one symbol: PLT[0] (16 bytes) + PLT[1] (16 bytes) = 32."
  (let ((plt (cl-cc/binary::add-plt-stubs '("foo"))))
    (assert-equal 32 (length plt))))

(deftest got-plt-add-plt-stubs-n-symbols-correct-size
  "add-plt-stubs with N symbols returns 16*(1+N) bytes."
  (dolist (n '(0 1 2 5 10))
    (let ((syms (loop repeat n collect "sym")))
      (assert-equal (* 16 (1+ n))
                    (length (cl-cc/binary::add-plt-stubs syms))))))

(deftest got-plt-add-plt-stubs-plt0-resolver-opcodes
  "PLT[0] starts with pushq (%rip+disp32): opcode bytes ff 35."
  (let* ((plt (cl-cc/binary::add-plt-stubs '()))
         (b0 (aref plt 0))
         (b1 (aref plt 1)))
    (assert-equal #xff b0)
    (assert-equal #x35 b1)))

(deftest got-plt-add-plt-stubs-entry-jmpq-opcode
  "PLT[1] starts with jmpq *GOT(%rip): opcode bytes ff 25."
  (let* ((plt (cl-cc/binary::add-plt-stubs '("foo")))
         (b16 (aref plt 16))
         (b17 (aref plt 17)))
    (assert-equal #xff b16)
    (assert-equal #x25 b17)))

(deftest got-plt-add-plt-stubs-entry-push-index
  "PLT[n] encodes push $0 at offset 22 for the first symbol (index 0)."
  (let* ((plt (cl-cc/binary::add-plt-stubs '("foo")))
         ;; push $0: opcode 68, then 4-byte little-endian index 0
         (push-opcode (aref plt 22))
         (index-lo    (aref plt 23)))
    (assert-equal #x68 push-opcode)
    (assert-equal 0    index-lo)))

(deftest got-plt-add-plt-stubs-second-entry-push-index-1
  "PLT[2] encodes push $1 for the second symbol (index 1)."
  (let* ((plt (cl-cc/binary::add-plt-stubs '("foo" "bar")))
         (push-opcode (aref plt 38))   ; 16 + 16 + 6 = 38
         (index-lo    (aref plt 39)))
    (assert-equal #x68 push-opcode)
    (assert-equal 1    index-lo)))

;;; ─── add-got-entries ─────────────────────────────────────────────────────

(deftest got-plt-add-got-entries-zero-symbols-has-3-reserved
  "add-got-entries with 0 symbols: 3 reserved slots × 8 bytes = 24 bytes."
  (let ((got (cl-cc/binary::add-got-entries 0)))
    (assert-equal 24 (length got))))

(deftest got-plt-add-got-entries-n-symbols-correct-size
  "add-got-entries with N symbols: (3+N) × 8 bytes."
  (dolist (n '(0 1 2 5 10))
    (assert-equal (* 8 (+ 3 n))
                  (length (cl-cc/binary::add-got-entries n)))))

(deftest got-plt-add-got-entries-all-zeros
  "All GOT slots are initialized to zero (filled lazily by ld.so)."
  (let ((got (cl-cc/binary::add-got-entries 2)))
    (assert-true (every #'zerop got))))

;;; ─── add-dynamic-relocations ─────────────────────────────────────────────

(deftest got-plt-add-dynamic-relocations-empty-returns-empty
  "add-dynamic-relocations with no symbols returns an empty vector."
  (let ((rela (cl-cc/binary::add-dynamic-relocations '() 0)))
    (assert-equal 0 (length rela))))

(deftest got-plt-add-dynamic-relocations-one-symbol-24-bytes
  "One symbol → one Elf64_Rela (24 bytes: r_offset u64 + r_info u64 + r_addend s64)."
  (let ((rela (cl-cc/binary::add-dynamic-relocations '("foo") 0)))
    (assert-equal 24 (length rela))))

(deftest got-plt-add-dynamic-relocations-n-symbols-correct-size
  "N symbols → N × 24-byte Elf64_Rela entries."
  (dolist (n '(1 2 5))
    (let ((syms (loop repeat n collect "sym")))
      (assert-equal (* 24 n)
                    (length (cl-cc/binary::add-dynamic-relocations syms 0))))))

(deftest got-plt-add-dynamic-relocations-r-info-type-is-jump-slot
  "r_info low 32 bits = R_X86_64_JUMP_SLOT (7) for the first symbol."
  ;; r_info is at bytes 8-15 of the first Rela entry (little-endian)
  (let* ((rela (cl-cc/binary::add-dynamic-relocations '("foo") 0))
         ;; Low 32 bits of r_info: bytes 8-11
         (r-type (logior (aref rela 8)
                         (ash (aref rela 9)  8)
                         (ash (aref rela 10) 16)
                         (ash (aref rela 11) 24))))
    (assert-equal cl-cc/binary::+r-x86-64-jump-slot+ r-type)))

(deftest got-plt-add-dynamic-relocations-r-offset-targets-got3
  "r_offset for symbol 0 targets GOT[3] = got-plt-addr + 24."
  (let* ((got-base #x1000)
         (rela (cl-cc/binary::add-dynamic-relocations '("foo") got-base))
         ;; r_offset: bytes 0-7 of first entry (little-endian u64)
         (r-offset (logior (aref rela 0)
                           (ash (aref rela 1) 8)
                           (ash (aref rela 2) 16)
                           (ash (aref rela 3) 24))))
    ;; GOT[3] = got-base + 3*8 = #x1018
    (assert-equal (+ got-base 24) r-offset)))

;;; ─── setup-got-plt (integration) ────────────────────────────────────────

(deftest got-plt-setup-returns-four-values
  "setup-got-plt returns exactly four values."
  (multiple-value-bind (plt got rela bind-now)
      (cl-cc/binary::setup-got-plt '("foo" "bar"))
    (assert-true (typep plt  '(simple-array (unsigned-byte 8) (*))))
    (assert-true (typep got  '(simple-array (unsigned-byte 8) (*))))
    (assert-true (typep rela '(simple-array (unsigned-byte 8) (*))))
    (assert-true bind-now)))

(deftest got-plt-setup-sizes-are-consistent
  "setup-got-plt sizes agree with per-function invariants for N=3 symbols."
  (let ((syms '("malloc" "free" "puts")))
    (multiple-value-bind (plt got rela _)
        (cl-cc/binary::setup-got-plt syms)
      (declare (ignore _))
      (assert-equal (* 16 (1+ (length syms))) (length plt))
      (assert-equal (* 8  (+ 3 (length syms))) (length got))
      (assert-equal (* 24 (length syms)) (length rela)))))

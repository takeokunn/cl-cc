;;;; packages/testing-framework/tests/framework-assertions-tests.lisp
;;;; Unit tests for composite assertion macros:
;;;;   assert-bool, assert-list-contains, assert-bitfield

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── assert-bool ─────────────────────────────────────────────────────────────

(deftest assert-bool-truthy-passes-when-form-truthy
  "assert-bool with a truthy expected passes when the form returns a truthy value."
  (assert-bool t (= 1 1)))

(deftest assert-bool-truthy-fails-when-form-falsy
  "assert-bool with a truthy expected signals test-failure when form is nil."
  (assert-signals test-failure (assert-bool t nil)))

(deftest assert-bool-falsy-passes-when-form-nil
  "assert-bool with a falsy expected passes when the form returns nil."
  (assert-bool nil nil))

(deftest assert-bool-falsy-passes-when-expected-nil
  "assert-bool with a nil expected also passes when form evaluates to nil."
  (assert-bool nil (= 1 2)))

(deftest assert-bool-falsy-fails-when-form-truthy
  "assert-bool with a falsy expected signals test-failure when form is truthy."
  (assert-signals test-failure (assert-bool nil t)))

(deftest assert-bool-truthy-with-numeric-expected
  "assert-bool treats any non-nil expected value as truthy."
  (assert-bool 42 :some-value))

;;; ─── assert-list-contains ─────────────────────────────────────────────────────

(deftest assert-list-contains-single-member
  "assert-list-contains passes when the list contains the expected member."
  (assert-list-contains '(a b c) '(b)))

(deftest assert-list-contains-multiple-members
  "assert-list-contains passes when all listed members are present."
  (assert-list-contains '(x y z) '(x z)))

(deftest assert-list-contains-with-correct-length
  "assert-list-contains passes when members and length both match."
  (assert-list-contains '(a b) '(a b) :length 2))

(deftest assert-list-contains-missing-member-fails
  "assert-list-contains signals test-failure when a member is absent."
  (assert-signals test-failure
    (assert-list-contains '(a b c) '(d))))

(deftest assert-list-contains-wrong-length-fails
  "assert-list-contains signals test-failure when length does not match."
  (assert-signals test-failure
    (assert-list-contains '(a b c) '(a) :length 2)))

(deftest assert-list-contains-empty-members-always-passes
  "assert-list-contains with an empty members list always passes."
  (assert-list-contains '(a b c) nil))

(deftest assert-list-contains-works-with-symbols-and-keywords
  "assert-list-contains works with keywords and symbols."
  (assert-list-contains '(:return :capture :throw) '(:return :capture) :length 3))

;;; ─── assert-bitfield ──────────────────────────────────────────────────────────

(deftest assert-bitfield-single-field-passes
  "assert-bitfield with a single matching field passes."
  (let ((word #xFF000000))
    (assert-bitfield word (24 8 #xFF))))

(deftest assert-bitfield-multiple-fields-pass
  "assert-bitfield verifies multiple byte fields in a single word."
  (let ((word (logior (ash #x10 24) (ash #x01 16) (ash #x02 8) #x03)))
    (assert-bitfield word (24 8 #x10) (16 8 #x01) (8 8 #x02) (0 8 #x03))))

(deftest assert-bitfield-wrong-field-fails
  "assert-bitfield signals test-failure when a field value does not match."
  (let ((word #xFF000000))
    (assert-signals test-failure
      (assert-bitfield word (24 8 #xFE)))))

(deftest assert-bitfield-zero-field-passes
  "assert-bitfield correctly asserts a zero field."
  (let ((word #xFF000000))
    (assert-bitfield word (0 8 0))))

(deftest-each assert-bitfield-instruction-encoding
  "assert-bitfield validates 32-bit instruction word field layout."
  :cases (("add-encoding"  #x10 1 2 3)
          ("zero-regs"     #x00 0 0 0)
          ("max-regs"      #x10 255 255 255))
  (op dst src1 src2)
  (let ((word (logior (ash op 24) (ash dst 16) (ash src1 8) src2)))
    (assert-bitfield word (24 8 op) (16 8 dst) (8 8 src1) (0 8 src2))))

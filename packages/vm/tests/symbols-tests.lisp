;;;; tests/unit/vm/symbols-tests.lisp — VM Symbol Instruction Tests

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Symbol Operations ───────────────────────────────────────────────────

(defun str-vm ()
  "Create a minimal vm-state for symbol tests."
  (make-instance 'cl-cc/vm::vm-io-state))

(defun str-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm::execute-instruction inst state 0 (make-hash-table :test #'equal)))

(deftest sym-symbol-name
  "vm-symbol-name returns symbol's name string."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 'hello)
    (str-exec (cl-cc:make-vm-symbol-name :dst :R0 :src :R1) s)
    (assert-equal "HELLO" (cl-cc/vm::vm-reg-get s :R0))))

(deftest sym-make-symbol
  "vm-make-symbol creates uninterned symbol from string."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "FOO")
    (str-exec (cl-cc:make-vm-make-symbol :dst :R0 :src :R1) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (symbolp result))
      (assert-equal "FOO" (symbol-name result)))))

(deftest sym-intern-symbol
  "vm-intern-symbol interns string as symbol."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "INTERN-TEST-SYM-12345")
    (str-exec (cl-cc:make-vm-intern-symbol :dst :R0 :src :R1 :pkg nil) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (symbolp result))
      (assert-equal "INTERN-TEST-SYM-12345" (symbol-name result)))))

(deftest sym-gensym
  "vm-gensym-inst creates unique uninterned symbol."
  (let ((s (str-vm)))
    (str-exec (cl-cc:make-vm-gensym-inst :dst :R0) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (symbolp result))
      (assert-equal nil (symbol-package result)))))

(deftest-each sym-keywordp
  "vm-keywordp returns 1 for keywords, 0 for other symbols."
  :cases (("keyword" :test  1)
          ("symbol"  'hello 0))
  (value expected)
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (str-exec (cl-cc:make-vm-keywordp :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

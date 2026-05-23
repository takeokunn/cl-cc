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

(deftest sym-find-package
  "vm-find-package resolves a package designator through the runtime/host package layer."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 :cl-user)
    (str-exec (cl-cc:make-vm-find-package :dst :R0 :src :R1) s)
    (assert-true (cl-cc/vm::vm-reg-get s :R0))))

(deftest sym-find-symbol
  "vm-find-symbol returns the symbol and status as multiple values."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "CAR")
    (cl-cc/vm::vm-reg-set s :R2 :cl)
    (str-exec (cl-cc:make-vm-find-symbol :dst :R0 :src :R1 :pkg :R2) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (symbolp result))
      (assert-equal "CAR" (symbol-name result))
      (assert-equal :external (second (cl-cc/vm::vm-values-list s))))))

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

;;; ─── Package-Local Nicknames (FR-275) ─────────────────────────────────────

(defun str-delete-package-if-exists (designator)
  "Delete DESIGNATOR's package when it exists."
  (let ((package (find-package designator)))
    (when package
      (delete-package package))))

(defun str-vm-constructor (name)
  "Return exported VM constructor NAME from CL-CC, or skip until FR-275 exists."
  (multiple-value-bind (symbol status)
      (find-symbol name :cl-cc)
    (unless (and symbol (eq status :external) (fboundp symbol))
      (skip (format nil "FR-275 constructor ~A is not available yet" name)))
    (symbol-function symbol)))

(defvar *str-package-lock*
  (sb-thread:make-mutex :name "cl-cc symbol test package lock"))

(defun str-with-local-nickname-packages (target-name user-name thunk)
  "Run THUNK with fresh TARGET-NAME and USER-NAME packages, then clean up."
  (sb-thread:with-mutex (*str-package-lock*)
    (str-delete-package-if-exists user-name)
    (str-delete-package-if-exists target-name)
    (let ((target (make-package target-name :use nil))
          (user (make-package user-name :use nil)))
      (unwind-protect
           (funcall thunk target user)
        (str-delete-package-if-exists user-name)
        (str-delete-package-if-exists target-name)))))

(deftest sym-defpackage-local-nicknames-expands
  "defpackage accepts :local-nicknames and installs the requested local nickname."
  (sb-thread:with-mutex (*str-package-lock*)
    (str-delete-package-if-exists :fr275-defpackage-local-user)
    (str-delete-package-if-exists :fr275-defpackage-local-target)
    (unwind-protect
         (progn
           (assert-true
            (macroexpand-1
             '(defpackage #:fr275-defpackage-local-user
                (:use #:cl)
                (:local-nicknames (#:ln #:fr275-defpackage-local-target)))))
           (eval '(defpackage #:fr275-defpackage-local-target (:use #:cl)))
           (eval '(defpackage #:fr275-defpackage-local-user
                   (:use #:cl)
                   (:local-nicknames (#:ln #:fr275-defpackage-local-target))))
           (let ((*package* (find-package :fr275-defpackage-local-user)))
             (assert-eq (find-package :fr275-defpackage-local-target)
                        (find-package :ln))))
      (str-delete-package-if-exists :fr275-defpackage-local-user)
      (str-delete-package-if-exists :fr275-defpackage-local-target))))

(deftest sym-vm-add-package-local-nickname
  "vm-add-package-local-nickname creates a package-local nickname mapping."
  (str-with-local-nickname-packages
   "FR275-VM-ADD-TARGET"
   "FR275-VM-ADD-USER"
   (lambda (target user)
     (let ((s (str-vm))
           (ctor (str-vm-constructor "MAKE-VM-ADD-PACKAGE-LOCAL-NICKNAME")))
       (cl-cc/vm::vm-reg-set s :R1 "LN")
       (cl-cc/vm::vm-reg-set s :R2 target)
       (cl-cc/vm::vm-reg-set s :R3 user)
       (str-exec (funcall ctor :dst :R0 :pkg :R3 :nick :R1 :target :R2) s)
       (let ((*package* user))
         (assert-eq target (find-package "LN")))))))

(deftest sym-vm-remove-package-local-nickname
  "vm-remove-package-local-nickname removes a package-local nickname mapping."
  (str-with-local-nickname-packages
   "FR275-VM-REMOVE-TARGET"
   "FR275-VM-REMOVE-USER"
   (lambda (target user)
     (let ((s (str-vm))
           (add-ctor (str-vm-constructor "MAKE-VM-ADD-PACKAGE-LOCAL-NICKNAME"))
           (remove-ctor (str-vm-constructor "MAKE-VM-REMOVE-PACKAGE-LOCAL-NICKNAME")))
       (cl-cc/vm::vm-reg-set s :R1 "LN")
       (cl-cc/vm::vm-reg-set s :R2 target)
       (cl-cc/vm::vm-reg-set s :R3 user)
       (str-exec (funcall add-ctor :dst :R0 :pkg :R3 :nick :R1 :target :R2) s)
       (str-exec (funcall remove-ctor :dst :R0 :pkg :R3 :nick :R1 :target nil) s)
       (let ((*package* user))
         (assert-null (find-package "LN")))))))

(deftest sym-vm-find-package-uses-local-nickname
  "vm-find-package resolves package-local nicknames relative to the VM current package."
  (str-with-local-nickname-packages
   "FR275-VM-RESOLVE-TARGET"
   "FR275-VM-RESOLVE-USER"
   (lambda (target user)
     (let ((s (str-vm))
           (add-ctor (str-vm-constructor "MAKE-VM-ADD-PACKAGE-LOCAL-NICKNAME")))
       (cl-cc/vm::vm-reg-set s :R1 "LN")
       (cl-cc/vm::vm-reg-set s :R2 target)
       (cl-cc/vm::vm-reg-set s :R3 user)
       (str-exec (funcall add-ctor :dst :R0 :pkg :R3 :nick :R1 :target :R2) s)
       (setf (gethash '*package* (cl-cc/vm::vm-global-vars s)) user)
       (let ((*package* user))
         (str-exec (cl-cc:make-vm-find-package :dst :R0 :src :R1) s))
        (assert-eq target (cl-cc/vm::vm-reg-get s :R0))))))

;;; ─── FR-895: Symbol Table Compaction ─────────────────────────────────────────

(deftest sym-symbol-table-freeze-thaw
  "freeze-symbol-table and thaw-symbol-table work correctly."
  (let ((test-sym (gensym "FR895-TEST-")))
    (setf (cl-cc/vm::lookup-symbol (symbol-name test-sym)) test-sym)
    (assert-eq test-sym (cl-cc/vm::lookup-symbol (symbol-name test-sym)))
    ;; Freeze — table becomes read-only
    (cl-cc/vm::freeze-symbol-table)
    (assert-true cl-cc/vm::*symbol-table-frozen*)
    (assert-true (vectorp cl-cc/vm::*symbol-table-compact*))
    (assert-eq test-sym (cl-cc/vm::lookup-symbol (symbol-name test-sym)))
    ;; Frozen — adding new symbol should error
    (let ((new-sym (gensym "FR895-FROZEN-")))
      (assert-signals error
        (setf (cl-cc/vm::lookup-symbol (symbol-name new-sym)) new-sym)))
    ;; Thaw — back to dynamic
    (cl-cc/vm::thaw-symbol-table)
    (assert-null cl-cc/vm::*symbol-table-frozen*)
    (assert-null cl-cc/vm::*symbol-table-compact*)
    (let ((new-sym (gensym "FR895-THAWED-")))
      (setf (cl-cc/vm::lookup-symbol (symbol-name new-sym)) new-sym)
      (assert-eq new-sym (cl-cc/vm::lookup-symbol (symbol-name new-sym))))))

(deftest sym-symbol-index
  "symbol-index returns a sequential integer for each symbol."
  (let ((a (gensym "FR895-IDX-A-"))
        (b (gensym "FR895-IDX-B-"))
        (c (gensym "FR895-IDX-C-")))
    (let ((ia (cl-cc/vm::symbol-index a))
          (ib (cl-cc/vm::symbol-index b))
          (ic (cl-cc/vm::symbol-index c)))
      (assert-true (integerp ia))
      (assert-true (integerp ib))
      (assert-true (integerp ic))
      ;; Each gets a unique index
      (assert-false (= ia ib))
      (assert-false (= ib ic))
      ;; Same symbol returns same index
      (assert-equal ia (cl-cc/vm::symbol-index a)))))

(deftest sym-register-weak-symbol
  "register-weak-symbol registers a symbol in the weak table."
  (let ((test-sym (gensym "FR895-WEAK-")))
    (cl-cc/vm::register-weak-symbol test-sym)
    (assert-eq test-sym
               (gethash (symbol-name test-sym) cl-cc/vm::*symbol-table-weak*))))

;;; ─── FR-896: Package Lock / Sealed ──────────────────────────────────────────

(defun %str-delete-package-if-exists (designator)
  "Delete DESIGNATOR's package when it exists, unlocking first if necessary."
  (let ((pkg (find-package designator)))
    (when pkg
      (cl-cc/vm::unlock-package pkg)
      (delete-package pkg))))

(deftest sym-lock-package
  "lock-package prevents intern/lock-package-locked-p reflects state."
  (sb-thread:with-mutex (*str-package-lock*)
    (%str-delete-package-if-exists :fr896-lock-test-a)
    (unwind-protect
         (let ((pkg (make-package :fr896-lock-test-a :use nil)))
           (assert-null (cl-cc/vm::package-locked-p pkg))
           (cl-cc/vm::lock-package pkg)
           (assert-true (cl-cc/vm::package-locked-p pkg))
           (cl-cc/vm::unlock-package pkg)
           (assert-null (cl-cc/vm::package-locked-p pkg)))
      (%str-delete-package-if-exists :fr896-lock-test-a))))

(deftest sym-package-locked-error-on-intern
  "Interning a symbol into a locked package signals package-locked-error."
  (sb-thread:with-mutex (*str-package-lock*)
    (%str-delete-package-if-exists :fr896-lock-test-b)
    (unwind-protect
         (let ((pkg (make-package :fr896-lock-test-b :use nil)))
           (cl-cc/vm::lock-package pkg)
           (assert-signals cl-cc/vm::package-locked-error
             (intern "LOCKED-SYMBOL" pkg))
           ;; Package should remain locked
           (assert-true (cl-cc/vm::package-locked-p pkg)))
      (%str-delete-package-if-exists :fr896-lock-test-b))))

(deftest sym-with-unlocked-packages
  "with-unlocked-packages temporarily unlocks packages."
  (sb-thread:with-mutex (*str-package-lock*)
    (%str-delete-package-if-exists :fr896-lock-test-c)
    (unwind-protect
         (let ((pkg (make-package :fr896-lock-test-c :use nil)))
           (cl-cc/vm::lock-package pkg)
           ;; Without unlock, intern should error
           (assert-signals cl-cc/vm::package-locked-error
             (intern "SHOULD-FAIL" pkg))
           ;; With unlock, intern should succeed
           (let ((result
                   (cl-cc/vm::with-unlocked-packages (:fr896-lock-test-c)
                     (intern "SHOULD-SUCCEED" pkg))))
             (assert-true (symbolp result))
             (assert-equal "SHOULD-SUCCEED" (symbol-name result)))
           ;; After unlock block, package should be re-locked
           (assert-true (cl-cc/vm::package-locked-p pkg))
           (assert-signals cl-cc/vm::package-locked-error
             (intern "SHOULD-FAIL-AGAIN" pkg)))
      (%str-delete-package-if-exists :fr896-lock-test-c))))

(deftest sym-default-locked-packages
  "*locked-packages* includes :cl by default."
  (assert-true (find-package :cl))
  (assert-true (cl-cc/vm::package-locked-p (find-package :cl))))

(deftest sym-check-package-lock-signals
  "check-package-lock signals package-locked-error for locked packages."
  (sb-thread:with-mutex (*str-package-lock*)
    (%str-delete-package-if-exists :fr896-lock-test-d)
    (unwind-protect
         (let ((pkg (make-package :fr896-lock-test-d :use nil)))
           (cl-cc/vm::lock-package pkg)
           (assert-signals cl-cc/vm::package-locked-error
             (cl-cc/vm::check-package-lock pkg :intern))
           (cl-cc/vm::unlock-package pkg)
           ;; Unlocked should not signal
           (assert-true (null (cl-cc/vm::check-package-lock pkg :intern))))
      (%str-delete-package-if-exists :fr896-lock-test-d))))

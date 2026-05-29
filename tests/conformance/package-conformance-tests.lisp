;;;; tests/conformance/package-conformance-tests.lisp
;;;; ANSI CL Package System Conformance Tests (expected-fail for known gaps)
;;;;
;;;; Tests package operations that SHOULD work per ANSI CL but currently
;;;; rely on host SBCL fallback or are not implemented in the runtime
;;;; package registry. Each test is tagged :expected-fail until the
;;;; corresponding gap is closed.

(in-package :cl-cc/test)

(defsuite ansi-conformance-package-suite
  :description "ANSI CL Package System Conformance Tests"
  :parent cl-cc-conformance-suite
  :parallel nil)

(in-suite ansi-conformance-package-suite)

;;; ──────────────────────────────────────────────────────────────────────
;;; Helper: compile and run a string, capturing stdout
;;; ──────────────────────────────────────────────────────────────────────

(defun run-cl-string (code &key (capture-output t))
  "Compile and run CODE string through cl-cc pipeline.
When output is captured, return stdout if the program wrote any; otherwise
return the printed representation of the primary result."
  (let ((out (make-string-output-stream)))
    (let* ((*standard-output* (if capture-output out *standard-output*))
           (value (cl-cc:run-string code)))
      (if capture-output
          (let ((output (get-output-stream-string out)))
            (if (plusp (length output))
                output
                (princ-to-string value)))
          value))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Basic Package Operations (self-host gaps)
;;; ──────────────────────────────────────────────────────────────────────
;;; These test operations that work when bridged to host SBCL but
;;; fail in self-host mode or are incomplete in the runtime registry.

(deftest pkg-find-package-self-host
  "find-package should work without host SBCL fallback in self-host mode."
  :timeout 30
  :tags '(:package :find-package :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    ;; Bootstrap the registry with CL-USER
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-find-package "CL-USER" nil)))
      (assert-true pkg)
      (assert-equal "CL-USER" (cl-cc/vm::vm-symbol-name (cl-cc/vm::vm-package-name pkg))))))

(deftest pkg-intern-self-host
  "intern should create symbols in runtime package registry without host CL."
  :timeout 30
  :tags '(:package :intern :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let* ((pkg (cl-cc/vm::vm-find-package "CL-USER" nil))
           (sym (cl-cc/vm::vm-intern-symbol "MY-TEST-SYM" pkg)))
      (assert-true sym)
      (assert-equal "MY-TEST-SYM" (cl-cc/vm::vm-symbol-name sym)))))

(deftest pkg-export-self-host
  "export should make symbols external in runtime package registry."
  :timeout 30
  :tags '(:package :export :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-find-package "CL-USER" nil)))
      (cl-cc/vm::vm-export (list (cl-cc/vm::vm-intern-symbol "EXPORTED-SYM" pkg)) pkg)
      (multiple-value-bind (sym status)
          (cl-cc/vm::vm-find-symbol "EXPORTED-SYM" pkg)
        (assert-true sym)
        (assert-eq :external status)))))

(deftest pkg-import-self-host
  "import should make symbols accessible in target package."
  :timeout 30
  :tags '(:package :import :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let* ((src (cl-cc/vm::vm-make-package "SRC-PKG"))
           (dst (cl-cc/vm::vm-make-package "DST-PKG"))
           (sym (cl-cc/vm::vm-intern-symbol "IMPORTED" src)))
      (cl-cc/vm::vm-export (list sym) src)
      (cl-cc/vm::vm-import (list sym) dst)
      (multiple-value-bind (found status)
          (cl-cc/vm::vm-find-symbol "IMPORTED" dst)
        (assert-true found)
        (assert-eq :internal status)))))

(deftest pkg-use-package-self-host
  "use-package should make all exported symbols accessible."
  :timeout 30
  :tags '(:package :use-package :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let* ((lib (cl-cc/vm::vm-make-package "LIB-PKG"))
           (user (cl-cc/vm::vm-make-package "USER-PKG"))
           (sym (cl-cc/vm::vm-intern-symbol "LIB-FN" lib)))
      (cl-cc/vm::vm-export (list sym) lib)
      (cl-cc/vm::vm-use-package lib user)
      (multiple-value-bind (found status)
          (cl-cc/vm::vm-find-symbol "LIB-FN" user)
        (assert-true found)
        (assert-eq :inherited status)))))

(deftest pkg-unuse-package-self-host
  "unuse-package should remove inherited symbols."
  :timeout 30
  :tags '(:package :unuse-package :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let* ((lib (cl-cc/vm::vm-make-package "UNUSE-LIB"))
           (user (cl-cc/vm::vm-make-package "UNUSE-USER"))
           (sym (cl-cc/vm::vm-intern-symbol "FN" lib)))
      (cl-cc/vm::vm-export (list sym) lib)
      (cl-cc/vm::vm-use-package lib user)
      (cl-cc/vm::vm-unuse-package lib user)
      (multiple-value-bind (found status)
          (cl-cc/vm::vm-find-symbol "FN" user)
        (assert-null found)))))

(deftest pkg-shadow-self-host
  "shadow should create shadowing symbols in package."
  :timeout 30
  :tags '(:package :shadow :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let* ((lib (cl-cc/vm::vm-make-package "SHADOW-LIB"))
           (user (cl-cc/vm::vm-make-package "SHADOW-USER"))
           (sym (cl-cc/vm::vm-intern-symbol "FN" lib)))
      (cl-cc/vm::vm-export (list sym) lib)
      (cl-cc/vm::vm-use-package lib user)
      (cl-cc/vm::vm-shadow (list "FN") user)
      ;; After shadowing, FN in user should be internal (not inherited)
      (multiple-value-bind (found status)
          (cl-cc/vm::vm-find-symbol "FN" user)
        (assert-true found)
        (assert-eq :internal status)))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Package Name Operations
;;; ──────────────────────────────────────────────────────────────────────

(deftest pkg-package-name-self-host
  "package-name should return the package name string."
  :timeout 30
  :tags '(:package :package-name :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-make-package "NAME-TEST-PKG")))
      (assert-equal "NAME-TEST-PKG" (cl-cc/vm::vm-package-name pkg)))))

(deftest pkg-package-nicknames-self-host
  "package-nicknames should return list of nickname strings."
  :timeout 30
  :tags '(:package :package-nicknames :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let* ((pkg (cl-cc/vm::vm-make-package "NICK-TEST" :nicknames '("N1" "N2")))
           (nicks (cl-cc/vm::vm-package-nicknames pkg)))
      (assert-true (member "N1" nicks :test #'equal))
      (assert-true (member "N2" nicks :test #'equal)))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Symbol Operations in Self-Host Mode
;;; ──────────────────────────────────────────────────────────────────────

(deftest pkg-make-symbol-self-host
  "make-symbol should create uninterned symbols without host CL."
  :timeout 30
  :tags '(:package :make-symbol :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (let ((sym (cl-cc/vm::vm-make-symbol "UNINTERNED")))
      (assert-true sym)
      (assert-equal "UNINTERNED" (cl-cc/vm::vm-symbol-name sym))
      ;; make-symbol creates uninterned symbols
      (assert-null (cl-cc/vm::vm-symbol-package sym)))))

(deftest pkg-gensym-self-host
  "gensym should generate unique symbols without host CL."
  :timeout 30
  :tags '(:package :gensym :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (let ((g1 (cl-cc/vm::vm-gensym-inst "G" nil))
          (g2 (cl-cc/vm::vm-gensym-inst "G" nil)))
      (assert-true g1)
      (assert-true g2)
      (assert-false (eq g1 g2)))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: defpackage Macro (E2E via run-string)
;;; ──────────────────────────────────────────────────────────────────────

(deftest pkg-defpackage-e2e
  "defpackage should create usable packages via run-string."
  :timeout 60
  :tags '(:package :defpackage :e2e)
  (let ((result (run-cl-string
                 "(progn
                    (defpackage :e2e-pkg
                      (:use :cl)
                      (:export :hello-world))
                    (in-package :e2e-pkg)
                    (defun hello-world () \"Hello from e2e-pkg\")
                    (in-package :cl-user)
                    (e2e-pkg:hello-world))"
                 :capture-output t)))
    (assert-equal "Hello from e2e-pkg" result)))

(deftest pkg-defpackage-conflict-detection
  "defpackage should detect symbol conflicts when using multiple packages."
  :timeout 60
  :tags '(:package :defpackage :conflict :e2e)
  (let ((result (run-cl-string
                 "(progn
                    (defpackage :conflict-a (:export :dup))
                    (defpackage :conflict-b (:export :dup))
                    (handler-case
                        (progn
                          (defpackage :conflict-user (:use :conflict-a :conflict-b))
                          :no-error)
                      (error (c) :conflict-detected)))"
                 :capture-output t)))
    ;; ANSI CL requires conflict detection on :USE
    (assert-equal "CONFLICT-DETECTED" result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Package Iteration
;;; ──────────────────────────────────────────────────────────────────────

(deftest pkg-do-symbols-self-host
  "do-symbols should iterate over package symbols."
  :timeout 30
  :tags '(:package :do-symbols :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-make-package "ITER-PKG"))
          (syms '()))
      (cl-cc/vm::vm-intern-symbol "A" pkg)
      (cl-cc/vm::vm-intern-symbol "B" pkg)
      (cl-cc/vm::vm-do-symbols (sym pkg)
        (push sym syms))
      (assert-= 2 (length syms)))))

(deftest pkg-list-all-packages-self-host
  "list-all-packages should return all registered packages."
  :timeout 30
  :tags '(:package :list-all-packages :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkgs (cl-cc/vm::vm-list-all-packages)))
      (assert-true (>= (length pkgs) 1))
      ;; CL-USER should be in the list
      (assert-true (find "CL-USER" pkgs
                         :test #'equal
                         :key #'cl-cc/vm::vm-package-name)))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: delete-package / rename-package
;;; ──────────────────────────────────────────────────────────────────────

(deftest pkg-delete-package-self-host
  "delete-package should remove package from registry."
  :timeout 30
  :tags '(:package :delete-package :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-make-package "TEMP-PKG")))
      (assert-true (cl-cc/vm::vm-find-package "TEMP-PKG" nil))
      (cl-cc/vm::vm-delete-package pkg)
      (assert-null (cl-cc/vm::vm-find-package "TEMP-PKG" nil)))))

(deftest pkg-rename-package-self-host
  "rename-package should change package name."
  :timeout 30
  :tags '(:package :rename-package :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-make-package "OLD-NAME")))
      (cl-cc/vm::vm-rename-package pkg "NEW-NAME")
      (assert-null (cl-cc/vm::vm-find-package "OLD-NAME" nil))
      (assert-true (cl-cc/vm::vm-find-package "NEW-NAME" nil)))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: unintern
;;; ──────────────────────────────────────────────────────────────────────

(deftest pkg-unintern-self-host
  "unintern should remove symbol from package."
  :timeout 30
  :tags '(:package :unintern :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-make-package "UNINTERN-TEST")))
      (cl-cc/vm::vm-intern-symbol "TEMP-SYM" pkg)
      (assert-true (cl-cc/vm::vm-find-symbol "TEMP-SYM" pkg))
      (cl-cc/vm::vm-unintern (cl-cc/vm::vm-find-symbol "TEMP-SYM" pkg) pkg)
      (assert-null (cl-cc/vm::vm-find-symbol "TEMP-SYM" pkg)))))

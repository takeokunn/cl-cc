;;;; tests/conformance/package-smoke-tests.lisp
;;;; Wave 1 Smoke Tests — verify the package self-hosting pipeline
;;;; These are NOT expected-fail; they should PASS using the runtime registry.

(in-package :cl-cc/test)

(defsuite ansi-conformance-package-smoke-suite
  :description "Package system self-hosting smoke tests (should PASS)"
  :parent cl-cc-unit-suite
  :parallel nil)

(in-suite ansi-conformance-package-smoke-suite)

;;; ──────────────────────────────────────────────────────────────────────
;;; Smoke Test 1: find-package → intern → export → find-symbol pipeline
;;; ──────────────────────────────────────────────────────────────────────

(deftest pkg-smoke-find-package
  "find-package should work via runtime registry."
  :timeout 10
  :tags '(:package :smoke :find-package)
  (let ((pkg (cl-cc/runtime::rt-find-package "CL-USER")))
    (assert-true pkg)
    (assert-equal "CL-USER" (cl-cc/runtime::rt-package-name pkg))))

(deftest pkg-smoke-find-package-cl
  "find-package should find the CL package."
  :timeout 10
  :tags '(:package :smoke :find-package)
  (let ((pkg (cl-cc/runtime::rt-find-package "CL")))
    (assert-true pkg)
    (assert-equal "CL" (cl-cc/runtime::rt-package-name pkg))))

(deftest pkg-smoke-intern
  "rt-intern should create/find symbols in a package."
  :timeout 10
  :tags '(:package :smoke :intern)
  (let* ((pkg (cl-cc/runtime::rt-find-package "CL-USER"))
         (sym (cl-cc/runtime::rt-intern "SMOKE-TEST-SYM" pkg)))
    (assert-true sym)
    (assert-equal "SMOKE-TEST-SYM" (cl-cc/runtime::rt-symbol-name sym))))

(deftest pkg-smoke-export
  "rt-export should add symbols to a package's export list."
  :timeout 10
  :tags '(:package :smoke :export)
  (let* ((pkg (cl-cc/runtime::rt-find-package "CL-USER"))
         (sym (cl-cc/runtime::rt-intern "SMOKE-EXPORT-SYM" pkg))
         (result (cl-cc/runtime::rt-export (list sym) pkg)))
    (assert-true result)))

(deftest pkg-smoke-find-symbol
  "rt-find-symbol should locate exported symbols."
  :timeout 10
  :tags '(:package :smoke :find-symbol)
  (let* ((pkg (cl-cc/runtime::rt-find-package "CL-USER"))
         (sym (cl-cc/runtime::rt-intern "SMOKE-FIND-SYM" pkg))
         (_ (cl-cc/runtime::rt-export (list sym) pkg)))
    (multiple-value-bind (found status)
        (cl-cc/runtime::rt-find-symbol "SMOKE-FIND-SYM" pkg)
      (assert-true found)
      (assert-eq :external status))))

(deftest pkg-smoke-use-package
  "rt-use-package and rt-unuse-package should work."
  :timeout 10
  :tags '(:package :smoke :use-package)
  (let* ((lib (cl-cc/runtime::rt-find-package "CL-USER"))
         (user (or (cl-cc/runtime::rt-find-package "SMOKE-USER-PKG")
                   (cl-cc/runtime::rt-make-package "SMOKE-USER-PKG")))
         (sym (cl-cc/runtime::rt-intern "SMOKE-USE-FN" lib))
         (_ (cl-cc/runtime::rt-export (list sym) lib)))
    ;; use-package
    (cl-cc/runtime::rt-use-package (list lib) user)
    (multiple-value-bind (found status)
        (cl-cc/runtime::rt-find-symbol "SMOKE-USE-FN" user)
      (assert-true found)
      (assert-eq :inherited status))
    ;; unuse-package
    (cl-cc/runtime::rt-unuse-package (list lib) user)
    (multiple-value-bind (found status)
        (cl-cc/runtime::rt-find-symbol "SMOKE-USE-FN" user)
      (assert-null found))))

(deftest pkg-smoke-list-all-packages
  "rt-list-all-packages should return known packages."
  :timeout 10
  :tags '(:package :smoke :list-all-packages)
  (let ((pkgs (cl-cc/runtime::rt-list-all-packages)))
    (assert-true (>= (length pkgs) 2))
    (assert-true (find "CL-USER" pkgs
                       :test #'equal
                       :key #'cl-cc/runtime::rt-package-name))
    (assert-true (find "CL" pkgs
                       :test #'equal
                       :key #'cl-cc/runtime::rt-package-name))))

(deftest pkg-smoke-import
  "rt-import should make external symbols internal in target package."
  :timeout 10
  :tags '(:package :smoke :import)
  (let* ((src (or (cl-cc/runtime::rt-find-package "SMOKE-SRC")
                  (cl-cc/runtime::rt-make-package "SMOKE-SRC")))
         (dst (or (cl-cc/runtime::rt-find-package "SMOKE-DST")
                  (cl-cc/runtime::rt-make-package "SMOKE-DST")))
         (sym (cl-cc/runtime::rt-intern "SMOKE-IMPORT-FN" src))
         (_ (cl-cc/runtime::rt-export (list sym) src)))
    (cl-cc/runtime::rt-import (list sym) dst)
    (multiple-value-bind (found status)
        (cl-cc/runtime::rt-find-symbol "SMOKE-IMPORT-FN" dst)
      (assert-true found)
      (assert-eq :internal status))))

(deftest pkg-smoke-symbol-name
  "rt-symbol-name should return symbol name string."
  :timeout 10
  :tags '(:package :smoke :symbol-name)
  (let* ((pkg (cl-cc/runtime::rt-find-package "CL-USER"))
         (sym (cl-cc/runtime::rt-intern "SYMBOL-NAME-TEST" pkg)))
    (assert-equal "SYMBOL-NAME-TEST" (cl-cc/runtime::rt-symbol-name sym))))

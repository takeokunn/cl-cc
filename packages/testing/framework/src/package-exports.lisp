(cl:in-package :cl-cc/test)

;;; Public test framework API.
;;; Originally lines 623-678 of package.lisp.

(export '(run-tests
          cl-cc-suite
          cl-cc-unit-suite
          cl-cc-integration-suite
          cl-cc-integration-serial-suite
          cl-cc-e2e-suite
          run-suite
          deftest
          defsuite
          in-suite
          assert-=
          assert-eq
          assert-eql
          assert-equal
          assert-string=
          assert-null
          assert-true
          assert-false
          assert-type
          assert-signals
          assert-values
          assert-type-equal
          assert-unifies
          assert-not-unifies
          assert-snapshot
          assert-compiles-to
          assert-pbt
          deftest-pbt
          assert-evaluates-to
          assert-macro-expands-to
          assert-infers-type
          deftest-each
          ;; High-level test macros (requirement #7)
          deftest-compile
          deftest-compile-each
          deftest-codegen
          deftest-codegen-each
          deftest-vm
          deftest-vm-each
          testing
          defbefore
          defafter
          skip
          pending
          deftest-fuzz
          assert-no-crash
          assert-terminates
          defmetamorphic
          definvariant
          run-mutation-test
          ;; Persistent map (phase 1 of immutable test framework)
          persistent-map
          persistent-map-p
          persist-empty
          persist-assoc
          persist-lookup
          persist-contains-p
          persist-remove
          persist-count
          persist-each
          persist-keys
          persist-values
          persist-to-alist
          persist-from-alist))

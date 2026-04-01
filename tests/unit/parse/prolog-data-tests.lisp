(in-package :cl-cc/test)

(in-suite cl-cc-suite)

(deftest prolog-data-built-in-handler-specs
  "The built-in predicate table stays data-only and exposes the expected handlers."
  (assert-equal '((cl-cc::! cl-cc::prolog-cut-handler)
                 (cl-cc::and cl-cc::prolog-and-handler)
                 (cl-cc::or cl-cc::prolog-or-handler)
                 (cl-cc::= cl-cc::prolog-unify-handler)
                 (cl-cc::/= cl-cc::prolog-not-unify-handler)
                 (:when cl-cc::prolog-when-handler)
                 (cl-cc::when cl-cc::prolog-when-handler))
                cl-cc::*builtin-predicate-specs*))

(deftest prolog-data-peephole-rules-present
  "The peephole rule data remains available after the data/logic split."
  (assert-true (listp cl-cc::*peephole-rules*))
  (assert-= 4 (length cl-cc::*peephole-rules*))
  (assert-equal '((:const cl-cc::?src cl-cc::?val) (:move cl-cc::?dst cl-cc::?src) ((:const cl-cc::?dst cl-cc::?val)))
                (first cl-cc::*peephole-rules*)))

(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Numeric Tower Extensions (redirector)
;;;
;;; Content has been split into three focused files loaded after this one:
;;;
;;;   vm-numeric-bignum-algorithms.lisp — schoolbook/karatsuba/Burnikel-Ziegler
;;;   vm-numeric-complex.lisp           — complex unboxing plan helpers
;;;   vm-numeric-tower.lisp             — native limb bignum, ratio, complex,
;;;                                       VM instructions (FR-301/305/952/955/956)
;;;
;;; Load order: after vm-transcendental.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

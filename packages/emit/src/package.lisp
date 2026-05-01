;;;; packages/emit/src/package.lisp — cl-cc/emit package definition
;;;;
;;;; Phase 6 (post 2026-05-01): :cl-cc/emit is now a facade. The actual
;;;; emit/codegen source files declare (in-package :cl-cc/codegen). The
;;;; eval-when block at the bottom imports all internal codegen symbols
;;;; into :cl-cc/emit so legacy `cl-cc/emit::*x86-64-instruction-sizes*`
;;;; references resolve to the codegen home symbol via inheritance.

(defpackage :cl-cc/emit
  (:use :cl :cl-cc/vm :cl-cc/mir :cl-cc/optimize :cl-cc/codegen)
  (:export
   ;; Re-export regalloc + emit symbols ---
   #:live-interval
   #:make-live-interval
   #:interval-vreg
   #:interval-start
   #:interval-end
   #:interval-phys-reg
   #:interval-spill-slot
   #:regalloc-result
   #:regalloc-assignment
   #:regalloc-spill-map
   #:regalloc-spill-count
   #:regalloc-instructions
   #:regalloc-lookup
   #:instruction-defs
   #:instruction-uses
   #:compute-live-intervals
   #:linear-scan-allocate
   #:allocate-registers
   #:vm-spill-store
   #:vm-spill-load
   #:make-vm-spill-store
   #:make-vm-spill-load
   #:vm-spill-src
   #:vm-spill-dst
   #:vm-spill-slot

   #:vm-reg-to-x86-with-alloc
   #:*current-regalloc*
   #:*phys-reg-to-x86-code*
   #:*phys-reg-to-asm-string*

   #:emit-instruction
   #:x86-64-target #:target-spill-base-reg
   #:aarch64-target
   #:compile-to-wasm-wat
   #:x86-64-red-zone-spill-p

   #:target-regalloc
   #:compile-to-native
   #:compile-file-to-native
   #:compile-to-x86-64-bytes
   #:compile-to-aarch64-bytes))

;;; Bridge: import all internal symbols of :cl-cc/codegen so legacy
;;; `cl-cc/emit::*x86-64-instruction-sizes*` references resolve to the
;;; codegen home symbol.
(eval-when (:load-toplevel :execute)
  (let ((codegen-pkg (find-package :cl-cc/codegen))
        (emit-pkg    (find-package :cl-cc/emit)))
    (when codegen-pkg
      (do-symbols (sym codegen-pkg)
        (when (eq (symbol-package sym) codegen-pkg)
          (handler-case
              (unless (find-symbol (symbol-name sym) emit-pkg)
                (import (list sym) emit-pkg))
            (package-error () nil)))))))

;;;; packages/emit/src/wasm.lisp - WebAssembly (WAT text) Backend
;;;
;;; Implements the WASM text format (WAT) emission backend for cl-cc.
;;; Uses the WASM GC proposal type system (struct/array/ref types).
;;; Control flow is handled via PC-dispatch trampoline (see wasm-trampoline.lisp).
;;;
;;; The emit-instruction generic is defined in x86-64.lisp (it is the shared
;;; backend interface). This file adds methods for the wasm-target class.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WASM target class
;;; ─────────────────────────────────────────────────────────────────────────────

(defclass wasm-target (target)
  ((module :initarg :module :initform nil :accessor wasm-target-module
    :documentation "The wasm-module-ir being built.")
   (reg-map :initarg :reg-map :initform nil :accessor wasm-target-reg-map
    :documentation "Current wasm-reg-map for the function being emitted.")
   (known-func-labels :initarg :known-func-labels
    :initform (make-hash-table)
    :accessor wasm-target-known-func-labels
    :documentation "Best-effort map from VM register to direct function label.")
   (known-slot-indexes :initarg :known-slot-indexes
    :initform (make-hash-table :test #'equal)
    :accessor wasm-target-known-slot-indexes
    :documentation "Best-effort map from slot-name symbol/string to slot index.")
   (class-slot-layouts :initarg :class-slot-layouts
    :initform (make-hash-table :test #'equal)
    :accessor wasm-target-class-slot-layouts
    :documentation "Map class-name => slot-name/index hash-table.")
   (class-slot-orders :initarg :class-slot-orders
    :initform (make-hash-table :test #'equal)
    :accessor wasm-target-class-slot-orders
    :documentation "Map class-name => effective slot-name order list.")
   (known-class-by-reg :initarg :known-class-by-reg
    :initform (make-hash-table)
    :accessor wasm-target-known-class-by-reg
    :documentation "Map VM register => class-name symbol/string.")
    (known-object-class-by-reg :initarg :known-object-class-by-reg
     :initform (make-hash-table)
     :accessor wasm-target-known-object-class-by-reg
     :documentation "Map VM object register => class-name symbol/string.")
    (try-stack :initarg :try-stack :initform nil :accessor wasm-target-try-stack
     :documentation "Structured Wasm EH frames active in direct instruction emission.")
    (try-depth :initarg :try-depth :initform 0 :accessor wasm-target-try-depth
     :documentation "Current native Wasm try/catch nesting depth."))
  (:documentation "WASM backend target. Emits WebAssembly Text Format (WAT)."))

;;; target-register: maps VM register to WAT local index (integer)
(defmethod target-register ((target wasm-target) virtual-register)
  "Map a VM register keyword (:R0, :R1, ...) to a WASM local index."
  (let ((reg-map (wasm-target-reg-map target)))
    (if reg-map
        (wasm-reg-to-local reg-map virtual-register)
        ;; Fallback: extract number from :R0 -> 0
        (let* ((name (symbol-name virtual-register))
               (num (parse-integer name :start 1 :junk-allowed t)))
          (or num 0)))))

(defun %wasm-function-index-for-label (module label)
  "Return LABEL's function index using MODULE's function table."
  (or (and module (wasm-module-function-index-for-label module label))
      (let ((func (find label (wasm-module-functions module)
                        :key (lambda (f)
                               (let ((wat-name (wasm-func-wat-name f)))
                                 (if (and wat-name (> (length wat-name) 0)
                                          (char= (char wat-name 0) #\$))
                                     (subseq wat-name 1)
                                     wat-name)))
                        :test #'equal)))
        (when func
          (wasm-func-index func)))))

(defun %wasm-main-function-type-index (module)
  "Return the canonical type index for backend-generated CL functions."
  (let ((table (and module (wasm-module-type-signature-table module)))
        (signature '(:params nil :results (:eqref))))
    (when table
      (setf (gethash signature table) +type-idx-main-func+))
    +type-idx-main-func+))

;;; (Structural sections — type section, globals, tags, table, elem — are in wasm-sections.lisp)
;;; (Function locals, full function emission, BigInt wrappers — are in wasm-functions.lisp)
;;; (AOT mode tracking, import filtering, emit-wat-imports — are in wasm-imports.lisp)
;;; (emit-wasm-module, binary backend, debug/DevTools, profiles — are in wasm-output.lisp)

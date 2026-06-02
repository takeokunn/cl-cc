;;;; packages/codegen/src/wasm-trampoline-arrays.lisp — FR-211 Wasm GC Array Helpers
;;;;
;;;; Specialized array WAT emitters for typed Wasm GC arrays.
;;;; Covers fixnum, float, char, and eqref array kinds with boxing/unboxing.
;;;;
;;;; Defines: wasm-normalize-array-element-kind, wasm-array-type-name,
;;;;           wasm-array-type-ref, wasm-array-reg-record-kind,
;;;;           wasm-array-reg-kind, wasm-array-reg-copy-kind,
;;;;           wasm-vector-literal-kind, wasm-value-to-array-element-wat,
;;;;           wasm-array-default-wat, wasm-reg-to-array-element-wat,
;;;;           wasm-array-element-to-eqref-wat, wasm-ref-as-non-null-wat,
;;;;           wasm-ref-test-wat, wasm-ref-eq-wat, wasm-struct-new-immutable-wat,
;;;;           emit-wasm-typed-select-wat, wasm-array-cast-wat,
;;;;           wasm-vector-literal-wat, wasm-array-new-wat,
;;;;           wasm-array-fill-wat, wasm-array-get-eqref-wat,
;;;;           wasm-array-set-wat, wasm-array-len-wat, wasm-array-copy-wat,
;;;;           define-wasm-unary-wat, define-wasm-binary-wat
;;;;
;;;; Load order: after wasm-trampoline.lisp, before wasm-trampoline-ranges.lisp.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-211: Wasm GC specialized array helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-normalize-array-element-kind (element-type)
  "Normalize a CL/VM array ELEMENT-TYPE designator to a Wasm array kind."
  (case element-type
    ((fixnum integer :fixnum :integer) :fixnum)
    ((single-float double-float float :single-float :double-float :float) :float)
    ((character :character :char) :char)
    ((t :any nil) :eqref)
    (otherwise :eqref)))

(defun wasm-array-type-name (kind)
  "Return the WAT type name for specialized array KIND."
  (case (wasm-normalize-array-element-kind kind)
    (:fixnum "$fixnum_array_t")
    (:float "$float_array_t")
    (:char "$char_array_t")
    (otherwise "$eqref_array_t")))

(defun wasm-array-type-ref (kind)
  "Return a non-null ref type for specialized array KIND."
  (format nil "(ref ~A)" (wasm-array-type-name kind)))

(defun wasm-array-reg-record-kind (reg-map reg kind)
  "Record that REG holds a Wasm GC array with element KIND."
  (let ((array-types (wasm-reg-map-array-element-types reg-map)))
    (when array-types
      (setf (gethash reg array-types) (wasm-normalize-array-element-kind kind)))))

(defun wasm-array-reg-kind (reg-map reg)
  "Return REG's known Wasm GC array element kind, defaulting to :EQREF."
  (or (let ((array-types (wasm-reg-map-array-element-types reg-map)))
        (and array-types (gethash reg array-types)))
      :eqref))

(defun wasm-array-reg-copy-kind (reg-map dst src)
  "Copy array element-kind metadata from SRC to DST when present."
  (let ((array-types (wasm-reg-map-array-element-types reg-map)))
    (when array-types
      (multiple-value-bind (kind present-p) (gethash src array-types)
        (if present-p
            (setf (gethash dst array-types) kind)
            (remhash dst array-types))))))

(defun wasm-vector-literal-kind (values)
  "Infer the narrowest specialized array kind for a CL vector literal VALUES."
  (cond
    ((and (> (length values) 0) (every #'integerp values)) :fixnum)
    ((and (> (length values) 0) (every #'floatp values)) :float)
    ((and (> (length values) 0) (every #'characterp values)) :char)
    (t :eqref)))

(defun wasm-value-to-array-element-wat (value kind)
  "Return WAT for VALUE as an element of specialized array KIND."
  (case (wasm-normalize-array-element-kind kind)
    (:fixnum (format nil "(i64.const ~D)" value))
    (:float (format nil "(f64.const ~F)" value))
    (:char (format nil "(i32.const ~D)" (char-code value)))
    (otherwise (%wasm-const-value-to-wat value))))

(defun wasm-array-default-wat (kind)
  "Return default initialization WAT for array KIND."
  (case (wasm-normalize-array-element-kind kind)
    (:fixnum "(i64.const 0)")
    (:float "(f64.const 0.0)")
    (:char "(i32.const 0)")
    (otherwise "(ref.null eq)")))

(defun wasm-reg-to-array-element-wat (reg-map reg kind)
  "Unbox VM register REG into a raw element value for array KIND."
  (case (wasm-normalize-array-element-kind kind)
    (:fixnum (wasm-fixnum-unbox reg-map reg))
    (:float (format nil "(struct.get $float_t 0 (ref.cast (ref $float_t) ~A))"
                    (reg-local-ref reg-map reg)))
    (:char (format nil "(i31.get_s ~A)" (reg-local-ref reg-map reg)))
    (otherwise (reg-local-ref reg-map reg))))

(defun wasm-array-element-to-eqref-wat (raw-wat kind)
  "Box RAW-WAT loaded from array KIND back to VM eqref representation."
  (case (wasm-normalize-array-element-kind kind)
    (:fixnum (wasm-fixnum-box raw-wat))
    (:float (format nil "(struct.new $float_t ~A)" raw-wat))
    (:char (format nil "(ref.i31 ~A)" raw-wat))
    (otherwise raw-wat)))

(defun wasm-ref-as-non-null-wat (ref-wat)
  "Return FR-270 ref.as_non_null WAT when null safety is enabled."
  (if (wasm-gc-null-safety-feature-enabled-p)
      (format nil "(ref.as_non_null ~A)" ref-wat)
      ref-wat))

(defun wasm-ref-test-wat (type-wat ref-wat)
  "Return FR-231 ref.test WAT for GC type checks."
  (format nil "(ref.test ~A ~A)" type-wat ref-wat))

(defun wasm-ref-eq-wat (lhs-wat rhs-wat)
  "Return FR-285 ref.eq WAT for GC reference identity."
  (format nil "(ref.eq ~A ~A)" lhs-wat rhs-wat))

(defun wasm-struct-new-immutable-wat (type-name &rest field-wats)
  "Return FR-247 struct.new_immutable WAT."
  (format nil "(struct.new_immutable ~A~{ ~A~})" type-name field-wats))

(defun emit-wasm-typed-select-wat (result-type cond-wat then-wat else-wat)
  "Return FR-279 typed select WAT for reference-typed conditionals."
  (if (wasm-typed-select-feature-enabled-p)
      (format nil "(select (result ~A) ~A ~A ~A)" result-type then-wat else-wat cond-wat)
      (format nil "(if (result ~A) ~A (then ~A) (else ~A))"
              result-type cond-wat then-wat else-wat)))

(defun wasm-array-cast-wat (array-wat kind)
  "Cast ARRAY-WAT to the concrete Wasm GC array type for KIND."
  (format nil "(ref.cast ~A ~A)" (wasm-array-type-ref kind)
          (wasm-ref-as-non-null-wat array-wat)))

(defun wasm-vector-literal-wat (values &optional forced-kind)
  "Return array.new_fixed WAT for a CL vector literal."
  (let* ((kind (or forced-kind (and *wasm-gc-array-types-enabled*
                                    (wasm-vector-literal-kind values))
                   :eqref))
         (type-name (wasm-array-type-name kind))
         (elems (loop for value across values
                      collect (wasm-value-to-array-element-wat value kind))))
    (format nil "(array.new_fixed ~A ~D~@[ ~A~])"
            type-name
            (length values)
            (and elems (format nil "~{~A~^ ~}" elems)))))

(defun wasm-array-new-wat (kind init-wat size-wat &key default-p)
  "Return array.new or array.new_default for KIND."
  (let ((type-name (wasm-array-type-name kind)))
    (if (and default-p
             *wasm-gc-more-array-constructors-enabled*
             (not (eq (wasm-normalize-array-element-kind kind) :eqref)))
        (format nil "(array.new_default ~A ~A)" type-name size-wat)
        (format nil "(array.new ~A ~A ~A)" type-name init-wat size-wat))))

(defun wasm-array-fill-wat (reg-map array-reg value-reg start-wat len-wat)
  "Return FR-284 array.fill WAT for ARRAY-REG."
  (let* ((kind (if *wasm-gc-array-types-enabled*
                   (wasm-array-reg-kind reg-map array-reg)
                   :eqref))
         (arr (wasm-array-cast-wat (reg-local-ref reg-map array-reg) kind))
         (val (wasm-reg-to-array-element-wat reg-map value-reg kind)))
    (format nil "(array.fill ~A ~A ~A ~A ~A)"
            (wasm-array-type-name kind) arr start-wat val len-wat)))

;;; Unary WAT emitters: (name opcode param). Logic: single-format string.
(defmacro define-wasm-unary-wat (name opcode param &optional doc)
  "Generate a one-argument WAT emitter that wraps PARAM in (OPCODE ...)."
  `(defun ,name (,param)
     ,(or doc (format nil "Return WAT for ~A." opcode))
     (format nil ,(concatenate 'string "(" opcode " ~A)") ,param)))

;;; Binary WAT emitters: (name opcode param1 param2). Logic: single-format string.
(defmacro define-wasm-binary-wat (name opcode param1 param2 &optional doc)
  "Generate a two-argument WAT emitter that wraps PARAM1 PARAM2 in (OPCODE ...)."
  `(defun ,name (,param1 ,param2)
     ,(or doc (format nil "Return WAT for ~A." opcode))
     (format nil ,(concatenate 'string "(" opcode " ~A ~A)") ,param1 ,param2)))

(defun wasm-array-get-eqref-wat (reg-map array-reg index-reg)
  "Return WAT for VM AREF on ARRAY-REG/INDEX-REG, boxing typed elements."
  (let* ((kind (if *wasm-gc-array-types-enabled*
                   (wasm-array-reg-kind reg-map array-reg)
                   :eqref))
         (arr (wasm-array-cast-wat (reg-local-ref reg-map array-reg) kind))
         (idx (wasm-fixnum-unbox reg-map index-reg :result-type :i32)))
    (wasm-array-element-to-eqref-wat
     (format nil "(array.get ~A ~A ~A)" (wasm-array-type-name kind) arr idx)
     kind)))

(defun wasm-array-set-wat (reg-map array-reg index-reg val-reg)
  "Return WAT for VM ASET on ARRAY-REG/INDEX-REG/VAL-REG."
  (let* ((kind (if *wasm-gc-array-types-enabled*
                   (wasm-array-reg-kind reg-map array-reg)
                   :eqref))
         (arr (wasm-array-cast-wat (reg-local-ref reg-map array-reg) kind))
         (idx (wasm-fixnum-unbox reg-map index-reg :result-type :i32))
         (val (wasm-reg-to-array-element-wat reg-map val-reg kind)))
    (format nil "(array.set ~A ~A ~A ~A)" (wasm-array-type-name kind) arr idx val)))

(defun wasm-array-len-wat (reg-map array-reg)
  "Return WAT for VM vector/array length on ARRAY-REG."
  (let* ((kind (if *wasm-gc-array-types-enabled*
                   (wasm-array-reg-kind reg-map array-reg)
                   :eqref))
         (arr (wasm-array-cast-wat (reg-local-ref reg-map array-reg) kind)))
    (format nil "(array.len ~A)" arr)))

(defun wasm-array-copy-wat (reg-map dst-array-reg src-array-reg len-reg)
  "Return WAT for FR-228 array.copy from SRC to DST starting at 0 for LEN."
  (let* ((dst-kind (if *wasm-gc-array-types-enabled*
                       (wasm-array-reg-kind reg-map dst-array-reg)
                       :eqref))
         (src-kind (if *wasm-gc-array-types-enabled*
                       (wasm-array-reg-kind reg-map src-array-reg)
                       :eqref)))
    (format nil "(array.copy ~A ~A ~A (i32.const 0) ~A (i32.const 0) ~A)"
            (wasm-array-type-name dst-kind)
            (wasm-array-type-name src-kind)
            (wasm-array-cast-wat (reg-local-ref reg-map dst-array-reg) dst-kind)
            (wasm-array-cast-wat (reg-local-ref reg-map src-array-reg) src-kind)
            (wasm-fixnum-unbox reg-map len-reg :result-type :i32))))

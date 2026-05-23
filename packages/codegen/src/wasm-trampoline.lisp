;;;; packages/emit/src/wasm-trampoline.lisp - PC-Dispatch Trampoline Builder
;;;
;;; Converts a flat list of VM instructions (a function body) into a WAT
;;; body using a PC-dispatch trampoline: loop { block { br_table } }.
;;; This bridges the VM's flat label-jump model to WASM structured control flow.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 1: Group instructions into basic blocks by label
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (wasm-basic-block (:conc-name wasm-bb-))
  "A basic block in the function: a label and its instructions."
  (label nil :type (or null string))   ; nil for the implicit entry block
  (pc-index nil :type (or null integer)) ; the $pc value that dispatches here
  (instructions nil :type list))       ; the instructions in this block

(defun group-into-basic-blocks (instructions)
  "Split INSTRUCTIONS into basic blocks at vm-label boundaries.
   Returns a list of wasm-basic-block structs in order."
  (let ((blocks nil)
        (current-label nil)
        (current-instrs nil)
        (pc-counter 0))
    (flet ((flush ()
             (when (or current-label current-instrs)
               (push (make-wasm-basic-block
                      :label current-label
                      :pc-index (prog1 pc-counter (incf pc-counter))
                      :instructions (nreverse current-instrs))
                     blocks)
               (setf current-label nil
                     current-instrs nil))))
      (dolist (inst instructions)
        (if (typep inst 'vm-label)
            (progn
              (flush)
              (setf current-label (vm-name inst)))
            (push inst current-instrs)))
      (flush))
    (nreverse blocks)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 2: Build a label -> pc-index map
;;; ─────────────────────────────────────────────────────────────────────────────

(defun build-label-pc-map (basic-blocks)
  "Build a hash table from label name (string) -> pc-index (integer)."
  (let ((map (make-hash-table :test #'equal)))
    (dolist (bb basic-blocks map)
      (when (wasm-bb-label bb)
        (setf (gethash (wasm-bb-label bb) map) (wasm-bb-pc-index bb))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Dynamic calling-convention state
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *wasm-label-to-table-idx* nil
  "Dynamic binding: hash table mapping function entry-label name (string) to its
   WASM funcref table index (= wasm-func-index).  Bound in build-all-wasm-functions
   so that emit-trampoline-instruction can emit real table indices for vm-closure.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 3: Emit WAT for a single instruction
;;; ─────────────────────────────────────────────────────────────────────────────
;;; Returns T if instruction was handled, NIL if not supported (emits comment).
;;; REG-MAP is a wasm-reg-map for mapping :R0 etc. to local indices.
;;; LABEL-PC-MAP maps label names to pc-index integers.
;;; NUM-BLOCKS is total number of basic blocks.

(defun reg-local-ref (reg-map reg)
  "Return WAT for getting a register's local variable, e.g. '(local.get 3)'."
  (format nil "(local.get ~D)" (wasm-reg-to-local reg-map reg)))

(defun reg-local-set (reg-map reg value-wat)
  "Return WAT for setting a register's local variable.
   Also clears any known type for the destination register (FR-142)."
  (let ((known-types (wasm-reg-map-known-types reg-map)))
    (when known-types (remhash reg known-types)))
  (let ((ranges (wasm-reg-map-fixnum-ranges reg-map)))
    (when ranges (remhash reg ranges)))
  (let ((array-types (wasm-reg-map-array-element-types reg-map)))
    (when array-types (remhash reg array-types)))
  (let ((dst (wasm-reg-to-local reg-map reg)))
    (if (and (stringp value-wat)
             (search "(local.get " value-wat)
             (= (position #\( value-wat) 0))
        (format nil "(local.tee ~D ~A)" dst value-wat)
        (format nil "(local.set ~D ~A)" dst value-wat))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-142: ref.cast elimination — type tracking helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun reg-record-type (reg-map reg type-keyword)
  "Record that VM register REG holds a value of wasm type TYPE-KEYWORD.
   TYPE-KEYWORD examples: :closure, :cons, :i31ref, :string, :symbol."
  (let ((known-types (wasm-reg-map-known-types reg-map)))
    (when known-types
      (setf (gethash reg known-types) type-keyword))))

(defun reg-known-type (reg-map reg)
  "Return the known wasm type for VM register REG, or NIL if unknown."
  (let ((known-types (wasm-reg-map-known-types reg-map)))
    (and known-types (gethash reg known-types))))

(defun reg-clear-type (reg-map reg)
  "Clear any known type for VM register REG."
  (let ((known-types (wasm-reg-map-known-types reg-map)))
    (when known-types (remhash reg known-types))))

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

(defun wasm-array-cast-wat (array-wat kind)
  "Cast ARRAY-WAT to the concrete Wasm GC array type for KIND."
  (format nil "(ref.cast ~A ~A)" (wasm-array-type-ref kind) array-wat))

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

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-209: i31ref fixnum native boxing/range analysis helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defconstant +wasm-i31-min+ (- (expt 2 30))
  "Minimum signed integer representable by Wasm GC i31ref.")

(defconstant +wasm-i31-max+ (1- (expt 2 30))
  "Maximum signed integer representable by Wasm GC i31ref.")

(defun wasm-i31-range-p (value)
  "Return true when integer VALUE fits in signed Wasm i31ref payload bits."
  (and (integerp value)
       (<= +wasm-i31-min+ value +wasm-i31-max+)))

(defun reg-record-fixnum-range (reg-map reg range)
  "Record RANGE for REG and mark REG as an i31ref fixnum when FR-209 is enabled."
  (when *wasm-i31ref-optimize-enabled*
    (reg-record-type reg-map reg :i31ref)
    (let ((ranges (wasm-reg-map-fixnum-ranges reg-map)))
      (when ranges (setf (gethash reg ranges) range)))))

(defun reg-known-fixnum-range (reg-map reg)
  "Return the known fixnum range for REG, or NIL when unknown."
  (let ((ranges (wasm-reg-map-fixnum-ranges reg-map)))
    (and ranges (gethash reg ranges))))

(defun wasm-range-binop (lhs-range rhs-range op)
  "Conservatively compute a result range for integer OP over LHS/RHS ranges."
  (when (and (consp lhs-range) (consp rhs-range))
    (let ((a (car lhs-range)) (b (cdr lhs-range))
          (c (car rhs-range)) (d (cdr rhs-range)))
      (cond
        ((string= op "i64.add") (cons (+ a c) (+ b d)))
        ((string= op "i64.sub") (cons (- a d) (- b c)))
        ((string= op "i64.mul")
         (let ((values (list (* a c) (* a d) (* b c) (* b d))))
           (cons (apply #'min values) (apply #'max values))))
        ((member op '("i64.and" "i64.or" "i64.xor") :test #'string=)
         (cons +wasm-i31-min+ +wasm-i31-max+))
        (t nil)))))

(defun wasm-range-unary (src-range format-string)
  "Conservatively compute a result range for a unary fixnum format string."
  (when (consp src-range)
    (cond
      ((string= format-string "(i64.add ~A (i64.const 1))")
       (cons (1+ (car src-range)) (1+ (cdr src-range))))
      ((string= format-string "(i64.sub ~A (i64.const 1))")
       (cons (1- (car src-range)) (1- (cdr src-range))))
      ((string= format-string "(i64.sub (i64.const 0) ~A)")
       (cons (- (cdr src-range)) (- (car src-range))))
      ((or (search "popcnt" format-string)
           (search "clz" format-string)
           (search "ctz" format-string))
       (cons 0 64))
      (t nil))))

(defun wasm-range-i31-or-unknown (range)
  "Return RANGE if it fits i31ref, otherwise the conservative i31 range."
  (if (and (consp range)
           (wasm-i31-range-p (car range))
           (wasm-i31-range-p (cdr range)))
      range
      (cons +wasm-i31-min+ +wasm-i31-max+)))

(defun wasm-i64-const-wat-value (wat)
  "Return integer N for WAT of the form (i64.const N), else NIL."
  (when (stringp wat)
    (let ((prefix "(i64.const "))
      (when (and (>= (length wat) (+ (length prefix) 2))
                 (string= wat prefix :end1 (length prefix))
                 (char= (char wat (1- (length wat))) #\)))
        (ignore-errors
          (parse-integer wat :start (length prefix) :end (1- (length wat))))))))

(defun wasm-i64-extended-i31-source (wat)
  "Return X from WAT shaped as (i64.extend_i32_s (i31.get_s X)), else NIL."
  (when (stringp wat)
    (let ((prefix "(i64.extend_i32_s (i31.get_s "))
      (when (and (>= (length wat) (+ (length prefix) 2))
                 (string= wat prefix :end1 (length prefix)))
        (subseq wat (length prefix) (- (length wat) 2))))))

(defun wasm-ref-cast-maybe (type-wat reg-map reg)
  "Return WAT for a ref.cast to TYPE-WAT for register REG, 
   SKIPPING the cast when FR-142 determines the register's type is already known
   to match TYPE-WAT."
  (if *wasm-ref-cast-elimination-enabled*
      (let ((known (reg-known-type reg-map reg)))
        (let ((type-str (if (stringp type-wat) type-wat (format nil "~A" type-wat))))
          (if (search type-str (or (format nil "~A" known) "") :test #'char-equal)
              (format nil "~A" (reg-local-ref reg-map reg))
              (format nil "(ref.cast ~A ~A)" type-wat (reg-local-ref reg-map reg)))))
      (format nil "(ref.cast ~A ~A)" type-wat (reg-local-ref reg-map reg))))

(defun wasm-fixnum-unbox (reg-map reg &key (result-type :i64))
  "Unbox a native i31ref fixnum.

RESULT-TYPE selects the consumer width.  :I64 preserves the historical fixnum
arithmetic path with i64.extend_i32_s; :I32 emits direct i31.get_s for Wasm
consumers that already accept i32, skipping the extend/wrap pair."
  (let* ((ref (reg-local-ref reg-map reg))
         (known (and *wasm-i31ref-optimize-enabled* (reg-known-type reg-map reg)))
         (i32-wat (format nil "(i31.get_s ~A)" ref)))
    (case result-type
      (:i32 i32-wat)
      (:i64 (if (and *wasm-i31ref-optimize-enabled* (eq known :i64-unboxed))
                ref
                (format nil "(i64.extend_i32_s ~A)" i32-wat)))
      (otherwise (error "Unsupported wasm-fixnum-unbox result type: ~S" result-type)))))

(defun wasm-fixnum-box (i64-wat)
  "Box an integer expression as an i31ref fixnum.

FR-209 optimizes constant i31 values to direct i32.const and removes dead
box/unbox pairs by returning the original i31ref expression when boxing a value
that was just unboxed from i31ref."
  (if *wasm-i31ref-optimize-enabled*
      (let ((const (wasm-i64-const-wat-value i64-wat))
            (inner-i31 (wasm-i64-extended-i31-source i64-wat)))
        (cond
          ((and const (wasm-i31-range-p const))
           (format nil "(ref.i31 (i32.const ~D))" const))
          (inner-i31 inner-i31)
          (t (format nil "(ref.i31 (i32.wrap_i64 ~A))" i64-wat))))
      (format nil "(ref.i31 (i32.wrap_i64 ~A))" i64-wat)))

(defun wasm-bool-to-i31 (cond-wat)
  "Convert a WASM i32 boolean (0/1) to i31ref (nil/t).
   0 -> ref.null eq (nil), 1 -> (ref.i31 (i32.const 1)) (truthy)."
  (format nil "(if (result eqref) ~A (then (ref.i31 (i32.const 1))) (else (ref.null eq)))"
          cond-wat))

(defun %wasm-captured-value-reg (capture)
  "Return the VM register carrying CAPTURE's value."
  (if (consp capture) (cdr capture) capture))

(defun emit-wasm-closure-allocation (reg-map dst entry-index captured stream indent)
  "Emit closure allocation using $closure_t and $env_t GC structs.
   FR-144: When *wasm-typed-closure-env-enabled*, uses array.new_fixed directly
   for the environment instead of wrapping in $env_t struct + array.new/set loop.
   FR-142: Records the closure type on the destination register for ref.cast elimination.
   
Captured values are materialized into a mutable $eqref_array_t with array.new and
array.set before the closure struct is created."
  (let ((prefix (make-string indent :initial-element #\Space)))
    (if captured
        (if *wasm-typed-closure-env-enabled*
            (progn
               ;; FR-144: Typed env path — use array.new_fixed directly
               (format stream "~%~A;; FR-144: typed closure env using array.new_fixed" prefix)
               (format stream "~%~A(local.set ~D (array.new_fixed $eqref_array_t ~D~{ ~A~}))"
                       prefix (wasm-reg-map-tmp-index reg-map)
                       (length captured)
                       (loop for capture in captured
                             for reg = (%wasm-captured-value-reg capture)
                             collect (reg-local-ref reg-map reg)))
               (format stream "~%~A~A"
                       prefix
                       (reg-local-set
                        reg-map dst
                        (format nil "(struct.new $closure_t (i32.const ~D) (local.get ~D))"
                                entry-index
                                (wasm-reg-map-tmp-index reg-map))))
               (reg-record-type reg-map dst :closure))
            (progn
              ;; Original env struct path
              (let ((tmp (wasm-reg-map-tmp-index reg-map)))
                (format stream "~%~A(local.set ~D (array.new $eqref_array_t (ref.null eq) (i32.const ~D)))"
                        prefix tmp (length captured))
                (loop for capture in captured
                      for idx from 0
                      for reg = (%wasm-captured-value-reg capture)
                      do (format stream "~%~A(array.set $eqref_array_t (ref.cast (ref $eqref_array_t) (local.get ~D)) (i32.const ~D) ~A)"
                                 prefix tmp idx (reg-local-ref reg-map reg)))
                (format stream "~%~A~A"
                        prefix
                        (reg-local-set
                         reg-map dst
                         (format nil "(struct.new $closure_t (i32.const ~D) (struct.new $env_t (ref.cast (ref $eqref_array_t) (local.get ~D)) (ref.null $env_t)))"
                                 entry-index tmp)))
                (reg-record-type reg-map dst :closure))))
        (progn
          (format stream "~%~A~A"
                  prefix
                  (reg-local-set
                   reg-map dst
                   (format nil "(struct.new $closure_t (i32.const ~D) (ref.null $env_t))"
                           entry-index)))
          (reg-record-type reg-map dst :closure)))))

(defun wasm-closure-ref-wat (reg-map closure-reg index)
  "Return WAT for reading captured INDEX from CLOSURE-REG.
   FR-144: When typed env enabled, uses array.get directly on env field."
  (if *wasm-typed-closure-env-enabled*
      (format nil "(array.get $eqref_array_t (struct.get $closure_t 1 ~A) (i32.const ~D))"
              (wasm-ref-cast-maybe "(ref $closure_t)" reg-map closure-reg)
              index)
      (format nil "(array.get $eqref_array_t (struct.get $env_t 0 (ref.cast (ref $env_t) (struct.get $closure_t 1 ~A))) (i32.const ~D))"
              (wasm-ref-cast-maybe "(ref $closure_t)" reg-map closure-reg)
              index)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Fixnum operation helpers (used by emit-trampoline-instruction)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-i64-binop (reg-map dst lhs rhs op)
  "WAT for: dst = box(op(unbox(lhs), unbox(rhs)))."
  (let* ((result-range (wasm-range-i31-or-unknown
                        (wasm-range-binop (reg-known-fixnum-range reg-map lhs)
                                          (reg-known-fixnum-range reg-map rhs)
                                          op)))
         (wat (reg-local-set reg-map dst
                             (wasm-fixnum-box
                              (format nil "(~A ~A ~A)" op
                                      (wasm-fixnum-unbox reg-map lhs)
                                      (wasm-fixnum-unbox reg-map rhs))))))
    (reg-record-fixnum-range reg-map dst result-range)
    wat))

(defun wasm-i64-cmp (reg-map dst lhs rhs cmp-op)
  "WAT for: dst = T if cmp-op(unbox(lhs), unbox(rhs)), else NIL."
  (reg-local-set reg-map dst
                 (wasm-bool-to-i31
                  (format nil "(~A ~A ~A)" cmp-op
                          (wasm-fixnum-unbox reg-map lhs)
                          (wasm-fixnum-unbox reg-map rhs)))))

(defun emit-trampoline-jump-to-label (label-name label-pc-map reg-map stream)
  "Emit WAT to set $pc to the index for LABEL-NAME and branch back to $dispatch.
   FR-143: When tail-calls enabled and label has a known table entry, 
   emit return_call_indirect directly without the br $dispatch."
  (let ((pc-idx (gethash label-name label-pc-map)))
    (if pc-idx
        (format stream "~%      (local.set ~D (i32.const ~D))"
                (wasm-reg-map-pc-index reg-map) pc-idx)
        (format stream "~%      ;; WARNING: unknown label ~S" label-name))
    (if *wasm-tail-call-enabled*
        (format stream "~%      (return_call_indirect (type $main_func_t) (table $funcref_table) (local.get ~D))"
                (wasm-reg-map-pc-index reg-map))
        (format stream "~%      (br $dispatch)"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-228: Bulk Memory Operations helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-bulk-memory-enabled* t
  "Feature flag for Wasm Bulk Memory Operations proposal (FR-228).")

(defun wasm-memory-copy-wat (dst-offset-wat src-offset-wat size-wat)
  "Return WAT for memory.copy (dst src size)."
  (format nil "(memory.copy ~A ~A ~A)" dst-offset-wat src-offset-wat size-wat))

(defun wasm-memory-fill-wat (dst-offset-wat value-wat size-wat)
  "Return WAT for memory.fill (dst value size)."
  (format nil "(memory.fill ~A ~A ~A)" dst-offset-wat value-wat size-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-324: copysign — float-sign implementation via f64.copysign
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-copysign-wat (magnitude-wat sign-wat)
  "Return WAT for f64.copysign (magnitude, sign) — IEEE 754 copySign operation."
  (format nil "(f64.copysign ~A ~A)" magnitude-wat sign-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-326: memory.grow OOM detection — storage-condition on allocation failure
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-memory-grow-oom-check-enabled* t
  "Feature flag for memory.grow OOM detection (FR-326).")

(defun wasm-memory-grow-checked-wat (pages-wat)
  "Return WAT for safe memory.grow with OOM check.
   Returns: i32 result or branches to $oom on failure."
  (format nil "(if (result i32) (i32.eq (memory.grow ~A) (i32.const -1)) (then (call $host_error (ref.null $string_t)) (unreachable)) (else (memory.size)))"
          pages-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-233: Non-trapping float-to-int — saturating conversion helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-trunc-sat-f64-i64-wat (f64-wat)
  "Return WAT for non-trapping f64→i64 conversion (saturating)."
  (format nil "(i64.trunc_sat_f64_s ~A)" f64-wat))

(defun wasm-trunc-sat-f64-i32-wat (f64-wat)
  "Return WAT for non-trapping f64→i32 conversion (saturating)."
  (format nil "(i32.trunc_sat_f64_s ~A)" f64-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-234: Sign-extension — 1-instruction replacements for shift pairs
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-sign-extend-32-8-wat (i32-wat)
  "Return WAT for i32.extend8_s."
  (format nil "(i32.extend8_s ~A)" i32-wat))

(defun wasm-sign-extend-32-16-wat (i32-wat)
  "Return WAT for i32.extend16_s."
  (format nil "(i32.extend16_s ~A)" i32-wat))

(defun wasm-sign-extend-64-32-wat (i64-wat)
  "Return WAT for i64.extend32_s."
  (format nil "(i64.extend32_s ~A)" i64-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-323: MVP Bit Operations — clz/ctz/popcnt for integer-length/logcount
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-i64-clz-wat (i64-wat)
  "Return WAT for i64.clz — count leading zeros."
  (format nil "(i64.clz ~A)" i64-wat))

(defun wasm-i64-ctz-wat (i64-wat)
  "Return WAT for i64.ctz — count trailing zeros."
  (format nil "(i64.ctz ~A)" i64-wat))

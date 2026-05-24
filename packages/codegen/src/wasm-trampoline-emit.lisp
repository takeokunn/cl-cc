;;;; packages/emit/src/wasm-trampoline-emit.lisp — WASM Trampoline Instruction Emitter
;;;;
;;;; Data tables: wasm-trampoline-tables.lisp (loads before this file).
;;;; Program builder: wasm-trampoline-build.lisp (loads after this file).
;;;;
;;;; Load order: after wasm-trampoline-tables.lisp.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; User-level dense CASE lowering helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter +wasm-case-br-table-min-arms+ 4
  "Minimum number of integer CASE arms before the trampoline considers br_table.")

(defparameter +wasm-case-br-table-max-density-ratio+ 2
  "Maximum SPAN/ARM-COUNT ratio for dense integer CASE br_table lowering.")

(defun %wasm-normalize-integer-case-targets (case-targets)
  "Return sorted unique (INTEGER . LABEL) CASE-TARGETS, or NIL if invalid.

CASE-TARGETS is an emitter-pass representation of a user CASE dispatch: each
entry maps an integer key to a VM label name. Duplicate keys are rejected so the
WASM table preserves Common Lisp CASE's first-match semantics only after an
upstream pass has already canonicalized clauses."
  (when (and case-targets
             (every (lambda (entry)
                      (and (consp entry)
                           (integerp (car entry))
                           (cdr entry)))
                    case-targets))
    (let ((seen (make-hash-table :test #'eql))
          (valid t)
          (result nil))
      (dolist (entry case-targets)
        (if (gethash (car entry) seen)
            (setf valid nil)
            (progn
              (setf (gethash (car entry) seen) t)
              (push entry result))))
      (when valid
        (sort result #'< :key #'car)))))

(defun wasm-dense-integer-case-targets-p (case-targets)
  "Return T when CASE-TARGETS are dense enough to lower to WASM br_table.

The density rule intentionally mirrors packages/expand's dense integer CASE
selection: at least four integer arms, and the min-to-max span no more than two
times the number of present arms. Sparse CASE dispatch should remain an if-chain
or balanced integer decision tree."
  (let ((targets (%wasm-normalize-integer-case-targets case-targets)))
    (when (and targets (>= (length targets) +wasm-case-br-table-min-arms+))
      (let* ((min-key (caar targets))
             (max-key (caar (last targets)))
             (span (1+ (- max-key min-key))))
        (<= span (* +wasm-case-br-table-max-density-ratio+
                    (length targets)))))))

(defun %wasm-case-target-pc (label label-pc-map)
  "Return LABEL's trampoline PC index from LABEL-PC-MAP, or NIL when unknown."
  (and label (gethash label label-pc-map)))

(defun maybe-emit-wasm-dense-case-br-table (selector-reg case-targets default-label
                                      label-pc-map reg-map stream)
  "Emit a user-level integer CASE dispatch as WASM br_table when dense.

SELECTOR-REG is the VM register holding the boxed fixnum key. CASE-TARGETS is an
alist of (integer-key . vm-label-name). DEFAULT-LABEL is the otherwise target.
Each br_table arm maps to a small local block that sets the trampoline $pc to
the corresponding VM basic block and branches back to $dispatch.

Returns T when a br_table was emitted, NIL when CASE-TARGETS are sparse or any
target label is unresolved. Callers should keep the existing if-chain/binary
tree emission on NIL."
  (let ((targets (%wasm-normalize-integer-case-targets case-targets)))
    (unless (and targets (wasm-dense-integer-case-targets-p targets))
      (return-from maybe-emit-wasm-dense-case-br-table nil))
    (let* ((min-key (caar targets))
           (max-key (caar (last targets)))
           (span (1+ (- max-key min-key)))
           (target-map (make-hash-table :test #'eql))
           (default-pc (%wasm-case-target-pc default-label label-pc-map))
           (pc-local (wasm-reg-map-pc-index reg-map)))
      (unless default-pc
        (return-from maybe-emit-wasm-dense-case-br-table nil))
      (dolist (target targets)
        (let ((pc (%wasm-case-target-pc (cdr target) label-pc-map)))
          (unless pc
            (return-from maybe-emit-wasm-dense-case-br-table nil))
          (setf (gethash (car target) target-map) pc)))
      (format stream "~%      ;; dense integer CASE lowered to WASM br_table [~D..~D]" min-key max-key)
      (dotimes (i span)
        (format stream "~%      (block $case_~D" i))
      (format stream "~%        (block $case_default")
      (format stream "~%          (br_table")
      (dotimes (i span)
        (format stream " $case_~D" i))
      (format stream " $case_default")
      (format stream "~%            (i64.sub ~A (i64.const ~D)))"
              (wasm-fixnum-unbox reg-map selector-reg)
              min-key)
      (format stream "~%        ) ;; end block $case_default")
      (format stream "~%        (local.set ~D (i32.const ~D))" pc-local default-pc)
      (format stream "~%        (br $dispatch)")
      (dotimes (i span)
        (let* ((key (+ min-key i))
               (pc (gethash key target-map default-pc)))
          (format stream "~%      ) ;; end block $case_~D" i)
          (format stream "~%      (local.set ~D (i32.const ~D))" pc-local pc)
          (format stream "~%      (br $dispatch)")))
      t)))

(defun emit-trampoline-instruction (inst label-pc-map reg-map num-blocks stream)
  "Emit WAT text for a single VM instruction to STREAM.
   Returns T if instruction was handled, NIL otherwise (emits warn comment)."
  (declare (ignore num-blocks))
  (let ((tp (type-of inst)))
    ;; FR-285: CL EQ/EQL over GC references uses ref.eq; known i31 fixnums keep
    ;; numeric equality so immediate integers preserve CL semantics.
    (when (eq tp 'vm-eq)
      (format stream "~%      ~A"
              (reg-local-set reg-map (vm-dst inst)
                             (wasm-bool-to-i31
                              (wasm-eq-wat reg-map (vm-lhs inst) (vm-rhs inst)))))
      (return-from emit-trampoline-instruction t))
    ;; Binary i64 and comparison operations via shared dispatch
    (loop for (table . emit-fn) in *wasm-binop-dispatch*
          for op = (gethash tp table)
          when op
            do (format stream "~%      ~A"
                        (funcall emit-fn reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) op))
               (return-from emit-trampoline-instruction t))
    ;; Unary fixnum operations
    (let ((unary-fmt (gethash tp *wasm-unary-fixnum-table*)))
      (when unary-fmt
        (let ((src (wasm-fixnum-unbox reg-map (vm-src inst))))
          (format stream "~%      ~A"
                  (reg-local-set reg-map (vm-dst inst)
                                 (wasm-fixnum-box (format nil unary-fmt src))))
          (reg-record-fixnum-range
           reg-map
           (vm-dst inst)
           (wasm-range-i31-or-unknown
            (wasm-range-unary (reg-known-fixnum-range reg-map (vm-src inst)) unary-fmt))))
        (return-from emit-trampoline-instruction t)))
    ;; Struct field access (vm-car, vm-cdr) — FR-142: use smart ref.cast
    (let ((struct-fmt (gethash tp *wasm-struct-get-table*)))
      (when struct-fmt
        (let ((type-keyword (if (eq tp 'vm-car) :cons (if (eq tp 'vm-cdr) :cons nil))))
          (format stream "~%      ~A"
                  (reg-local-set reg-map (vm-dst inst)
                                 (let ((src-wat (if (eq type-keyword :cons)
                                                    (wasm-ref-cast-maybe "(ref $cons_t)" reg-map (vm-src inst))
                                                    (reg-local-ref reg-map (vm-src inst)))))
                                   (format nil struct-fmt src-wat))))
          (return-from emit-trampoline-instruction t))))
    ;; Min/max — conditional-select pattern, differs only in comparison opcode
    (let ((cmp-op (gethash tp *wasm-minmax-table*)))
      (when cmp-op
        (let ((l (wasm-fixnum-unbox reg-map (vm-lhs inst)))
              (r (wasm-fixnum-unbox reg-map (vm-rhs inst))))
          (format stream "~%      ~A"
                  (reg-local-set reg-map (vm-dst inst)
                                 (format nil "(if (result eqref) (~A ~A ~A) (then ~A) (else ~A))"
                                         cmp-op l r (wasm-fixnum-box l) (wasm-fixnum-box r))))
          (return-from emit-trampoline-instruction t))))
    ;; FR-234: Sign-extension operations
    (let ((sext-fmt (gethash tp *wasm-sign-extend-table*)))
      (when sext-fmt
        (let ((src (wasm-fixnum-unbox reg-map (vm-src inst))))
          (format stream "~%      (local.set ~D ~A)"
                  (wasm-reg-to-local reg-map (vm-dst inst))
                  (wasm-fixnum-box (format nil sext-fmt src))))
        (return-from emit-trampoline-instruction t)))
    ;; FR-233: Non-trapping float-to-int conversion
    (let ((fti-op (gethash tp *wasm-float-to-int-table*)))
      (when fti-op
        (let ((src-wat (reg-local-ref reg-map (vm-src inst))))
          (format stream "~%      ~A"
                  (reg-local-set reg-map (vm-dst inst)
                                 (wasm-fixnum-box
                                   (format nil "(~A ~A)" fti-op src-wat)))))
        (return-from emit-trampoline-instruction t))))
  ;; FR-218/FR-295/FR-320: native Wasm string builtins.
  (when (maybe-emit-wasm-string-instruction inst reg-map stream :indent 6)
    (return-from emit-trampoline-instruction t))
  ;; Remaining instructions handled by typecase (unique logic per instruction)
  (typecase inst
    (vm-const
     (let ((val (vm-value inst)))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (%wasm-const-value-to-wat val)))
         ;; FR-209: Record type/range info for integer constants (known i31ref)
         (typecase val
           (integer (reg-record-fixnum-range reg-map (vm-dst inst) (cons val val)))
           (vector  (wasm-array-reg-record-kind reg-map (vm-dst inst)
                                                (wasm-vector-literal-kind val)))
           (null    (reg-record-type reg-map (vm-dst inst) :null))))
      t)
     (vm-move
      (format stream "~%      ~A"
              (reg-local-set reg-map (vm-dst inst)
                             (reg-local-ref reg-map (vm-src inst))))
      ;; FR-142: Copy known type from source to destination register
      (let ((known (reg-known-type reg-map (vm-src inst))))
        (when known
          (reg-record-type reg-map (vm-dst inst) known)))
       (let ((range (reg-known-fixnum-range reg-map (vm-src inst))))
         (when range
           (reg-record-fixnum-range reg-map (vm-dst inst) range)))
        (wasm-array-reg-copy-kind reg-map (vm-dst inst) (vm-src inst))
        t)
    (vm-simd-vector-op
     (emit-wasm-vm-simd-vector-op reg-map inst stream)
     t)
    (vm-jump
     (emit-trampoline-jump-to-label (vm-label-name inst) label-pc-map reg-map stream)
     t)
    (vm-jump-zero
     (let ((pc-idx (gethash (vm-label-name inst) label-pc-map)))
       (unless pc-idx
         (format stream "~%      ;; WARNING: unknown label ~S in jump-zero"
                 (vm-label-name inst)))
       (format stream
               "~%      (if (ref.is_null ~A)~%        (then (local.set ~D (i32.const ~D)) (br $dispatch)))"
               (reg-local-ref reg-map (vm-reg inst))
               (wasm-reg-map-pc-index reg-map)
               (or pc-idx 0)))
     t)
    ((or vm-ret vm-halt)
     (format stream "~%      ~A" (reg-local-ref reg-map (vm-reg inst)))
     (format stream "~%      (br $exit)")
     t)
    (vm-get-global
     (format stream "~%      ;; global.get globalidx resolved in module global table")
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                             (format nil "(global.get ~A)"
                                    (vm-global-wat-name (vm-global-name inst)))))
     t)
    (vm-set-global
     (format stream "~%      ;; global.set globalidx resolved in module global table")
     (format stream "~%      (global.set ~A ~A)"
             (vm-global-wat-name (vm-global-name inst))
             (reg-local-ref reg-map (vm-src inst)))
     t)
    (vm-abs
     (let ((src (wasm-fixnum-unbox reg-map (vm-src inst))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (%wasm-if-eqref (format nil "(i64.ge_s ~A (i64.const 0))" src)
                                              (wasm-fixnum-box src)
                                              (wasm-fixnum-box (format nil "(i64.sub (i64.const 0) ~A)" src)))))
       t))
    (vm-integer-length
     (let ((src (wasm-fixnum-unbox reg-map (vm-src inst))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (%wasm-if-eqref (format nil "(i64.eqz ~A)" src)
                                               (wasm-fixnum-box "(i64.const 0)")
                                               (wasm-fixnum-box (format nil "(i64.sub (i64.const 64) (i64.clz ~A))" src)))))
        t))
    (cl-cc/vm::vm-float-sign
     (let ((src (format nil "(struct.get $float_t 0 (ref.cast (ref $float_t) ~A))"
                        (reg-local-ref reg-map (vm-src inst)))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (format nil "(struct.new $float_t ~A)"
                                      (wasm-copysign-wat "(f64.const 1.0)" src))))
       t))
    (vm-ash
     (let ((lhs (wasm-fixnum-unbox reg-map (vm-lhs inst)))
           (rhs (wasm-fixnum-unbox reg-map (vm-rhs inst))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (%wasm-if-eqref (format nil "(i64.ge_s ~A (i64.const 0))" rhs)
                                              (wasm-fixnum-box (format nil "(i64.shl ~A ~A)" lhs rhs))
                                              (wasm-fixnum-box (format nil "(i64.shr_s ~A (i64.sub (i64.const 0) ~A))" lhs rhs)))))
       t))
    (vm-print
     (format stream "~%      (call $host_print_val ~A)"
             (reg-local-ref reg-map (vm-reg inst)))
     t)
    (vm-cons
      (format stream "~%      ~A"
              (reg-local-set reg-map (vm-dst inst)
                             (format nil "(struct.new $cons_t ~A ~A)"
                                     (reg-local-ref reg-map (vm-car-reg inst))
                                     (reg-local-ref reg-map (vm-cdr-reg inst)))))
      ;; FR-142: Record that dst now holds a $cons_t reference
      (reg-record-type reg-map (vm-dst inst) :cons)
      t)
     (vm-closure
      (let* ((label (vm-label-name inst))
              (table-idx (if (and *wasm-label-to-table-idx* label)
                             (gethash label *wasm-label-to-table-idx* 0)
                             0)))
       (format stream "~%      ;; struct.new typeidx ~D; array.new typeidx ~D"
               +type-idx-closure+ +type-idx-eqref-array+)
        (emit-wasm-closure-allocation reg-map (vm-dst inst) table-idx
                                      (vm-captured-vars inst) stream 6)
        t))
    (vm-func-ref
     (let* ((label (vm-label-name inst))
            (table-idx (if (and *wasm-label-to-table-idx* label)
                           (gethash label *wasm-label-to-table-idx* 0)
                           0)))
       (format stream "~%      ;; struct.new typeidx ~D; funcidx ~D"
               +type-idx-closure+ table-idx)
       (emit-wasm-closure-allocation reg-map (vm-dst inst) table-idx nil stream 6)
       t))
    (vm-register-function
     (format stream "~%      ;; register-function ~S from local ~D (module table already populated)"
             (vm-func-name inst)
             (wasm-reg-to-local reg-map (vm-src inst)))
     t)
     (vm-closure-ref-idx
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (wasm-closure-ref-wat reg-map
                                                  (vm-closure-reg inst)
                                                  (vm-closure-index inst))))
      t)
    (vm-make-array
     (let* ((kind (if (and *wasm-gc-array-types-enabled*
                           (not (vm-element-type-reg inst)))
                      (wasm-normalize-array-element-kind (vm-element-type inst))
                      :eqref))
            (init (if (vm-initial-element inst)
                      (wasm-reg-to-array-element-wat reg-map (vm-initial-element inst) kind)
                      (wasm-array-default-wat kind)))
            (size (wasm-fixnum-unbox reg-map (vm-size-reg inst) :result-type :i32)))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (wasm-array-new-wat kind init size
                                                  :default-p (null (vm-initial-element inst)))))
       (wasm-array-reg-record-kind reg-map (vm-dst inst) kind)
       t))
    (vm-aref
     (when (opt-bounds-check-eliminable-marked-p inst)
       (format stream "~%      ;; BCE: explicit bounds check eliminated; array.get remains spec-checked"))
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (wasm-array-get-eqref-wat reg-map
                                                      (vm-array-reg inst)
                                                      (vm-index-reg inst))))
     t)
    (vm-aset
     (when (opt-bounds-check-eliminable-marked-p inst)
       (format stream "~%      ;; BCE: explicit bounds check eliminated; array.set remains spec-checked"))
     (format stream "~%      ~A"
             (wasm-array-set-wat reg-map
                                 (vm-array-reg inst)
                                 (vm-index-reg inst)
                                 (vm-val-reg inst)))
     t)
     (vm-array-length
      (format stream "~%      ~A"
              (reg-local-set reg-map (vm-dst inst)
                             (wasm-fixnum-box
                             (format nil "(i64.extend_i32_u ~A)"
                                      (wasm-array-len-wat reg-map (vm-src inst))))))
      t)
     (vm-fill
      (let* ((arr (vm-array-reg inst))
             (len (wasm-array-len-wat reg-map arr)))
        (format stream "~%      ~A"
                (wasm-array-fill-wat reg-map arr (vm-val-reg inst) "(i32.const 0)" len))
        t))
     (vm-select
      (format stream "~%      ~A"
              (reg-local-set reg-map (vm-dst inst)
                             (emit-wasm-typed-select-wat
                              "eqref"
                              (format nil "(i32.eqz (ref.is_null ~A))"
                                      (reg-local-ref reg-map (vm-select-cond-reg inst)))
                              (reg-local-ref reg-map (vm-select-then-reg inst))
                              (reg-local-ref reg-map (vm-select-else-reg inst)))))
      t)
     (vm-string-length
      (let* ((str (wasm-ref-cast-maybe "(ref $string_t)" reg-map (vm-src inst)))
             (chars (format nil "(struct.get $string_t 0 ~A)" str)))
        (format stream "~%      ~A"
                (reg-local-set reg-map (vm-dst inst)
                               (wasm-fixnum-box
                                (format nil "(i64.extend_i32_u (array.len ~A))" chars))))
        t))
     (vm-char
      (let* ((str (wasm-ref-cast-maybe "(ref $string_t)" reg-map (vm-string-reg inst)))
             (chars (format nil "(struct.get $string_t 0 ~A)" str))
             (idx (wasm-fixnum-unbox reg-map (vm-index inst) :result-type :i32)))
        (format stream "~%      ~A"
                (reg-local-set reg-map (vm-dst inst)
                               (format nil "(ref.i31 ~A)"
                                       (if (wasm-gc-packed-fields-feature-enabled-p)
                                           (format nil "(array.get_u $bytes_array_t ~A ~A)" chars idx)
                                           (format nil "(array.get $bytes_array_t ~A ~A)" chars idx)))))
        t))
     (vm-vector
      (let ((elems (loop for reg in (vm-element-regs inst)
                         collect (reg-local-ref reg-map reg))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (format nil "(array.new_fixed $eqref_array_t ~D~@[ ~A~])"
                                      (length elems)
                                      (and elems (format nil "~{~A~^ ~}" elems)))))
        (wasm-array-reg-record-kind reg-map (vm-dst inst) :eqref)
        t))
    (vm-values
     (format stream "~%      ~A"
             (wasm-values-wat reg-map (vm-dst inst) (vm-src-regs inst) :indent 6))
     t)
    (vm-copy-vector
     (format stream "~%      ~A"
             (wasm-array-copy-wat reg-map
                                  (vm-dst-array-reg inst)
                                  (vm-src-array-reg inst)
                                  (vm-len-reg inst)))
      t)
    (cl-cc/vm::vm-atomic-cas
     (let ((width (wasm-atomic-width-for-inst inst 64)))
       (if (wasm-threads-feature-enabled-p)
           (progn
             (format stream "~%      ;; FR-203/FR-327 atomic CAS width=~D" width)
             (format stream "~%      ~A"
                     (reg-local-set reg-map (vm-dst inst)
                                    (wasm-atomic-result-box-wat
                                     (wasm-atomic-rmw-cmpxchg-wat
                                       reg-map (cl-cc/vm::vm-acas-addr inst)
                                       (cl-cc/vm::vm-acas-expected inst)
                                       (cl-cc/vm::vm-acas-newval inst)
                                      :width width)
                                     :width width))))
           (progn
             (format stream "~%      ;; Wasm threads disabled; non-atomic fallback for vm-atomic-cas")
             (format stream "~%      ~A"
                     (reg-local-set reg-map (vm-dst inst)
                                      (reg-local-ref reg-map (cl-cc/vm::vm-acas-addr inst))))))
       t))
    (cl-cc/vm::vm-atomic-load
     (let ((width (wasm-atomic-width-for-inst inst 64)))
       (if (wasm-threads-feature-enabled-p)
           (progn
             (format stream "~%      ;; FR-203 atomic load width=~D" width)
             (format stream "~%      ~A"
                     (reg-local-set reg-map (vm-dst inst)
                                    (wasm-atomic-result-box-wat
                                      (wasm-atomic-load-wat reg-map (cl-cc/vm::vm-aload-addr inst)
                                                           :width width)
                                     :width width))))
           (progn
             (format stream "~%      ;; Wasm threads disabled; non-atomic fallback for vm-atomic-load")
             (format stream "~%      ~A"
                     (reg-local-set reg-map (vm-dst inst)
                                      (reg-local-ref reg-map (cl-cc/vm::vm-aload-addr inst))))))
       t))
    (cl-cc/vm::vm-atomic-store
     (let ((width (wasm-atomic-width-for-inst inst 64)))
       (if (wasm-threads-feature-enabled-p)
           (progn
             (format stream "~%      ;; FR-203 atomic store width=~D" width)
             (format stream "~%      ~A"
                      (wasm-atomic-store-wat reg-map (cl-cc/vm::vm-astore-addr inst)
                                             (cl-cc/vm::vm-astore-val inst)
                                            :width width)))
           (progn
             (format stream "~%      ;; Wasm threads disabled; non-atomic fallback for vm-atomic-store")
             (format stream "~%      (local.set ~D ~A)"
                      (wasm-reg-to-local reg-map (cl-cc/vm::vm-astore-addr inst))
                       (reg-local-ref reg-map (cl-cc/vm::vm-astore-val inst)))))
       t))
    (cl-cc/vm::vm-atomic-incf
     (let ((width (wasm-atomic-width-for-inst inst 64)))
       (if (wasm-threads-feature-enabled-p)
           (progn
             (format stream "~%      ;; FR-203/FR-327 atomic fetch-add width=~D" width)
             (format stream "~%      ~A"
                     (reg-local-set reg-map (vm-dst inst)
                                    (wasm-atomic-result-box-wat
                                     (wasm-atomic-rmw-add-wat
                                       reg-map (cl-cc/vm::vm-aincf-addr inst)
                                       (cl-cc/vm::vm-aincf-delta inst)
                                      :width width)
                                     :width width))))
            (let ((old (reg-local-ref reg-map (cl-cc/vm::vm-aincf-addr inst)))
                 (new (wasm-fixnum-box
                       (format nil "(i64.add ~A ~A)"
                                (wasm-fixnum-unbox reg-map (cl-cc/vm::vm-aincf-addr inst))
                                (wasm-fixnum-unbox reg-map (cl-cc/vm::vm-aincf-delta inst))))))
             (format stream "~%      ;; Wasm threads disabled; non-atomic fallback for vm-atomic-incf")
             (format stream "~%      ~A" (reg-local-set reg-map (vm-dst inst) old))
              (format stream "~%      ~A" (reg-local-set reg-map (cl-cc/vm::vm-aincf-addr inst) new))))
       t))
    (cl-cc/vm::vm-atomic-swap
     (format stream "~%      ;; vm-atomic-swap fallback; Wasm xchg lowering pending")
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                             (reg-local-ref reg-map (cl-cc/vm::vm-aswap-addr inst))))
     (format stream "~%      ~A"
             (reg-local-set reg-map (cl-cc/vm::vm-aswap-addr inst)
                            (reg-local-ref reg-map (cl-cc/vm::vm-aswap-newval inst))))
     t)
    ((or cl-cc/vm::vm-memory-barrier cl-cc/vm::vm-load-fence cl-cc/vm::vm-store-fence)
     (if (wasm-threads-feature-enabled-p)
         (format stream "~%      atomic.fence")
         (format stream "~%      ;; Wasm threads disabled; fence elided"))
     t)
     (vm-call
       (let* ((args (vm-args inst))
              (func-local (reg-local-ref reg-map (vm-func-reg inst)))
             (dst-idx (wasm-reg-to-local reg-map (vm-dst inst)))
             ;; FR-142: Use wasm-ref-cast-maybe to skip ref.cast when type is known
              (entry-idx-wat (format nil
                                     "(struct.get $closure_t 0 ~A)"
                                     (wasm-ref-cast-maybe "(ref $closure_t)" reg-map (vm-func-reg inst)))))
         (loop for arg in args for i from 0 do
           (format stream "~%      (global.set $cl_arg~D ~A)" i (reg-local-ref reg-map arg)))
         (format stream "~%      ;; call_indirect typeidx ~D tableidx 0"
                 +type-idx-main-func+)
          (format stream "~%      (local.set ~D ~A)"
                  dst-idx
                  (wasm-call-indirect-wat "$main_func_t" "$funcref_table" entry-idx-wat))
          t))
    (vm-establish-catch
      ;; FR-204: The trampoline builder wraps the PC-dispatch loop in one native
      ;; try/catch and projects this dynamic extent into its catch dispatch table.
      ;; Keep the stub visible, but do not emit an unmatched local try here.
      (push (list :kind :catch
                  :handler-label (vm-catch-handler-label inst)
                  :result-reg (vm-catch-result-reg inst)
                  :tag-reg (vm-catch-tag-reg inst))
            (wasm-reg-map-try-stack reg-map))
      (incf (wasm-reg-map-try-depth reg-map))
      (format stream "~%      ;; native try/catch catch-frame => ~S (depth ~D)"
              (vm-catch-handler-label inst)
              (wasm-reg-map-try-depth reg-map))
       t)
     (vm-establish-handler
      (push (list :kind :handler
                  :handler-label (vm-handler-label inst)
                  :result-reg (vm-handler-result-reg inst)
                  :condition-type (vm-error-type inst))
            (wasm-reg-map-try-stack reg-map))
      (incf (wasm-reg-map-try-depth reg-map))
      (format stream "~%      ;; native try/catch handler-frame => ~S (depth ~D)"
              (vm-handler-label inst)
              (wasm-reg-map-try-depth reg-map))
       t)
     (vm-remove-handler
      (when (wasm-reg-map-try-stack reg-map)
        (pop (wasm-reg-map-try-stack reg-map))
        (decf (wasm-reg-map-try-depth reg-map)))
      (format stream "~%      ;; native try/catch frame closed by trampoline wrapper (depth ~D)"
              (wasm-reg-map-try-depth reg-map))
       t)
    (vm-sync-handler-regs
      (format stream "~%      ;; FR-204: handler regs synced — locals update automatically in WASM")
      t)
    (vm-signal-error
      ;; FR-204/FR-252: Native exception throw.  EH v2 uses throw_ref through
      ;; the JS/host bridge so a fresh CL condition can be represented as exnref.
      (if (wasm-eh-v2-feature-enabled-p)
          (format stream "~%      (throw_ref (call $cl_condition_to_exnref ~A))"
                  (reg-local-ref reg-map (vm-error-reg inst)))
          (format stream "~%      (throw $cl_condition_tag (ref.null eq) ~A)"
                  (reg-local-ref reg-map (vm-error-reg inst))))
      t)
    (vm-throw
      ;; FR-204: General throw with tag and value
      (format stream "~%      ;; throw tagidx ~D" +tag-idx-cl-condition+)
      (format stream "~%      (throw $cl_condition_tag ~A ~A)"
              (reg-local-ref reg-map (vm-throw-tag-reg inst))
              (reg-local-ref reg-map (vm-throw-value-reg inst)))
      t)
    (vm-null-p
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (wasm-bool-to-i31
                             (format nil "(ref.is_null ~A)"
                                     (reg-local-ref reg-map (vm-src inst))))))
     t)
    (vm-not
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (%wasm-if-eqref (format nil "(ref.is_null ~A)" (reg-local-ref reg-map (vm-src inst)))
                                            "(ref.i31 (i32.const 1))"
                                            "(ref.null eq)")))
     t)
    (t
     (format stream "~%      ;; UNSUPPORTED: ~A" (type-of inst))
     nil)))

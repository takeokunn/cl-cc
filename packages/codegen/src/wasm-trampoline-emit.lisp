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
      (format stream "~%            (i32.wrap_i64 (i64.sub ~A (i64.const ~D))))"
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
                                 (wasm-fixnum-box (format nil unary-fmt src)))))
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
  ;; Remaining instructions handled by typecase (unique logic per instruction)
  (typecase inst
    (vm-const
     (let ((val (vm-value inst)))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (%wasm-const-value-to-wat val)))
       ;; FR-145: Record type info for integer constants (known fixnum range)
       (typecase val
         (integer (reg-record-type reg-map (vm-dst inst) :i31ref))
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
         (format stream "~%      (local.set ~D (call_indirect (type $main_func_t) (table $funcref_table) ~A))"
                 dst-idx entry-idx-wat)
         t))
    (vm-establish-catch
     (format stream "~%      ;; establish catch: native EH tag $cl_condition_tag, handler ~S, result local ~D"
             (vm-catch-handler-label inst)
             (wasm-reg-to-local reg-map (vm-catch-result-reg inst)))
     t)
    (vm-establish-handler
     (format stream "~%      ;; establish handler: native EH tag $cl_condition_tag, handler ~S, result local ~D"
             (vm-handler-label inst)
             (wasm-reg-to-local reg-map (vm-handler-result-reg inst)))
     t)
    (vm-remove-handler
     (format stream "~%      ;; remove handler/catch frame (structured by WASM try/catch)")
     t)
    (vm-sync-handler-regs
     (format stream "~%      ;; sync handler registers (implicit in WASM locals)")
     t)
    (vm-signal-error
     (format stream "~%      (throw $cl_condition_tag (ref.null eq) ~A)"
             (reg-local-ref reg-map (vm-error-reg inst)))
     t)
    (vm-throw
     (format stream "~%      ;; throw tagidx ~D"
             +tag-idx-cl-condition+)
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

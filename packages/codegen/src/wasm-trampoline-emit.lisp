;;;; packages/emit/src/wasm-trampoline-emit.lisp — WASM Trampoline Instruction Emitter
;;;;
;;;; Data tables: wasm-trampoline-tables.lisp (loads before this file).
;;;; Program builder: wasm-trampoline-build.lisp (loads after this file).
;;;;
;;;; Load order: after wasm-trampoline-tables.lisp.

(in-package :cl-cc/codegen)

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
    ;; Struct field access (vm-car, vm-cdr)
    (let ((struct-fmt (gethash tp *wasm-struct-get-table*)))
      (when struct-fmt
        (format stream "~%      ~A"
                (reg-local-set reg-map (vm-dst inst)
                               (format nil struct-fmt
                                       (reg-local-ref reg-map (vm-src inst)))))
        (return-from emit-trampoline-instruction t)))
    ;; Min/max — conditional-select pattern, differs only in comparison opcode
    (let ((cmp-op (gethash tp *wasm-minmax-table*)))
      (when cmp-op
        (let ((l (wasm-fixnum-unbox reg-map (vm-lhs inst)))
              (r (wasm-fixnum-unbox reg-map (vm-rhs inst))))
          (format stream "~%      ~A"
                  (reg-local-set reg-map (vm-dst inst)
                                 (format nil "(if (result eqref) (~A ~A ~A) (then ~A) (else ~A))"
                                         cmp-op l r (wasm-fixnum-box l) (wasm-fixnum-box r))))
          (return-from emit-trampoline-instruction t)))))
  ;; Remaining instructions handled by typecase (unique logic per instruction)
  (typecase inst
    (vm-const
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (%wasm-const-value-to-wat (vm-value inst))))
     t)
    (vm-move
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (reg-local-ref reg-map (vm-src inst))))
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
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (format nil "(global.get ~A)"
                                    (vm-global-wat-name (vm-global-name inst)))))
     t)
    (vm-set-global
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
     t)
    (vm-closure
     (let* ((label (vm-label-name inst))
            (table-idx (if (and *wasm-label-to-table-idx* label)
                           (gethash label *wasm-label-to-table-idx* 0)
                           0)))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (format nil "(struct.new $closure_t (i32.const ~D) (ref.null $env_t))"
                                      table-idx)))
       t))
    (vm-call
     (let* ((args (vm-args inst))
            (func-local (reg-local-ref reg-map (vm-func-reg inst)))
            (dst-idx (wasm-reg-to-local reg-map (vm-dst inst)))
            (entry-idx-wat (format nil
                                   "(struct.get $closure_t 0 (ref.cast (ref $closure_t) ~A))"
                                   func-local)))
       (loop for arg in args for i from 0 do
         (format stream "~%      (global.set $cl_arg~D ~A)" i (reg-local-ref reg-map arg)))
       (format stream "~%      (local.set ~D (call_indirect (type $main_func_t) (table $funcref_table) ~A))"
               dst-idx entry-idx-wat)
       t))
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

;;;; compile/builtin-registry-emitters.lisp — Per-convention emitter functions
;;;
;;; Contains:
;;;   - 27 emit-builtin-* functions (one per calling convention)
;;;   - *builtin-emitter-table* (convention keyword → #'emitter)
;;;   - *convention-arity* (convention → (min . max) arg count)
;;;   - emit-registered-builtin — unified dispatcher (public entry point)
;;;
;;; Registry struct, *builtin-registry* hash table, and registration helpers
;;; are in builtin-registry.lisp (loads before).
;;;
;;; Load order: after builtin-registry.lisp.
(in-package :cl-cc)

;;; ─── Generic Emitters ──────────────────────────────────────────────────────
;;;
;;; Each emitter takes (entry args result-reg ctx) and emits VM instructions
;;; for the corresponding calling convention.  Returns result-reg.

(defun emit-builtin-unary (entry args result-reg ctx)
  (let ((arg-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :src arg-reg))
    result-reg))

(defun emit-builtin-binary (entry args result-reg ctx)
  (let ((lhs-reg (compile-ast (first args) ctx))
        (rhs-reg (compile-ast (second args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :lhs lhs-reg :rhs rhs-reg))
    result-reg))

(defun emit-builtin-string-cmp (entry args result-reg ctx)
  (let ((arg1-reg (compile-ast (first args) ctx))
        (arg2-reg (compile-ast (second args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :str1 arg1-reg :str2 arg2-reg))
    result-reg))

(defun emit-builtin-char-cmp (entry args result-reg ctx)
  (let ((c1-reg (compile-ast (first args) ctx))
        (c2-reg (compile-ast (second args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :char1 c1-reg :char2 c2-reg))
    result-reg))

(defun emit-builtin-table-query (entry args result-reg ctx)
  (let ((table-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :table table-reg))
    result-reg))

(defun emit-builtin-handle-input (entry args result-reg ctx)
  (let ((handle-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :handle handle-reg))
    result-reg))

(defun emit-builtin-side-effect (entry args result-reg ctx)
  (let ((arg-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :src arg-reg))
    (emit ctx (make-vm-move :dst result-reg :src arg-reg))
    result-reg))

(defun emit-builtin-void-side-eff (entry _args result-reg ctx)
  (declare (ignore _args))
  (emit ctx (funcall (be-ctor entry)))
  (emit ctx (make-vm-const :dst result-reg :value nil))
  result-reg)

(defun emit-builtin-nullary (entry _args result-reg ctx)
  (declare (ignore _args))
  (emit ctx (funcall (be-ctor entry) :dst result-reg))
  result-reg)

(defun emit-builtin-string-trim (entry args result-reg ctx)
  (let ((bag-reg (compile-ast (first args) ctx))
        (str-reg (compile-ast (second args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :char-bag bag-reg :string str-reg))
    result-reg))

(defun emit-builtin-handle-effect (entry args result-reg ctx)
  (let ((handle-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :handle handle-reg))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

(defun emit-builtin-zero-compare (entry args result-reg ctx)
  "Emit a comparison of a single argument against zero."
  (let ((arg-reg (compile-ast (first args) ctx))
        (zero-reg (make-register ctx)))
    (emit ctx (make-vm-const :dst zero-reg :value 0))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :lhs arg-reg :rhs zero-reg))
    result-reg))

(defun emit-builtin-stream-input-opt (entry args result-reg ctx)
  "Emit (fn &optional stream): instruction has :dst :handle.
   Slots = (default-handle-value).  If no arg, synthesize default handle."
  (let* ((default-handle (first (be-slots entry)))
         (handle-reg (if (>= (length args) 1)
                         (compile-ast (first args) ctx)
                         (let ((r (make-register ctx)))
                           (emit ctx (make-vm-const :dst r :value default-handle))
                           r))))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :handle handle-reg))
    result-reg))

(defun emit-builtin-stream-void-opt (entry args result-reg ctx)
  "Emit (fn &optional stream): instruction has :handle only, returns nil.
   Slots = (default-handle-value)."
  (let* ((default-handle (first (be-slots entry)))
         (handle-reg (if (>= (length args) 1)
                         (compile-ast (first args) ctx)
                         (let ((r (make-register ctx)))
                           (emit ctx (make-vm-const :dst r :value default-handle))
                           r))))
    (emit ctx (funcall (be-ctor entry) :handle handle-reg))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

(defun emit-builtin-stream-write-val (entry args result-reg ctx)
  "Emit (fn value &optional stream): instruction has :handle + custom value slot.
   Slots = (value-slot-keyword default-handle-value).  Returns value."
  (let* ((val-slot (first (be-slots entry)))
         (default-handle (second (be-slots entry)))
         (val-reg (compile-ast (first args) ctx))
         (handle-reg (if (>= (length args) 2)
                         (compile-ast (second args) ctx)
                         (let ((r (make-register ctx)))
                           (emit ctx (make-vm-const :dst r :value default-handle))
                           r))))
    (emit ctx (funcall (be-ctor entry) :handle handle-reg val-slot val-reg))
    (emit ctx (make-vm-move :dst result-reg :src val-reg))
    result-reg))

(defun emit-builtin-ternary-custom (entry args result-reg ctx)
  "Emit (fn a b c) with 3 custom slot names from be-slots.
   Slots = (slot1 slot2 slot3 return-style).
   :dst return — instruction has :dst, result comes from it.
   :move-third return — void instruction, result←third-arg via move."
  (let* ((slots (be-slots entry))
         (s1 (first slots)) (s2 (second slots)) (s3 (third slots))
         (ret (fourth slots))
         (a-reg (compile-ast (first args) ctx))
         (b-reg (compile-ast (second args) ctx))
         (c-reg (compile-ast (third args) ctx)))
    (ecase ret
      (:dst
       (emit ctx (funcall (be-ctor entry) :dst result-reg
                          s1 a-reg s2 b-reg s3 c-reg))
       result-reg)
      (:move-third
       (emit ctx (funcall (be-ctor entry) s1 a-reg s2 b-reg s3 c-reg))
       (emit ctx (make-vm-move :dst result-reg :src c-reg))
       result-reg))))

(defun emit-builtin-binary-custom (entry args result-reg ctx)
  "Parametric binary emitter: reads slot names from (be-slots entry)."
  (let ((lhs-reg (compile-ast (first args) ctx))
        (rhs-reg (compile-ast (second args) ctx))
        (slots (be-slots entry)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg
                       (first slots) lhs-reg (second slots) rhs-reg))
    result-reg))

(defun emit-builtin-binary-move-first (entry args result-reg ctx)
  "Binary emitter that emits a void instruction (no :dst), then moves arg1→result.
   Used for RPLACA/RPLACD which return the modified cons."
  (let ((lhs-reg (compile-ast (first args) ctx))
        (rhs-reg (compile-ast (second args) ctx))
        (slots (be-slots entry)))
    (emit ctx (funcall (be-ctor entry) (first slots) lhs-reg (second slots) rhs-reg))
    (emit ctx (make-vm-move :dst result-reg :src lhs-reg))
    result-reg))

(defun emit-builtin-binary-void (entry args result-reg ctx)
  "Binary emitter that emits a void instruction (no :dst) and returns nil.
   Used for REMHASH, UNREAD-CHAR."
  (let ((lhs-reg (compile-ast (first args) ctx))
        (rhs-reg (compile-ast (second args) ctx))
        (slots (be-slots entry)))
    (emit ctx (funcall (be-ctor entry) (first slots) lhs-reg (second slots) rhs-reg))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

(defun emit-builtin-unary-custom-void (entry args result-reg ctx)
  "Unary emitter with a custom slot name (no :dst), returns nil.
   Used for %PROGV-EXIT, ERROR, WARN."
  (let ((arg-reg (compile-ast (first args) ctx))
        (slot (first (be-slots entry))))
    (emit ctx (funcall (be-ctor entry) slot arg-reg))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

(defun emit-builtin-binary-synth-zero (entry args result-reg ctx)
  "Binary emitter with :dst + 3 custom slots. Synthesizes 0-const for 3rd slot.
   Used for SEARCH."
  (let* ((slots (be-slots entry))
         (a-reg (compile-ast (first args) ctx))
         (b-reg (compile-ast (second args) ctx))
         (zero-reg (make-register ctx)))
    (emit ctx (make-vm-const :dst zero-reg :value 0))
    (emit ctx (funcall (be-ctor entry) :dst result-reg
                       (first slots) a-reg (second slots) b-reg (third slots) zero-reg))
    result-reg))

(defun emit-builtin-binary-opt-nil-slot (entry args result-reg ctx)
  "Binary emitter with :dst + 2 custom slots. 2nd arg optional, nil slot when absent.
   Used for INTERN."
  (let* ((slots (be-slots entry))
         (a-reg (compile-ast (first args) ctx))
         (b-reg (when (second args) (compile-ast (second args) ctx))))
    (emit ctx (funcall (be-ctor entry) :dst result-reg
                       (first slots) a-reg (second slots) b-reg))
    result-reg))

(defun emit-builtin-ternary-opt-nil-custom (entry args result-reg ctx)
  "Ternary emitter with :dst + 3 custom slots. 3rd arg optional, nil when absent.
   Used for GET, SUBSEQ."
  (let* ((slots (be-slots entry))
         (a-reg (compile-ast (first args) ctx))
         (b-reg (compile-ast (second args) ctx))
         (c-reg (if (>= (length args) 3)
                    (compile-ast (third args) ctx)
                    (let ((r (make-register ctx)))
                      (emit ctx (make-vm-const :dst r :value nil)) r))))
    (emit ctx (funcall (be-ctor entry) :dst result-reg
                       (first slots) a-reg (second slots) b-reg (third slots) c-reg))
    result-reg))

(defun emit-builtin-binary-opt-one (entry args result-reg ctx)
  "Binary emitter with optional 2nd arg defaulting to 1.
   Standard :dst :lhs :rhs slots.  Used for FFLOOR/FCEILING/FTRUNCATE/FROUND."
  (let* ((lhs-reg (compile-ast (first args) ctx))
         (rhs-reg (if (= (length args) 2)
                      (compile-ast (second args) ctx)
                      (let ((r (make-register ctx)))
                        (emit ctx (make-vm-const :dst r :value 1)) r))))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :lhs lhs-reg :rhs rhs-reg))
    result-reg))

(defun emit-builtin-unary-opt-nil (entry args result-reg ctx)
  "Unary emitter with optional arg defaulting to nil.
   Standard :dst :src slots.  Used for MAKE-RANDOM-STATE."
  (let ((src-reg (if args
                     (compile-ast (first args) ctx)
                     (let ((r (make-register ctx)))
                       (emit ctx (make-vm-const :dst r :value nil)) r))))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :src src-reg))
    result-reg))

(defun emit-builtin-unary-custom (entry args result-reg ctx)
  "Unary emitter with :dst and a custom slot name.
   Used for MAKE-LIST."
  (let ((arg-reg (compile-ast (first args) ctx))
        (slot (first (be-slots entry))))
    (emit ctx (funcall (be-ctor entry) :dst result-reg slot arg-reg))
    result-reg))

;;; *builtin-emitter-table*, *convention-arity*, and emit-registered-builtin
;;; are in builtin-registry-dispatch.lisp (loads next).

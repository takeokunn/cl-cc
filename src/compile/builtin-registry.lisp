;;;; compile/builtin-registry.lisp — Registry struct, emitters, and dispatcher
;;;
;;; Entry data tables live in builtin-registry-data.lisp (loaded first).
;;; This file defines the builtin-entry struct, registration helpers,
;;; per-convention emitter functions, and the unified dispatcher.
;;;
;;; 26 calling conventions (keys in *builtin-emitter-table*):
;;;
;;; ── Standard (original 17) ──────────────────────────────────────────
;;;   :unary              (fn arg)              → (:dst :src)
;;;   :binary             (fn a b)              → (:dst :lhs :rhs)
;;;   :binary-custom      (fn a b)              → (:dst slot1 slot2) parametric
;;;   :string-cmp         (fn s1 s2)            → (:dst :str1 :str2)
;;;   :char-cmp           (fn c1 c2)            → (:dst :char1 :char2)
;;;   :table-query        (fn table)            → (:dst :table)
;;;   :handle-input       (fn handle)           → (:dst :handle)
;;;   :side-effect        (fn arg)              → (:src) + move dst←src
;;;   :void-side-eff      (fn)                  → () + const dst←nil
;;;   :nullary            (fn)                  → (:dst)
;;;   :string-trim        (fn bag str)          → (:dst :char-bag :string)
;;;   :handle-effect      (fn handle)           → (:handle) + const dst←nil
;;;   :zero-compare       (fn arg)              → const zero←0, (:dst :lhs arg :rhs zero)
;;;   :stream-input-opt   (fn &opt stream)      → (:dst :handle), default from slots
;;;   :stream-void-opt    (fn &opt stream)      → (:handle), const dst←nil, default from slots
;;;   :stream-write-val   (fn val &opt stream)  → (:handle :val-slot), move dst←val
;;;   :ternary-custom     (fn a b c)            → 3 custom slots + :dst
;;;
;;; ── Phase 2 additions (9 new) ───────────────────────────────────────
;;;   :binary-move-first  (fn a b)              → emit both, move dst←first
;;;   :binary-void        (fn a b)              → 2 custom slots, const dst←nil
;;;   :unary-custom-void  (fn arg)              → 1 custom slot, no :dst, const dst←nil
;;;   :unary-custom       (fn arg)              → :dst + 1 custom slot
;;;   :unary-opt-nil      (fn &opt arg)         → (:dst :src), nil default
;;;   :binary-opt-one     (fn a &opt b)         → (:dst :lhs :rhs), 1 default
;;;   :ternary-opt-nil-custom (fn a b &opt c)   → :dst + 3 custom slots, nil default
;;;   :binary-opt-nil-slot (fn a &opt b)        → :dst + 2 custom slots, nil default
;;;   :binary-synth-zero  (fn a b)              → :dst + 3 custom slots, zero synth
(in-package :cl-cc)

;;; ─── Registry Entry ────────────────────────────────────────────────────────

(defstruct (builtin-entry (:conc-name be-))
  "A single builtin dispatch entry."
  (name-str  ""     :type string)       ; uppercase CL name string
  (convention :unary :type keyword)     ; calling convention tag
  (ctor       nil    :type symbol)      ; make-vm-* constructor symbol
  (slots      nil    :type list))       ; optional: slot keywords for parametric conventions

;;; Data tables are in builtin-registry-data.lisp (loaded before this file).

;;; ─── Unified Registry Hash Table ───────────────────────────────────────────

(defparameter *builtin-registry* (make-hash-table :test #'equal)
  "Maps uppercase CL function name strings to builtin-entry structs.
   Populated at load time from the category alists above.")

(defun %register-builtins (alist convention)
  "Register all entries from ALIST under CONVENTION in *builtin-registry*.
   Also emits a Prolog fact (builtin-<convention> cl-sym vm-ctor) for each entry."
  (dolist (pair alist)
    (let ((name-str (symbol-name (car pair)))
          (cl-sym   (car pair))
          (ctor     (cdr pair)))
      (setf (gethash name-str *builtin-registry*)
            (make-builtin-entry :name-str name-str
                                :convention convention
                                :ctor ctor))
      (let ((pred (intern (format nil "BUILTIN-~A" (symbol-name convention)) :cl-cc)))
        (add-rule pred (make-prolog-rule :head (list pred cl-sym ctor)))))))

(defun %register-slots-builtins (entries convention)
  "Register parametric entries under CONVENTION in *builtin-registry*.
   Each entry is (cl-sym vm-ctor . slots): the tail after ctor is stored as be-slots.
   Handles all multi-slot conventions (binary-custom, stream-*, ternary-*, unary-custom, etc.)."
  (dolist (e entries)
    (let* ((cl-sym   (first e))
           (ctor     (second e))
           (slots    (cddr e))
           (name-str (symbol-name cl-sym)))
      (setf (gethash name-str *builtin-registry*)
            (make-builtin-entry :name-str name-str
                                :convention convention
                                :ctor ctor
                                :slots slots))
      (let ((pred (intern (format nil "BUILTIN-~A" (symbol-name convention)) :cl-cc)))
        (add-rule pred (make-prolog-rule :head (list* pred cl-sym ctor slots)))))))

;; Populate at load time
;; Simple (sym . ctor) alist entries — no per-entry slots
(%register-builtins *builtin-unary-entries*            :unary)
(%register-builtins *builtin-binary-entries*           :binary)
(%register-builtins *builtin-string-cmp-entries*       :string-cmp)
(%register-builtins *builtin-char-cmp-entries*         :char-cmp)
(%register-builtins *builtin-table-query-entries*      :table-query)
(%register-builtins *builtin-handle-input-entries*     :handle-input)
(%register-builtins *builtin-side-effect-entries*      :side-effect)
(%register-builtins *builtin-void-side-effect-entries* :void-side-eff)
(%register-builtins *builtin-nullary-entries*          :nullary)
(%register-builtins *builtin-string-trim-entries*      :string-trim)
(%register-builtins *builtin-handle-effect-entries*    :handle-effect)
(%register-builtins *builtin-unary-opt-nil-entries*    :unary-opt-nil)
(%register-builtins *builtin-binary-opt-one-entries*   :binary-opt-one)
(%register-builtins *builtin-zero-compare-entries*     :zero-compare)
;; Parametric (sym ctor . slots) entries — slots stored in be-slots
(%register-slots-builtins *builtin-binary-custom-entries*          :binary-custom)
(%register-slots-builtins *builtin-binary-move-first-entries*      :binary-move-first)
(%register-slots-builtins *builtin-binary-void-entries*            :binary-void)
(%register-slots-builtins *builtin-unary-custom-void-entries*      :unary-custom-void)
(%register-slots-builtins *builtin-binary-opt-nil-slot-entries*    :binary-opt-nil-slot)
(%register-slots-builtins *builtin-binary-synth-zero-entries*      :binary-synth-zero)
(%register-slots-builtins *builtin-ternary-opt-nil-custom-entries* :ternary-opt-nil-custom)
(%register-slots-builtins *builtin-unary-custom-entries*           :unary-custom)
(%register-slots-builtins *builtin-stream-input-opt-entries*       :stream-input-opt)
(%register-slots-builtins *builtin-stream-void-opt-entries*        :stream-void-opt)
(%register-slots-builtins *builtin-stream-write-val-entries*       :stream-write-val)
(%register-slots-builtins *builtin-ternary-custom-entries*         :ternary-custom)

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

;;; ─── Convention Dispatcher ─────────────────────────────────────────────────

(defparameter *builtin-emitter-table*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (pair '((:unary              . emit-builtin-unary)
                    (:binary             . emit-builtin-binary)
                    (:string-cmp         . emit-builtin-string-cmp)
                    (:char-cmp           . emit-builtin-char-cmp)
                    (:table-query        . emit-builtin-table-query)
                    (:handle-input       . emit-builtin-handle-input)
                    (:side-effect        . emit-builtin-side-effect)
                    (:void-side-eff      . emit-builtin-void-side-eff)
                    (:nullary            . emit-builtin-nullary)
                    (:string-trim        . emit-builtin-string-trim)
                    (:handle-effect      . emit-builtin-handle-effect)
                    (:binary-custom      . emit-builtin-binary-custom)
                    (:binary-move-first  . emit-builtin-binary-move-first)
                    (:binary-void        . emit-builtin-binary-void)
                    (:unary-custom-void  . emit-builtin-unary-custom-void)
                    (:unary-custom       . emit-builtin-unary-custom)
                    (:unary-opt-nil      . emit-builtin-unary-opt-nil)
                    (:binary-opt-one     . emit-builtin-binary-opt-one)
                    (:ternary-opt-nil-custom . emit-builtin-ternary-opt-nil-custom)
                    (:binary-opt-nil-slot . emit-builtin-binary-opt-nil-slot)
                    (:binary-synth-zero  . emit-builtin-binary-synth-zero)
                    (:zero-compare       . emit-builtin-zero-compare)
                    (:stream-input-opt   . emit-builtin-stream-input-opt)
                    (:stream-void-opt    . emit-builtin-stream-void-opt)
                    (:stream-write-val   . emit-builtin-stream-write-val)
                    (:ternary-custom     . emit-builtin-ternary-custom)))
      (setf (gethash (car pair) ht)
            (symbol-function (cdr pair))))
    ht)
  "Maps convention keywords to their emitter functions.")

(defparameter *convention-arity*
  '((:unary          1 . 1)  (:binary          2 . 2)  (:binary-custom    2 . 2)
    (:string-cmp     2 . 2)  (:char-cmp        2 . 2)  (:table-query      1 . 1)
    (:handle-input   1 . 1)  (:side-effect     1 . 1)  (:void-side-eff    0 . 0)
    (:nullary        0 . 0)  (:string-trim     2 . 2)  (:handle-effect    1 . 1)
    (:zero-compare   1 . 1)  (:unary-custom    1 . 1)  (:unary-custom-void 1 . 1)
    (:binary-move-first 2 . 2) (:binary-void  2 . 2)  (:binary-synth-zero 2 . 2)
    (:unary-opt-nil  0 . 1)  (:binary-opt-one  1 . 2)  (:binary-opt-nil-slot 1 . 2)
    (:ternary-custom 3 . 3)  (:ternary-opt-nil-custom 2 . 3)
    (:stream-input-opt 0 . 1) (:stream-void-opt 0 . 1) (:stream-write-val 1 . 2))
  "Alist of (convention min-args . max-args) for argument count validation.")

(defun emit-registered-builtin (entry args result-reg ctx)
  "Dispatch to the correct emitter for ENTRY's calling convention.
   Returns result-reg on success, or NIL if arg count is out of range."
  (let* ((conv (be-convention entry))
         (arity (cdr (assoc conv *convention-arity* :test #'eq)))
         (nargs (length args)))
    (when (and arity (or (< nargs (car arity)) (> nargs (cdr arity))))
      (return-from emit-registered-builtin nil))
    (let ((emitter (gethash conv *builtin-emitter-table*)))
      (when emitter
        (funcall emitter entry args result-reg ctx)))))

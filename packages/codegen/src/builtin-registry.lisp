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
(in-package :cl-cc/codegen)

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
      (let ((pred (intern (format nil "BUILTIN-~A" (symbol-name convention)))))
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
      (let ((pred (intern (format nil "BUILTIN-~A" (symbol-name convention)))))
        (add-rule pred (make-prolog-rule :head (list* pred cl-sym ctor slots)))))))

;; Populate at load time — each spec is (variable-name . convention-keyword).
;; symbol-value resolves the defparameter at load time (safe: defparameters
;; are defined in builtin-registry-data*.lisp, which loads before this file).
;;
;; Simple (sym . ctor) alist entries — no per-entry slots
(dolist (spec '((*builtin-unary-entries*            . :unary)
                (*builtin-binary-entries*           . :binary)
                (*builtin-string-cmp-entries*       . :string-cmp)
                (*builtin-char-cmp-entries*         . :char-cmp)
                (*builtin-table-query-entries*      . :table-query)
                (*builtin-handle-input-entries*     . :handle-input)
                (*builtin-side-effect-entries*      . :side-effect)
                (*builtin-void-side-effect-entries* . :void-side-eff)
                (*builtin-nullary-entries*          . :nullary)
                (*builtin-string-trim-entries*      . :string-trim)
                (*builtin-handle-effect-entries*    . :handle-effect)
                (*builtin-unary-opt-nil-entries*    . :unary-opt-nil)
                (*builtin-binary-opt-one-entries*   . :binary-opt-one)
                (*builtin-zero-compare-entries*     . :zero-compare)))
  (%register-builtins (symbol-value (car spec)) (cdr spec)))
;; Parametric (sym ctor . slots) entries — slots stored in be-slots
(dolist (spec '((*builtin-binary-custom-entries*          . :binary-custom)
                (*builtin-binary-move-first-entries*      . :binary-move-first)
                (*builtin-binary-void-entries*            . :binary-void)
                (*builtin-unary-custom-void-entries*      . :unary-custom-void)
                (*builtin-binary-opt-nil-slot-entries*    . :binary-opt-nil-slot)
                (*builtin-binary-synth-zero-entries*      . :binary-synth-zero)
                (*builtin-ternary-opt-nil-custom-entries* . :ternary-opt-nil-custom)
                (*builtin-unary-custom-entries*           . :unary-custom)
                (*builtin-stream-input-opt-entries*       . :stream-input-opt)
                (*builtin-stream-void-opt-entries*        . :stream-void-opt)
                (*builtin-stream-write-val-entries*       . :stream-write-val)
                (*builtin-ternary-custom-entries*         . :ternary-custom)))
  (%register-slots-builtins (symbol-value (car spec)) (cdr spec)))


;;; (Emitter functions, *builtin-emitter-table*, *convention-arity*,
;;;  and emit-registered-builtin are in builtin-registry-emitters.lisp
;;;  which loads after this file.)


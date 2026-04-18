;;;; cli/src/main-dump.lisp — ANSI color codes, dump functions, compile-opts struct
;;;
;;; Extracted from main-utils.lisp.
;;; Contains:
;;;   - +ansi-*+ color code parameters
;;;   - %dump-{ast,cps,vm,opt,ssa,asm}-phase — IR phase dump functions
;;;   - %string-suffix-p, %dump-ir-phase, %trace-emit-stages
;;;   - %arch-keyword, %compile-target-keyword, %parse-opt-remarks-mode
;;;   - compile-opts struct + %parse-compile-opts + %compile-opts-kwargs
;;;
;;; Depends on main-utils.lisp (%ensure-list, %source-location-comment,
;;;   %print-source-comment, %ssa-block-name).
;;; Load order: immediately after main-utils.lisp.

(in-package :cl-cc/cli)

(defparameter +ansi-esc+     (string (code-char 27)))
(defparameter +ansi-reset+   (concatenate 'string +ansi-esc+ "[0m"))
(defparameter +ansi-label+   (concatenate 'string +ansi-esc+ "[32m"))
(defparameter +ansi-opcode+  (concatenate 'string +ansi-esc+ "[34m"))
(defparameter +ansi-comment+ (concatenate 'string +ansi-esc+ "[90m"))

(defun %dump-ast-phase (result stream annotate-source)
  (let ((asts (%ensure-list (cl-cc:compilation-result-ast result))))
    (when (null asts)
      (format stream "; no AST available~%"))
    (dolist (ast asts)
      (when annotate-source
        (%print-source-comment stream (%source-location-comment ast)))
      (format stream "~S~%" (cl-cc:ast-to-sexp ast)))))

(defun %dump-cps-phase (result stream annotate-source)
  (declare (ignore annotate-source))
  (let ((cps (cl-cc:compilation-result-cps result)))
    (if cps
        (format stream "~S~%" cps)
        (format stream "; no CPS available~%"))))

(defun %dump-vm-phase (result stream annotate-source)
  (let ((insts (or (cl-cc:compilation-result-vm-instructions result)
                   (cl-cc:vm-program-instructions (cl-cc:compilation-result-program result)))))
    (when annotate-source
      (%print-source-comment stream (%source-location-comment
                                     (car (%ensure-list (cl-cc:compilation-result-ast result))))))
    (dolist (inst insts)
      (format stream "~S~%" (cl-cc:instruction->sexp inst)))))

(defun %dump-opt-phase (result stream annotate-source)
  (let ((insts (or (cl-cc:compilation-result-optimized-instructions result)
                   (cl-cc:vm-program-instructions (cl-cc:compilation-result-program result)))))
    (when annotate-source
      (%print-source-comment stream (%source-location-comment
                                     (car (%ensure-list (cl-cc:compilation-result-ast result))))))
    (dolist (inst insts)
      (format stream "~S~%" (cl-cc:instruction->sexp inst)))))

(defun %dump-ssa-phase (result stream annotate-source)
  (declare (ignore annotate-source))
  (let ((insts (or (cl-cc:compilation-result-optimized-instructions result)
                   (cl-cc:compilation-result-vm-instructions result)
                   (cl-cc:vm-program-instructions (cl-cc:compilation-result-program result)))))
    (multiple-value-bind (cfg phi-map renamed-map)
        (cl-cc/optimize:ssa-construct insts)
      (format stream "; SSA CFG (~D block~:P)~%" (length (cl-cc/optimize:cfg-blocks cfg)))
      (dolist (blk (cl-cc/optimize:cfg-compute-rpo cfg))
        (format stream "~A:~%" (%ssa-block-name blk))
        (format stream "  ; preds: ~{~A~^, ~}~%"
                (or (mapcar (lambda (p) (%ssa-block-name p))
                            (cl-cc/optimize:bb-predecessors blk))
                    (list "(none)")))
        (dolist (phi (gethash blk phi-map))
          (format stream "  ; phi ~A <- ~{~A~^, ~}~%"
                  (cl-cc/optimize:phi-dst phi)
                  (mapcar (lambda (arg)
                            (format nil "~A:~A"
                                    (%ssa-block-name (car arg))
                                    (cdr arg)))
                          (cl-cc/optimize:phi-args phi))))
        (dolist (inst (gethash blk renamed-map))
          (format stream "  ~S~%" (cl-cc:instruction->sexp inst)))))))

(defun %dump-asm-phase (result stream annotate-source)
  (declare (ignore annotate-source))
  (format stream "~A~A~A~%"
          +ansi-opcode+
          (cl-cc:compilation-result-assembly result)
          +ansi-reset+))

(defun %string-suffix-p (suffix string)
  (let ((suffix-len (length suffix))
        (string-len (length string)))
    (and (<= suffix-len string-len)
         (string= suffix string :start1 0 :end1 suffix-len
                           :start2 (- string-len suffix-len) :end2 string-len))))

(defparameter *ir-phase-dump-fns*
  '((:ast . %dump-ast-phase)
    (:cps . %dump-cps-phase)
    (:ssa . %dump-ssa-phase)
    (:vm  . %dump-vm-phase)
    (:opt . %dump-opt-phase)
    (:asm . %dump-asm-phase))
  "Data table mapping IR phase keywords to their dump functions.")

(defun %dump-ir-phase (phase result stream annotate-source)
  (let ((fn (cdr (assoc phase *ir-phase-dump-fns*))))
    (if fn
        (funcall (symbol-function fn) result stream annotate-source)
        (error "Unknown IR phase: ~S" phase))))

(defun %trace-emit-stages (result stream &key annotate-source)
  "Emit a simple VM/OPT/ASM trace for RESULT to STREAM."
  (format stream ";; --trace-emit: vm --~%")
  (%dump-vm-phase result stream annotate-source)
  (format stream ";; --trace-emit: opt --~%")
  (%dump-opt-phase result stream annotate-source)
  (format stream ";; --trace-emit: asm --~%")
  (%dump-asm-phase result stream annotate-source))

(defparameter *arch-aliases*
  '(("x86-64"  :x86-64 :x86_64)
    ("x86_64"  :x86-64 :x86_64)
    ("arm64"   :arm64  :aarch64)
    ("aarch64" :arm64  :aarch64))
  "Architecture string aliases: (input-string arch-keyword compile-target-keyword).")

(defun %arch-keyword (arch-str)
  "Convert ARCH-STR to its canonical arch keyword. Calls (uiop:quit 2) on unknown values."
  (let ((entry (assoc arch-str *arch-aliases* :test #'string=)))
    (or (and entry (second entry))
        (progn
          (format *error-output* "Unknown architecture: ~A (use x86-64 or arm64)~%" arch-str)
          (uiop:quit 2)))))

(defun %compile-target-keyword (arch-str)
  "Convert ARCH-STR to its compilation target keyword. Signals error on unknown values."
  (let ((entry (assoc arch-str *arch-aliases* :test #'string=)))
    (or (and entry (third entry))
        (error "Unknown architecture for compilation: ~A" arch-str))))

(defparameter *opt-remarks-modes*
  '(("all"     . :all)
    ("changed" . :changed)
    ("missed"  . :missed))
  "Valid opt-remarks mode strings and their keyword equivalents.")

(defun %parse-opt-remarks-mode (mode-str)
  (let ((s (string-downcase (or mode-str ""))))
    (if (string= s "")
        nil
        (or (cdr (assoc s *opt-remarks-modes* :test #'string=))
            (progn
              (format *error-output* "Unknown opt-remarks mode: ~A (use all|changed|missed)~%" mode-str)
              (uiop:quit 2))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Compile options — shared pipeline/tracing flags
;;; ─────────────────────────────────────────────────────────────────────────
;;;
;;; All three commands (run/compile/eval) parse the same 7 pipeline flags and
;;; pass the same 6 keyword args to compile-string.  This struct captures that
;;; shared data so each command can call %parse-compile-opts once and then use
;;; %compile-opts-kwargs to spread the args onto compile-string.

(defstruct (compile-opts (:constructor make-compile-opts))
  "Pipeline control and diagnostic flags shared by run / compile / eval."
  (pass-pipeline      nil)
  (print-pass-timings nil)
  (trace-json-path    nil)
  (flamegraph-path    nil)
  (print-pass-stats   nil)
  (trace-emit         nil)
  (opt-remarks-mode   nil))

(defun %parse-compile-opts (parsed)
  "Extract all pipeline/tracing flags from PARSED into a compile-opts struct."
  (make-compile-opts
   :pass-pipeline      (flag parsed "--pass-pipeline")
   :print-pass-timings (or (flag parsed "--print-pass-timings")
                           (flag parsed "--time-passes"))
   :trace-json-path    (flag parsed "--trace-json")
   :flamegraph-path    (flag parsed "--flamegraph")
   :print-pass-stats   (flag parsed "--stats")
   :trace-emit         (flag parsed "--trace-emit")
   :opt-remarks-mode   (%parse-opt-remarks-mode (flag parsed "--opt-remarks"))))

(defun %compile-opts-kwargs (opts stream)
  "Return a flat keyword plist for compile-string.
STREAM is the resolved trace-json output stream (may be nil)."
  (let ((remarks (compile-opts-opt-remarks-mode opts)))
    (list :trace-json-stream  stream
          :print-pass-stats   (compile-opts-print-pass-stats opts)
          :pass-pipeline      (compile-opts-pass-pipeline opts)
          :print-pass-timings (compile-opts-print-pass-timings opts)
          :print-opt-remarks  (not (null remarks))
          :opt-remarks-mode   (or remarks :all))))

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

(defparameter *selfhost-profile-path* ".cl-cc-profile.sexp"
  "Default instruction histogram emitted by `cl-cc selfhost --profile` and read by later compilations.")

(defun %dump-image-and-exit (parsed)
  "Dump the current SBCL image to --dump-image and exit.
When --stdlib is also supplied, warm the stdlib cache first so the saved image
starts with parsed/expanded stdlib forms and the VM snapshot already resident."
  (let ((path (flag parsed "--dump-image")))
    (when (flag parsed "--stdlib")
      (ignore-errors (cl-cc:warm-stdlib-cache)))
    (format *error-output* "; cl-cc: dumping initialized image to ~A~%" path)
    (finish-output *error-output*)
    #+sbcl
    (sb-ext:save-lisp-and-die path
                              :toplevel #'main
                              :executable t
                              :compression t)
    #-sbcl
    (error "--dump-image is only supported on SBCL")))

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
              (let* ((candidates (mapcar #'car *opt-remarks-modes*))
                     (suggestions (cl-cc/optimize:opt-diagnostic-did-you-mean s candidates :limit 2)))
                (format *error-output* "Unknown opt-remarks mode: ~A (use all|changed|missed)~%" mode-str)
                (when suggestions
                  (format *error-output* "did you mean: ~{~A~^, ~}~%" suggestions)))
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
  (debug-info         nil)
  (sanitize           nil)
  (lto                nil)
  (eh-model           nil)
  (incremental        nil)
  (perf-map           nil)
  (bolt               nil)
  (verify-transforms  nil)
  (parallel           nil)
  (tier               nil)
  (block-compile      nil)
  (print-pass-timings nil)
  (trace-json-path    nil)
  (coverage           nil)
  (pgo-generate-path  nil)
  (pgo-use-path       nil)
  (spectre-mitigations nil)
  (jit-cache-stats    nil)
  (flamegraph-path    nil)
  (profile            nil)
  (print-pass-stats   nil)
  (trace-emit         nil)
  (opt-remarks-mode   nil)
  (retpoline          nil)
  (stack-protector    nil)
  (shadow-stack       nil)
  (deterministic      nil)
  (build-id           nil)
  (asan               nil)
  (msan               nil)
  (tsan               nil)
  (ubsan              nil)
  (hwasan             nil)
  (werror             nil))

(defun %parse-compile-opts (parsed)
  "Extract all pipeline/tracing flags from PARSED into a compile-opts struct."
  (make-compile-opts
    :pass-pipeline      (flag parsed "--pass-pipeline")
    :debug-info         (flag parsed "--debug-info")
    :sanitize           (flag parsed "--sanitize")
    :lto                (flag parsed "--lto")
    :eh-model           (flag parsed "--eh-model")
    :incremental        (flag parsed "--incremental")
    :perf-map           (flag parsed "--perf-map")
    :bolt               (flag parsed "--bolt")
    :verify-transforms  (flag parsed "--verify-transforms")
    :parallel           (flag parsed "--parallel")
    :tier               (flag parsed "--tier")
    :block-compile      (flag parsed "--block-compile")
    :print-pass-timings (or (flag parsed "--print-pass-timings")
                           (flag parsed "--time-passes"))
   :trace-json-path    (flag parsed "--trace-json")
   :coverage           (flag parsed "--coverage")
    :pgo-generate-path  (flag parsed "--pgo-generate")
     :pgo-use-path       (flag parsed "--pgo-use")
     :spectre-mitigations (flag parsed "--spectre-mitigations")
     :jit-cache-stats    (flag parsed "--jit-cache-stats")
    :flamegraph-path    (flag parsed "--flamegraph")
    :profile            (flag parsed "--profile")
    :print-pass-stats   (flag parsed "--stats")
   :trace-emit         (flag parsed "--trace-emit")
   :opt-remarks-mode   (%parse-opt-remarks-mode (flag parsed "--opt-remarks"))
   :retpoline          (flag parsed "--retpoline")
   :stack-protector    (flag parsed "--stack-protector")
    :shadow-stack       (flag parsed "--shadow-stack")
    :deterministic      (flag parsed "--deterministic")
    :build-id           (flag parsed "--build-id")
    :asan               (flag parsed "--asan")
   :msan               (flag parsed "--msan")
   :tsan               (flag parsed "--tsan")
   :ubsan              (flag parsed "--ubsan")
   :hwasan             (flag parsed "--hwasan")
   :werror             (flag parsed "--Werror")))

(defun %compile-opts-kwargs (opts stream)
  "Return a flat keyword plist for compile-string.
STREAM is the resolved trace-json output stream (may be nil)."
  (labels ((safe-read-profile (path)
             (when path
               (handler-case
                   (with-open-file (in path :direction :input)
                     (read in nil nil))
                  (error () nil))))
           (instruction-profile-alist-p (profile)
             (and (listp profile)
                  (every (lambda (entry)
                           (and (consp entry)
                                (symbolp (car entry))
                                (integerp (cdr entry))))
                         profile)))
           (profile-op-counts (profile)
             (cond
               ((instruction-profile-alist-p profile) profile)
               ((consp profile) (getf profile :op-counts))
               (t nil)))
           (profile-total (profile ops)
             (or (and (consp profile) (getf profile :total-instructions))
                 (and (listp ops) (reduce #'+ ops :key #'cdr :initial-value 0))))
           (profile-op-count (ops name)
             (cond
               ((assoc name ops) (cdr (assoc name ops)))
               ((assoc (string-upcase (symbol-name name)) ops :test #'string=)
                (cdr (assoc (string-upcase (symbol-name name)) ops :test #'string=)))
               (t 0)))
            (profile-speed (profile)
              (let* ((ops (profile-op-counts profile))
                     (total (profile-total profile ops))
                     (call-count (+ (profile-op-count ops 'cl-cc/vm::vm-call)
                                    (profile-op-count ops 'cl-cc/vm::vm-tail-call)
                                    (profile-op-count ops 'cl-cc/vm::vm-generic-call))))
                (cond
                  ;; Heuristic: call-heavy or larger prior traces favor aggressive mode.
                  ((or (and (integerp total) (>= total 120))
                      (and (integerp call-count) (>= call-count 12)))
                  3)
                  ((and (integerp total) (>= total 40))
                  2)
                  (t nil))))
           (profile-inline-scale (profile)
             (let* ((ops (profile-op-counts profile))
                    (total (max 1 (or (profile-total profile ops) 0)))
                    (call-count (+ (profile-op-count ops 'cl-cc/vm::vm-call)
                                   (profile-op-count ops 'cl-cc/vm::vm-tail-call)
                                   (profile-op-count ops 'cl-cc/vm::vm-generic-call))))
               (if (>= (/ (float call-count) total) 0.10) 2 1))))
    (let ((remarks (compile-opts-opt-remarks-mode opts)))
      (let* ((profile (or (safe-read-profile (compile-opts-pgo-use-path opts))
                          (safe-read-profile *selfhost-profile-path*)))
             (speed (profile-speed profile))
             (inline-scale (profile-inline-scale profile)))
        (append (list :trace-json-stream  stream
                       :print-pass-stats   (compile-opts-print-pass-stats opts)
                       :pass-pipeline      (compile-opts-pass-pipeline opts)
                       :inline-threshold-scale inline-scale)
                (if (compile-opts-tier opts)
                    (list :compilation-tier
                          (cl-cc/pipeline:normalize-compilation-tier
                           (compile-opts-tier opts)))
                    nil)
                (if (compile-opts-coverage opts)
                    (list :coverage t)
                    nil)
                 (if (compile-opts-block-compile opts)
                     (list :block-compile t)
                     nil)
                 (if (compile-opts-debug-info opts) (list :debug-info t) nil)
                 (if (compile-opts-sanitize opts) (list :sanitize (compile-opts-sanitize opts)) nil)
                 (if (compile-opts-lto opts) (list :lto (compile-opts-lto opts)) nil)
                 (if (compile-opts-eh-model opts) (list :eh-model (compile-opts-eh-model opts)) nil)
                 (if (compile-opts-incremental opts) (list :incremental t) nil)
                   (if (compile-opts-perf-map opts) (list :perf-map t) nil)
                   (if (compile-opts-verify-transforms opts)
                       (list :verify-transforms t)
                       nil)
                   (if (compile-opts-parallel opts) (list :parallel (compile-opts-parallel opts)) nil)
                 (if (and profile (not (instruction-profile-alist-p profile)))
                    (list :pgo-profile-data profile)
                    nil)
                 (if speed (list :speed speed) nil)
                  (if (or (compile-opts-retpoline opts)
                          (compile-opts-spectre-mitigations opts))
                      (list :retpoline t)
                      nil)
                  (if (compile-opts-spectre-mitigations opts)
                      (list :spectre-mitigations t)
                      nil)
                 (if (compile-opts-stack-protector opts)
                     (list :stack-protector t)
                     nil)
                  (if (compile-opts-shadow-stack opts)
                      (list :shadow-stack t)
                      nil)
                  (if (compile-opts-asan opts)
                      (list :asan t)
                      nil)
                  (if (compile-opts-msan opts)
                      (list :msan t)
                      nil)
                  (if (compile-opts-tsan opts)
                      (list :tsan t)
                      nil)
                  (if (compile-opts-ubsan opts)
                      (list :ubsan t)
                      nil)
                   (if (compile-opts-hwasan opts)
                       (list :hwasan t)
                       nil)
                    (list :print-pass-timings (compile-opts-print-pass-timings opts)
                         :print-opt-remarks  (not (null remarks))
                       :opt-remarks-mode   (or remarks :all)))))))

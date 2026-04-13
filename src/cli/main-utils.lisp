;;;; src/cli/main-utils.lisp — CLI Utilities, Dump Functions, and Compile Options
;;;
;;; Contains:
;;;   - %read-file, %detect-language — I/O and language detection helpers
;;;   - %dump-phase-label, %parse-ir-phase — IR phase helpers
;;;   - %ensure-list, %source-location-comment, %print-source-comment
;;;   - %call-with-optional-output-file
;;;   - %svg-escape, %flamegraph-color, %flamegraph-build-tree,
;;;     %flamegraph-children-list, %write-flamegraph-svg — flamegraph rendering
;;;   - %ssa-block-name, %dump-{ast,cps,vm,opt,ssa,asm}-phase,
;;;     %string-suffix-p, %dump-ir-phase, %trace-emit-stages — dump helpers
;;;   - %arch-keyword, %compile-target-keyword, %parse-opt-remarks-mode
;;;   - %compile-opts struct + %parse-compile-opts + %compile-opts-kwargs
;;;
;;; Help system (%print-global-help, %print-command-help, %print-help)
;;; is in main.lisp (loads before).
;;;
;;; Load order: after cli/main.lisp.
(in-package :cl-cc)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Utilities
;;; ─────────────────────────────────────────────────────────────────────────

(defun %read-file (path)
  "Read the entire contents of PATH as a string.
Signals an error when the file does not exist."
  (with-open-file (in path :direction :input
                       :element-type 'character
                       :if-does-not-exist nil)
    (unless in
      (error "File not found: ~A" path))
    (let* ((buf (make-string (file-length in)))
           (n   (read-sequence buf in)))
      (subseq buf 0 n))))

(defun %detect-language (file lang-flag)
  "Determine source language from LANG-FLAG string or FILE extension.
Returns :lisp or :php."
  (cond
    ((string= lang-flag "php")  :php)
    ((string= lang-flag "lisp") :lisp)
    ((let ((ext (and file (pathname-type file))))
       (and ext (string= ext "php"))) :php)
    (t :lisp)))

(defun %dump-phase-label (phase)
  (string-downcase (string phase)))

(defun %parse-ir-phase (phase-str)
  (let ((phase (string-downcase phase-str)))
    (case (intern (string-upcase phase) :keyword)
      (:ast :ast)
      (:cps :cps)
      (:ssa :ssa)
      (:vm  :vm)
      (:opt :opt)
      (:asm :asm)
      (t nil))))

(defun %ensure-list (thing)
  (cond
    ((null thing) nil)
    ((listp thing) thing)
    (t (list thing))))

(defun %source-location-comment (node)
  (when (typep node 'cl-cc:ast-node)
    (let ((loc (cl-cc:ast-location-string node)))
      (unless (string= loc "<unknown location>")
        loc))))

(defun %print-source-comment (stream loc)
  (when loc
    (format stream "; source: ~A~%" loc)))

(defun %call-with-optional-output-file (path thunk)
  "Call THUNK with an output stream for PATH, or NIL when PATH is NIL."
  (if path
      (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
        (funcall thunk out))
      (funcall thunk nil)))

(defun %svg-escape (text)
  (with-output-to-string (out)
    (loop for ch across (princ-to-string text)
          do (case ch
               (#\& (princ "&amp;" out))
               (#\< (princ "&lt;" out))
               (#\> (princ "&gt;" out))
               (#\" (princ "&quot;" out))
               (t (write-char ch out))))))

(defun %flamegraph-color (name)
  (let ((s (string-downcase (princ-to-string name))))
    (cond ((search "gc" s) "rgb(90,140,255)")
          ((search "jit" s) "rgb(255,165,0)")
          (t (format nil "hsl(8,75%,~D%)" (+ 45 (mod (sxhash s) 20)))))))

(defun %flamegraph-build-tree (samples)
  (let ((root (list :name "root" :count 0 :children (make-hash-table :test #'equal))))
    (maphash
     (lambda (stack count)
       (incf (getf root :count) count)
       (let ((node root))
         (dolist (name (uiop:split-string stack :separator '(#\;)))
           (let* ((children (getf node :children))
                  (child (or (gethash name children)
                             (setf (gethash name children)
                                   (list :name name :count 0 :children (make-hash-table :test #'equal))))))
             (incf (getf child :count) count)
             (setf node child)))))
     samples)
    root))

(defun %flamegraph-children-list (node)
  (let (children)
    (maphash (lambda (_ child) (declare (ignore _)) (push child children)) (getf node :children))
    (sort children #'string< :key (lambda (c) (princ-to-string (getf c :name))))))

(defun %write-flamegraph-svg (path samples)
  (let* ((tree (%flamegraph-build-tree samples))
         (total (max 1 (getf tree :count)))
         (frame-height 18)
         (width 1200)
         (max-depth 0))
    (labels ((depth-of (node depth)
               (setf max-depth (max max-depth depth))
               (dolist (child (%flamegraph-children-list node))
                 (depth-of child (1+ depth))))
             (emit-node (stream node depth x scale)
               (let ((children (%flamegraph-children-list node))
                     (cursor x))
                 (dolist (child children)
                   (let* ((count (getf child :count))
                          (w (* count scale))
                          (y (- (+ 20 (* max-depth frame-height)) (* depth frame-height))))
                     (format stream "<g><title>~A (~D samples)</title><rect x='~,2f' y='~,2f' width='~,2f' height='~D' fill='~A' stroke='white' stroke-width='0.5'/><text x='~,2f' y='~,2f' font-size='12' fill='black'>~A</text></g>~%"
                             (%svg-escape (getf child :name)) count cursor y w (- frame-height 2)
                             (%flamegraph-color (getf child :name)) (+ cursor 3) (+ y 13)
                             (%svg-escape (getf child :name)))
                     (emit-node stream child (1+ depth) cursor scale)
                     (incf cursor w))))))
      (depth-of tree 0)
      (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format out "<svg xmlns='http://www.w3.org/2000/svg' width='~D' height='~D'>~%" width (+ 40 (* (1+ max-depth) frame-height)))
        (format out "<rect width='100%' height='100%' fill='rgb(250,250,250)'/>~%")
        (emit-node out tree 0 0 (/ width total))
        (format out "</svg>~%")))
    path))

(defun %ssa-block-name (blk)
  (let ((label (cl-cc:bb-label blk)))
    (string-downcase
     (format nil "~A"
             (or (and label (cl-cc:vm-name label))
                 (format nil "block-~D" (cl-cc:bb-id blk)))))))

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
        (cl-cc:ssa-construct insts)
      (format stream "; SSA CFG (~D block~:P)~%" (length (cl-cc:cfg-blocks cfg)))
      (dolist (blk (cl-cc:cfg-compute-rpo cfg))
        (format stream "~A:~%" (%ssa-block-name blk))
        (format stream "  ; preds: ~{~A~^, ~}~%"
                (or (mapcar (lambda (p) (%ssa-block-name p))
                            (cl-cc:bb-predecessors blk))
                    (list "(none)")))
        (dolist (phi (gethash blk phi-map))
          (format stream "  ; phi ~A <- ~{~A~^, ~}~%"
                  (cl-cc:phi-dst phi)
                  (mapcar (lambda (arg)
                            (format nil "~A:~A"
                                    (%ssa-block-name (car arg))
                                    (cdr arg)))
                          (cl-cc:phi-args phi))))
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


(defun %dump-ir-phase (phase result stream annotate-source)
  (case phase
    (:ast (%dump-ast-phase result stream annotate-source))
    (:cps (%dump-cps-phase result stream annotate-source))
    (:ssa (%dump-ssa-phase result stream annotate-source))
    (:vm  (%dump-vm-phase result stream annotate-source))
    (:opt (%dump-opt-phase result stream annotate-source))
    (:asm (%dump-asm-phase result stream annotate-source))
    (t (error "Unknown IR phase: ~S" phase))))

(defun %trace-emit-stages (result stream &key annotate-source)
  "Emit a simple VM/OPT/ASM trace for RESULT to STREAM."
  (format stream ";; --trace-emit: vm --~%")
  (%dump-vm-phase result stream annotate-source)
  (format stream ";; --trace-emit: opt --~%")
  (%dump-opt-phase result stream annotate-source)
  (format stream ";; --trace-emit: asm --~%")
  (%dump-asm-phase result stream annotate-source))

(defun %arch-keyword (arch-str)
  "Convert ARCH-STR (\"x86-64\" or \"arm64\"/\"aarch64\") to a keyword.
Calls (uiop:quit 2) on unrecognised values."
  (cond
    ((or (string= arch-str "x86-64")
         (string= arch-str "x86_64"))  :x86-64)
    ((or (string= arch-str "arm64")
         (string= arch-str "aarch64")) :arm64)
    (t
     (format *error-output* "Unknown architecture: ~A (use x86-64 or arm64)~%" arch-str)
     (uiop:quit 2))))

(defun %compile-target-keyword (arch-str)
  (cond
    ((or (string= arch-str "x86-64")
         (string= arch-str "x86_64")) :x86_64)
    ((or (string= arch-str "arm64")
         (string= arch-str "aarch64")) :aarch64)
    (t (error "Unknown architecture for compilation: ~A" arch-str))))

(defun %parse-opt-remarks-mode (mode-str)
  (let ((s (string-downcase (or mode-str ""))))
    (cond
      ((string= s "") nil)
      ((string= s "all") :all)
      ((string= s "changed") :changed)
      ((string= s "missed") :missed)
      (t
       (format *error-output* "Unknown opt-remarks mode: ~A (use all|changed|missed)~%" mode-str)
       (uiop:quit 2)))))

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

;;; ─────────────────────────────────────────────────────────────────────────
;;; Subcommand handlers
;;; ─────────────────────────────────────────────────────────────────────────

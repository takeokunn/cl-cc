(in-package :cl-cc/pipeline)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compiler Pipeline — Native Code Generation Helpers
;;;
;;; Contains: %write-native-binary, *compile-cache-root*, %compile-cache-key,
;;; %compile-cache-path, %copy-file-bytes, compile-to-native,
;;; and compile-file-to-native.
;;;
;;; Core pipeline (compile-expression, compile-string, run-string, our-eval,
;;; run-string-typed) is in pipeline.lisp (loads before).
;;;
;;; Load order: after pipeline.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Native Executable Generation (Mach-O, ELF, PE dispatch)

(defparameter *native-command-timeout-seconds* 10
  "Timeout in seconds for short external helper commands used by native compilation.")

(defparameter *use-mir-pipeline* nil
  "When true, native compilation routes VM instructions through MIR SSA and ISel.
The direct VM-to-codegen path remains the fallback and the default.")

(defun %run-short-native-command (argv)
  "Run a short external helper command with an explicit timeout.
Returns NIL on timeout or command failure so native compilation can continue
without hanging forever on platform utility calls like chmod."
  (handler-case
      (sb-ext:with-timeout *native-command-timeout-seconds*
        (uiop:run-program argv :ignore-error-status t))
    (sb-ext:timeout () nil)
    (error () nil)))

(defun %native-host-os ()
  "Return a keyword identifying the host operating system.
Values: :DARWIN, :LINUX, :WINDOWS, or :UNKNOWN."
  #+darwin :darwin
  #+linux :linux
  #+win32 :windows
  #-(or darwin linux win32) :unknown)

(defun %native-binary-format (target-os)
  "Return the native binary format keyword for TARGET-OS.
Values: :MACH-O, :ELF, :PE, or NIL for unknown."
  (ecase target-os
    (:darwin :mach-o)
    (:linux :elf)
    (:windows :pe)
    (:unknown nil)))

(defun %write-native-binary (builder code-bytes output-path &key compress)
  "Finalize BUILDER with CODE-BYTES, write the native binary to OUTPUT-PATH,
apply platform-appropriate post-processing (executable bit, code signing),
and return OUTPUT-PATH.
The codesign step in write-mach-o-file is suppressed; we sign with a timeout
guard immediately after."
  (cl-cc/binary:add-text-segment builder code-bytes)
  (cl-cc/binary:add-symbol builder "_main" :value 0 :type #x0F :sect 1)
  (let ((mach-o-bytes (cl-cc/binary:build-mach-o builder code-bytes :compress compress)))
    (cl-cc/binary:write-mach-o-file output-path mach-o-bytes :codesign nil))
  (%run-short-native-command (list "chmod" "+x" (namestring output-path)))
  (%run-short-native-command (list "codesign" "-s" "-" "--force"
                                   (namestring output-path)))
  output-path)

(defun %write-elf64-native-binary (code-bytes reloc-entries output-path
                                   &key (arch :x86-64) compress
                                     (bss-size 0) (type :exec)
                                     needed-libraries)
  "Build an ELF64 executable from CODE-BYTES and RELOC-ENTRIES, write to OUTPUT-PATH,
and return OUTPUT-PATH."
  (cl-cc/binary:compile-to-elf64-exec
   code-bytes reloc-entries
   :output-file output-path :arch arch
   :bss-size bss-size :type type
   :needed-libraries needed-libraries)
  (%run-short-native-command (list "chmod" "+x" (namestring output-path)))
  output-path)

(defun %write-pe-native-binary (code-bytes reloc-entries output-path
                                &key (arch :x86-64) (subsystem :console)
                                  exports)
  "Build a PE32+ executable from CODE-BYTES and RELOC-ENTRIES, write to OUTPUT-PATH,
and return OUTPUT-PATH."
  (cl-cc/binary:compile-to-pe
   code-bytes reloc-entries
   :output-file output-path :arch arch
   :subsystem subsystem :exports exports)
  output-path)

(defun %write-native-output (code-bytes reloc-entries output-path
                             &key format arch compress bss-size type
                               needed-libraries subsystem exports)
  "Dispatch native binary output to the appropriate format writer.
FORMAT is :MACH-O, :ELF, or :PE. CODE-BYTES and RELOC-ENTRIES are the compiled
code and relocation data from the backend. OUTPUT-PATH is the target file path."
  (ecase format
    (:mach-o
     (let* ((builder (cl-cc/binary:make-mach-o-builder arch)))
       (%write-native-binary builder code-bytes output-path :compress compress)))
    (:elf
     (%write-elf64-native-binary code-bytes reloc-entries output-path
                                 :arch arch :compress compress
                                 :bss-size bss-size :type type
                                 :needed-libraries needed-libraries))
    (:pe
     (%write-pe-native-binary code-bytes reloc-entries output-path
                              :arch arch :subsystem subsystem
                              :exports exports))))

(defparameter *compile-cache-root*
  #P".cache/cl-cc/native/"
  "Directory for cached native build outputs.")

(defun %native-cache-relevant-opts (opts)
  "Return the native compile options that can change cached artifact bytes."
  (list :pass-pipeline (getf opts :pass-pipeline)
        :inline-threshold-scale (getf opts :inline-threshold-scale)
        :block-compile (getf opts :block-compile)
         :speed (getf opts :speed)
         :retpoline (getf opts :retpoline)
         :spectre-mitigations (getf opts :spectre-mitigations)
        :stack-protector (getf opts :stack-protector)
        :shadow-stack (getf opts :shadow-stack)
         :compress (getf opts :compress)
         :bolt (getf opts :bolt)
         :bolt-profile (getf opts :bolt-profile)
          :mir-isel (getf opts :mir-isel)
        :asan (getf opts :asan)
        :msan (getf opts :msan)
        :tsan (getf opts :tsan)
        :ubsan (getf opts :ubsan)
        :hwasan (getf opts :hwasan)
        :target-os (getf opts :target-os)))

(defun %compile-cache-key (source arch language &optional opts)
  (format nil "~A-~A-~A-~A"
          (sxhash source)
          arch
          language
          (sxhash (%native-cache-relevant-opts opts))))

(defun %compile-cache-path (key output-file)
  (merge-pathnames
   (make-pathname :directory (list :relative key)
                  :name (pathname-name output-file)
                  :type (pathname-type output-file))
   *compile-cache-root*))

(defun %copy-file-bytes (from to)
  (with-open-file (in from :direction :input :element-type '(unsigned-byte 8))
    (with-open-file (out to :direction :output :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte in nil nil)
            while byte do (write-byte byte out))))
  to)

(defun %non-package-top-level-forms (forms)
  "Return FORMS with in-package declarations removed, mirroring compile-toplevel-forms."
  (remove-if (lambda (f) (and (consp f) (eq (car f) 'in-package))) forms))

;;; Native compilation options — bundle 10 recurring keyword params into a plist.
;;; Internal functions accept (target opts) and apply opts directly as &key args.

(defun %make-native-opts (&key pass-pipeline speed (inline-threshold-scale 1)
                                   block-compile debug-info sanitize lto eh-model incremental perf-map bolt bolt-profile parallel
                                   verify-transforms
                                   print-pass-timings timing-stream coverage
                                 print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                 print-pass-stats stats-stream trace-json-stream
                                   retpoline spectre-mitigations stack-protector shadow-stack
                                     compress asan msan tsan ubsan hwasan strict-no-alloc
                                     target-os mir-isel compilation-tier)
  "Build a native-compile options plist suitable for APPLYing to compile-* functions."
  (append (list :pass-pipeline pass-pipeline)
          (if speed (list :speed speed) nil)
            (list :inline-threshold-scale inline-threshold-scale
                  :block-compile     block-compile
                  :debug-info        debug-info
                  :sanitize          sanitize
                  :lto               lto
                  :eh-model          eh-model
                  :incremental       incremental
                   :perf-map          perf-map
                    :bolt              bolt
                    :bolt-profile      bolt-profile
                    :verify-transforms verify-transforms
                    :parallel          parallel
                :print-pass-timings  print-pass-timings
                :coverage            coverage
                :timing-stream       timing-stream
                :print-opt-remarks   print-opt-remarks
                :opt-remarks-stream  opt-remarks-stream
                :opt-remarks-mode    opt-remarks-mode
                :print-pass-stats    print-pass-stats
                :stats-stream        stats-stream
                 :trace-json-stream   trace-json-stream
                 :compilation-tier    (normalize-compilation-tier compilation-tier)
                 :retpoline          retpoline
                 :spectre-mitigations spectre-mitigations
                 :stack-protector    stack-protector
                 :shadow-stack       shadow-stack
                  :compress           compress
                  :mir-isel           mir-isel
                 :asan               asan
                :msan               msan
                :tsan               tsan
                :ubsan              ubsan
                :hwasan             hwasan
                :strict-no-alloc    strict-no-alloc
                :target-os          (or target-os (%native-host-os)))))

(defun %maybe-compile-native-via-cps (form target opts &key type-check (safety 1))
  "Try compiling FORM through a narrow CPS-backed native path.
Returns two values: the compilation result and whether the CPS-native path was used."
  (let ((ast (optimize-ast (%prepare-ast form))))
    (if (%cps-native-compile-safe-ast-p ast)
        (let ((cps (cps-transform-ast* ast)))
          (values (apply #'compile-expression (%cps-identity-entry-form cps)
                         :target target :type-check type-check :safety safety
                         opts)
                  t))
        (values nil nil))))

(defun %compile-native-string (source target language opts)
  "Compile SOURCE through the generic string entrypoint for native codegen."
  (apply #'compile-string source :target target :language language opts))

(defun %compile-native-expression (form target opts)
  "Compile a single already-read FORM through the generic native entrypoint."
  (apply #'compile-expression form :target target opts))

(defun %compile-native-toplevel-forms (forms target opts)
  "Compile FORMS through the generic native top-level entrypoint after optional CPS rewriting."
  (let ((native-opts (apply #'%make-pipeline-opts :target target opts)))
    (apply #'compile-toplevel-forms
           (%maybe-cps-toplevel-forms forms native-opts)
           :target target opts)))

(defun %compile-native-lisp-forms (forms target opts)
  "Compile Lisp FORMS for native emission, preferring the CPS route for a single safe form."
  (let ((effective-forms (%non-package-top-level-forms forms)))
    (if (= (length effective-forms) 1)
        (multiple-value-bind (cps-result cps-used)
            (%maybe-compile-native-via-cps (first effective-forms) target opts)
          (if cps-used
              cps-result
              (%compile-native-toplevel-forms forms target opts)))
        (%compile-native-toplevel-forms forms target opts))))

(defun %compile-native-source (source target language opts)
  "Compile SOURCE for native emission, choosing the narrowest readable entrypoint."
  (if (stringp source)
      (if (eq language :lisp)
          (%compile-native-lisp-forms (parse-all-forms source) target opts)
          (%compile-native-string source target language opts))
      (multiple-value-bind (cps-result cps-used)
          (%maybe-compile-native-via-cps source target opts)
        (if cps-used
            cps-result
            (%compile-native-expression source target opts)))))

(defun %native-target-for-arch (arch)
  (if (eq arch :x86-64)
      :x86_64
      (if (eq arch :arm64)
          :arm64
          (error "Unknown native architecture: ~S" arch))))

;;; Function layout pass — FR-186
;;;
;;; The VM stream emitted by top-level compilation stores function bodies inline as:
;;;   top-level setup, (jump end), (label function), body..., (label end)
;;; Native codegen emits that flat stream verbatim after branch/label resolution.  To
;;; improve I-cache locality without changing execution order, this pass leaves a
;;; small jump-over stub at the original top-level location and moves function
;;; bodies to a compact pool after the top-level halt.  The pool is ordered by a
;;; direct-call graph heuristic: original-order DFS/BFS roots, with direct callees
;;; placed immediately after their callers and higher-frequency callees first.

(defstruct pipeline-function-block
  name
  label
  instructions
  original-index)

(defun %pipeline-function-label-name-map (instructions)
  "Return a hash table mapping function entry labels to source function names."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (inst instructions table)
      (when (typep inst 'vm-func-ref)
        (let ((tag (cl-cc/vm::vm-func-ref-dispatch-tag inst)))
          (when (and (consp tag) (eq (car tag) :known-function))
            (setf (gethash (vm-label-name inst) table) (cdr tag))))))))

(defun %pipeline-function-entry-labels (instructions)
  "Return labels that denote emitted function/closure bodies."
  (let ((labels (make-hash-table :test #'equal)))
    (dolist (inst instructions labels)
      (when (typep inst '(or vm-func-ref vm-closure))
        (setf (gethash (vm-label-name inst) labels) t)))))

(defun %pipeline-split-function-block (instructions end-label body-acc)
  "Collect a function body until END-LABEL.
Returns three values: body instructions, remaining instructions, and success-p."
  (cond
    ((null instructions) (values nil nil nil))
    ((and (typep (first instructions) 'vm-label)
          (string= (vm-lbl-name (first instructions)) end-label))
     (values (nreverse body-acc) (rest instructions) t))
    (t
     (%pipeline-split-function-block (rest instructions)
                                      end-label
                                      (cons (first instructions) body-acc)))))

(defun %pipeline-extract-function-blocks (instructions)
  "Split INSTRUCTIONS into top-level skeleton and movable function blocks."
  (let ((function-labels (%pipeline-function-entry-labels instructions))
        (label->name (%pipeline-function-label-name-map instructions))
        (skeleton '())
        (blocks '())
        (index 0)
        (remaining instructions))
    (loop while remaining
          do (let ((inst (first remaining))
                   (next (second remaining)))
               (if (and (typep inst 'vm-jump)
                        (typep next 'vm-label)
                        (gethash (vm-lbl-name next) function-labels))
                   (multiple-value-bind (body tail ok)
                       (%pipeline-split-function-block (rest remaining)
                                                        (vm-label-name inst)
                                                        nil)
                     (if ok
                         (progn
                           ;; Preserve top-level control flow at the original site.
                           (push inst skeleton)
                           (push (make-vm-label :name (vm-label-name inst)) skeleton)
                           (push (make-pipeline-function-block
                                  :name (gethash (vm-lbl-name next) label->name)
                                  :label (vm-lbl-name next)
                                  :instructions body
                                  :original-index index)
                                 blocks)
                           (incf index)
                           (setf remaining tail))
                         (progn
                           (push inst skeleton)
                           (setf remaining (rest remaining)))))
                   (progn
                     (push inst skeleton)
                     (setf remaining (rest remaining))))))
    (values (nreverse skeleton) (nreverse blocks))))

(defun %pipeline-note-direct-call-edge (caller callee graph)
  "Increment CALLER -> CALLEE edge count in GRAPH."
  (when (and caller callee (not (eq caller callee)))
    (let ((edges (or (gethash caller graph)
                     (setf (gethash caller graph) (make-hash-table :test #'eq)))))
      (incf (gethash callee edges 0)))))

(defun %pipeline-clear-reg-symbol (reg reg->symbol)
  (when reg (remhash reg reg->symbol)))

(defun %pipeline-block-call-graph (blocks)
  "Build caller -> callee frequency table from direct vm-call/vm-tail-call sites."
  (let ((known (make-hash-table :test #'eq))
        (graph (make-hash-table :test #'eq)))
    (dolist (block blocks)
      (when (pipeline-function-block-name block)
        (setf (gethash (pipeline-function-block-name block) known) t)))
    (dolist (block blocks graph)
      (let ((caller (pipeline-function-block-name block))
            (reg->symbol (make-hash-table :test #'eq)))
        (dolist (inst (pipeline-function-block-instructions block))
          (cond
            ((typep inst 'vm-const)
             (if (and (symbolp (vm-value inst)) (gethash (vm-value inst) known))
                 (setf (gethash (vm-dst inst) reg->symbol) (vm-value inst))
                 (%pipeline-clear-reg-symbol (vm-dst inst) reg->symbol)))
            ((typep inst 'vm-move)
             (let ((callee (gethash (vm-src inst) reg->symbol)))
               (if callee
                   (setf (gethash (vm-dst inst) reg->symbol) callee)
                   (%pipeline-clear-reg-symbol (vm-dst inst) reg->symbol))))
            ((typep inst '(or vm-call vm-tail-call vm-apply))
             (%pipeline-note-direct-call-edge caller
                                              (gethash (vm-func-reg inst) reg->symbol)
                                              graph)
             (%pipeline-clear-reg-symbol (vm-dst inst) reg->symbol))
            ((typep inst 'vm-values)
             (%pipeline-clear-reg-symbol (vm-dst inst) reg->symbol))
            ((typep inst 'vm-instruction)
             (when (ignore-errors (vm-dst inst))
               (%pipeline-clear-reg-symbol (vm-dst inst) reg->symbol)))))))))

(defun %pipeline-block-original-index-map (blocks)
  (let ((table (make-hash-table :test #'eq)))
    (dolist (block blocks table)
      (when (pipeline-function-block-name block)
        (setf (gethash (pipeline-function-block-name block) table)
              (pipeline-function-block-original-index block))))))

(defun %pipeline-sorted-callees (caller graph original-index)
  "Return CALLER's callees by descending edge count, then original order."
  (let ((edges (gethash caller graph)))
    (when edges
      (sort (loop for callee being the hash-keys of edges collect callee)
            (lambda (a b)
              (let ((wa (gethash a edges 0))
                    (wb (gethash b edges 0)))
                (if (= wa wb)
                    (< (gethash a original-index most-positive-fixnum)
                       (gethash b original-index most-positive-fixnum))
                    (> wa wb))))))))

(defun %pipeline-order-function-blocks (blocks)
  "Order BLOCKS with a direct-call locality heuristic."
  (let* ((graph (%pipeline-block-call-graph blocks))
         (original-index (%pipeline-block-original-index-map blocks))
         (name->block (make-hash-table :test #'eq))
         (visited (make-hash-table :test #'eq))
         (ordered '()))
    (dolist (block blocks)
      (when (pipeline-function-block-name block)
        (setf (gethash (pipeline-function-block-name block) name->block) block)))
    (labels ((visit (block)
               (when (and block (not (gethash block visited)))
                 (setf (gethash block visited) t)
                 (push block ordered)
                 (dolist (callee (%pipeline-sorted-callees
                                  (pipeline-function-block-name block)
                                  graph
                                  original-index))
                   (visit (gethash callee name->block))))))
      (dolist (block blocks)
        (visit block)))
    (nreverse ordered)))

(defun pipeline-reorder-functions (program)
  "Return PROGRAM with emitted function bodies reordered for native I-cache locality.

The pass preserves top-level execution order by leaving jump-over stubs in place,
then appends extracted function bodies in call-graph order.  If no movable
function bodies are present, PROGRAM is returned unchanged.
Returns PROGRAM unchanged if it is not a VM-PROGRAM."
  (unless (typep program 'cl-cc/vm:vm-program)
    (return-from pipeline-reorder-functions program))
  (let ((instructions (vm-program-instructions program)))
    (multiple-value-bind (skeleton blocks)
        (%pipeline-extract-function-blocks instructions)
      (if (null blocks)
          program
          (make-vm-program
           :instructions (append skeleton
                                 (mapcan #'copy-list
                                         (mapcar #'pipeline-function-block-instructions
                                                 (%pipeline-order-function-blocks blocks))))
           :result-register (vm-program-result-register program)
           :leaf-p (vm-program-leaf-p program))))))

(defun %native-code-bytes-for-arch (arch program &optional opts)
  (if (eq arch :x86-64)
      (apply #'compile-to-x86-64-bytes
             program
             (if opts
                  (list :retpoline (getf opts :retpoline)
                        :spectre-mitigations (getf opts :spectre-mitigations)
                        :stack-protector (getf opts :stack-protector)
                       :shadow-stack (getf opts :shadow-stack)
                       :asan (getf opts :asan)
                       :msan (getf opts :msan)
                        :tsan (getf opts :tsan)
                        :ubsan (getf opts :ubsan)
                        :hwasan (getf opts :hwasan)
                        :eh-model (getf opts :eh-model))
                  nil))
      (if (eq arch :arm64)
          (apply #'compile-to-aarch64-bytes
                 program
                 (if opts
                     (list :retpoline (getf opts :retpoline)
                           :stack-protector (getf opts :stack-protector)
                           :shadow-stack (getf opts :shadow-stack)
                           :asan (getf opts :asan)
                           :msan (getf opts :msan)
                           :tsan (getf opts :tsan)
                           :ubsan (getf opts :ubsan)
                           :hwasan (getf opts :hwasan))
                     nil))
           (error "Unknown native architecture: ~S" arch))))

(defun %native-mir-target (arch)
  "Return the MIR instruction-selection target keyword for native ARCH."
  (ecase arch
    (:x86-64 :x86-64)
    (:arm64 :aarch64)))

(defun %mir-pipeline-enabled-p (opts)
  "Return true when the MIR pipeline should be attempted."
  (or *use-mir-pipeline* (getf opts :mir-isel)))

(defun %maybe-route-program-through-mir (program arch opts)
  "Route PROGRAM through MIR/ISel when enabled, falling back on any failure."
  (if (%mir-pipeline-enabled-p opts)
      (handler-case
          (cl-cc/codegen:isel-vm-program program :target (%native-mir-target arch))
        (error (condition)
          (format *error-output* "; MIR pipeline disabled for this unit: ~A~%" condition)
          program))
      program))

(defun %strip-internal-opts (opts)
  "Remove pipeline-internal options from OPTS that downstream compile functions
don't accept as keyword arguments."
  (let ((stripped (copy-list opts)))
    ;; Remove pipeline-only keys before passing to internal compile functions.
    ;; :target-os and :compress are consumed by the native output layer
    ;; and must not reach compile-string / compile-expression / compile-toplevel-forms.
    (remf stripped :target-os)
    (remf stripped :compress)
    (remf stripped :bolt)
    (remf stripped :bolt-profile)
    (remf stripped :mir-isel)
    stripped))

(defun compile-to-native (source &key (arch :x86-64) (output-file "a.out") (language :lisp)
                                     pass-pipeline speed (inline-threshold-scale 1)
                                      debug-info sanitize lto eh-model incremental perf-map parallel
                                     print-pass-timings timing-stream coverage
                                    print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                    print-pass-stats stats-stream trace-json-stream
                                    retpoline spectre-mitigations stack-protector shadow-stack
                                     compress asan msan tsan ubsan hwasan
                                      target-os mir-isel bolt bolt-profile)
  "Compile SOURCE to a native executable.
SOURCE can be a string (single expression) or a list of forms.
ARCH is :X86-64 or :ARM64.
OUTPUT-FILE is the path for the executable.
LANGUAGE is :LISP (default) or :PHP.
TARGET-OS is :DARWIN (default on macOS), :LINUX, or :WINDOWS. When NIL, auto-detected
from the host OS. The binary format is derived from TARGET-OS: Mach-O on Darwin,
ELF on Linux, PE on Windows.

Returns the output file path on success."
  (let* ((effective-target-os (or target-os (%native-host-os)))
         (binary-format (%native-binary-format effective-target-os))
         (native-target (%native-target-for-arch arch))
            (opts (%make-native-opts :pass-pipeline pass-pipeline
                                      :speed speed
                                      :inline-threshold-scale inline-threshold-scale
                                      :debug-info debug-info
                                      :sanitize sanitize
                                      :lto lto
                                      :eh-model eh-model
                                      :incremental incremental
                                       :perf-map perf-map
                                       :bolt bolt
                                       :bolt-profile bolt-profile
                                       :parallel parallel
                                      :print-pass-timings print-pass-timings
                                      :coverage coverage
                                     :timing-stream timing-stream
                                     :print-opt-remarks print-opt-remarks
                                     :opt-remarks-stream opt-remarks-stream
                                     :opt-remarks-mode opt-remarks-mode
                                      :print-pass-stats print-pass-stats
                                      :stats-stream stats-stream
                                       :trace-json-stream trace-json-stream
                                        :retpoline retpoline
                                        :spectre-mitigations spectre-mitigations
                                        :stack-protector stack-protector
                                        :shadow-stack shadow-stack
                                         :compress compress
                                         :mir-isel mir-isel
                                        :asan asan
                                       :msan msan
                                       :tsan tsan
                                       :ubsan ubsan
                                       :hwasan hwasan
                                       :strict-no-alloc strict-no-alloc
                                       :target-os effective-target-os))
         (compile-opts (%strip-internal-opts opts))
         (result (%compile-native-source source native-target language compile-opts))
            (program (maybe-pipeline-bolt-optimize-program
                      (pipeline-reorder-functions
                       (%maybe-route-program-through-mir
                        (compilation-result-program result) arch opts))
                      opts))
         (code-bytes (let ((cl-cc/codegen::*x86-64-use-retpoline*
                             (or (getf opts :retpoline)
                                 (getf opts :spectre-mitigations)
                                 cl-cc/codegen::*x86-64-use-retpoline*))
                            (cl-cc/codegen::*x86-64-spectre-mitigations-enabled*
                             (or (getf opts :spectre-mitigations)
                                 cl-cc/codegen::*x86-64-spectre-mitigations-enabled*))
                            (cl-cc/codegen::*x86-64-stack-protector-enabled*
                             (or (getf opts :stack-protector)
                                 cl-cc/codegen::*x86-64-stack-protector-enabled*))
                            (cl-cc/codegen::*x86-64-shadow-stack-enabled*
                             (or (getf opts :shadow-stack)
                                  cl-cc/codegen::*x86-64-shadow-stack-enabled*))
                             (cl-cc/codegen::*eh-model*
                              (cl-cc/codegen:normalize-x86-64-eh-model
                               (or (getf opts :eh-model) cl-cc/codegen::*eh-model*))))
                         (%native-code-bytes-for-arch arch program opts)))
         (compilation-result (compilation-result-program result))
         (reloc-entries (and compilation-result
                             (ignore-errors
                              (cl-cc/codegen::program-reloc-entries compilation-result)))))
    (%write-native-output code-bytes reloc-entries output-file
                          :format binary-format :arch arch
                          :compress (getf opts :compress))))

(defun %native-file-language (input-file language)
  (if language
      language
      (let ((file-type (pathname-type input-file)))
        (if file-type
            (if (string= file-type "php")
                :php
                :lisp)
            :lisp))))

(defun %native-output-file (input-file output-file)
  (if output-file
      output-file
      (make-pathname :type nil :defaults input-file)))

(defun %native-read-lisp-forms-loop (stream eof-marker forms)
  (let ((form (read stream nil eof-marker)))
    (if (eq form eof-marker)
        (%native-reverse-list forms nil)
        (%native-read-lisp-forms-loop stream eof-marker (cons form forms)))))

(defun %native-reverse-list (items acc)
  (if (consp items)
      (%native-reverse-list (cdr items) (cons (car items) acc))
      acc))

(defun %native-read-character-stream-loop (in buf index limit)
  (if (< index limit)
      (let ((ch (read-char in nil nil)))
        (if ch
            (progn
              (setf (aref buf index) ch)
              (%native-read-character-stream-loop in buf (+ index 1) limit))
            buf))
      buf))

(defun %native-read-character-file (input-file)
  (let ((in (open input-file :direction :input :element-type 'character)))
    (unwind-protect
        (let ((buf (make-string (file-length in))))
          (%native-read-character-stream-loop in buf 0 (length buf)))
      (close in))))

(defun %native-strip-shebang-line (source)
  "Return SOURCE with a leading POSIX #! interpreter line removed."
  (let* ((text (or source ""))
         (newline (position #\Newline text))
         (first-line (if newline (subseq text 0 newline) text)))
    (if (and (>= (length first-line) 2)
             (char= (char first-line 0) #\#)
             (char= (char first-line 1) #\!))
        (if newline (subseq text (1+ newline)) "")
        text)))

(defun %native-read-lisp-file (input-file)
  (let ((source (%native-strip-shebang-line (%native-read-character-file input-file))))
    (with-input-from-string (in source)
      (let ((*read-eval* nil))
        (%native-read-lisp-forms-loop in (list :eof) nil)))))

(defun %native-read-file-source (input-file language)
  (if (eq language :php)
      (%native-read-character-file input-file)
      (%native-read-lisp-file input-file)))

(defun %compile-native-file-source (source target language opts)
  (if (eq language :php)
      (%compile-native-string source target :php opts)
      (%compile-native-lisp-forms source target opts)))

(defun compile-file-to-native (input-file &key (arch :x86-64) (output-file nil) (language nil)
                                               pass-pipeline speed (inline-threshold-scale 1)
                                                block-compile debug-info sanitize lto eh-model incremental perf-map parallel
                                              print-pass-timings timing-stream coverage
                                             print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                             print-pass-stats stats-stream trace-json-stream
                                               retpoline spectre-mitigations stack-protector shadow-stack
                                               compress asan msan tsan ubsan hwasan
                                                target-os mir-isel bolt bolt-profile (compilation-tier *compilation-tier*))
  "Compile a CL-CC source file to a native executable.
INPUT-FILE is the path to the source file.
OUTPUT-FILE defaults to INPUT-FILE with no extension.
LANGUAGE is :LISP or :PHP. When nil, auto-detected from the file extension.
TARGET-OS is :DARWIN (default on macOS), :LINUX, or :WINDOWS. When NIL, auto-detected."
  (let* ((effective-language (%native-file-language input-file language))
         (output (%native-output-file input-file output-file))
         (source (%native-read-file-source input-file effective-language))
         (effective-target-os (or target-os (%native-host-os)))
         (binary-format (%native-binary-format effective-target-os))
            (opts (%make-native-opts :pass-pipeline pass-pipeline
                                      :speed speed
                                      :inline-threshold-scale inline-threshold-scale
                                      :block-compile block-compile
                                      :debug-info debug-info
                                      :sanitize sanitize
                                      :lto lto
                                      :eh-model eh-model
                                      :incremental incremental
                                       :perf-map perf-map
                                       :bolt bolt
                                       :bolt-profile bolt-profile
                                       :parallel parallel
                                     :print-pass-timings print-pass-timings
                                     :coverage coverage
                                    :timing-stream timing-stream
                                    :print-opt-remarks print-opt-remarks
                                    :opt-remarks-stream opt-remarks-stream
                                    :opt-remarks-mode opt-remarks-mode
                                     :print-pass-stats print-pass-stats
                                     :stats-stream stats-stream
                                      :trace-json-stream trace-json-stream
                                       :retpoline retpoline
                                       :spectre-mitigations spectre-mitigations
                                       :stack-protector stack-protector
                                       :shadow-stack shadow-stack
                                        :compress compress
                                        :mir-isel mir-isel
                                       :asan asan
                                      :msan msan
                                      :tsan tsan
                                      :ubsan ubsan
                                       :hwasan hwasan
                                       :strict-no-alloc strict-no-alloc
                                       :compilation-tier compilation-tier
                                       :target-os effective-target-os))
           (cache-key (%compile-cache-key source arch effective-language opts))
          (cache-path (%compile-cache-path cache-key output)))
    (ensure-directories-exist cache-path)
    (if (probe-file cache-path)
        (progn
          (format *error-output* "; cache hit ~A~%" cache-path)
          (%copy-file-bytes cache-path output)
          (%run-short-native-command (list "chmod" "+x" (namestring output)))
          output)
        (let* ((native-target (%native-target-for-arch arch))
               (compile-opts (%strip-internal-opts opts))
               (result (%compile-native-file-source source native-target effective-language compile-opts))
                  (program (maybe-pipeline-bolt-optimize-program
                            (pipeline-reorder-functions
                             (%maybe-route-program-through-mir
                              (compilation-result-program result) arch opts))
                            opts))
                (code-bytes (let ((cl-cc/codegen::*x86-64-use-retpoline*
                                    (or (getf opts :retpoline)
                                        (getf opts :spectre-mitigations)
                                        cl-cc/codegen::*x86-64-use-retpoline*))
                                   (cl-cc/codegen::*x86-64-spectre-mitigations-enabled*
                                    (or (getf opts :spectre-mitigations)
                                        cl-cc/codegen::*x86-64-spectre-mitigations-enabled*))
                                  (cl-cc/codegen::*x86-64-stack-protector-enabled*
                                   (or (getf opts :stack-protector)
                                       cl-cc/codegen::*x86-64-stack-protector-enabled*))
                                   (cl-cc/codegen::*x86-64-shadow-stack-enabled*
                                    (or (getf opts :shadow-stack)
                                        cl-cc/codegen::*x86-64-shadow-stack-enabled*))
                                   (cl-cc/codegen::*eh-model*
                                    (cl-cc/codegen:normalize-x86-64-eh-model
                                     (or (getf opts :eh-model) cl-cc/codegen::*eh-model*))))
                               (%native-code-bytes-for-arch arch program opts))))
          (%write-native-output code-bytes nil output
                               :format binary-format :arch arch
                               :compress (getf opts :compress))
          (%copy-file-bytes output cache-path)
          output))))

;;; Typeclass macros (deftype-class, deftype-instance) are registered in
;;; pipeline-native-typeclass.lisp (loaded after this file).

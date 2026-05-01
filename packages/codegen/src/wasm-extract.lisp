;;;; packages/emit/src/wasm-extract.lisp - WASM Function Extraction Pass
;;;
;;; Splits the flat vm-program instruction list into per-function bodies.
;;; The flat instruction list interleaves closure bodies (jump-over pattern)
;;; with top-level code. This pass separates them and produces a wasm-module-ir.

(in-package :cl-cc/codegen)

(defun %scan-instructions (instructions fn)
  "Call FN on each instruction in INSTRUCTIONS."
  (dolist (inst instructions)
    (funcall fn inst)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 1: Collect all function entry labels
;;; ─────────────────────────────────────────────────────────────────────────────

(defun collect-entry-labels (instructions)
  "Scan INSTRUCTIONS and return a hash-set of all function entry label names.
   An entry label appears as (vm-label-name inst) on vm-closure and vm-func-ref."
  (let ((entry-labels (make-hash-table :test #'equal)))
    (%scan-instructions
     instructions
     (lambda (inst)
       (typecase inst
         (vm-closure
          (setf (gethash (vm-label-name inst) entry-labels) t))
         (vm-func-ref
          (setf (gethash (vm-label-name inst) entry-labels) t))
         (vm-register-function
           ;; vm-register-function stores name in a slot, not a label -- skip
          nil))))
    entry-labels))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 2: Segment the flat instruction list
;;; ─────────────────────────────────────────────────────────────────────────────

;;; A segment is either:
;;;   (:toplevel instructions) -- top-level code
;;;   (:function entry-label instructions) -- a function body

(defun segment-instructions (instructions entry-labels)
  "Split INSTRUCTIONS into segments based on entry labels.
   Returns a list of segments: (:toplevel ...) or (:function label ...)."
  (let ((segments nil)
        (current-toplevel nil)
        (in-function nil)
        (current-function-label nil)
        (current-function-body nil))
    (flet ((flush-toplevel ()
             (when current-toplevel
               (push (cons :toplevel (nreverse current-toplevel)) segments)
               (setf current-toplevel nil)))
           (flush-function ()
             (when in-function
               (push (list :function current-function-label
                           (nreverse current-function-body))
                     segments)
               (setf in-function nil
                     current-function-label nil
                     current-function-body nil))))
      (dolist (inst instructions)
        (cond
          ;; Entering a function body?
          ((and (typep inst 'vm-label)
                (gethash (vm-name inst) entry-labels))
           ;; Flush pending top-level
           (flush-toplevel)
           ;; Flush any previous function (shouldn't happen, but safe)
           (flush-function)
           ;; Start new function body
           (setf in-function t
                 current-function-label (vm-name inst)
                 current-function-body (list inst)))
          ;; Inside a function body
          (in-function
           (push inst current-function-body)
           ;; vm-ret ends the function body
           (when (typep inst 'vm-ret)
             (flush-function)))
          ;; Top-level instruction
          (t
           (push inst current-toplevel))))
      ;; Flush remaining
      (flush-toplevel)
      (flush-function))
    (nreverse segments)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 2b: Collect function parameter registers from vm-closure instructions
;;; ─────────────────────────────────────────────────────────────────────────────

(defun collect-function-params (instructions)
  "Scan all INSTRUCTIONS for vm-closure and return a hash table mapping each
   function entry label name (string) to its VM parameter register list
   (list of keywords such as (:R0 :R1)).  Used to propagate arity info into
   wasm-function-def so the trampoline can emit a correct arg-load prologue."
  (let ((map (make-hash-table :test #'equal)))
    (%scan-instructions
     instructions
     (lambda (inst)
       (when (typep inst 'vm-closure)
         (let ((label (vm-label-name inst)))
           (when label
             (setf (gethash label map)
                   (or (vm-closure-params inst) nil)))))))
    map))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 3: Build wasm-function-def records from segments
;;; ─────────────────────────────────────────────────────────────────────────────

(defun entry-label-to-wat-name (label)
  "Convert a VM entry label string to a WAT function name.
   E.g. 'func_3_entry' -> '$func_3_entry', 'lambda_7' -> '$lambda_7'."
  (format nil "$~A" label))

(defun make-wasm-func-from-segment (label body-instructions func-index)
  "Create a wasm-function-def from a function segment.
   LABEL is the entry label string, BODY-INSTRUCTIONS is the instruction list."
  (make-wasm-function-def
   :index func-index
   :wat-name (entry-label-to-wat-name label)
   :source-instructions body-instructions
   :params nil    ; filled by trampoline builder
   :locals nil    ; filled by trampoline builder
   :body nil      ; filled by trampoline builder
   :exported-p nil))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Main entry point: extract-wasm-functions
;;; ─────────────────────────────────────────────────────────────────────────────

(defun extract-wasm-functions (program)
  "Extract function bodies from a flat vm-program.
   Returns a wasm-module-ir with function defs (bodies not yet built).
   The top-level code is stored as the 'main' function (exported as \"main\")."
  (let* ((instructions (vm-program-instructions program))
         (entry-labels (collect-entry-labels instructions))
         (segments (segment-instructions instructions entry-labels))
         (module (make-empty-wasm-module))
         (func-index 0)
         (toplevel-instructions nil))
    ;; Collect all global variable names and register them in the module.
    ;; Must happen before function body building so WAT references resolve.
    (let ((seen-globals (make-hash-table :test #'equal)))
      (dolist (inst instructions)
        (typecase inst
          ((or vm-set-global vm-get-global)
           (let ((gname (vm-global-name inst)))
             (unless (gethash gname seen-globals)
               (setf (gethash gname seen-globals) t)
               (wasm-module-add-global module
                 (make-wasm-global-def
                  :wat-name (vm-global-wat-name gname)
                  :value-type :eqref
                  :mutability :mutable
                  :init-value :null))))))))
    ;; Process each segment
    (dolist (segment segments)
      (ecase (car segment)
        (:toplevel
         (setf toplevel-instructions
               (append toplevel-instructions (cdr segment))))
        (:function
         (let* ((label (second segment))
                (body (third segment))
                (func (make-wasm-func-from-segment label body func-index)))
           (incf func-index)
           (wasm-module-add-function module func)))))
    ;; Create the "$main" function for top-level code
    (let ((main-func (make-wasm-function-def
                      :index func-index
                      :wat-name "$main"
                      :source-instructions toplevel-instructions
                      :exported-p t
                      :export-name "main")))
      (wasm-module-add-function module main-func))
    ;; Reverse the functions list so it is in definition order
    ;; (wasm-module-add-function pushes to the front)
    (setf (wasm-module-functions module)
          (nreverse (wasm-module-functions module)))
    ;; Enrich each function def with its VM parameter register list.
    ;; The params-map was built from vm-closure instructions in the flat list.
    (let ((params-map (collect-function-params instructions)))
      (dolist (func (wasm-module-functions module))
        (let* ((wat-name (wasm-func-wat-name func))
               ;; WAT name is "$label_name"; strip the leading $
               (label-name (if (and wat-name (> (length wat-name) 0)
                                    (char= (char wat-name 0) #\$))
                               (subseq wat-name 1)
                               wat-name))
               (params (gethash label-name params-map)))
          (when params
            (setf (wasm-func-params func) params)))))
    module))

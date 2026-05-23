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

(defun %wasm-dense-case-entry-labels (instructions)
  "Return closure labels that belong to a dense CASE table dispatch.

Those lambda bodies must stay in the enclosing trampoline function so the later
br_table lowering can branch to their VM labels instead of call_indirect-ing
through extracted standalone functions."
  (let ((const-values (make-hash-table :test #'eq))
        (closure-labels (make-hash-table :test #'eq))
        (list-labels (make-hash-table :test #'eq))
        (vector-labels (make-hash-table :test #'eq))
        (index-info (make-hash-table :test #'eq))
        (case-labels (make-hash-table :test #'equal)))
    (labels ((dense-labels-p (labels)
               (and labels (>= (length labels) 4)))
             (remember (label-list)
               (when (dense-labels-p label-list)
                 (dolist (label label-list)
                   (setf (gethash label case-labels) t)))))
      (dolist (inst instructions case-labels)
        (cond
          ((typep inst 'vm-const)
           (setf (gethash (vm-dst inst) const-values) (vm-value inst)))
          ((typep inst 'vm-closure)
           (setf (gethash (vm-dst inst) closure-labels) (vm-label-name inst)))
          ((typep inst 'vm-cons)
           (let ((car-label (gethash (vm-car-reg inst) closure-labels))
                 (cdr-labels (gethash (vm-cdr-reg inst) list-labels)))
             (multiple-value-bind (cdr-value cdr-const-p)
                 (gethash (vm-cdr-reg inst) const-values)
               (when (and car-label (or (and cdr-const-p (null cdr-value)) cdr-labels))
                 (setf (gethash (vm-dst inst) list-labels)
                       (cons car-label cdr-labels))))))
          ((typep inst 'cl-cc/vm::vm-coerce-to-vector)
           (let ((labels (gethash (vm-src inst) list-labels)))
             (when labels
               (setf (gethash (vm-dst inst) vector-labels) labels))))
          ((typep inst 'vm-sub)
           (let ((min-key (gethash (vm-rhs inst) const-values)))
             (when (integerp min-key)
               (setf (gethash (vm-dst inst) index-info)
                     (cons (vm-lhs inst) min-key)))))
          ((typep inst 'vm-move)
           (dolist (table (list vector-labels index-info))
             (let ((value (gethash (vm-src inst) table)))
               (when value
                 (setf (gethash (vm-dst inst) table) value)))))
          ((typep inst 'cl-cc/vm::vm-svref)
           (when (gethash (vm-rhs inst) index-info)
             (remember (gethash (vm-lhs inst) vector-labels)))))))))

(defun collect-entry-labels (instructions)
  "Scan INSTRUCTIONS and return a hash-set of all function entry label names.
   An entry label appears as (vm-label-name inst) on vm-closure and vm-func-ref."
  (let ((entry-labels (make-hash-table :test #'equal))
        (case-entry-labels (%wasm-dense-case-entry-labels instructions)))
    (%scan-instructions
     instructions
     (lambda (inst)
       (typecase inst
          (vm-closure
           (unless (gethash (vm-label-name inst) case-entry-labels)
             (setf (gethash (vm-label-name inst) entry-labels) t)))
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
   :type-index +type-idx-main-func+
   :source-instructions body-instructions
   :params nil    ; filled by trampoline builder
   :locals nil    ; filled by trampoline builder
    :body nil      ; filled by trampoline builder
    :exported-p nil))

(defun %wasm-instruction-pc-index (instructions)
  "Return an EQ hash table mapping each instruction object to its flat PC."
  (let ((index (make-hash-table :test #'eq)))
    (loop for inst in instructions
          for pc from 0
          do (setf (gethash inst index) pc))
    index))

(defun %wasm-label-at-pc (instructions pc)
  "Return the VM label name at PC when PC denotes a vm-label instruction."
  (let ((inst (and (integerp pc) (nth pc instructions))))
    (when (typep inst 'vm-label)
      (vm-name inst))))

(defun %wasm-function-local-exception-table (full-instructions body-instructions exception-table)
  "Project PROGRAM's exception table onto BODY-INSTRUCTIONS using local PCs."
  (let ((local-index (%wasm-instruction-pc-index body-instructions))
        (entries nil))
    (when exception-table
      (loop for entry across exception-table
            for start-inst = (nth (cl-cc/vm::vm-exception-entry-start-pc entry)
                                  full-instructions)
            for end-inst = (nth (1- (cl-cc/vm::vm-exception-entry-end-pc entry))
                                full-instructions)
            for start-local = (and start-inst (gethash start-inst local-index))
            for end-local = (and end-inst (gethash end-inst local-index))
            for handler-label = (%wasm-label-at-pc
                                 full-instructions
                                 (cl-cc/vm::vm-exception-entry-handler-pc entry))
            when (and start-local end-local handler-label)
              do (push (list :kind :handler
                             :start-pc start-local
                             :end-pc (1+ end-local)
                             :handler-label handler-label
                             :result-reg (cl-cc/vm::vm-exception-entry-result-reg entry)
                             :condition-type (cl-cc/vm::vm-exception-entry-condition-type entry))
                       entries)))
    (nreverse entries)))

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
          (exception-table (gethash program cl-cc/vm::*vm-program-exception-tables*))
          (func-index 0)
          (toplevel-instructions nil))
    (setf (wasm-module-exception-table module) exception-table)
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
            (setf (wasm-func-exception-table func)
                  (%wasm-function-local-exception-table instructions body exception-table))
            (incf func-index)
            (wasm-module-add-function module func)))))
    ;; Create the "$main" function for top-level code
    (let ((main-func (make-wasm-function-def
                      :index func-index
                       :wat-name "$main"
                       :type-index +type-idx-main-func+
                        :source-instructions toplevel-instructions
                       :exception-table (%wasm-function-local-exception-table
                                         instructions toplevel-instructions exception-table)
                       :exported-p t
                      :export-name "main")))
      (wasm-module-add-function module main-func))
    ;; Reverse the functions list so it is in definition order
    ;; (wasm-module-add-function pushes to the front)
    (setf (wasm-module-functions module)
          (nreverse (wasm-module-functions module)))
    (dolist (func (wasm-module-functions module))
      (wasm-module-record-function-label! module func))
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

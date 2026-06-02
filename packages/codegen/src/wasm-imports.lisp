;;;; packages/codegen/src/wasm-imports.lisp - WAT imports section
;;;
;;; AOT mode tracking, import filtering/dead-import elimination,
;;; and emit-wat-imports for standard cl-cc I/O host imports.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT imports section (I/O host functions)
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-aot-current-used-imports* nil
  "Dynamic hash-set of host bridge import names used by the module being emitted.")

(defun %wasm-aot-mode-active-p ()
  "Return T when the backend should avoid mandatory host imports."
  (wasm-feature-enabled-p "CLCC_WASM_AOT" *wasm-aot-mode-enabled*))

(defun %wasm-import-needed-p (name)
  "Return T when host import NAME should be emitted."
  (or (not *wasm-dead-import-elimination-enabled*)
      (null *wasm-aot-current-used-imports*)
      (gethash name *wasm-aot-current-used-imports*)))

(defun %wasm-call-string-mentions-p (needle string)
  (and (stringp string) (search needle string :test #'char=)))

(defun wasm-module-used-host-imports (module)
  "Build a best-effort dependency graph of host bridge functions used by MODULE."
  (let ((used (make-hash-table :test #'equal)))
    (labels ((mark (name) (setf (gethash name used) t))
             (scan-string (text)
               (dolist (entry '(("$host_write_char" . "write_char")
                                ("$host_read_char" . "read_char")
                                ("$host_write_string" . "write_string")
                                ("$host_error" . "error")
                                ("$host_print_val" . "print_val")
                                ("$host_rt_register_method" . "register_method")
                                ("$host_rt_call_generic" . "call_generic")
                                ("$cl_condition_to_exnref" . "condition_to_exnref")
                                ("$cl_exnref_payload" . "exnref_payload")
                                ("$cl_exnref_tag" . "exnref_tag")))
                 (when (%wasm-call-string-mentions-p (car entry) text)
                   (mark (cdr entry))))))
      (dolist (func (wasm-module-functions module))
        (dolist (body (wasm-func-body func))
          (scan-string body))
        (dolist (inst (wasm-func-source-instructions func))
          (typecase inst
            (vm-print (mark "print_val"))
            (vm-register-method (mark "register_method"))
            (vm-generic-call (mark "call_generic"))
            ((or vm-signal-error vm-throw)
             (when (wasm-eh-v2-feature-enabled-p)
               (mark "condition_to_exnref")))))))
    (when (wasm-import-cl-condition-tag-enabled-p)
      (setf (gethash "condition-tag" used) t))
    (when (wasm-eh-v2-feature-enabled-p)
      (setf (gethash "condition_to_exnref" used) t
            (gethash "exnref_payload" used) t
            (gethash "exnref_tag" used) t))
    used))

(defun emit-wat-aot-host-stubs (stream)
  "Emit no-op host bridge stubs so AOT modules are self-contained."
  (format stream "~%  ;; FR-219: AOT host bridge stubs (no mandatory JS imports)")
  (when (%wasm-import-needed-p "write_char")
    (format stream "~%  (func $host_write_char (param i32))"))
  (when (%wasm-import-needed-p "read_char")
    (format stream "~%  (func $host_read_char (result i32) (i32.const -1))"))
  (when (%wasm-import-needed-p "write_string")
    (format stream "~%  (func $host_write_string (param (ref $string_t)))"))
  (when (%wasm-import-needed-p "error")
    (format stream "~%  (func $host_error (param (ref $string_t)))"))
  (when (%wasm-import-needed-p "print_val")
    (format stream "~%  (func $host_print_val (param eqref))"))
  (when (%wasm-import-needed-p "register_method")
    (format stream "~%  (func $host_rt_register_method (param eqref) (param eqref) (param eqref) (param eqref))"))
  (when (%wasm-import-needed-p "call_generic")
    (format stream "~%  (func $host_rt_call_generic (param eqref) (param i32) (result eqref) (ref.null eq))")))

(defun emit-wat-imports (stream)
  "Emit standard cl-cc I/O imports."
  (if (%wasm-aot-mode-active-p)
      (emit-wat-aot-host-stubs stream)
      (progn
  (format stream "~%  ;; Host I/O imports")
  (when (and (wasm-import-cl-condition-tag-enabled-p)
             (%wasm-import-needed-p "condition-tag"))
    (format stream "~%  ;; FR-310: external exception tag import")
    (format stream "~%  (import \"cl-core\" \"condition-tag\" (tag $cl_condition_tag (param eqref eqref)))"))
  (when (and (wasm-eh-v2-feature-enabled-p)
             (%wasm-import-needed-p "condition_to_exnref"))
    (format stream "~%  ;; FR-252/FR-262: host bridge for fresh throw_ref exceptions")
    (format stream "~%  (import \"cl_exception\" \"condition_to_exnref\" (func $cl_condition_to_exnref (param eqref) (result exnref)))")
    (format stream "~%  (import \"cl_exception\" \"exnref_payload\" (func $cl_exnref_payload (param exnref) (result eqref)))")
    (format stream "~%  (import \"cl_exception\" \"exnref_tag\" (func $cl_exnref_tag (param exnref) (result eqref)))"))
  (when *wasm-ref-types-externref-enabled*
    (format stream "~%  ;; FR-226: opaque JavaScript object import")
    (format stream "~%  (import \"js\" \"host-object\" (func $js_host_object (result externref)))"))
  (when *wasm-custom-descriptors-enabled*
    (emit-wasm-annotation-custom-section stream "FR-241.import"
                                         "typed externref descriptor import required: js.descriptor -> externref"))
  (when *wasm-type-imports-enabled*
    (emit-wasm-annotation-custom-section stream "FR-244.import"
                                         "dynamic type import required: cl-core.$cons_t"))
  (when *wasm-wasi-random-crypto-enabled*
    (emit-wasm-annotation-custom-section stream "FR-299.import"
                                         "WASI random/crypto imports required: get-random-bytes, hash"))
  (when (%wasm-import-needed-p "write_char")
    (format stream "~%  (import \"cl_io\" \"write_char\" (func $host_write_char (param i32)))"))
  (when (%wasm-import-needed-p "read_char")
    (format stream "~%  (import \"cl_io\" \"read_char\" (func $host_read_char (result i32)))"))
  (when (%wasm-import-needed-p "write_string")
    (format stream "~%  (import \"cl_io\" \"write_string\" (func $host_write_string (param (ref $string_t))))"))
  (when (%wasm-import-needed-p "error")
    (format stream "~%  (import \"cl_io\" \"error\" (func $host_error (param (ref $string_t))))"))
  ;; print_val: host-side formatter for any eqref value (used by vm-print).
  ;; Keep this standard import present for module-structure compatibility.
  (format stream "~%  (import \"cl_io\" \"print_val\" (func $host_print_val (param eqref)))")
  ;; FR-321 staged runtime MOP bridge imports
  (when (%wasm-import-needed-p "register_method")
    (format stream "~%  (import \"cl_runtime\" \"register_method\" (func $host_rt_register_method (param eqref) (param eqref) (param eqref) (param eqref)))"))
  (when (%wasm-import-needed-p "call_generic")
    (format stream "~%  (import \"cl_runtime\" \"call_generic\" (func $host_rt_call_generic (param eqref) (param i32) (result eqref)))")))))

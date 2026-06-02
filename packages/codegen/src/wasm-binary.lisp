;;;; packages/codegen/src/wasm-binary.lisp — WASM Binary Backend (FR-297)
;;;
;;; Binary encoding: LEB128, section buffers, type/function/export sections,
;;; module serialization, and compile-to-wasm-binary entry point.
;;;
;;; Load order: after wasm.lisp (WAT text backend).
;;; AOT helpers live in wasm-binary-aot.lisp.
;;; Debug/devtools custom sections live in wasm-binary-debug.lisp.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WASM binary backend (FR-297)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-binary-write-u8 (buffer byte)
  "Write BYTE to BUFFER using the unified byte-buffer API."
  (cl-cc/binary::buffer-write-byte buffer byte))

(defun wasm-binary-write-bytes (buffer bytes)
  "Write BYTES to BUFFER using the unified byte-buffer API."
  (cl-cc/binary::buffer-write-bytes buffer bytes))

(defun wasm-encode-unsigned-leb128 (value)
  "Return VALUE encoded as an unsigned LEB128 byte vector."
  (check-type value (integer 0 *))
  (let ((bytes nil)
        (n value))
    (loop
      (let ((byte (logand n #x7f)))
        (setf n (ash n -7))
        (when (plusp n)
          (setf byte (logior byte #x80)))
        (push byte bytes)
        (unless (plusp n)
          (return (coerce (nreverse bytes)
                          '(simple-array (unsigned-byte 8) (*)))))))))

(defun wasm-encode-signed-leb128 (value)
  "Return VALUE encoded as a signed LEB128 byte vector."
  (check-type value integer)
  (let ((bytes nil)
        (n value)
        (more t))
    (loop while more do
      (let ((byte (logand n #x7f)))
        (setf n (ash n -7))
        (if (or (and (zerop n) (zerop (logand byte #x40)))
                (and (= n -1) (not (zerop (logand byte #x40)))))
            (setf more nil)
            (setf byte (logior byte #x80)))
        (push byte bytes)))
    (coerce (nreverse bytes) '(simple-array (unsigned-byte 8) (*)))))

(defun wasm-binary-write-uleb128 (buffer value)
  "Write VALUE as unsigned LEB128 to BUFFER."
  (wasm-binary-write-bytes buffer (wasm-encode-unsigned-leb128 value)))

(defun wasm-binary-section-buffer ()
  "Create a temporary byte-buffer for a section payload."
  (cl-cc/binary::make-byte-buffer 32))

(defun wasm-binary-section-bytes (writer)
  "Return bytes produced by calling WRITER with a temporary section buffer."
  (let ((payload (wasm-binary-section-buffer)))
    (funcall writer payload)
    (cl-cc/binary::buffer-get-bytes payload)))

(defun wasm-binary-write-section (buffer section-id writer)
  "Write a standard WASM section with SECTION-ID and payload from WRITER."
  (let ((payload (wasm-binary-section-bytes writer)))
    (wasm-binary-write-u8 buffer section-id)
    (wasm-binary-write-uleb128 buffer (length payload))
    (wasm-binary-write-bytes buffer payload)))

(defun wasm-binary-write-name (buffer name)
  "Write NAME as a WASM binary name (byte length + bytes)."
  (let ((bytes (map 'vector #'char-code name)))
    (wasm-binary-write-uleb128 buffer (length bytes))
    (wasm-binary-write-bytes buffer bytes)))

(defparameter *wasm-value-type-byte-table*
  ;; Populated at load time so the constants are already bound.
  (list (cons :i32      +wasm-i32+)
        (cons :i64      +wasm-i64+)
        (cons :f32      +wasm-f32+)
        (cons :f64      +wasm-f64+)
        (cons :f16      +wasm-f16+)
        (cons :funcref  +wasm-funcref+)
        (cons :externref +wasm-externref+)
        (cons :stringref +wasm-stringref+)
        (cons :eqref    +wasm-eqref+))
  "Alist mapping WASM value-type keywords to their binary encoding bytes.")

(defun wasm-binary-value-type-byte (type)
  "Return binary value-type byte for TYPE."
  (let ((entry (assoc type *wasm-value-type-byte-table*)))
    (or (and entry (cdr entry))
        (error "Unknown WASM value type: ~S" type))))

(defun wasm-binary-write-valtype-vector (buffer types)
  "Write a vector of value TYPES."
  (wasm-binary-write-uleb128 buffer (length types))
  (dolist (type types)
    (wasm-binary-write-u8 buffer (wasm-binary-value-type-byte type))))

(defun wasm-binary-write-func-type (buffer params results)
  "Write a WASM function type."
  (wasm-binary-write-u8 buffer +wasm-type-func+)
  (wasm-binary-write-valtype-vector buffer params)
  (wasm-binary-write-valtype-vector buffer results))

(defun wasm-encode-vm-instruction-opcode (inst)
  "Return core WASM opcode bytes corresponding to VM instruction INST."
  (typecase inst
    (vm-const
     (let ((value (vm-value inst)))
       (cond
         ((integerp value)
          (concatenate '(simple-array (unsigned-byte 8) (*))
                       (vector +wasm-i64-const+)
                       (wasm-encode-signed-leb128 value)))
         ((null value) (vector +wasm-ref-null+ +heap-none+))
         (t (vector +wasm-nop+)))))
    ((or vm-add vm-integer-add) (vector +wasm-i64-add+))
    ((or vm-sub vm-integer-sub) (vector +wasm-i64-sub+))
    ((or vm-mul vm-integer-mul) (vector +wasm-i64-mul+))
    (vm-rotate (vector +wasm-i64-rotr+))
    (vm-logcount (vector +wasm-i64-popcnt+))
    (vm-integer-length (vector +wasm-i64-clz+))
    (vm-call (concatenate '(simple-array (unsigned-byte 8) (*))
                          (vector +wasm-call-indirect+)
                          (wasm-encode-unsigned-leb128 +type-idx-main-func+)
                          (wasm-encode-unsigned-leb128 0)))
    (vm-ret (vector +wasm-return+))
    (vm-halt (vector +wasm-return+))
    (t (vector +wasm-nop+))))

(defun wasm-binary-module-functions (module)
  "Return MODULE functions in emission order."
  (or (wasm-module-functions module) nil))

(defun wasm-binary-write-type-section (buffer &key (params nil) (results nil))
  "Write a Type section containing a single MVP () -> () function signature."
  (wasm-binary-write-section
   buffer +wasm-section-type+
   (lambda (section)
     (wasm-binary-write-uleb128 section 1)
     (wasm-binary-write-func-type section params results))))

(defun wasm-binary-write-function-section (buffer functions)
  "Write the Function section for FUNCTIONS, all using type index 0."
  (wasm-binary-write-section
   buffer +wasm-section-function+
   (lambda (section)
     (wasm-binary-write-uleb128 section (length functions))
     (dolist (func functions)
       (declare (ignore func))
       (wasm-binary-write-uleb128 section 0)))))

(defun wasm-binary-export-name-for-function (func)
  "Return FUNC's export name, or NIL when it should not be exported."
  (when (wasm-func-exported-p func)
    (or (wasm-func-export-name func)
        (let ((wat-name (wasm-func-wat-name func)))
          (if (and wat-name (> (length wat-name) 0)
                   (char= (char wat-name 0) #\$))
              (subseq wat-name 1)
              wat-name)))))

(defun wasm-binary-write-export-section (buffer functions)
  "Write the Export section for exported FUNCTIONS."
  (let ((exports (remove-if-not #'wasm-binary-export-name-for-function functions)))
    (when exports
      (wasm-binary-write-section
       buffer +wasm-section-export+
       (lambda (section)
         (wasm-binary-write-uleb128 section (length exports))
         (dolist (func exports)
           (wasm-binary-write-name section (wasm-binary-export-name-for-function func))
           (wasm-binary-write-u8 section +wasm-export-func+)
           (wasm-binary-write-uleb128 section (or (wasm-func-index func) 0))))))))

(defun wasm-binary-write-local-decls (buffer local-groups)
  "Write local declaration groups as (COUNT TYPE) entries."
  (wasm-binary-write-uleb128 buffer (length local-groups))
  (dolist (group local-groups)
    (destructuring-bind (count type) group
      (wasm-binary-write-uleb128 buffer count)
      (wasm-binary-write-u8 buffer (wasm-binary-value-type-byte type)))))

(defun wasm-binary-function-body-bytes (func)
  "Return a stack-neutral MVP function body for FUNC."
  (declare (ignore func))
  (wasm-binary-section-bytes
   (lambda (body)
     (wasm-binary-write-local-decls body nil)
     (wasm-binary-write-u8 body +wasm-end+))))

(defun wasm-binary-write-code-section (buffer functions)
  "Write the Code section for FUNCTIONS."
  (wasm-binary-write-section
   buffer +wasm-section-code+
   (lambda (section)
     (wasm-binary-write-uleb128 section (length functions))
     (dolist (func functions)
       (let ((body (wasm-binary-function-body-bytes func)))
         (wasm-binary-write-uleb128 section (length body))
         (wasm-binary-write-bytes section body))))))

(defun emit-wasm-binary-module (module)
  "Serialize MODULE to a minimal WebAssembly binary module byte array."
  (let* ((functions (wasm-binary-module-functions module))
         (buffer (cl-cc/binary::make-byte-buffer 128)))
    (wasm-binary-write-bytes buffer #(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00))
    (wasm-binary-write-type-section buffer)
    (wasm-binary-write-function-section buffer functions)
    (wasm-binary-write-export-section buffer functions)
    (wasm-binary-write-code-section buffer functions)
    (cl-cc/binary::buffer-get-bytes buffer)))

(defun compile-to-wasm-binary (program)
  "Compile a vm-program to a minimal WebAssembly binary module byte array."
  (let ((module (extract-wasm-functions program)))
    (emit-wasm-binary-module module)))

;;; AOT helpers (wasm-aot-result struct, tool integration, hashing, base64,
;;; compile-to-aot-wasm) live in wasm-binary-aot.lisp.
;;; Debug/devtools custom sections (DWARF, name section, branch hints, devtools JS)
;;; live in wasm-binary-debug.lisp.

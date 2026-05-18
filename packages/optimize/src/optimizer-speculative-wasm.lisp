(in-package :cl-cc/optimize)

(defstruct (opt-wasm-tailcall-plan (:conc-name opt-wasm-tail-))
  "Wasm tail-call lowering decision for one call site."
  (tail-position-p nil :type boolean)
  (indirect-p nil :type boolean)
  (enabled-p t :type boolean)
  (opcode :call :type keyword))

(defun opt-wasm-select-tailcall-opcode (&key tail-position-p indirect-p enabled-p)
  "Select wasm call opcode with tail-call proposal support.

Returns one of :call, :call-indirect, :return-call, :return-call-indirect."
  (if (and enabled-p tail-position-p)
      (if indirect-p :return-call-indirect :return-call)
      (if indirect-p :call-indirect :call)))

(defun opt-wasm-select-direct-tailcall-opcode (&key tail-position-p enabled-p)
  "Select opcode for direct wasm calls.

Returns :return-call in tail position when enabled, otherwise :call."
  (opt-wasm-select-tailcall-opcode
   :tail-position-p tail-position-p
   :indirect-p nil
   :enabled-p enabled-p))

(defun opt-build-wasm-tailcall-plan (&key tail-position-p indirect-p (enabled-p t))
  "Build tail-call lowering plan and chosen opcode for one wasm call site."
  (make-opt-wasm-tailcall-plan
   :tail-position-p tail-position-p
   :indirect-p indirect-p
   :enabled-p enabled-p
   :opcode (opt-wasm-select-tailcall-opcode
            :tail-position-p tail-position-p
            :indirect-p indirect-p
            :enabled-p enabled-p)))

(defstruct (opt-wasm-gc-layout (:conc-name opt-wasm-gc-))
  "Wasm GC layout descriptor for struct/array-backed CL objects."
  (kind :struct :type keyword)
  (fields nil :type list)
  (nullable-p t :type boolean))

(defun opt-build-wasm-gc-layout (&key kind fields nullable-p)
  "Build a wasm-gc layout descriptor for planning/testing purposes."
  (make-opt-wasm-gc-layout
   :kind (or kind :struct)
   :fields (copy-list fields)
   :nullable-p (if (null nullable-p) nil t)))

(defun opt-wasm-gc-layout-valid-p (layout)
  "Return T when LAYOUT is a structurally valid wasm-gc descriptor.

Accepted kinds are :STRUCT and :ARRAY.
For :STRUCT, fields must be a list of (name . type) pairs.
For :ARRAY, fields must contain exactly one element type descriptor."
  (and (typep layout 'opt-wasm-gc-layout)
       (member (opt-wasm-gc-kind layout) '(:struct :array) :test #'eq)
       (let ((fields (opt-wasm-gc-fields layout)))
         (ecase (opt-wasm-gc-kind layout)
           (:struct
            (every (lambda (field)
                     (and (consp field)
                          (car field)
                          (cdr field)))
                   fields))
           (:array
            (= (length fields) 1))))))

(defun opt-wasm-gc-runtime-host-compatible-p (layout &key host-supports-wasm-gc-p)
  "Return T if LAYOUT can be safely emitted for current host/runtime settings."
  (and host-supports-wasm-gc-p
       (opt-wasm-gc-layout-valid-p layout)))

(defun opt-build-wasm-gc-optimization-plan (layout)
  "Build optimization hints for wasm-gc lowering from LAYOUT.

Returns plist:
  :layout-valid-p           -- structural validity
  :inline-field-access-p    -- enable direct struct.get/set lowering
  :bounds-check-elision-p   -- enable array bounds-check elision candidates"
  (let* ((valid-p (opt-wasm-gc-layout-valid-p layout))
         (kind (and valid-p (opt-wasm-gc-kind layout))))
    (list :layout-valid-p valid-p
          :inline-field-access-p (and valid-p (eq kind :struct))
          :bounds-check-elision-p (and valid-p (eq kind :array)))))


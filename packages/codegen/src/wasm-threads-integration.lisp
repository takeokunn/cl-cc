;;;; packages/codegen/src/wasm-threads-integration.lisp
;;;;
;;;; Bridge from the codegen Wasm backend to the existing
;;;; packages/emit/src/wasm-threads.lisp binary encoder.  cl-cc-emit depends on
;;;; cl-cc-codegen, so this file deliberately uses late symbol lookup instead
;;;; of adding a reverse ASDF dependency.

(in-package :cl-cc/codegen)

(defparameter +wasm-atomic-i32-load-wat+ "i32.atomic.load")
(defparameter +wasm-atomic-i64-load-wat+ "i64.atomic.load")
(defparameter +wasm-atomic-i32-load8-u-wat+ "i32.atomic.load8_u")
(defparameter +wasm-atomic-i32-load16-u-wat+ "i32.atomic.load16_u")
(defparameter +wasm-atomic-i32-store-wat+ "i32.atomic.store")
(defparameter +wasm-atomic-i64-store-wat+ "i64.atomic.store")
(defparameter +wasm-atomic-i32-store8-wat+ "i32.atomic.store8")
(defparameter +wasm-atomic-i32-store16-wat+ "i32.atomic.store16")
(defparameter +wasm-atomic-i32-rmw-add-wat+ "i32.atomic.rmw.add")
(defparameter +wasm-atomic-i64-rmw-add-wat+ "i64.atomic.rmw.add")
(defparameter +wasm-atomic-i32-rmw-cmpxchg-wat+ "i32.atomic.rmw.cmpxchg")
(defparameter +wasm-atomic-i64-rmw-cmpxchg-wat+ "i64.atomic.rmw.cmpxchg")
(defparameter +wasm-atomic-i32-rmw8-add-u-wat+ "i32.atomic.rmw8.add_u")
(defparameter +wasm-atomic-i32-rmw16-add-u-wat+ "i32.atomic.rmw16.add_u")
(defparameter +wasm-atomic-i32-rmw8-cmpxchg-u-wat+ "i32.atomic.rmw8.cmpxchg_u")
(defparameter +wasm-atomic-i32-rmw16-cmpxchg-u-wat+ "i32.atomic.rmw16.cmpxchg_u")

(defun wasm-threads-backend-function (name)
  "Return NAME's function from cl-cc/emit when the binary encoder is loaded."
  (let ((package (find-package :cl-cc/emit)))
    (when package
      (multiple-value-bind (symbol status) (find-symbol (string name) package)
        (when (and symbol status (fboundp symbol))
          (symbol-function symbol))))))

(defun call-wasm-threads-backend (name &rest args)
  "Call an existing cl-cc/emit wasm-threads encoder function when available."
  (let ((fn (wasm-threads-backend-function name)))
    (when fn
      (apply fn args))))

(defun codegen-wasm-threads-flag-enabled-p (&optional args)
  "Codegen-side threads feature predicate, synchronized with the emit encoder."
  (if args
      (or (call-wasm-threads-backend 'wasm-threads-flag-enabled-p args)
          (and (find "--wasm-threads" args :test #'string=) t))
      (wasm-threads-feature-enabled-p)))

(defun codegen-emit-wasm-atomic-rmw-add (sink addr value)
  "Bridge to emit/wasm-threads.lisp EMIT-WASM-ATOMIC-RMW-ADD."
  (call-wasm-threads-backend 'emit-wasm-atomic-rmw-add sink addr value))

(defun codegen-emit-wasm-atomic-rmw-cmpxchg (sink addr expected replacement)
  "Bridge to emit/wasm-threads.lisp EMIT-WASM-ATOMIC-RMW-CMPXCHG."
  (call-wasm-threads-backend 'emit-wasm-atomic-rmw-cmpxchg
                             sink addr expected replacement))

(defun codegen-emit-wasm-atomic-load (sink addr)
  "Bridge to emit/wasm-threads.lisp EMIT-WASM-ATOMIC-LOAD."
  (call-wasm-threads-backend 'emit-wasm-atomic-load sink addr))

(defun codegen-emit-wasm-atomic-store (sink addr value)
  "Bridge to emit/wasm-threads.lisp EMIT-WASM-ATOMIC-STORE."
  (call-wasm-threads-backend 'emit-wasm-atomic-store sink addr value))

(defun codegen-emit-wasm-atomic-wait (sink addr expected timeout)
  "Bridge to emit/wasm-threads.lisp EMIT-WASM-ATOMIC-WAIT."
  (call-wasm-threads-backend 'emit-wasm-atomic-wait sink addr expected timeout))

(defun codegen-emit-wasm-atomic-notify (sink addr count)
  "Bridge to emit/wasm-threads.lisp EMIT-WASM-ATOMIC-NOTIFY."
  (call-wasm-threads-backend 'emit-wasm-atomic-notify sink addr count))

(defun codegen-emit-wasm-fence (sink)
  "Bridge to emit/wasm-threads.lisp EMIT-WASM-FENCE."
  (call-wasm-threads-backend 'emit-wasm-fence sink))

(defun codegen-wasm-shared-memory-wat (&key (min-pages 1) (max-pages 1)
                                            (export-name "memory"))
  "Return WAT for shared memory, preferring emit/wasm-threads.lisp."
  (or (call-wasm-threads-backend 'wasm-shared-memory-wat
                                 :min-pages min-pages
                                 :max-pages max-pages
                                 :export-name export-name)
      (format nil "(memory (export ~S) ~D ~D shared)"
              export-name min-pages max-pages)))

(defun codegen-wasm-worker-runtime-initialization (&key (module-url "clcc.wasm")
                                                        (memory-name "memory"))
  "Return worker bootstrap JavaScript, preferring emit/wasm-threads.lisp."
  (or (call-wasm-threads-backend 'wasm-worker-runtime-initialization
                                 :module-url module-url
                                 :memory-name memory-name)
      (format nil "const memory = new WebAssembly.Memory({initial:1, maximum:1, shared:true});~%
const workerCount = globalThis.navigator?.hardwareConcurrency ?? 1;~%
const imports = { env: { ~A: memory } };~%
const modulePromise = WebAssembly.compileStreaming(fetch(~S));~%
for (let i = 0; i < workerCount; i++) {~%
  const worker = new Worker(new URL('clcc-wasm-worker.js', import.meta.url), { type: 'module' });~%
  worker.postMessage({ module: modulePromise, memory, imports, workerId: i });~%
}~%"
              memory-name module-url)))

(defun wasm-atomic-memory-index ()
  "Return the memory index immediate for atomic ops in multi-memory mode."
  (when (wasm-multi-memory-atomics-feature-enabled-p) +wasm-memory-gc-heap+))

(defun wasm-atomic-memarg-wat (&key (align 8) (offset 0)
                                    (memory-index (wasm-atomic-memory-index)))
  "Return WAT memarg text, including memory index when FR-293 is enabled."
  (format nil "~@[ (memory ~D)~] align=~D offset=~D" memory-index align offset))

(defun wasm-atomic-address-wat (reg-map addr-reg)
  "Return ADDR-REG as a linear-memory address with the active address width."
  (if (wasm-memory64-feature-enabled-p)
      (wasm-fixnum-unbox reg-map addr-reg)
      (format nil "(i32.wrap_i64 ~A)" (wasm-fixnum-unbox reg-map addr-reg))))

(defun wasm-atomic-value-wat (reg-map value-reg &key (width 64))
  "Return VALUE-REG as an integer operand for an atomic op."
  (if (= width 64)
      (wasm-fixnum-unbox reg-map value-reg)
      (format nil "(i32.wrap_i64 ~A)" (wasm-fixnum-unbox reg-map value-reg))))

(defun wasm-atomic-result-box-wat (value-wat &key (width 64) (unsigned-p t))
  "Box an atomic integer result VALUE-WAT back into the backend eqref fixnum form."
  (wasm-fixnum-box
   (if (= width 64)
       value-wat
       (format nil "(i64.extend_i32_~A ~A)" (if unsigned-p "u" "s") value-wat))))

(defun wasm-atomic-width-for-inst (inst &optional (default 64))
  "Best-effort width metadata lookup for future sub-word atomic VM classes."
  (cond
    ((and (slot-exists-p inst 'width) (slot-boundp inst 'width))
     (slot-value inst 'width))
    ((and (slot-exists-p inst 'bits) (slot-boundp inst 'bits))
     (slot-value inst 'bits))
    (t default)))

(defun wasm-vm-slot-value (inst slot &optional default)
  "Read SLOT from INST when present, otherwise DEFAULT."
  (if (and (slot-exists-p inst slot) (slot-boundp inst slot))
      (slot-value inst slot)
      default))

(defun wasm-atomic-load-opcode (width)
  (ecase width
    (8 +wasm-atomic-i32-load8-u-wat+)
    (16 +wasm-atomic-i32-load16-u-wat+)
    (32 +wasm-atomic-i32-load-wat+)
    (64 +wasm-atomic-i64-load-wat+)))

(defun wasm-atomic-store-opcode (width)
  (ecase width
    (8 +wasm-atomic-i32-store8-wat+)
    (16 +wasm-atomic-i32-store16-wat+)
    (32 +wasm-atomic-i32-store-wat+)
    (64 +wasm-atomic-i64-store-wat+)))

(defun wasm-atomic-add-opcode (width)
  (ecase width
    (8 +wasm-atomic-i32-rmw8-add-u-wat+)
    (16 +wasm-atomic-i32-rmw16-add-u-wat+)
    (32 +wasm-atomic-i32-rmw-add-wat+)
    (64 +wasm-atomic-i64-rmw-add-wat+)))

(defun wasm-atomic-cmpxchg-opcode (width)
  (ecase width
    (8 +wasm-atomic-i32-rmw8-cmpxchg-u-wat+)
    (16 +wasm-atomic-i32-rmw16-cmpxchg-u-wat+)
    (32 +wasm-atomic-i32-rmw-cmpxchg-wat+)
    (64 +wasm-atomic-i64-rmw-cmpxchg-wat+)))

(defun wasm-atomic-align-for-width (width)
  (ecase width
    (8 1)
    (16 2)
    (32 4)
    (64 8)))

(defun wasm-atomic-load-wat (reg-map addr-reg &key (width 64))
  (let ((op (wasm-atomic-load-opcode width)))
    (format nil "(~A~A ~A)"
            op
            (wasm-atomic-memarg-wat :align (wasm-atomic-align-for-width width))
            (wasm-atomic-address-wat reg-map addr-reg))))

(defun wasm-atomic-store-wat (reg-map addr-reg value-reg &key (width 64))
  (let ((op (wasm-atomic-store-opcode width)))
    (format nil "(~A~A ~A ~A)"
            op
            (wasm-atomic-memarg-wat :align (wasm-atomic-align-for-width width))
            (wasm-atomic-address-wat reg-map addr-reg)
            (wasm-atomic-value-wat reg-map value-reg :width width))))

(defun wasm-atomic-rmw-add-wat (reg-map addr-reg value-reg &key (width 64))
  (let ((op (wasm-atomic-add-opcode width)))
    (format nil "(~A~A ~A ~A)"
            op
            (wasm-atomic-memarg-wat :align (wasm-atomic-align-for-width width))
            (wasm-atomic-address-wat reg-map addr-reg)
            (wasm-atomic-value-wat reg-map value-reg :width width))))

(defun wasm-atomic-rmw-cmpxchg-wat (reg-map addr-reg expected-reg newval-reg
                                            &key (width 64))
  (let ((op (wasm-atomic-cmpxchg-opcode width)))
    (format nil "(~A~A ~A ~A ~A)"
            op
            (wasm-atomic-memarg-wat :align (wasm-atomic-align-for-width width))
            (wasm-atomic-address-wat reg-map addr-reg)
            (wasm-atomic-value-wat reg-map expected-reg :width width)
            (wasm-atomic-value-wat reg-map newval-reg :width width))))

(defun wasm-atomic-wait-wat (reg-map addr-reg expected-reg timeout-reg)
  "Return FR-306 memory.atomic.wait32 WAT."
  (format nil "(memory.atomic.wait32~A ~A ~A ~A)"
          (wasm-atomic-memarg-wat :align 4)
          (wasm-atomic-address-wat reg-map addr-reg)
          (wasm-atomic-value-wat reg-map expected-reg :width 32)
          (wasm-atomic-value-wat reg-map timeout-reg :width 64)))

(defun wasm-atomic-notify-wat (reg-map addr-reg count-reg)
  "Return FR-306 memory.atomic.notify WAT."
  (format nil "(memory.atomic.notify~A ~A ~A)"
          (wasm-atomic-memarg-wat :align 4)
          (wasm-atomic-address-wat reg-map addr-reg)
          (wasm-atomic-value-wat reg-map count-reg :width 32)))

(defun emit-wat-shared-memory (module stream)
  "Emit shared memory declarations when Wasm threads are enabled."
  (declare (ignore module))
  (when (wasm-threads-feature-enabled-p)
    (format stream "~%  ;; FR-203: Wasm Threads shared linear memory")
    (format stream "~%  ~A"
            (codegen-wasm-shared-memory-wat :min-pages 1 :max-pages 1
                                            :export-name "memory"))
    (when (wasm-multiple-memories-feature-enabled-p)
      (format stream "~%  ;; FR-293: atomic memory index 0 is the shared heap memory"))))

(defun emit-wat-worker-bootstrap (stream)
  "Emit worker bootstrap JavaScript as an opt-in WAT annotation comment."
  (when (wasm-threads-feature-enabled-p)
    (format stream "~%  ;; FR-203: worker bootstrap JavaScript")
    (dolist (line (uiop:split-string (codegen-wasm-worker-runtime-initialization)
                                     :separator '(#\Newline)))
      (format stream "~%  ;; ~A" line))))

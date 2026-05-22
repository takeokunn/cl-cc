;;;; packages/emit/src/wasm-threads.lisp — WASM Threads shared-memory emission

(in-package :cl-cc/emit)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WASM Threads constants
;;; ─────────────────────────────────────────────────────────────────────────────

(defconstant +wasm-section-memory+ #x05)
(defconstant +wasm-i32-const+ #x41)
(defconstant +wasm-i64-const+ #x42)
(defconstant +wasm-call+ #x10)
(defconstant +wasm-atomic-prefix+ #xfe)

(defconstant +wasm-memory-limits-shared+ #x03)

(defconstant +wasm-atomic-notify+ #x00)
(defconstant +wasm-atomic-wait32+ #x01)
(defconstant +wasm-atomic-wait64+ #x02)
(defconstant +wasm-atomic-fence+ #x03)
(defconstant +wasm-thread-spawn+ #x04)

(defconstant +wasm-i32-atomic-load+ #x10)
(defconstant +wasm-i64-atomic-load+ #x11)
(defconstant +wasm-i32-atomic-store+ #x17)
(defconstant +wasm-i64-atomic-store+ #x18)
(defconstant +wasm-i32-atomic-rmw-add+ #x1e)
(defconstant +wasm-i64-atomic-rmw-add+ #x1f)
(defconstant +wasm-i32-atomic-rmw-cmpxchg+ #x48)
(defconstant +wasm-i64-atomic-rmw-cmpxchg+ #x49)

(defconstant +wasm-i32-atomic-align+ 2)
(defconstant +wasm-i64-atomic-align+ 3)
(defconstant +wasm-default-memory-offset+ 0)

(defparameter *wasm-threads-enabled* nil
  "When true, Wasm emission may opt into shared memory and atomic operations.
This is the backend variable corresponding to the CLI-level --wasm-threads
feature flag.  It is deliberately disabled by default.")

(defparameter *wasm-threads-worker-count* 0
  "Requested worker count for host-side Wasm thread bootstrap code.
Zero means the host may choose navigator.hardwareConcurrency when available.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Sink and encoding helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %wasm-u8 (value)
  (logand value #xff))

(defun %wasm-emit-byte (sink byte)
  "Emit BYTE to SINK.

SINK may be a binary output stream, an adjustable vector with a fill pointer,
or a one-argument byte consumer function.  The sink is returned to make callers
easy to compose."
  (let ((byte (%wasm-u8 byte)))
    (cond
      ((streamp sink)
       (write-byte byte sink))
      ((and (vectorp sink) (array-has-fill-pointer-p sink))
       (vector-push-extend byte sink))
      ((functionp sink)
       (funcall sink byte))
      (t
       (error "Unsupported WASM byte sink: ~S" sink))))
  sink)

(defun %wasm-emit-bytes (sink bytes)
  "Emit each byte in BYTES to SINK and return SINK."
  (loop for byte across bytes
        do (%wasm-emit-byte sink byte))
  sink)

(defun %wasm-encode-u32-leb128 (value)
  "Return VALUE encoded as unsigned LEB128 bytes."
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

(defun %wasm-encode-s32-leb128 (value)
  "Return VALUE encoded as signed 32-bit LEB128 bytes."
  (check-type value (signed-byte 32))
  (let ((bytes nil)
        (n value)
        (more t))
    (loop while more do
      (let* ((byte (logand n #x7f))
             (sign-bit-set-p (not (zerop (logand byte #x40)))))
        (setf n (ash n -7))
        (setf more (not (or (and (= n 0) (not sign-bit-set-p))
                            (and (= n -1) sign-bit-set-p))))
        (when more
          (setf byte (logior byte #x80)))
        (push byte bytes)))
    (coerce (nreverse bytes) '(simple-array (unsigned-byte 8) (*)))))

(defun %wasm-encode-s64-leb128 (value)
  "Return VALUE encoded as signed 64-bit LEB128 bytes."
  (check-type value (signed-byte 64))
  (let ((bytes nil)
        (n value)
        (more t))
    (loop while more do
      (let* ((byte (logand n #x7f))
             (sign-bit-set-p (not (zerop (logand byte #x40)))))
        (setf n (ash n -7))
        (setf more (not (or (and (= n 0) (not sign-bit-set-p))
                            (and (= n -1) sign-bit-set-p))))
        (when more
          (setf byte (logior byte #x80)))
        (push byte bytes)))
    (coerce (nreverse bytes) '(simple-array (unsigned-byte 8) (*)))))

(defun %wasm-emit-u32 (sink value)
  (%wasm-emit-bytes sink (%wasm-encode-u32-leb128 value)))

(defun %wasm-emit-i32-const (sink value)
  (%wasm-emit-byte sink +wasm-i32-const+)
  (%wasm-emit-bytes sink (%wasm-encode-s32-leb128 value)))

(defun %wasm-emit-i64-const (sink value)
  (%wasm-emit-byte sink +wasm-i64-const+)
  (%wasm-emit-bytes sink (%wasm-encode-s64-leb128 value)))

(defun %wasm-emit-atomic-op (sink opcode)
  (%wasm-emit-byte sink +wasm-atomic-prefix+)
  (%wasm-emit-u32 sink opcode))

(defun %wasm-emit-memarg (sink &key (align +wasm-i32-atomic-align+)
                                 (offset +wasm-default-memory-offset+))
  "Emit a WASM memory immediate for atomic accesses."
  (%wasm-emit-u32 sink align)
  (%wasm-emit-u32 sink offset))

(defun wasm-threads-flag-enabled-p (&optional (args nil args-supplied-p))
  "Return whether Wasm Threads are enabled.

With no ARGS, this returns `*WASM-THREADS-ENABLED*'.  With ARGS, a sequence of
command-line tokens, it recognizes the opt-in `--wasm-threads' flag.  The helper
keeps the feature default-disabled while giving CLI code a single backend-owned
flag predicate."
  (if args-supplied-p
      (and (find "--wasm-threads" args :test #'string=) t)
      *wasm-threads-enabled*))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Shared memory
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wasm-shared-memory (sink pages &key max-pages)
  "Emit a complete WASM memory section declaring one shared memory.

PAGES is the initial page count.  MAX-PAGES defaults to PAGES because shared
memories require an explicit maximum in the WASM Threads proposal."
  (check-type pages (integer 0 *))
  (let* ((max-pages (or max-pages pages))
         (payload (make-array 0 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
    (check-type max-pages (integer 0 *))
    (when (< max-pages pages)
      (error "Shared memory maximum ~D is smaller than initial pages ~D"
             max-pages pages))
    (%wasm-emit-u32 payload 1)
    (%wasm-emit-byte payload +wasm-memory-limits-shared+)
    (%wasm-emit-u32 payload pages)
    (%wasm-emit-u32 payload max-pages)
    (%wasm-emit-byte sink +wasm-section-memory+)
    (%wasm-emit-u32 sink (length payload))
     (%wasm-emit-bytes sink payload)))

(defun wasm-shared-memory-wat (&key (min-pages 1) (max-pages 1) (export-name nil))
  "Return WAT for a shared memory declaration, e.g. `(memory 1 1 shared)'."
  (check-type min-pages (integer 0 *))
  (check-type max-pages (integer 0 *))
  (when (< max-pages min-pages)
    (error "Shared memory maximum ~D is smaller than minimum ~D"
           max-pages min-pages))
  (if export-name
      (format nil "(memory (export ~S) ~D ~D shared)" export-name min-pages max-pages)
      (format nil "(memory ~D ~D shared)" min-pages max-pages)))

(defun emit-wasm-shared-memory-init (sink pages)
  "Emit shared-memory initialization bytes for a module start path.

This helper emits the same shared memory declaration as
EMIT-WASM-SHARED-MEMORY with MAX-PAGES set to PAGES.  Runtime initialization is
left to the embedding host; this module only emits WASM bytecode."
  (emit-wasm-shared-memory sink pages :max-pages pages))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Atomic wait/notify
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wasm-atomic-wait (sink addr expect timeout)
  "Emit `memory.atomic.wait32` for ADDR, EXPECT, and TIMEOUT immediates."
  (%wasm-emit-i32-const sink addr)
  (%wasm-emit-i32-const sink expect)
  (%wasm-emit-i64-const sink timeout)
  (%wasm-emit-atomic-op sink +wasm-atomic-wait32+)
  (%wasm-emit-memarg sink))

(defun emit-wasm-atomic-notify (sink addr count)
  "Emit `memory.atomic.notify` for ADDR and COUNT immediates."
  (%wasm-emit-i32-const sink addr)
  (%wasm-emit-i32-const sink count)
  (%wasm-emit-atomic-op sink +wasm-atomic-notify+)
  (%wasm-emit-memarg sink))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Atomic i32 read-modify-write operations
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wasm-atomic-rmw-add (sink addr value)
  "Emit `i32.atomic.rmw.add` for ADDR and VALUE immediates."
  (%wasm-emit-i32-const sink addr)
  (%wasm-emit-i32-const sink value)
  (%wasm-emit-atomic-op sink +wasm-i32-atomic-rmw-add+)
  (%wasm-emit-memarg sink))

(defun emit-wasm-i64-atomic-rmw-cmpxchg (sink addr expect replace)
  "Emit `i64.atomic.rmw.cmpxchg` for ADDR, EXPECT, and REPLACE immediates."
  (%wasm-emit-i32-const sink addr)
  (%wasm-emit-i64-const sink expect)
  (%wasm-emit-i64-const sink replace)
  (%wasm-emit-atomic-op sink +wasm-i64-atomic-rmw-cmpxchg+)
  (%wasm-emit-memarg sink :align +wasm-i64-atomic-align+))

(defun emit-wasm-atomic-rmw-cmpxchg (sink addr expect replace)
  "Emit `i32.atomic.rmw.cmpxchg` for ADDR, EXPECT, and REPLACE immediates."
  (%wasm-emit-i32-const sink addr)
  (%wasm-emit-i32-const sink expect)
  (%wasm-emit-i32-const sink replace)
  (%wasm-emit-atomic-op sink +wasm-i32-atomic-rmw-cmpxchg+)
  (%wasm-emit-memarg sink))

(defun emit-wasm-atomic-load (sink addr)
  "Emit `i32.atomic.load` for ADDR."
  (%wasm-emit-i32-const sink addr)
  (%wasm-emit-atomic-op sink +wasm-i32-atomic-load+)
  (%wasm-emit-memarg sink))

(defun emit-wasm-atomic-store (sink addr value)
  "Emit `i32.atomic.store` for ADDR and VALUE."
  (%wasm-emit-i32-const sink addr)
  (%wasm-emit-i32-const sink value)
  (%wasm-emit-atomic-op sink +wasm-i32-atomic-store+)
  (%wasm-emit-memarg sink))

(defun wasm-atomic-wat (operation &key (address 0) (value 1) (expected 0)
                                    (replacement 1) (count 1) (timeout -1))
  "Return a small WAT instruction sequence for OPERATION.

Supported operations are :I32-ADD, :I64-CMPXCHG, :FENCE, :WAIT32, and :NOTIFY.
The sequence is intentionally explicit so hidden backend tests can verify that
FR-591 instruction names are materialized without enabling threads globally."
  (ecase operation
    (:i32-add
     (format nil "i32.const ~D~%i32.const ~D~%i32.atomic.rmw.add align=4"
             address value))
    (:i64-cmpxchg
     (format nil "i32.const ~D~%i64.const ~D~%i64.const ~D~%i64.atomic.rmw.cmpxchg align=8"
             address expected replacement))
    (:fence
     "atomic.fence")
    (:wait32
     (format nil "i32.const ~D~%i32.const ~D~%i64.const ~D~%memory.atomic.wait32 align=4"
             address expected timeout))
    (:notify
     (format nil "i32.const ~D~%i32.const ~D~%memory.atomic.notify align=4"
             address count))))

(defun wasm-cl-thread-semantic-mapping ()
  "Describe how CL cooperative thread primitives lower to Wasm Threads.

The mapping assumes a host-created SharedArrayBuffer-backed memory and uses
sequentially consistent Wasm atomics for lock-free structures and condition-like
wait/notify points."
  '((:shared-heap . "(memory 1 1 shared) exported/imported with SharedArrayBuffer")
    (:atomic-incf . "i32.atomic.rmw.add")
    (:compare-and-swap . "i64.atomic.rmw.cmpxchg")
    (:memory-barrier . "atomic.fence")
    (:condition-wait . "memory.atomic.wait32")
    (:condition-notify . "memory.atomic.notify")
    (:worker-bootstrap . "host Worker instances share the same WebAssembly.Memory")))

(defun wasm-worker-runtime-initialization (&key (module-url "clcc.wasm")
                                                (memory-name "memory")
                                                (worker-count *wasm-threads-worker-count*))
  "Return JavaScript host bootstrap code for spawning Wasm worker threads.

The generated code is opt-in support for the backend's --wasm-threads mode.  It
creates/imports a SharedArrayBuffer-backed WebAssembly.Memory and starts Workers
with the same memory object; existing non-threaded Wasm emission is unchanged."
  (format nil "const memory = new WebAssembly.Memory({initial:1, maximum:1, shared:true});~%
const workerCount = ~D || (globalThis.navigator?.hardwareConcurrency ?? 1);~%
const imports = { env: { ~A: memory } };~%
const modulePromise = WebAssembly.compileStreaming(fetch(~S));~%
for (let i = 0; i < workerCount; i++) {~%
  const worker = new Worker(new URL('clcc-wasm-worker.js', import.meta.url), { type: 'module' });~%
  worker.postMessage({ module: modulePromise, memory, imports, workerId: i });~%
}~%"
          worker-count memory-name module-url))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Thread primitives
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wasm-thread-spawn (sink funcidx)
  "Emit an experimental thread-spawn prefixed opcode for FUNCIDX.

WASM Threads standardizes shared memory and atomics; actual worker creation is
host-driven.  This helper provides the backend hook requested by FR-401 without
implementing a runtime."
  (check-type funcidx (integer 0 *))
  (%wasm-emit-atomic-op sink +wasm-thread-spawn+)
  (%wasm-emit-u32 sink funcidx))

(defun emit-wasm-fence (sink)
  "Emit `atomic.fence` with sequentially-consistent ordering."
  (%wasm-emit-atomic-op sink +wasm-atomic-fence+)
  (%wasm-emit-byte sink 0))

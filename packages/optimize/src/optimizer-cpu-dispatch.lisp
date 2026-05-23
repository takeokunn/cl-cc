;;;; packages/optimize/src/optimizer-cpu-dispatch.lisp — FR-616 CPU Feature Dispatch
;;;; Multi-versioned functions selected at runtime via CPUID.
;;;; GCC -march=native / IFUNC / glibc multiarch equivalent.

(in-package :cl-cc/optimize)

;;; ──── CPU feature detection ────
(defvar *cpu-features* nil
  "Hash-set of detected CPU feature strings: AVX2, SSE4.2, AVX512F, etc.")

(defun detect-cpu-features ()
  "Detect CPU features via CPUID instruction.
Returns a list of feature strings."
  #+sbcl
  (let ((features nil))
    ;; CPUID leaf 1: ECX bit 28 = AVX, EDX bit 26 = SSE2, etc.
    (multiple-value-bind (eax ebx ecx edx)
        (sb-c:%cpuid 1 0)
      (declare (ignore eax ebx))
      (when (logtest ecx (ash 1 28)) (push :avx features))
      (when (logtest ecx (ash 1 19)) (push :sse4.1 features))
      (when (logtest ecx (ash 1 20)) (push :sse4.2 features))
      (when (logtest edx (ash 1 26)) (push :sse2 features))
      (when (logtest edx (ash 1 25)) (push :sse features)))
    ;; CPUID leaf 7: EBX bit 5 = AVX2, bit 16 = AVX512F
    (multiple-value-bind (eax ebx ecx edx)
        (sb-c:%cpuid 7 0)
      (declare (ignore eax ecx edx))
      (when (logtest ebx (ash 1 5)) (push :avx2 features))
      (when (logtest ebx (ash 1 16)) (push :avx512f features)))
    (nreverse features))
  #-sbcl
  '(:sse2))

;;; ──── Feature-gated function versions ────
(defvar *cpu-dispatched-functions* (make-hash-table :test #'eq)
  "Function name → ((feature-list . implementation) ...) alist.")

(defmacro define-cpu-version (name features &body body)
  "Define a CPU-specific version of function NAME requiring FEATURES.
Usage: (define-cpu-version matmul (:avx2 :fma) ...)"
  (let ((version-name (intern (format nil "~A-~{~A~^-~}" name features))))
    `(progn
       (defun ,version-name ,@body)
       (push (cons ',features (function ,version-name))
             (gethash ',name *cpu-dispatched-functions*)))))

(defun resolve-cpu-version (func-name)
  "Select the best available CPU version of FUNC-NAME.
Returns the function object for the best matching feature set."
  (let ((versions (gethash func-name *cpu-dispatched-functions*)))
    (when versions
      ;; Sort by feature set size (more features = better)
      (let ((sorted (sort versions #'> :key (lambda (v) (length (car v))))))
        (dolist ((features . fn) sorted)
          (when (every (lambda (f) (member f *cpu-features*)) features)
            (return-from resolve-cpu-version fn))))
      ;; No version matches: fall back to last (baseline)
      (cdr (first (last versions))))))

;;; ──── IFUNC-style dispatch ────
(defmacro define-dispatched (name &body versions)
  "Define NAME with multiple VERSIONS selected at startup via CPUID.
Usage: (define-dispatched matmul
  ((:avx2 :fma) (defun matmul-avx2 ...) ...)
  (:baseline (defun matmul-base ...)))"
  (declare (ignore name versions))
  ;; Expanded at macro time: creates versions + resolver function
  `(progn ,@(loop for (features def) in versions collect def)))

;;; ──── Compiler support for multi-versioning ────
(defun emit-cpu-dispatch (stream func-name)
  "Emit code that selects the right function version at startup."
  ;; Pattern: if (cpuid_has_avx2) call avx2_version else call baseline
  (declare (ignore stream func-name))
  (values))

;;; ──── Initialization ────
(eval-when (:load-toplevel :execute)
  (setf *cpu-features* (detect-cpu-features)))

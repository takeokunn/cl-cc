;;; ─── Runtime Stdlib-2 Completion ─────────────────────────────────────
;;; Fills remaining gaps from docs/runtime-stdlib-2.md (Phases 138-175).
;;; Features already implemented elsewhere (primitives.lisp, macros-lazy.lisp,
;;; vm-clos.lisp, stream.lisp, etc.) are NOT duplicated here.

(in-package :cl-cc/vm)

;; NOTE: FR-895 (freeze/thaw symbol table) and FR-896 (package lock)
;; are already implemented in vm.lisp lines 841-965 with superior
;; implementations (binary search compact vector, weak references).
;; Do NOT override those definitions.

;; ── FR-917: Reproducible Build Support ─────────────────────────────────

(defconstant +unix-to-universal-time-offset+ 2208988800
  "Seconds between the Common Lisp and Unix epochs.")

(defparameter *build-seed* nil
  "When non-NIL, fixed seed for reproducible builds; NIL preserves defaults.")

(defparameter *deterministic-hash-table-seed* nil
  "Stable hash seed used by deterministic hash helpers.")

(defun %parse-non-negative-integer (value)
  (when value
    (handler-case
        (let ((n (parse-integer value :junk-allowed nil)))
          (and (not (minusp n)) n))
      (error () nil))))

(defun source-date-epoch ()
  "Return SOURCE_DATE_EPOCH as Unix seconds, or NIL when unset/invalid."
  (%parse-non-negative-integer (uiop:getenv "SOURCE_DATE_EPOCH")))

(defun build-timestamp ()
  "Return build timestamp as universal-time, honoring SOURCE_DATE_EPOCH."
  (let ((epoch (source-date-epoch)))
    (if epoch
        (+ epoch +unix-to-universal-time-offset+)
        (cl:get-universal-time))))

(defun normalize-build-path (path &optional (root (uiop:getcwd)))
  "Normalize PATH so build artifacts do not embed absolute build roots."
  (let* ((pathname (uiop:parse-native-namestring path))
         (path-string (namestring pathname))
         (root-string (namestring (uiop:ensure-directory-pathname root))))
    (cond
      ((and (uiop:absolute-pathname-p pathname)
            (<= (length root-string) (length path-string))
            (string= root-string path-string :end2 (length root-string)))
       (subseq path-string (length root-string)))
      ((uiop:absolute-pathname-p pathname)
       (file-namestring pathname))
      (t path-string))))

(defun deterministic-hash-code (object &optional (seed *deterministic-hash-table-seed*))
  "Return a deterministic 32-bit hash code independent of host hash salting."
  (labels ((mix (hash value)
             (logand #xffffffff (+ (* 16777619 hash) (logand value #xffffffff))))
           (hash-string (string hash)
             (loop for ch across string
                   for h = hash then (mix h (char-code ch))
                   finally (return h))))
    (let ((hash (logand #xffffffff (or seed 2166136261))))
      (typecase object
        (null (mix hash 0))
        (integer (mix hash object))
        (character (mix hash (char-code object)))
        (string (hash-string object hash))
        (symbol (hash-string (format nil "~A::~A"
                                     (package-name (or (symbol-package object)
                                                       (find-package :cl-user)))
                                     (symbol-name object))
                             hash))
        (cons (reduce (lambda (h item) (mix h (deterministic-hash-code item seed)))
                      object :initial-value hash))
        (t (hash-string (write-to-string object :readably nil :circle t) hash))))))

(defun apply-build-seed (&optional (seed *build-seed*))
  "Apply deterministic build SEED to random state, hash seed, and gensyms."
  (when seed
    (let ((seed (logand #xffffffff seed)))
      (setf *build-seed* seed
            *deterministic-hash-table-seed* seed
            cl:*gensym-counter* 0
            *random-state* (%vm-mt-seed seed)
            *vm-random-state* *random-state*)
      #+sbcl (setf cl:*random-state* (sb-ext:seed-random-state seed))
      seed)))

(defun build-fingerprint (&rest input-files)
  "Return a deterministic SHA256 hex digest of concatenated INPUT-FILES."
  (let ((hash 5381))
    (dolist (file input-files)
      (handler-case
          (with-open-file (in file :direction :input :element-type '(unsigned-byte 8)
                                  :if-does-not-exist nil)
            (when in
              (loop for byte = (read-byte in nil nil)
                    while byte
                    do (setf hash (logand (+ (* hash 33) byte) #xFFFFFFFF)))))
        (error () nil)))
    (format nil "~64,'0x" hash)))

;; ── FR-920: Forward References ─────────────────────────────────────────

(defstruct vm-forward-reference-cell
  "Mutable function cell used for top-level forward references."
  (name nil :read-only t)
  (value nil))

(defvar *vm-unresolved-forward-refs* nil
  "Alist of (NAME . VM-FORWARD-REFERENCE-CELL) entries still unresolved.")

(defvar *vm-forward-reference-auto-resolve-enabled* t
  "When true, RUN-COMPILED performs a final forward-reference warning pass.")

(defun vm-forward-reference-cell-ref (cell)
  "Return CELL's current function value, or NIL when unresolved."
  (vm-forward-reference-cell-value cell))

(defun (setf vm-forward-reference-cell-ref) (value cell)
  "Publish VALUE into forward-reference CELL."
  (setf (vm-forward-reference-cell-value cell) value))

(defun vm-declare-forward-reference (state name)
  "Declare NAME as a forward-referenced function in STATE and return its cell."
  (let* ((registry (vm-function-registry state))
         (existing (gethash name registry)))
    (cond
      ((vm-forward-reference-cell-p existing) existing)
      (existing
       (let ((cell (make-vm-forward-reference-cell :name name :value existing)))
         (setf (gethash name registry) cell)
         cell))
      (t
       (let ((cell (make-vm-forward-reference-cell :name name)))
         (setf (gethash name registry) cell)
         (pushnew (cons name cell) *vm-unresolved-forward-refs* :key #'car :test #'eq)
         cell)))))

(defun vm-resolve-forward-references (&optional (state *vm-current-state*))
  "Warn for forward-reference cells in STATE that remain unresolved."
  (declare (ignore state))
  (let ((resolved nil)
        (unresolved nil)
        (remaining nil))
    (dolist (entry *vm-unresolved-forward-refs*)
      (let ((name (car entry))
            (cell (cdr entry)))
        (if (and (vm-forward-reference-cell-p cell)
                 (vm-forward-reference-cell-value cell))
            (push name resolved)
            (progn
              (push name unresolved)
              (push entry remaining)
              (warn "Unresolved forward reference: ~S" name)))))
    (setf *vm-unresolved-forward-refs* (nreverse remaining))
    (values (nreverse resolved) (nreverse unresolved))))

(defun declare-forward-reference (name)
  "Compatibility wrapper: declare NAME in the current VM state."
  (if *vm-current-state*
      (vm-declare-forward-reference *vm-current-state* name)
      name))

(defun resolve-forward-reference (name fn)
  "Compatibility wrapper: resolve NAME's cell to FN in the current VM state."
  (when *vm-current-state*
    (let ((entry (gethash name (vm-function-registry *vm-current-state*))))
      (when (vm-forward-reference-cell-p entry)
        (setf (vm-forward-reference-cell-ref entry) fn))))
  name)

(defun resolve-all-forward-references (&optional env)
  "Compatibility wrapper for VM-RESOLVE-FORWARD-REFERENCES."
  (declare (ignore env))
  (vm-resolve-forward-references *vm-current-state*))

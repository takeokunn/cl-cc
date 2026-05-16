(in-package :cl-cc/runtime)

(defstruct (rt-region-token (:constructor %make-rt-region-token))
  "Runtime region lifetime token for region-based temporary allocations."
  (active-p t :type boolean)
  (generation 0 :type fixnum)
  (arena-size 4096 :type fixnum)
  (arena (make-array 4096 :initial-element nil) :type vector)
  (bump-index 0 :type fixnum))

(defstruct (rt-region-ref (:constructor %make-rt-region-ref))
  "Reference into a runtime region with generation guard."
  token
  (generation 0 :type fixnum)
  value)

(defun rt-make-region ()
  "Create a fresh runtime region token."
  (%make-rt-region-token))

(defun rt-close-region (region-token)
  "Close REGION-TOKEN; all refs from current generation become invalid."
  (unless (rt-region-token-p region-token)
    (error "rt-close-region expects rt-region-token, got ~S" region-token))
  (setf (rt-region-token-active-p region-token) nil)
  (fill (rt-region-token-arena region-token) nil)
  (setf (rt-region-token-bump-index region-token) 0)
  (incf (rt-region-token-generation region-token))
  region-token)

(defun rt-region-capacity (region-token)
  (rt-region-token-arena-size region-token))

(defun rt-region-used (region-token)
  (rt-region-token-bump-index region-token))

(defun rt-region-active-p (region-token)
  "Return T when REGION-TOKEN is active."
  (and (rt-region-token-p region-token)
       (rt-region-token-active-p region-token)))

(defun rt-region-alloc (region-token value)
  "Allocate VALUE inside REGION-TOKEN using bump-pointer semantics.
Returns a guarded reference."
  (unless (rt-region-active-p region-token)
    (error "Cannot allocate in inactive runtime region ~S" region-token))
  (let ((index (rt-region-token-bump-index region-token))
        (cap (rt-region-token-arena-size region-token)))
    (when (>= index cap)
      (error "Runtime region out of space: used=~D capacity=~D" index cap))
    (setf (aref (rt-region-token-arena region-token) index) value)
    (incf (rt-region-token-bump-index region-token)))
  (%make-rt-region-ref :token region-token
                       :generation (rt-region-token-generation region-token)
                       :value value))

(defun rt-region-ref-valid-p (region-ref)
  "Return T when REGION-REF points to a still-live region generation."
  (and (rt-region-ref-p region-ref)
       (rt-region-token-p (rt-region-ref-token region-ref))
       (rt-region-active-p (rt-region-ref-token region-ref))
       (= (rt-region-ref-generation region-ref)
          (rt-region-token-generation (rt-region-ref-token region-ref)))))

(defun rt-region-deref (region-ref)
  "Dereference REGION-REF or signal an error when region has expired."
  (unless (rt-region-ref-valid-p region-ref)
    (error "Runtime region reference is no longer valid: ~S" region-ref))
  (rt-region-ref-value region-ref))

(defmacro rt-with-region ((name) &body body)
  "Evaluate BODY with NAME bound to a fresh runtime region token."
  `(let ((,name (rt-make-region)))
     (unwind-protect
          (progn ,@body)
       (rt-close-region ,name))))

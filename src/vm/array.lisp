(in-package :cl-cc)

;;; VM Array/Vector Operations
;;;
;;; This file extends the VM with array and vector instructions including
;;; construction, element access/mutation, dimension queries, fill-pointer
;;; management, bit-array operations, and displaced-array support.
;;;

;;; ─── Basic Array Instructions ────────────────────────────────────────────

(define-vm-instruction vm-make-array (vm-instruction)
  "Create an array of given size. Supports :initial-element, :fill-pointer, :adjustable."
  (dst nil :reader vm-dst)
  (size-reg nil :reader vm-size-reg)
  (initial-element nil :reader vm-initial-element)
  (fill-pointer nil :reader vm-fill-pointer)
  (adjustable nil :reader vm-adjustable)
  (:sexp-tag :make-array)
  (:sexp-slots dst size-reg initial-element fill-pointer adjustable))

(define-vm-instruction vm-aref (vm-instruction)
  "Get element at INDEX from ARRAY, store in DST."
  (dst nil :reader vm-dst)
  (array-reg nil :reader vm-array-reg)
  (index-reg nil :reader vm-index-reg)
  (:sexp-tag :aref)
  (:sexp-slots dst array-reg index-reg))

;; vm-aref-multi: N-dimensional array read (2+ indices) — custom sexp like vm-format-inst
(define-vm-instruction vm-aref-multi (vm-instruction)
  "Multi-dimensional array read. DST = (apply #'aref ARRAY INDICES...)."
  (dst nil :reader vm-dst)
  (array-reg nil :reader vm-array-reg)
  (index-regs nil :reader vm-index-regs))

(defmethod instruction->sexp ((inst vm-aref-multi))
  (list* :aref-multi (vm-dst inst) (vm-array-reg inst) (vm-index-regs inst)))

(setf (gethash :aref-multi *instruction-constructors*)
      (lambda (sexp)
        (make-vm-aref-multi :dst (second sexp)
                            :array-reg (third sexp)
                            :index-regs (cdddr sexp))))

(define-vm-instruction vm-aset (vm-instruction)
  "Set element at INDEX in ARRAY to VAL."
  (array-reg nil :reader vm-array-reg)
  (index-reg nil :reader vm-index-reg)
  (val-reg nil :reader vm-val-reg)
  (:sexp-tag :aset)
  (:sexp-slots array-reg index-reg val-reg))

(define-vm-instruction vm-vector-push-extend (vm-instruction)
  "Push VAL onto adjustable ARRAY, extending if needed. Store new index in DST."
  (dst nil :reader vm-dst)
  (val-reg nil :reader vm-val-reg)
  (array-reg nil :reader vm-array-reg)
  (:sexp-tag :vector-push-extend)
  (:sexp-slots dst val-reg array-reg))

(define-vm-instruction vm-array-length (vm-instruction)
  "Get the length of array/vector in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :array-length)
  (:sexp-slots dst src))

(define-vm-instruction vm-vectorp (vm-instruction)
  "Check if value is a vector."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :vectorp)
  (:sexp-slots dst src))

;;; ─── Execute basic array instructions ───────────────────────────────────

(defmethod execute-instruction ((inst vm-make-array) state pc labels)
  (declare (ignore labels))
  (let* ((size (vm-reg-get state (vm-size-reg inst)))
         (init-elem (when (vm-initial-element inst)
                      (vm-reg-get state (vm-initial-element inst))))
         (fp (vm-fill-pointer inst))
         (adj (vm-adjustable inst))
         (arr (cond
                ((and fp adj)
                 (make-array size :initial-element (or init-elem 0)
                                  :fill-pointer (if (eq fp t) 0 fp)
                                  :adjustable t))
                (fp
                 (make-array size :initial-element (or init-elem 0)
                                  :fill-pointer (if (eq fp t) 0 fp)))
                (init-elem
                 (make-array size :initial-element init-elem))
                (t
                 (make-array size :initial-element 0)))))
    (vm-reg-set state (vm-dst inst) arr)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aref) state pc labels)
  (declare (ignore labels))
  (let ((arr (vm-reg-get state (vm-array-reg inst)))
        (idx (vm-reg-get state (vm-index-reg inst))))
    (vm-reg-set state (vm-dst inst) (aref arr idx))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aref-multi) state pc labels)
  (declare (ignore labels))
  (let ((arr (vm-reg-get state (vm-array-reg inst)))
        (idxs (mapcar (lambda (r) (vm-reg-get state r)) (vm-index-regs inst))))
    (vm-reg-set state (vm-dst inst) (apply #'aref arr idxs))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aset) state pc labels)
  (declare (ignore labels))
  (let ((arr (vm-reg-get state (vm-array-reg inst)))
        (idx (vm-reg-get state (vm-index-reg inst)))
        (val (vm-reg-get state (vm-val-reg inst))))
    (setf (aref arr idx) val)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-vector-push-extend) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-val-reg inst)))
        (arr (vm-reg-get state (vm-array-reg inst))))
    (vm-reg-set state (vm-dst inst) (vector-push-extend val arr))
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-array-length :unary length)
(define-simple-instruction vm-vectorp :pred1 vectorp)

;;; ─── FR-601: Array dimension queries ─────────────────────────────────────

(define-vm-instruction vm-array-rank (vm-instruction)
  "Return number of dimensions of ARRAY."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-rank) (:sexp-slots dst src))
(define-simple-instruction vm-array-rank :unary array-rank)

(define-vm-instruction vm-array-total-size (vm-instruction)
  "Return total number of elements in ARRAY."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-total-size) (:sexp-slots dst src))
(define-simple-instruction vm-array-total-size :unary array-total-size)

(define-vm-instruction vm-array-dimensions (vm-instruction)
  "Return list of dimension sizes of ARRAY."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-dimensions) (:sexp-slots dst src))
(define-simple-instruction vm-array-dimensions :unary array-dimensions)

(define-vm-instruction vm-array-dimension (vm-instruction)
  "Return size of dimension AXIS of ARRAY."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :array-dimension) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-array-dimension) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (array-dimension (vm-reg-get state (vm-lhs inst))
                               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

;;; ─── FR-602: Row-major access ────────────────────────────────────────────

(define-vm-instruction vm-row-major-aref (vm-instruction)
  "Access array element by row-major index."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :row-major-aref) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-row-major-aref) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (row-major-aref (vm-reg-get state (vm-lhs inst))
                              (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-array-row-major-index (vm-instruction)
  "Compute the row-major index for ARRAY given a list of SUBSCRIPTS."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (subs nil :reader vm-subs)
  (:sexp-tag :array-row-major-index) (:sexp-slots dst arr subs))
(defmethod execute-instruction ((inst vm-array-row-major-index) state pc labels)
  (declare (ignore labels))
  (let ((arr (vm-reg-get state (vm-arr inst)))
        (subs (vm-reg-get state (vm-subs inst))))
    (vm-reg-set state (vm-dst inst) (apply #'array-row-major-index arr subs))
    (values (1+ pc) nil nil)))

;;; ─── FR-603: svref — simple-vector element access ────────────────────────

(define-vm-instruction vm-svref (vm-instruction)
  "Access element of simple-vector by index."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :svref) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-svref) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (svref (vm-reg-get state (vm-lhs inst))
                     (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-svset (vm-instruction)
  "Set element of simple-vector by index. Returns new value."
  (dst nil :reader vm-dst) (array-reg nil :reader vm-array-reg)
  (index-reg nil :reader vm-index-reg) (val-reg nil :reader vm-val-reg)
  (:sexp-tag :svset) (:sexp-slots dst array-reg index-reg val-reg))
(defmethod execute-instruction ((inst vm-svset) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-val-reg inst))))
    (setf (svref (vm-reg-get state (vm-array-reg inst))
                 (vm-reg-get state (vm-index-reg inst))) val)
    (vm-reg-set state (vm-dst inst) val)
    (values (1+ pc) nil nil)))

;;; ─── FR-604: fill-pointer and vector-push ────────────────────────────────

(define-vm-instruction vm-fill-pointer-inst (vm-instruction)
  "Return fill pointer of vector."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :fill-pointer-inst) (:sexp-slots dst src))
(define-simple-instruction vm-fill-pointer-inst :unary fill-pointer)

(define-vm-instruction vm-array-has-fill-pointer-p (vm-instruction)
  "Return T if array has a fill pointer."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-has-fill-pointer-p) (:sexp-slots dst src))
(define-simple-instruction vm-array-has-fill-pointer-p :pred1 array-has-fill-pointer-p)

(define-vm-instruction vm-array-adjustable-p (vm-instruction)
  "Return T if array is adjustable."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-adjustable-p) (:sexp-slots dst src))
(define-simple-instruction vm-array-adjustable-p :pred1 adjustable-array-p)

(define-vm-instruction vm-vector-push (vm-instruction)
  "Push VAL onto ARRAY if below fill-pointer limit. Returns new index or NIL."
  (dst nil :reader vm-dst) (val-reg nil :reader vm-val-reg)
  (array-reg nil :reader vm-array-reg)
  (:sexp-tag :vector-push) (:sexp-slots dst val-reg array-reg))
(defmethod execute-instruction ((inst vm-vector-push) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-val-reg inst)))
        (arr (vm-reg-get state (vm-array-reg inst))))
    (vm-reg-set state (vm-dst inst) (vector-push val arr))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-vector-pop (vm-instruction)
  "Pop last element from ARRAY (decrement fill pointer). Returns element."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :vector-pop) (:sexp-slots dst src))
(define-simple-instruction vm-vector-pop :unary vector-pop)

(define-vm-instruction vm-set-fill-pointer (vm-instruction)
  "Set fill pointer of ARRAY to NEW-FP. Returns NEW-FP."
  (dst nil :reader vm-dst) (array-reg nil :reader vm-array-reg)
  (val-reg nil :reader vm-val-reg)
  (:sexp-tag :set-fill-pointer) (:sexp-slots dst array-reg val-reg))
(defmethod execute-instruction ((inst vm-set-fill-pointer) state pc labels)
  (declare (ignore labels))
  (let* ((arr (vm-reg-get state (vm-array-reg inst)))
         (fp (vm-reg-get state (vm-val-reg inst))))
    (setf (fill-pointer arr) fp)
    (vm-reg-set state (vm-dst inst) fp)
    (values (1+ pc) nil nil)))

;;; Bit array operations (FR-606), adjust-array/displacement (FR-605),
;;; and simple-vector-p (FR-648) are in array-bits.lisp (loaded next).

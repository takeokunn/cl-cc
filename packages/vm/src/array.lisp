(in-package :cl-cc/vm)

(defstruct (vm-cow-vector (:constructor %make-vm-cow-vector))
  (backing #() :type vector)
  (refcount 1 :type integer))

(defparameter *vm-cow-vector-enabled* t)

(defparameter +vm-specialized-array-tag+ :specialized-array
  "Header tag for VM arrays whose elements are stored unboxed by the host.")

(defparameter +vm-bit-vector-tag+ :bit-vector
  "Header tag for VM bit vectors packed at 64 bits per word.")

(defstruct (vm-specialized-array (:constructor %make-vm-specialized-array))
  "VM specialized one-dimensional array.

HEADER records the array tag, normalized element type, and GC-skip flag.  STORAGE
is a host specialized vector; bit arrays use (UNSIGNED-BYTE 64) words and pack
64 logical bits per storage word."
  (header nil :type list)
  (element-type :any :type keyword)
  (length 0 :type fixnum)
  (storage #() :type vector)
  (gc-skip-p nil :type boolean))

(defun %vm-normalize-specialized-element-type (element-type)
  "Normalize ELEMENT-TYPE to the VM specialized-array type vocabulary."
  (case element-type
    ((:fixnum fixnum integer :integer) :fixnum)
    ((:double-float double-float :double) :double-float)
    ((:character character :char) :character)
    ((:bit bit) :bit)
    ((t :any nil) :any)
    (otherwise element-type)))

(defun vm-specialized-array-element-pointer-free-p (element-type)
  "Return T (boolean) when ELEMENT-TYPE contains no GC-visible pointers."
  (not (null (member (%vm-normalize-specialized-element-type element-type)
                      '(:fixnum :double-float :character :bit)
                      :test #'eq))))

(defun %make-vm-specialized-array-header (element-type length &key (gc-skip-p t))
  "Build a metadata header for a VM specialized array."
  (let* ((type (%vm-normalize-specialized-element-type element-type))
         (tag (if (eq type :bit) +vm-bit-vector-tag+ +vm-specialized-array-tag+)))
    (list :type-tag tag
          :element-type type
          :length length
          :gc-skip-p (and gc-skip-p
                          (vm-specialized-array-element-pointer-free-p type)))))

(defun %vm-specialized-storage (length element-type)
  "Allocate host storage for a specialized VM array."
  (case (%vm-normalize-specialized-element-type element-type)
    (:fixnum (make-array length :element-type 'fixnum :initial-element 0))
    (:double-float (make-array length :element-type 'double-float :initial-element 0.0d0))
    (:character (make-array length :element-type 'character :initial-element #\Nul))
    (:bit (make-array (ceiling length 64)
                      :element-type '(unsigned-byte 64)
                      :initial-element 0))
    (otherwise (make-array length :initial-element nil))))

(defun vm-make-specialized-array (length element-type)
  "Create a VM specialized array of LENGTH and ELEMENT-TYPE.

Supported pointer-free ELEMENT-TYPE values are :FIXNUM, :DOUBLE-FLOAT,
:CHARACTER, and :BIT.  :BIT arrays are VM bit vectors packed at 64 bits per
storage word.  :ANY/T arrays retain pointer-scannable element semantics."
  (check-type length (integer 0 *))
  (let* ((type (%vm-normalize-specialized-element-type element-type))
         (gc-skip-p (vm-specialized-array-element-pointer-free-p type)))
    (%make-vm-specialized-array
     :header (%make-vm-specialized-array-header type length :gc-skip-p gc-skip-p)
     :element-type type
     :length length
     :storage (%vm-specialized-storage length type)
     :gc-skip-p gc-skip-p)))

(defun vm-specialized-array-ref (array index)
  "Read INDEX from specialized ARRAY."
  (check-type index (integer 0 *))
  (unless (< index (vm-specialized-array-length array))
    (error "Specialized array index ~D out of bounds for length ~D"
           index (vm-specialized-array-length array)))
  (if (eq (vm-specialized-array-element-type array) :bit)
      (multiple-value-bind (word-index bit-index) (floor index 64)
        (if (logbitp bit-index (aref (vm-specialized-array-storage array)
                                     word-index))
            1
            0))
      (aref (vm-specialized-array-storage array) index)))

(defun (setf vm-specialized-array-ref) (value array index)
  "Write VALUE at INDEX in specialized ARRAY."
  (check-type index (integer 0 *))
  (unless (< index (vm-specialized-array-length array))
    (error "Specialized array index ~D out of bounds for length ~D"
           index (vm-specialized-array-length array)))
  (case (vm-specialized-array-element-type array)
    (:bit
     (multiple-value-bind (word-index bit-index) (floor index 64)
       (let* ((storage (vm-specialized-array-storage array))
              (word (aref storage word-index))
              (mask (ash 1 bit-index)))
         (setf (aref storage word-index)
               (if (zerop value)
                   (logand word (lognot mask))
                   (logior word mask)))
         (if (zerop value) 0 1))))
    (:double-float
     (setf (aref (vm-specialized-array-storage array) index)
           (coerce value 'double-float)))
    (:character
     (check-type value character)
     (setf (aref (vm-specialized-array-storage array) index) value))
    (:fixnum
     (check-type value fixnum)
     (setf (aref (vm-specialized-array-storage array) index) value))
    (otherwise
     (setf (aref (vm-specialized-array-storage array) index) value))))

(defun vm-bit-vector-p (value)
  "Return true when VALUE is a VM packed bit vector."
  (and (vm-specialized-array-p value)
       (eq (vm-specialized-array-element-type value) :bit)))

(defun vm-bit-vector-ref (bit-vector index)
  "Return bit at INDEX from packed VM BIT-VECTOR."
  (unless (vm-bit-vector-p bit-vector)
    (error "Expected VM bit vector, got ~S" bit-vector))
  (vm-specialized-array-ref bit-vector index))

(defun (setf vm-bit-vector-ref) (value bit-vector index)
  "Set bit at INDEX in packed VM BIT-VECTOR."
  (unless (vm-bit-vector-p bit-vector)
    (error "Expected VM bit vector, got ~S" bit-vector))
  (setf (vm-specialized-array-ref bit-vector index) value))

(defun %vm-cow-vector-materialize (value)
  (cond
    ((vm-cow-vector-p value) (vm-cow-vector-backing value))
    (t value)))

(defun %vm-cow-vector-share (value)
  (cond
    ((vm-specialized-array-p value) value)
    ((vm-cow-vector-p value)
      (progn
        (incf (vm-cow-vector-refcount value))
        (%make-vm-cow-vector :backing (vm-cow-vector-backing value)
                             :refcount (vm-cow-vector-refcount value))))
    (t (%make-vm-cow-vector :backing value :refcount 2))))

(defun %vm-cow-vector-ensure-writable (value)
  (if (vm-cow-vector-p value)
      (progn
        (when (> (vm-cow-vector-refcount value) 1)
          (decf (vm-cow-vector-refcount value))
          (setf (vm-cow-vector-backing value) (copy-seq (vm-cow-vector-backing value))
                (vm-cow-vector-refcount value) 1))
        (vm-cow-vector-backing value))
      value))

(defun %vm-array-dimensions-designator (size dimensions)
  "Return a MAKE-ARRAY dimensions designator from SIZE and optional DIMENSIONS."
  (let ((raw (or dimensions size)))
    (cond
      ((vectorp raw) (coerce raw 'list))
      ((listp raw) raw)
      (t raw))))

(defun %vm-array-total-size (dimensions-designator)
  "Return row-major total size for DIMENSIONS-DESIGNATOR."
  (if (listp dimensions-designator)
      (reduce #'* dimensions-designator :initial-value 1)
      dimensions-designator))

(defun %vm-array-vector-dimensions-p (dimensions-designator)
  "Return true when DIMENSIONS-DESIGNATOR denotes a one-dimensional vector."
  (or (integerp dimensions-designator)
      (and (consp dimensions-designator)
           (null (rest dimensions-designator)))))

(defun %vm-array-object (value)
  "Return the host array object for VALUE, materializing VM COW vectors."
  (%vm-cow-vector-materialize value))

(defun vm-array-rank-value (array)
  "VM-aware ARRAY-RANK supporting specialized one-dimensional arrays."
  (if (vm-specialized-array-p array)
      1
      (array-rank (%vm-array-object array))))

(defun vm-array-dimensions-value (array)
  "VM-aware ARRAY-DIMENSIONS supporting specialized one-dimensional arrays."
  (if (vm-specialized-array-p array)
      (list (vm-specialized-array-length array))
      (array-dimensions (%vm-array-object array))))

(defun vm-array-dimension-value (array axis)
  "VM-aware ARRAY-DIMENSION supporting specialized one-dimensional arrays."
  (if (vm-specialized-array-p array)
      (if (zerop axis)
          (vm-specialized-array-length array)
          (error "Invalid specialized array dimension axis: ~D" axis))
      (array-dimension (%vm-array-object array) axis)))

(defun vm-array-total-size-value (array)
  "VM-aware ARRAY-TOTAL-SIZE supporting specialized one-dimensional arrays."
  (if (vm-specialized-array-p array)
      (vm-specialized-array-length array)
      (array-total-size (%vm-array-object array))))

(defun vm-array-element-type-value (array)
  "Return the element type for host or VM specialized ARRAY."
  (if (vm-specialized-array-p array)
      (case (vm-specialized-array-element-type array)
        (:fixnum 'fixnum)
        (:double-float 'double-float)
        (:character 'character)
        (:bit 'bit)
        (:any t)
        (otherwise (vm-specialized-array-element-type array)))
      (array-element-type (%vm-array-object array))))

(defun vm-array-in-bounds-p-value (array subscripts)
  "Return true when SUBSCRIPTS are within ARRAY bounds."
  (let ((dimensions (vm-array-dimensions-value array))
        (subs (if (listp subscripts) subscripts (list subscripts))))
    (and (= (length dimensions) (length subs))
         (every (lambda (dimension subscript)
                  (and (integerp subscript)
                       (<= 0 subscript)
                       (< subscript dimension)))
                dimensions subs))))

;;; VM Array/Vector Operations
;;;
;;; This file extends the VM with array and vector instructions including
;;; construction, element access/mutation, dimension queries, fill-pointer
;;; management, bit-array operations, and displaced-array support.
;;;

;;; ─── Basic Array Instructions ────────────────────────────────────────────

(define-vm-instruction vm-make-array (vm-instruction)
  "Create an array of given size. Supports make-array keyword arguments."
  (dst nil :reader vm-dst)
  (size-reg nil :reader vm-size-reg)
  (dimensions-reg nil :reader vm-dimensions-reg)
  (initial-element nil :reader vm-initial-element)
  (fill-pointer nil :reader vm-fill-pointer)
  (fill-pointer-reg nil :reader vm-fill-pointer-reg)
  (adjustable nil :reader vm-adjustable)
  (adjustable-reg nil :reader vm-adjustable-reg)
  (element-type nil :reader vm-element-type)
  (element-type-reg nil :reader vm-element-type-reg)
  (displaced-to-reg nil :reader vm-displaced-to-reg)
  (displaced-index-offset-reg nil :reader vm-displaced-index-offset-reg)
  (:sexp-tag :make-array)
  (:sexp-slots dst size-reg initial-element fill-pointer adjustable element-type
                fill-pointer-reg adjustable-reg element-type-reg displaced-to-reg
                dimensions-reg displaced-index-offset-reg))

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

(define-vm-instruction vm-fill (vm-instruction)
  "Fill ARRAY with VAL."
  (array-reg nil :reader vm-array-reg)
  (val-reg nil :reader vm-val-reg)
  (:sexp-tag :fill)
  (:sexp-slots array-reg val-reg))

(define-vm-instruction vm-copy-vector (vm-instruction)
  "Copy LEN elements from SRC-ARRAY to DST-ARRAY."
  (dst-array-reg nil :reader vm-dst-array-reg)
  (src-array-reg nil :reader vm-src-array-reg)
  (len-reg nil :reader vm-len-reg)
  (:sexp-tag :copy-vector)
  (:sexp-slots dst-array-reg src-array-reg len-reg))

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
          (dimensions (and (vm-dimensions-reg inst)
                           (vm-reg-get state (vm-dimensions-reg inst))))
          (dimensions-designator (%vm-array-dimensions-designator size dimensions))
          (total-size (%vm-array-total-size dimensions-designator))
          (vector-dimensions-p (%vm-array-vector-dimensions-p dimensions-designator))
          (init-present-p (vm-initial-element inst))
          (fp (if (vm-fill-pointer-reg inst)
                  (vm-reg-get state (vm-fill-pointer-reg inst))
                  (vm-fill-pointer inst)))
          (adj (if (vm-adjustable-reg inst)
                   (vm-reg-get state (vm-adjustable-reg inst))
                   (vm-adjustable inst)))
          (elt-type (if (vm-element-type-reg inst)
                        (vm-reg-get state (vm-element-type-reg inst))
                        (vm-element-type inst)))
          (displaced-to (and (vm-displaced-to-reg inst)
                              (vm-reg-get state (vm-displaced-to-reg inst))))
          (displaced-index-offset (if (vm-displaced-index-offset-reg inst)
                                      (vm-reg-get state (vm-displaced-index-offset-reg inst))
                                      0))
          (default-init (case elt-type
                          (character #\Nul)
                          (single-float 0.0f0)
                         (double-float 0.0d0)
                         (bit 0)
                         (otherwise 0)))
          (init-elem (if init-present-p
                         (vm-reg-get state (vm-initial-element inst))
                         default-init))
           (specialized-type (%vm-normalize-specialized-element-type elt-type))
           (arr (cond
                     (displaced-to
                      (cond
                        ((and fp adj)
                         (make-array dimensions-designator :element-type (or elt-type t)
                                     :displaced-to displaced-to
                                     :displaced-index-offset displaced-index-offset
                                     :fill-pointer (if (eq fp t) 0 fp)
                                     :adjustable t))
                        (fp
                         (make-array dimensions-designator :element-type (or elt-type t)
                                     :displaced-to displaced-to
                                     :displaced-index-offset displaced-index-offset
                                     :fill-pointer (if (eq fp t) 0 fp)))
                        (adj
                         (make-array dimensions-designator :element-type (or elt-type t)
                                     :displaced-to displaced-to
                                     :displaced-index-offset displaced-index-offset
                                     :adjustable t))
                        (t
                         (make-array dimensions-designator :element-type (or elt-type t)
                                     :displaced-index-offset displaced-index-offset
                                     :displaced-to displaced-to))))
                    ((and vector-dimensions-p
                          (member specialized-type '(:fixnum :double-float :character :bit)
                                   :test #'eq)
                           (not fp)
                          (not adj)
                          (not init-present-p))
                     (vm-make-specialized-array total-size specialized-type))
                    ((and fp adj)
                    (make-array dimensions-designator :element-type (or elt-type t) :initial-element init-elem
                                :fill-pointer (if (eq fp t) 0 fp)
                                :adjustable t))
                   (fp
                    (make-array dimensions-designator :element-type (or elt-type t) :initial-element init-elem
                                :fill-pointer (if (eq fp t) 0 fp)))
                   (adj
                    (make-array dimensions-designator :element-type (or elt-type t) :initial-element init-elem
                                :adjustable t))
                   (init-present-p
                    (make-array dimensions-designator :element-type (or elt-type t) :initial-element init-elem))
                   (t
                    (make-array dimensions-designator :element-type (or elt-type t) :initial-element init-elem)))))
    (vm-reg-set state (vm-dst inst) arr)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aref) state pc labels)
  (declare (ignore labels))
  (let ((arr (%vm-cow-vector-materialize (vm-reg-get state (vm-array-reg inst))))
        (idx (vm-reg-get state (vm-index-reg inst))))
    (vm-reg-set state (vm-dst inst)
                (if (vm-specialized-array-p arr)
                    (vm-specialized-array-ref arr idx)
                    (aref arr idx)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aref-multi) state pc labels)
  (declare (ignore labels))
  (let ((arr (%vm-cow-vector-materialize (vm-reg-get state (vm-array-reg inst))))
        (idxs (mapcar (lambda (r) (vm-reg-get state r)) (vm-index-regs inst))))
    (vm-reg-set state (vm-dst inst) (apply #'aref arr idxs))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aset) state pc labels)
  (declare (ignore labels))
  (let ((arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-array-reg inst))))
        (idx (vm-reg-get state (vm-index-reg inst)))
        (val (vm-reg-get state (vm-val-reg inst))))
    (if (vm-specialized-array-p arr)
        (setf (vm-specialized-array-ref arr idx) val)
        (setf (aref arr idx) val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-fill) state pc labels)
  (declare (ignore labels))
  (let ((arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-array-reg inst))))
        (val (vm-reg-get state (vm-val-reg inst))))
    (if (vm-specialized-array-p arr)
        (loop for i below (vm-specialized-array-length arr)
              do (setf (vm-specialized-array-ref arr i) val))
        (fill arr val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-copy-vector) state pc labels)
  (declare (ignore labels))
  (let ((dst (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-dst-array-reg inst))))
        (src (%vm-cow-vector-materialize (vm-reg-get state (vm-src-array-reg inst))))
        (len (vm-reg-get state (vm-len-reg inst))))
    (cond
      ((or (vm-specialized-array-p dst) (vm-specialized-array-p src))
       (loop for i below len
             for value = (if (vm-specialized-array-p src)
                             (vm-specialized-array-ref src i)
                             (aref src i))
             do (if (vm-specialized-array-p dst)
                    (setf (vm-specialized-array-ref dst i) value)
                    (setf (aref dst i) value))))
      (t
       (replace dst src :end1 len :end2 len)))
    (values (1+ pc) nil nil)))

(defun %vm-bridge-replace (sequence-1 sequence-2 &key end1 end2)
  "VM-safe bridge for CL:REPLACE over ordinary, COW, and specialized arrays."
  (let ((dst (%vm-cow-vector-ensure-writable sequence-1))
        (src (%vm-cow-vector-materialize sequence-2)))
    (cond
      ((or (vm-specialized-array-p dst) (vm-specialized-array-p src))
       (let ((limit (min (or end1 (if (vm-specialized-array-p dst)
                                      (vm-specialized-array-length dst)
                                      (length dst)))
                         (or end2 (if (vm-specialized-array-p src)
                                      (vm-specialized-array-length src)
                                      (length src))))))
         (loop for i below limit
               for value = (if (vm-specialized-array-p src)
                               (vm-specialized-array-ref src i)
                               (aref src i))
               do (if (vm-specialized-array-p dst)
                      (setf (vm-specialized-array-ref dst i) value)
                      (setf (aref dst i) value)))
         sequence-1))
      (t
       (replace dst src :end1 end1 :end2 end2)
       sequence-1))))

(eval-when (:load-toplevel :execute)
  (vm-register-host-bridge 'cl:replace #'%vm-bridge-replace))

(defmethod execute-instruction ((inst vm-vector-push-extend) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-val-reg inst)))
        (arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-array-reg inst)))))
    (vm-reg-set state (vm-dst inst) (vector-push-extend val arr))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-array-length) state pc labels)
  (declare (ignore labels))
  (let ((src (%vm-cow-vector-materialize (vm-reg-get state (vm-src inst)))))
    (vm-reg-set state (vm-dst inst)
                (if (vm-specialized-array-p src)
                    (vm-specialized-array-length src)
                    (length src)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-vectorp) state pc labels)
  (declare (ignore labels))
  (let ((src (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (or (vectorp src) (vm-cow-vector-p src)
                        (vm-specialized-array-p src)) 1 0))
    (values (1+ pc) nil nil)))

;;; ─── FR-601: Array dimension queries ─────────────────────────────────────

(define-vm-instruction vm-array-rank (vm-instruction)
  "Return number of dimensions of ARRAY."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-rank) (:sexp-slots dst src))
(define-simple-instruction vm-array-rank :unary vm-array-rank-value)

(define-vm-instruction vm-array-total-size (vm-instruction)
  "Return total number of elements in ARRAY."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-total-size) (:sexp-slots dst src))
(define-simple-instruction vm-array-total-size :unary vm-array-total-size-value)

(define-vm-instruction vm-array-dimensions (vm-instruction)
  "Return list of dimension sizes of ARRAY."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-dimensions) (:sexp-slots dst src))
(define-simple-instruction vm-array-dimensions :unary vm-array-dimensions-value)

(define-vm-instruction vm-array-element-type (vm-instruction)
  "Return ARRAY's element type."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-element-type) (:sexp-slots dst src))
(define-simple-instruction vm-array-element-type :unary vm-array-element-type-value)

(define-vm-instruction vm-array-dimension (vm-instruction)
  "Return size of dimension AXIS of ARRAY."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :array-dimension) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-array-dimension) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (vm-array-dimension-value (vm-reg-get state (vm-lhs inst))
                                        (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-array-in-bounds-p (vm-instruction)
  "Return true if SUBSCRIPTS are valid for ARRAY."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (subs nil :reader vm-subs)
  (:sexp-tag :array-in-bounds-p) (:sexp-slots dst arr subs))
(defmethod execute-instruction ((inst vm-array-in-bounds-p) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (vm-array-in-bounds-p-value (vm-reg-get state (vm-arr inst))
                                              (vm-reg-get state (vm-subs inst)))
                  t
                  nil))
  (values (1+ pc) nil nil))

;;; ─── FR-602: Row-major access ────────────────────────────────────────────

(define-vm-instruction vm-row-major-aref (vm-instruction)
  "Access array element by row-major index."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :row-major-aref) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-row-major-aref) state pc labels)
  (declare (ignore labels))
  (let ((array (vm-reg-get state (vm-lhs inst)))
        (index (vm-reg-get state (vm-rhs inst))))
    (vm-reg-set state (vm-dst inst)
                (if (vm-specialized-array-p array)
                    (vm-specialized-array-ref array index)
                    (row-major-aref (%vm-array-object array) index))))
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
              (svref (%vm-cow-vector-materialize (vm-reg-get state (vm-lhs inst)))
                     (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-svset (vm-instruction)
  "Set element of simple-vector by index. Returns new value."
  (dst nil :reader vm-dst) (array-reg nil :reader vm-array-reg)
  (index-reg nil :reader vm-index-reg) (val-reg nil :reader vm-val-reg)
  (:sexp-tag :svset) (:sexp-slots dst array-reg index-reg val-reg))
(defmethod execute-instruction ((inst vm-svset) state pc labels)
  (declare (ignore labels))
  (let* ((arr (%vm-cow-vector-ensure-writable (vm-reg-get state (vm-array-reg inst))))
         (idx (vm-reg-get state (vm-index-reg inst)))
         (val (vm-reg-get state (vm-val-reg inst))))
    (setf (svref arr idx) val)
    (vm-reg-set state (vm-dst inst) val)
    (values (1+ pc) nil nil)))

(defun vm-cow-copy-seq (sequence)
  "Return COW wrapper for vectors and list COW wrappers for lists when enabled."
  (cond
    ((listp sequence)
     (copy-list sequence))
    ((vectorp sequence)
     (if *vm-cow-vector-enabled*
         (%vm-cow-vector-share sequence)
         (copy-seq sequence)))
    (t
     (copy-seq sequence))))

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

(define-vm-instruction vm-adjustable-array-p (vm-instruction)
  "Return T if array is adjustable. ANSI spelling alias."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :adjustable-array-p) (:sexp-slots dst src))
(define-simple-instruction vm-adjustable-array-p :pred1 adjustable-array-p)

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

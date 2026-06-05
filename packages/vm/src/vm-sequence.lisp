(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Extensible Sequence Protocol (FR-821/838)
;;;
;;; Provides the abstract SEQUENCE base class and generic functions (elt, length,
;;; subseq, make-sequence-like, sequence-protocol-p) with methods for built-in
;;; CL types and a user-extensible default protocol.
;;;
;;; Load order: before vm-clos.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; ── FR-821/838: Copy and extensible sequence protocol runtime helpers ───

(defclass sequence () ()
  (:documentation "Abstract base class for user-defined sequence-like objects."))

(defgeneric elt (sequence index)
  (:documentation "Return the element of SEQUENCE at INDEX."))

(defgeneric (setf elt) (value sequence index)
  (:documentation "Set the element of SEQUENCE at INDEX to VALUE."))

(defgeneric length (sequence)
  (:documentation "Return the number of elements in SEQUENCE."))

(defgeneric subseq (sequence start &optional end)
  (:documentation "Return a sequence slice from START below END."))

(defgeneric make-sequence-like (sequence size &key initial-element)
  (:documentation "Create a sequence with the same representation family as SEQUENCE."))

(defgeneric sequence-protocol-p (object)
  (:documentation "Return true when OBJECT supports the extensible sequence protocol."))

;;; ── define-builtin-sequence-methods ─────────────────────────────────────
;;; Macro that registers length/elt/setf-elt/subseq/make-sequence-like methods
;;; for a built-in CL sequence type in a single declarative form.
;;;
;;; TYPE-SPECIFIER  — the specializer class name (e.g. list, vector, string)
;;; LENGTH-FORM     — body expression computing the length (object bound)
;;; ELT-FORM        — body expression computing (elt object index)
;;; SETF-ELT-FORM   — body expression mutating (setf (elt object index) value)
;;; SUBSEQ-FORM     — body expression computing (subseq object start end)
;;; MAKE-LIKE-ARGS  — keyword args for the lambda-list (&key ...)
;;; MAKE-LIKE-FORM  — body expression building the new sequence

(defmacro define-builtin-sequence-methods
    (type-specifier
     &key length-form
          elt-form
          setf-elt-form
          subseq-form
          make-like-key
          make-like-form)
  `(progn
     (defmethod length ((object ,type-specifier))
       ,length-form)
     (defmethod elt ((object ,type-specifier) index)
       ,elt-form)
     (defmethod (setf elt) (value (object ,type-specifier) index)
       ,setf-elt-form)
     (defmethod subseq ((object ,type-specifier) start &optional end)
       ,subseq-form)
     (defmethod make-sequence-like ((object ,type-specifier) size
                                    &key ,@make-like-key)
       (declare (ignore object))
       ,make-like-form)
     (defmethod sequence-protocol-p ((object ,type-specifier))
       t)))

(define-builtin-sequence-methods list
  :length-form   (cl:length object)
  :elt-form      (cl:nth index object)
  :setf-elt-form (setf (cl:nth index object) value)
  :subseq-form   (cl:subseq object start end)
  :make-like-key (initial-element)
  :make-like-form (make-list size :initial-element initial-element))

(define-builtin-sequence-methods vector
  :length-form   (cl:length object)
  :elt-form      (cl:aref object index)
  :setf-elt-form (setf (cl:aref object index) value)
  :subseq-form   (cl:subseq object start end)
  :make-like-key (initial-element)
  :make-like-form (make-array size :initial-element initial-element))

(define-builtin-sequence-methods string
  :length-form   (cl:length object)
  :elt-form      (cl:char object index)
  :setf-elt-form (setf (cl:char object index) value)
  :subseq-form   (cl:subseq object start end)
  :make-like-key ((initial-element #\Space))
  :make-like-form (make-string size :initial-element initial-element))

;;; ── Fallback methods for unimplemented sequence subclasses ───────────────

(defmethod length ((object sequence))
  (error "LENGTH is not implemented for sequence object ~S" object))

(defmethod elt ((object sequence) index)
  (declare (ignore index))
  (error "ELT is not implemented for sequence object ~S" object))

(defmethod (setf elt) (value (object sequence) index)
  (declare (ignore value index))
  (error "(SETF ELT) is not implemented for sequence object ~S" object))

(defmethod subseq ((object sequence) start &optional end)
  (let* ((limit (or end (length object)))
         (size (- limit start))
         (copy (make-sequence-like object size)))
    (dotimes (i size copy)
      (setf (elt copy i) (elt object (+ start i))))))

(defmethod make-sequence-like ((object sequence) size &key initial-element)
  (declare (ignore initial-element))
  (error "MAKE-SEQUENCE-LIKE is not implemented for ~S with size ~D" object size))

(defmethod sequence-protocol-p ((object sequence))
  t)

(defmethod sequence-protocol-p ((object t))
  nil)

(defun copy-instance (instance)
  "Return a shallow copy of a CLOS/VM instance, preserving all slot contents."
  (cond
    ((hash-table-p instance)
     (let ((class (gethash :__class__ instance))
           (copy (make-hash-table :test (hash-table-test instance))))
       (unless class
         (error "copy-instance: ~S is not a VM CLOS instance" instance))
       (maphash (lambda (key value)
                  (setf (gethash key copy) value))
                instance)
       copy))
    ((and (vectorp instance)
          (> (cl:length instance) 0)
          (hash-table-p (cl:aref instance 0)))
     (copy-seq instance))
    ((typep instance 'standard-object)
     (let* ((class (class-of instance))
            (copy (allocate-instance class)))
       (dolist (slot (sb-mop:class-slots class) copy)
         (let ((slot-name (sb-mop:slot-definition-name slot)))
           (when (slot-boundp instance slot-name)
             (setf (slot-value copy slot-name)
                   (slot-value instance slot-name)))))))
    (t
     (error "copy-instance: ~S is not a CLOS instance" instance))))

(defun copy-structure (structure)
  "Return an ANSI shallow copy of STRUCTURE."
  (cond
    ((or (hash-table-p structure)
         (and (vectorp structure)
              (> (cl:length structure) 0)
              (hash-table-p (cl:aref structure 0))))
     (copy-instance structure))
    ((or (listp structure) (vectorp structure) (stringp structure))
     (copy-seq structure))
    ((typep structure 'structure-object)
     (cl:copy-structure structure))
    ((typep structure 'standard-object)
     (copy-instance structure))
    (t
     (error "copy-structure: unsupported object ~S" structure))))

(defun deep-copy (object &optional (seen (make-hash-table :test #'eq)))
  "Recursively copy OBJECT, preserving sharing and cycles reachable by EQ."
  (cond
    ((or (null object) (numberp object) (characterp object) (symbolp object) (functionp object))
     object)
    ((gethash object seen))
    ((consp object)
     (let ((copy (cons nil nil)))
       (setf (gethash object seen) copy)
       (setf (car copy) (deep-copy (car object) seen)
             (cdr copy) (deep-copy (cdr object) seen))
       copy))
    ((hash-table-p object)
     (let ((copy (make-hash-table :test (hash-table-test object))))
       (setf (gethash object seen) copy)
       (maphash (lambda (key value)
                  (setf (gethash (deep-copy key seen) copy)
                        (deep-copy value seen)))
                object)
       copy))
    ((stringp object)
     (copy-seq object))
    ((vectorp object)
     (let ((copy (copy-seq object)))
       (setf (gethash object seen) copy)
       (dotimes (i (cl:length copy) copy)
         (setf (cl:aref copy i) (deep-copy (cl:aref object i) seen)))))
    ((typep object 'standard-object)
     (let ((copy (copy-instance object)))
       (setf (gethash object seen) copy)
       (dolist (slot (sb-mop:class-slots (class-of object)) copy)
         (let ((slot-name (sb-mop:slot-definition-name slot)))
           (when (slot-boundp object slot-name)
             (setf (slot-value copy slot-name)
                   (deep-copy (slot-value object slot-name) seen)))))))
    ((typep object 'structure-object)
     (copy-structure object))
    (t object)))


(in-package :cl-cc/vm)

(define-vm-instruction vm-string-coerce (vm-instruction)
  "Coerce value to string (CL string function)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :string)
  (:sexp-slots dst src))

(define-vm-instruction vm-coerce-to-string (vm-instruction)
  "Coerce sequence (char list, vector) to string via CL coerce."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :coerce-to-string)
  (:sexp-slots dst src))

(define-vm-instruction vm-coerce-to-list (vm-instruction)
  "Coerce sequence to list via CL coerce."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :coerce-to-list)
  (:sexp-slots dst src))

(define-vm-instruction vm-coerce-to-vector (vm-instruction)
  "Coerce sequence to vector via CL coerce."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :coerce-to-vector)
  (:sexp-slots dst src))

(define-simple-instruction vm-string-coerce :unary string)

(defmethod execute-instruction ((inst vm-coerce-to-string) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (coerce (vm-reg-get state (vm-src inst)) 'string))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-coerce-to-list) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (coerce (vm-reg-get state (vm-src inst)) 'list))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-coerce-to-vector) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (coerce (vm-reg-get state (vm-src inst)) 'vector))
  (values (1+ pc) nil nil))

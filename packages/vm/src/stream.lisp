;;;; stream.lisp -- VM instructions for host-backed ANSI streams

(in-package :cl-cc/vm)

;; FR-600: Stream types
(define-vm-instruction vm-make-broadcast-stream (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :make-broadcast-stream) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-make-broadcast-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:make-broadcast-stream (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-concatenated-stream (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :make-concatenated-stream) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-make-concatenated-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:make-concatenated-stream (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-echo-stream (vm-instruction)
  (dst nil :reader vm-dst) (src1 nil :reader vm-src1) (src2 nil :reader vm-src2)
  (:sexp-tag :make-echo-stream) (:sexp-slots dst src1 src2))

(defmethod execute-instruction ((inst vm-make-echo-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (cl:make-echo-stream (vm-reg-get state (vm-src1 inst))
                                   (vm-reg-get state (vm-src2 inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-synonym-stream (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :make-synonym-stream) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-make-synonym-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:make-synonym-stream (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-two-way-stream (vm-instruction)
  (dst nil :reader vm-dst) (src1 nil :reader vm-src1) (src2 nil :reader vm-src2)
  (:sexp-tag :make-two-way-stream) (:sexp-slots dst src1 src2))

(defmethod execute-instruction ((inst vm-make-two-way-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (cl:make-two-way-stream (vm-reg-get state (vm-src1 inst))
                                      (vm-reg-get state (vm-src2 inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-string-input-stream (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :make-string-input-stream) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-make-string-input-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:make-string-input-stream (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-string-output-stream (vm-instruction)
  (dst nil :reader vm-dst)
  (:sexp-tag :make-string-output-stream) (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-make-string-output-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:make-string-output-stream))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-get-output-stream-string (vm-instruction)
  (dst nil :reader vm-dst) (stream-reg nil :reader vm-goss-stream-reg))

(defmethod execute-instruction ((inst vm-get-output-stream-string) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (cl:get-output-stream-string (vm-reg-get state (vm-goss-stream-reg inst))))
  (values (1+ pc) nil nil))

;; FR-601: Gray streams - delegate to host CL
(define-vm-unary-instruction vm-stream-read-char :stream-read-char "Read char from stream.")
(define-simple-instruction vm-stream-read-char :unary cl:read-char)

(define-vm-instruction vm-stream-write-char (vm-instruction)
  (stream-reg nil :reader vm-swc-stream-reg) (char-reg nil :reader vm-swc-char-reg)
  (:sexp-tag :stream-write-char) (:sexp-slots stream-reg char-reg))

(defmethod execute-instruction ((inst vm-stream-write-char) state pc labels)
  (declare (ignore labels))
  (cl:write-char (vm-reg-get state (vm-swc-char-reg inst))
                 (vm-reg-get state (vm-swc-stream-reg inst)))
  (values (1+ pc) nil nil))

;; FR-602: Bivalent streams are provided by io-instructions/io-predicates as
;; vm-read-byte and vm-write-byte.  Keep this file focused on stream-object
;; instructions to avoid redefining those existing instruction structures.

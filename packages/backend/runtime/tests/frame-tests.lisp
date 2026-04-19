;;;; tests/unit/runtime/frame-tests.lisp - vm-frame and Frame Pool Tests
;;;
;;; Tests for the fixed-size register array, frame pool acquire/release,
;;; register read/write, and frame-reset.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------

(defsuite frame-suite
  :description "vm-frame register file and frame pool tests"
  :parent cl-cc-unit-suite)

(in-suite frame-suite)

;;; ------------------------------------------------------------
;;; Frame construction
;;; ------------------------------------------------------------

(deftest frame-pool-acquire-cases
  "Freshly acquired frame is vm-frame-p; all 256 registers are +val-nil+; sp and pc are 0."
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (assert-true (cl-cc/runtime:vm-frame-p f)))
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (dotimes (i 256)
      (assert-= cl-cc/runtime:+val-nil+
                (cl-cc/runtime:frame-reg-get f i))))
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (assert-= 0 (cl-cc/runtime:vm-frame-sp f))
    (assert-= 0 (cl-cc/runtime:vm-frame-pc f))))

;;; ------------------------------------------------------------
;;; Register read/write
;;; ------------------------------------------------------------

(deftest frame-reg-set-and-get
  "frame-reg-set stores a value; frame-reg-get retrieves it."
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (cl-cc/runtime:frame-reg-set f 0 (cl-cc/runtime:encode-fixnum 42))
    (assert-= 42 (cl-cc/runtime:decode-fixnum (cl-cc/runtime:frame-reg-get f 0)))))

(deftest frame-reg-set-all-256
  "All 256 registers can be written and read independently."
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (dotimes (i 256)
      (cl-cc/runtime:frame-reg-set f i (cl-cc/runtime:encode-fixnum i)))
    (dotimes (i 256)
      (assert-= i (cl-cc/runtime:decode-fixnum (cl-cc/runtime:frame-reg-get f i))))))

(deftest frame-reg-edge-cases
  "frame-reg-set returns value written; overwrites correctly; unwritten regs are +val-nil+."
  (cl-cc/runtime:initialize-frame-pool)
  (let* ((f   (cl-cc/runtime:frame-pool-acquire))
         (val (cl-cc/runtime:encode-fixnum 7))
         (ret (cl-cc/runtime:frame-reg-set f 3 val)))
    (assert-= val ret))
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (cl-cc/runtime:frame-reg-set f 5 (cl-cc/runtime:encode-fixnum 100))
    (cl-cc/runtime:frame-reg-set f 5 (cl-cc/runtime:encode-fixnum 200))
    (assert-= 200 (cl-cc/runtime:decode-fixnum (cl-cc/runtime:frame-reg-get f 5))))
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (assert-= cl-cc/runtime:+val-nil+ (cl-cc/runtime:frame-reg-get f 255))))

;;; ------------------------------------------------------------
;;; frame-reset
;;; ------------------------------------------------------------

(deftest frame-reset-behavior
  "frame-reset: clears all registers to +val-nil+; zeroes pc/sp; returns frame."
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    ;; fill registers, then reset
    (dotimes (i 256)
      (cl-cc/runtime:frame-reg-set f i (cl-cc/runtime:encode-fixnum i)))
    (setf (cl-cc/runtime:vm-frame-pc f) 99
          (cl-cc/runtime:vm-frame-sp f) 42)
    (let ((ret (cl-cc/runtime:frame-reset f)))
      ;; returns the frame
      (assert-equal f ret)
      ;; all registers cleared
      (dotimes (i 256)
        (assert-= cl-cc/runtime:+val-nil+ (cl-cc/runtime:frame-reg-get f i)))
      ;; pc and sp zeroed
      (assert-= 0 (cl-cc/runtime:vm-frame-pc f))
      (assert-= 0 (cl-cc/runtime:vm-frame-sp f)))))

;;; ------------------------------------------------------------
;;; frame-pool-release
;;; ------------------------------------------------------------

(deftest frame-pool-release-clears-all-fields
  "A released-then-acquired frame has all registers as +val-nil+ and pc/sp zeroed."
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (cl-cc/runtime:frame-reg-set f 10 (cl-cc/runtime:encode-fixnum 999))
    (cl-cc/runtime:frame-pool-release f)
    ;; Acquire a frame; it may be the same one just released.
    (let ((f2 (cl-cc/runtime:frame-pool-acquire)))
      (assert-= cl-cc/runtime:+val-nil+ (cl-cc/runtime:frame-reg-get f2 10))))
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (setf (cl-cc/runtime:vm-frame-pc f) 5
          (cl-cc/runtime:vm-frame-sp f) 3)
    (cl-cc/runtime:frame-pool-release f)
    (let ((f2 (cl-cc/runtime:frame-pool-acquire)))
      (assert-= 0 (cl-cc/runtime:vm-frame-pc f2))
      (assert-= 0 (cl-cc/runtime:vm-frame-sp f2)))))

;;; ------------------------------------------------------------
;;; Frame register count constant
;;; ------------------------------------------------------------

(deftest frame-register-range-cases
  "Register count is 256; arg range within caller-save; spill region above callee-save."
  (assert-= 256 cl-cc/runtime:+frame-register-count+)
  (assert-true (<= cl-cc/runtime:+frame-arg-start+
                   cl-cc/runtime:+frame-arg-end+
                   cl-cc/runtime:+frame-caller-save-end+))
  (assert-true (> cl-cc/runtime:+frame-spill-start+
                  cl-cc/runtime:+frame-callee-save-end+)))

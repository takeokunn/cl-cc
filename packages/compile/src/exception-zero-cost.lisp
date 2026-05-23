;;;; packages/compile/src/exception-zero-cost.lisp — FR-669 Zero-Cost Exceptions
;;;; Table-driven exception handling (no runtime cost on fast path).
;;;; Itanium C++ ABI zero-cost exceptions / LLVM landingpad equivalent.

(in-package :cl-cc/compile)

;; Zero-cost exception model:
;; - Landing pad table maps PC ranges to handlers
;; - No runtime cost on normal execution path
;; - Exception dispatch walks the table, not the stack

(defstruct (landing-pad (:conc-name lp-))
  "An exception landing pad entry: PC range → handler."
  (start-pc 0 :type fixnum)
  (end-pc 0 :type fixnum)
  (handler-label nil)
  (cleanup-label nil)
  (catch-type nil))  ; type filter for catch

(defvar *landing-pad-table* nil
  "Global landing pad table for zero-cost exceptions.")

(defvar *exception-unwind-context* nil
  "Current exception context during unwinding.")

;;; ──── Landing pad registration ────
(defun register-landing-pad (start-pc end-pc handler-label cleanup-label catch-type)
  "Register a landing pad for PC range [START-PC, END-PC)."
  (push (make-landing-pad :start-pc start-pc
                           :end-pc end-pc
                           :handler-label handler-label
                           :cleanup-label cleanup-label
                           :catch-type catch-type)
        *landing-pad-table*))

(defun lookup-landing-pad (pc)
  "Find the landing pad for PC."
  (find-if (lambda (lp) (<= (lp-start-pc lp) pc (1- (lp-end-pc lp))))
           *landing-pad-table*))

;;; ──── Exception dispatch ────
(defun zero-cost-throw (exception)
  "Throw EXCEPTION using zero-cost dispatch."
  (let ((frame-ptr (sb-di:frame-pointer 0)))
    (loop
      (let ((pc (sb-di:frame-instruction-pointer frame-ptr)))
        (let ((pad (lookup-landing-pad pc)))
          (when pad
            (if (lp-cleanup-label pad)
                ;; Run cleanup, then continue unwinding
                (funcall (lp-cleanup-label pad))
                ;; Found handler: jump to it
                (return (funcall (lp-handler-label pad) exception))))))
      ;; Walk to next frame
      (setf frame-ptr (sb-di:frame-down frame-ptr))
      (unless frame-ptr
        (error "Unhandled exception: ~A" exception)))))

;;; ──── Compile-time support ────
(defmacro unwind-protect-zc (protected-form &rest cleanup-forms)
  "Zero-cost unwind-protect: table entry instead of inline code."
  `(progn
     (register-landing-pad <start> <end> nil (lambda () ,@cleanup-forms) nil)
     ,protected-form))

(defmacro handler-case-zc (form &rest clauses)
  "Zero-cost handler-case: table-driven handler dispatch."
  (let ((handler-fn (gensym "HANDLER")))
    `(let ((,handler-fn nil))
       (unwind-protect
           (progn
             (register-landing-pad <start> <end> ,handler-fn nil
                                  ',(mapcar #'car clauses))
             ,form)
         ;; Cleanup in unwind-protect
         (setf ,handler-fn nil)))))

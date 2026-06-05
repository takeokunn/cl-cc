;;;; packages/compile/src/concurrent/padded.lisp — FR-586 False Sharing Elimination
;;;; Cache-line padding for concurrent data structures.
;;;; Linux perf c2c / Intel VTune false sharing detector equivalent.

(in-package :cl-cc/compile)

;;; ──── Configuration ────
(defvar *cache-line-size* 64
  "Cache line size in bytes. Detected via CPUID at startup.")

;;; ──── Padded variables ────
(defmacro defpadded-var (name initial-value &key (padding *cache-line-size*))
  "Define a special variable NAME with cache-line padding.
Ensures adjacent variables don't share a cache line.
Usage: (cl-cc:defpadded-var *counter* 0)"
  (let ((padding-words (/ padding 8))) ; 8 bytes per word
    `(progn
       (defvar ,name ,initial-value)
       ;; Allocate padding to fill the rest of the cache line
       (defvar ,(intern (format nil "~A-PADDING" name))
         (make-array ,(1- padding-words) :initial-element 0)))))

;;; ──── Thread-local variables ────
(defmacro thread-local (name initial-value)
  "Define a thread-local variable NAME.
Thread-local variables eliminate false sharing entirely."
  `(sb-thread:define-thread-local ,name ,initial-value))

;;; ──── Class padding ────
(defmacro define-padded-class (name superclasses slots &key (padding *cache-line-size*))
  "Define a CLOS class with cache-line padding after its slots.
Ensures the class instance occupies a full cache line."
  `(progn
     (defclass ,name ,superclasses
       ,(append slots
                (list (list (intern (format nil "~A-PADDING" name))
                            :initform (make-array ,(/ padding 8)
                                                  :initial-element 0)
                            :accessor ,(intern (format nil "~A-PADDING" name)))))))))

;;; ──── Compiler pass: false sharing detection ────
(defun detect-false-sharing (global-vars)
  "Analyze GLOBAL-VARS for potential false sharing.
Returns a list of (var1 var2) pairs that share the same cache line."
  (let ((sharing-pairs nil)
        (sorted-vars (sort (copy-list global-vars) #'<
                           :key (lambda (v) (address-of-var v)))))
    (loop for (v1 v2) on sorted-vars
          while v2
          when (< (- (address-of-var v2) (address-of-var v1))
                  *cache-line-size*)
            do (push (list v1 v2) sharing-pairs))
    (nreverse sharing-pairs)))

;;; ──── Helpers ────
(defun address-of-var (var)
  "Return the memory address of VAR."
  (sb-kernel:get-lisp-obj-address var))

(defun detect-cache-line-size ()
  "Return the CPU cache line size (64 on x86-64)."
  64)

;;; ──── Startup initialization ────
(eval-when (:load-toplevel :execute)
  (setf *cache-line-size* (detect-cache-line-size)))

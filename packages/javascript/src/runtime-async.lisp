;;;; packages/javascript/src/runtime-async.lisp — JS Console, Promise, Generator
;;;;
;;;; Simplified synchronous model for Promise and Generator.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Console
;;; -----------------------------------------------------------------------

(defun %js-console-log (&rest args)
  (format t "~{~A~^ ~}~%" (mapcar #'%js-to-string args))
  +js-undefined+)

(defun %js-console-error (&rest args)
  (format *error-output* "~{~A~^ ~}~%" (mapcar #'%js-to-string args))
  +js-undefined+)

(defun %js-console-warn (&rest args)
  (format *error-output* "Warning: ~{~A~^ ~}~%" (mapcar #'%js-to-string args))
  +js-undefined+)

;;; Methods with no observable side-effects in our synchronous model.
(defparameter *%js-console-noop-methods*
  '("group" "groupEnd" "time" "timeEnd" "count" "countReset" "clear")
  "console methods that are no-ops (seeded as (constantly +js-undefined+)).")

(defun %js-make-console ()
  "Construct the JS `console' global object."
  (let ((obj (%js-make-object
              "log"     #'%js-console-log
              "info"    #'%js-console-log
              "debug"   #'%js-console-log
              "error"   #'%js-console-error
              "warn"    #'%js-console-warn
              "dir"     (lambda (o &rest _) (declare (ignore _))
                          (format t "~A~%" (%js-to-string o)) +js-undefined+)
              "table"   (lambda (&rest args)
                          (format t "~{~A~^ ~}~%" (mapcar #'%js-to-string args)) +js-undefined+)
              "trace"   (lambda (&rest args)
                          (format t "Trace: ~{~A~^ ~}~%" (mapcar #'%js-to-string args)) +js-undefined+)
              "assert"  (lambda (cond &rest args)
                          (unless (%js-truthy cond)
                            (format *error-output* "Assertion failed: ~{~A~^ ~}~%"
                                    (mapcar #'%js-to-string args)))
                          +js-undefined+))))
    (dolist (name *%js-console-noop-methods*)
      (setf (gethash name obj) (constantly +js-undefined+)))
    obj))

;;; -----------------------------------------------------------------------
;;;  Promise (simplified synchronous model)
;;; -----------------------------------------------------------------------

(defstruct (js-promise (:conc-name js-promise-))
  value
  settled-p
  rejected-p)

(defun %js-promise-resolve (value)
  "Create a resolved promise."
  (make-js-promise :value value :settled-p t :rejected-p nil))

(defun %js-promise-reject (reason)
  "Create a rejected promise."
  (make-js-promise :value reason :settled-p t :rejected-p t))

(defun %js-await (promise)
  "Synchronously unwrap a promise (for simplified async model)."
  (cond
    ((js-promise-p promise)
     (if (js-promise-rejected-p promise)
         (%js-throw (js-promise-value promise))
         (js-promise-value promise)))
    (t promise)))  ; non-promise passthrough

(defun %js-for-await-of (iterable body-fn)
  "Synchronous for-await-of: resolves each element through %js-await eagerly."
  (%js-for-of iterable (lambda (item)
                         (%js-funcall body-fn (%js-await item)))))

(defun %js-async (thunk)
  "Execute THUNK, wrapping result/exception in a promise.
THUNK is a VM closure so use %js-funcall (not CL:FUNCALL) to re-enter the VM."
  (handler-case
      (%js-promise-resolve (%js-funcall thunk))
    (js-exception (c)
      (%js-promise-reject (js-exception-value c)))))

;;; Call HANDLER with VALUE; propagate JS exceptions as a rejected promise.
(defun %js-promise-apply-handler (value handler)
  (handler-case
      (%js-promise-resolve (%js-funcall handler value))
    (js-exception (c) (%js-promise-reject (js-exception-value c)))))

(defun %js-promise-then (promise on-fulfilled &optional on-rejected)
  "Chain a promise through on-fulfilled / on-rejected callbacks."
  (if (js-promise-rejected-p promise)
      (if on-rejected (%js-promise-apply-handler (js-promise-value promise) on-rejected) promise)
      (if on-fulfilled (%js-promise-apply-handler (js-promise-value promise) on-fulfilled) promise)))


(defun %js-promise-finally (promise on-finally)
  "Run ON-FINALLY regardless of outcome."
  (%js-funcall on-finally)
  promise)

;;; Ensure PROMISES is a JS vector (convert from iterator/array-like if needed).
(defun %js-promises-as-vec (promises)
  (if (%js-vec-p promises) promises (%js-array-from promises)))

(defun %js-promise-all (promises)
  "Resolve all promises; reject on first rejection."
  (let ((results (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (arr (%js-promises-as-vec promises)))
    (loop for i below (length arr)
          do (vector-push-extend (%js-await (aref arr i)) results))
    (%js-promise-resolve results)))

(defun %js-promise-all-settled (promises)
  "Return array of status objects for all promises."
  (let ((results (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (arr (%js-promises-as-vec promises)))
    (loop for p across arr
          for outcome = (if (and (js-promise-p p) (js-promise-rejected-p p))
                            (%js-make-object "status" "rejected"
                                             "reason" (js-promise-value p))
                            (%js-make-object "status" "fulfilled"
                                             "value"  (if (js-promise-p p)
                                                          (js-promise-value p)
                                                          p)))
          do (vector-push-extend outcome results))
    (%js-promise-resolve results)))

(defun %js-promise-any (promises)
  "Resolve with first fulfillment; reject if all reject."
  (let ((errors (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (arr (%js-promises-as-vec promises)))
    (loop for p across arr
          do (if (and (js-promise-p p) (js-promise-rejected-p p))
                 (vector-push-extend (js-promise-value p) errors)
                 (return-from %js-promise-any
                   (%js-promise-resolve (if (js-promise-p p)
                                            (js-promise-value p)
                                            p)))))
    (%js-promise-reject
     (%js-make-object "errors" errors "message" "All promises were rejected"))))

(defun %js-promise-race (promises)
  "Return the first settled promise."
  (let ((arr (%js-promises-as-vec promises)))
    (if (zerop (length arr))
        (make-js-promise :settled-p nil :rejected-p nil :value +js-undefined+)
        (aref arr 0))))

(defun %js-promise-with-resolvers ()
  "Return object with promise, resolve, reject."
  (let* ((p (make-js-promise :settled-p nil :rejected-p nil :value +js-undefined+))
         (resolve (lambda (v)
                    (setf (js-promise-value p) v
                          (js-promise-settled-p p) t
                          (js-promise-rejected-p p) nil)))
         (reject (lambda (r)
                   (setf (js-promise-value p) r
                         (js-promise-settled-p p) t
                         (js-promise-rejected-p p) t))))
    (%js-make-object "promise" p "resolve" resolve "reject" reject)))

;;; -----------------------------------------------------------------------
;;;  Async / generator wrappers
;;; -----------------------------------------------------------------------

(defun %js-make-async (fn)
  "Wrap FN as an async function: invoking it returns a resolved/rejected promise."
  (lambda (&rest args)
    (handler-case
        (%js-promise-resolve (funcall *js-apply-fn* fn args))
      (js-exception (c)
        (%js-promise-reject (js-exception-value c))))))

(defun %js-make-async-generator (fn)
  "Simplified async generator: delegates to %js-make-async."
  (%js-make-async fn))

;;; -----------------------------------------------------------------------
;;;  Generator — eager-collection model with full iterator protocol
;;;
;;; function* bodies compile as ordinary functions; yield calls %js-yield.
;;; %js-make-generator runs the body eagerly with a dynamic collector so
;;; every yield pushes its value into a list. The returned object is a
;;; plain hash-table with "next"/"return"/"throw"/@@iterator — exactly
;;; what %js-for-of, spread, and Array.from expect.
;;; -----------------------------------------------------------------------

(defvar *%js-yield-collector* nil
  "When non-nil, a (values-reversed) cons cell owned by the active generator.")

(defun %js-yield (value)
  "Push VALUE into the active generator collector, or return undefined."
  (when *%js-yield-collector*
    (push value (car *%js-yield-collector*)))
  value)

(defun %js-yield-from (iterable)
  "yield* — drain ITERABLE into the active generator by reusing %js-for-of."
  (when *%js-yield-collector*
    (%js-for-of iterable #'%js-yield))
  +js-undefined+)

(defun %js-make-generator (body-fn)
  "Eagerly run BODY-FN collecting yields; return a JS iterator object."
  (let ((collector (list nil))            ; (yields-reversed)
        (return-val +js-undefined+))
    ;; Run the body with the collector bound.
    ;; BODY-FN is a VM closure, so use %js-funcall (not CL:FUNCALL) so the
    ;; *js-apply-fn* invoker can re-enter the VM for compiled-JS bodies.
    (let ((*%js-yield-collector* collector))
      (handler-case (setf return-val (%js-funcall body-fn))
        (t () nil)))
    ;; Build the iterator in FIFO order
    (let* ((values (reverse (car collector)))
           (remaining (list values))      ; boxed for mutation by closure
           (done-box  (list nil)))
      (let ((gen (%js-make-object
                  "next"
                  (lambda (&optional _send)
                    (declare (ignore _send))
                    (cond
                      ((car done-box)
                       (%js-make-object "value" +js-undefined+ "done" t))
                      ((null (car remaining))
                       (setf (car done-box) t)
                       (%js-make-object "value" return-val "done" t))
                      (t
                       (let ((v (pop (car remaining))))
                         (%js-make-object "value" v "done" nil)))))
                  "return"
                  (lambda (&optional (val +js-undefined+))
                    (setf (car done-box) t)
                    (%js-make-object "value" val "done" t))
                  "throw"
                  (lambda (&optional err)
                    (setf (car done-box) t)
                    (%js-throw (or err +js-undefined+))))))
        ;; @@iterator + ES2025 Iterator.prototype helpers (.map/.filter/.take/etc.)
        (%js-add-iterator-helpers! gen)
        gen))))

(defun %js-generator-next (gen &optional (value +js-undefined+))
  "Advance GEN by one step (delegates to its 'next' method)."
  (let ((next-fn (and (%js-ht-p gen) (gethash "next" gen))))
    (if next-fn
        (%js-funcall next-fn value)
        (%js-make-object "value" +js-undefined+ "done" t))))

(defun %js-wrap-generator-body (body-fn)
  "Return a function that, when called, produces a fresh generator from BODY-FN.
The compiler inserts this wrapper for every function* declaration."
  (lambda (&rest args)
    (%js-make-generator (lambda () (apply body-fn args)))))

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
                         (funcall body-fn (%js-await item)))))

(defun %js-async (thunk)
  "Execute THUNK, wrapping result/exception in a promise."
  (handler-case
      (%js-promise-resolve (funcall thunk))
    (js-exception (c)
      (%js-promise-reject (js-exception-value c)))))

(defun %js-promise-then (promise on-fulfilled &optional on-rejected)
  "Chain a promise."
  (if (js-promise-rejected-p promise)
      (if on-rejected
          (handler-case
              (%js-promise-resolve (funcall on-rejected (js-promise-value promise)))
            (js-exception (c) (%js-promise-reject (js-exception-value c))))
          promise)
      (if on-fulfilled
          (handler-case
              (%js-promise-resolve (funcall on-fulfilled (js-promise-value promise)))
            (js-exception (c) (%js-promise-reject (js-exception-value c))))
          promise)))


(defun %js-promise-finally (promise on-finally)
  "Run ON-FINALLY regardless of outcome."
  (funcall on-finally)
  promise)

(defun %js-promise-all (promises)
  "Resolve all promises; reject on first rejection."
  (let ((results (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (arr (if (%js-vec-p promises) promises (%js-array-from promises))))
    (loop for i below (length arr)
          for p = (aref arr i)
          do (let ((resolved (%js-await p)))
               (vector-push-extend resolved results)))
    (%js-promise-resolve results)))

(defun %js-promise-all-settled (promises)
  "Return array of outcome objects for all promises."
  (let ((results (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (arr (if (%js-vec-p promises) promises (%js-array-from promises))))
    (loop for i below (length arr)
          for p = (aref arr i)
          do (let ((outcome
                    (if (and (js-promise-p p) (js-promise-rejected-p p))
                        (%js-make-object "status" "rejected"
                                         "reason" (js-promise-value p))
                        (%js-make-object "status" "fulfilled"
                                         "value"  (if (js-promise-p p)
                                                      (js-promise-value p)
                                                      p)))))
               (vector-push-extend outcome results)))
    (%js-promise-resolve results)))

(defun %js-promise-any (promises)
  "Resolve with first fulfillment; reject if all reject."
  (let ((errors (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (arr (if (%js-vec-p promises) promises (%js-array-from promises))))
    (loop for i below (length arr)
          for p = (aref arr i)
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
  (let ((arr (if (%js-vec-p promises) promises (%js-array-from promises))))
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
;;;  Generator (simplified coroutine model via CL closures)
;;; -----------------------------------------------------------------------

(defstruct (js-generator (:conc-name js-generator-))
  thunk
  done-p
  value)

(defun %js-make-generator (thunk)
  "Create a generator from a function that accepts a next-value continuation."
  (make-js-generator :thunk thunk :done-p nil :value +js-undefined+))

(defun %js-generator-next (gen &optional (value +js-undefined+))
  "Advance generator by one step."
  (if (js-generator-done-p gen)
      (%js-make-object "value" +js-undefined+ "done" t)
      (handler-case
          (let ((result (funcall (js-generator-thunk gen) value)))
            (if (and (%js-ht-p result)
                     (gethash "done" result))
                (progn
                  (setf (js-generator-done-p gen) t)
                  result)
                result))
        (js-exception (c)
          (setf (js-generator-done-p gen) t)
          (%js-throw (js-exception-value c))))))

(defun %js-yield (value)
  "Yield from a generator — placeholder; real yield requires compiler support."
  (%js-make-object "value" value "done" nil))

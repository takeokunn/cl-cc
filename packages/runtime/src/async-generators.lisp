;;;; Async iterator and async generator runtime (FR-411)

(in-package :cl-cc/runtime)

(defstruct rt-aiter
  "Runtime async iterator. NEXT-FN returns a future resolving to
two values: the next item and a DONE-P flag."
  (next-fn (lambda () (rt-async (values nil t))) :type function))

(defun %rt-resolved-future (&rest values)
  (let ((future (rt-make-future)))
    (apply #'rt-future-resolve future values)
    future))

(defun rt-aiter-next (aiter)
  "Return a future for the next item from AITER.
The future resolves to (values ITEM DONE-P)."
  (funcall (rt-aiter-next-fn aiter)))

(defun rt-aiter-from-list (items)
  "Create an async iterator over ITEMS."
  (let ((remaining (copy-list items)))
    (make-rt-aiter
     :next-fn (lambda ()
                (if remaining
                    (%rt-resolved-future (pop remaining) nil)
                    (%rt-resolved-future nil t))))))

(defstruct rt-async-generator-state
  "Mutable state backing an async generator."
  (items nil)
  (done-p nil)
  (error nil)
  (waiters nil))

(defun rt-make-async-generator ()
  "Create an empty async generator state."
  (make-rt-async-generator-state))

(defun %rt-async-generator-pop-item (gen)
  (let ((item (car (rt-async-generator-state-items gen))))
    (setf (rt-async-generator-state-items gen)
          (cdr (rt-async-generator-state-items gen)))
    item))

(defun %rt-async-generator-pop-waiter (gen)
  (let ((waiter (car (rt-async-generator-state-waiters gen))))
    (setf (rt-async-generator-state-waiters gen)
          (cdr (rt-async-generator-state-waiters gen)))
    waiter))

(defun rt-async-yield (gen item)
  "Yield ITEM from GEN. If a consumer is waiting, resolve it immediately;
otherwise queue ITEM for a later RT-ASYNC-GENERATOR-NEXT call."
  (when (rt-async-generator-state-done-p gen)
    (error "cannot yield to a completed async generator"))
  (let ((waiter (%rt-async-generator-pop-waiter gen)))
    (if waiter
        (rt-future-resolve waiter item nil)
        (setf (rt-async-generator-state-items gen)
              (nconc (rt-async-generator-state-items gen) (list item)))))
  item)

(defun rt-async-generator-close (gen)
  "Mark GEN complete and resolve all pending waiters with DONE-P true."
  (setf (rt-async-generator-state-done-p gen) t)
  (loop for waiter = (%rt-async-generator-pop-waiter gen)
        while waiter
        do (rt-future-resolve waiter nil t))
  t)

(defun rt-async-generator-fail (gen condition)
  "Record CONDITION as GEN's terminal error and resolve waiters as done."
  (setf (rt-async-generator-state-error gen) condition
        (rt-async-generator-state-done-p gen) t)
  (loop for waiter = (%rt-async-generator-pop-waiter gen)
        while waiter
        do (rt-future-resolve waiter nil t))
  condition)

(defun rt-async-generator-next (gen)
  "Return a future resolving to the next generated item and DONE-P."
  (let ((future (rt-make-future)))
    (cond
      ((rt-async-generator-state-error gen)
       (error (rt-async-generator-state-error gen)))
      ((rt-async-generator-state-items gen)
       (rt-future-resolve future (%rt-async-generator-pop-item gen) nil))
      ((rt-async-generator-state-done-p gen)
       (rt-future-resolve future nil t))
      (t
       (setf (rt-async-generator-state-waiters gen)
             (nconc (rt-async-generator-state-waiters gen) (list future)))))
    future))

(defun rt-aiter-map (aiter fn)
  "Return an async iterator that applies FN to each item from AITER."
  (make-rt-aiter
   :next-fn (lambda ()
              (multiple-value-bind (item done-p) (rt-await (rt-aiter-next aiter))
                (if done-p
                    (%rt-resolved-future nil t)
                    (%rt-resolved-future (funcall fn item) nil))))))

(defun rt-aiter-filter (aiter pred)
  "Return an async iterator containing only items satisfying PRED."
  (labels ((next-matching ()
             (loop
               (multiple-value-bind (item done-p) (rt-await (rt-aiter-next aiter))
                 (when done-p
                   (return (%rt-resolved-future nil t)))
                 (when (funcall pred item)
                   (return (%rt-resolved-future item nil)))))))
    (make-rt-aiter :next-fn #'next-matching)))

(defun rt-aiter-take (aiter n)
  "Return an async iterator for the first N items from AITER."
  (let ((remaining (max 0 n)))
    (make-rt-aiter
     :next-fn (lambda ()
                (if (<= remaining 0)
                    (%rt-resolved-future nil t)
                    (multiple-value-bind (item done-p) (rt-await (rt-aiter-next aiter))
                      (if done-p
                          (%rt-resolved-future nil t)
                          (progn
                            (decf remaining)
                            (%rt-resolved-future item nil)))))))))

(defun rt-aiter-collect (aiter)
  "Return a future resolving to a list of all items from AITER."
  (let ((items nil))
    (loop
      (multiple-value-bind (item done-p) (rt-await (rt-aiter-next aiter))
        (when done-p
          (return (%rt-resolved-future (nreverse items))))
        (push item items)))))

(defmacro rt-async-for ((var aiter) &body body)
  "Await each item from AITER, bind it to VAR, and evaluate BODY."
  (let ((aiter-var (gensym "AITER"))
        (done-var (gensym "DONE")))
    `(let ((,aiter-var ,aiter))
       (loop
         (multiple-value-bind (,var ,done-var)
             (rt-await (rt-aiter-next ,aiter-var))
           (when ,done-var
             (return nil))
           ,@body)))))

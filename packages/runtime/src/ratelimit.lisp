(in-package :cl-cc/runtime)

(defstruct rt-token-bucket
  (rate 10.0 :type real)
  (burst 10.0 :type real)
  (tokens 10.0 :type real)
  (last 0.0 :type real)
  (dropped 0 :type integer)
  (accepted 0 :type integer))

(defun rt-make-token-bucket (&key (rate 10.0) (burst 10.0))
  (make-rt-token-bucket :rate rate :burst burst :tokens burst :last (rt-gettime-monotonic)))

(defun rt-token-bucket-refill (bucket)
  (let* ((now (rt-gettime-monotonic))
         (elapsed (max 0.0 (- now (rt-token-bucket-last bucket))))
         (tokens (min (rt-token-bucket-burst bucket)
                      (+ (rt-token-bucket-tokens bucket)
                         (* elapsed (rt-token-bucket-rate bucket))))))
    (setf (rt-token-bucket-tokens bucket) tokens
          (rt-token-bucket-last bucket) now)
    tokens))

(defun rt-token-bucket-try-acquire (bucket &optional (n 1))
  (rt-token-bucket-refill bucket)
  (if (>= (rt-token-bucket-tokens bucket) n)
      (progn
        (decf (rt-token-bucket-tokens bucket) n)
        (incf (rt-token-bucket-accepted bucket))
        t)
      (progn
        (incf (rt-token-bucket-dropped bucket))
        nil)))

(defun rt-token-bucket-acquire (bucket &optional (n 1))
  (rt-token-bucket-try-acquire bucket n))

(defun rt-token-bucket-wait (bucket &optional (n 1))
  (loop until (rt-token-bucket-try-acquire bucket n)
        do (sleep (max 0.001 (/ n (max 0.001 (rt-token-bucket-rate bucket))))))
  t)

(defun rt-token-bucket-stats (bucket)
  (list :tokens (rt-token-bucket-tokens bucket)
        :accepted (rt-token-bucket-accepted bucket)
        :dropped (rt-token-bucket-dropped bucket)))

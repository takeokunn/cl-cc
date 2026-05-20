(in-package :cl-cc/runtime)
(defstruct rt-token-bucket (rate 10.0) (burst 10.0) (tokens 10.0) (last 0))
(defun rt-make-token-bucket (&key (rate 10.0) (burst 10.0)) (make-rt-token-bucket :rate rate :burst burst :tokens burst :last (get-internal-real-time)))
(defun rt-token-bucket-acquire (tb &optional (n 1)) (let* ((now (get-internal-real-time)) (elapsed (/ (- now (rt-token-bucket-last tb)) internal-time-units-per-second)) (new (min (rt-token-bucket-burst tb) (+ (rt-token-bucket-tokens tb) (* elapsed (rt-token-bucket-rate tb)))))) (setf (rt-token-bucket-tokens tb) new (rt-token-bucket-last tb) now) (when (>= new n) (decf (rt-token-bucket-tokens tb) n) t)))

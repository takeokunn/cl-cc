(in-package :cl-cc/runtime)

(defconstant +rt-event-read+ 1)
(defconstant +rt-event-write+ 2)
(defconstant +rt-event-error+ 4)

(defstruct rt-event-registration
  (fd nil)
  (callback nil)
  (events +rt-event-read+ :type integer))

(defstruct rt-event-timer
  (deadline 0.0 :type real)
  (callback nil)
  (repeat nil))

(defstruct rt-event-loop
  (fds (make-hash-table))
  (timers nil)
  (stopped-p nil)
  (tick-interval 0.01 :type real))

(defun rt-make-event-loop (&key (tick-interval 0.01))
  (make-rt-event-loop :tick-interval tick-interval))

(defun rt-event-loop-register (loop fd cb &key (events +rt-event-read+))
  (setf (gethash fd (rt-event-loop-fds loop))
        (make-rt-event-registration :fd fd :callback cb :events events))
  fd)

(defun rt-event-loop-unregister (loop fd)
  (remhash fd (rt-event-loop-fds loop))
  t)

(defun rt-event-loop-add-timer (loop delay callback &key repeat)
  (let ((timer (make-rt-event-timer :deadline (+ (rt-gettime-monotonic) delay)
                                    :callback callback :repeat repeat)))
    (push timer (rt-event-loop-timers loop))
    timer))

(defun %rt-event-loop-run-timers (loop)
  (let ((now (rt-gettime-monotonic)) (remaining nil))
    (dolist (timer (rt-event-loop-timers loop))
      (if (<= (rt-event-timer-deadline timer) now)
          (progn
            (funcall (rt-event-timer-callback timer))
            (when (rt-event-timer-repeat timer)
              (setf (rt-event-timer-deadline timer) (+ now (rt-event-timer-repeat timer)))
              (push timer remaining)))
          (push timer remaining)))
    (setf (rt-event-loop-timers loop) (nreverse remaining))))

(defun %rt-event-loop-run-fds (loop)
  (maphash (lambda (fd reg)
             (declare (ignore fd))
             (funcall (rt-event-registration-callback reg) reg))
           (rt-event-loop-fds loop)))

(defun rt-event-loop-run-once (loop &key timeout)
  (%rt-event-loop-run-timers loop)
  (%rt-event-loop-run-fds loop)
  (sleep (or timeout (rt-event-loop-tick-interval loop)))
  t)

(defun rt-event-loop-run (loop &key timeout)
  (let ((deadline (and timeout (+ (rt-gettime-monotonic) timeout))))
    (loop until (or (rt-event-loop-stopped-p loop)
                    (and deadline (>= (rt-gettime-monotonic) deadline)))
          do (rt-event-loop-run-once loop)))
  t)

(defun rt-event-loop-stop (loop)
  (setf (rt-event-loop-stopped-p loop) t)
  t)

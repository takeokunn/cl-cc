(in-package :cl-cc/vm)

(export '(random-state
          random-state-p
          make-random-state
          *random-state*
          random
          vm-random-state
          vm-random-state-p
          vm-make-random-state
          vm-random))

(defconstant +mt19937-n+ 624)
(defconstant +mt19937-m+ 397)
(defconstant +mt19937-matrix-a+ #x9908b0df)
(defconstant +mt19937-upper-mask+ #x80000000)
(defconstant +mt19937-lower-mask+ #x7fffffff)

(defstruct (vm-random-state (:constructor %make-vm-random-state))
  "MT19937 state used by the CL-CC VM random API."
  (mt (make-array +mt19937-n+ :element-type '(unsigned-byte 32) :initial-element 0))
  (index +mt19937-n+ :type fixnum))

(deftype random-state () 'vm-random-state)

(defun random-state-p (object)
  "Return true when OBJECT is a CL-CC VM random-state."
  (vm-random-state-p object))

(defun %vm-u32 (value)
  (logand value #xffffffff))

(defun %vm-mt-seed (seed)
  (let ((state (%make-vm-random-state)))
    (setf (aref (vm-random-state-mt state) 0) (%vm-u32 seed))
    (loop for i from 1 below +mt19937-n+
          for prev = (aref (vm-random-state-mt state) (1- i))
          do (setf (aref (vm-random-state-mt state) i)
                   (%vm-u32 (+ (* 1812433253 (logxor prev (ash prev -30))) i))))
    state))

(defun %vm-mt-copy (source)
  (let ((copy (%make-vm-random-state)))
    (replace (vm-random-state-mt copy) (vm-random-state-mt source))
    (setf (vm-random-state-index copy) (vm-random-state-index source))
    copy))

(defun %vm-mt-twist (state)
  (let ((mt (vm-random-state-mt state)))
    (loop for i from 0 below +mt19937-n+
          for x = (logior (logand (aref mt i) +mt19937-upper-mask+)
                          (logand (aref mt (mod (1+ i) +mt19937-n+)) +mt19937-lower-mask+))
          for xa = (ash x -1)
          do (when (oddp x) (setf xa (logxor xa +mt19937-matrix-a+)))
             (setf (aref mt i) (%vm-u32 (logxor (aref mt (mod (+ i +mt19937-m+) +mt19937-n+)) xa)))))
  (setf (vm-random-state-index state) 0))

(defun vm-random-state-next-u32 (state)
  "Return the next tempered 32-bit MT19937 word from STATE."
  (when (>= (vm-random-state-index state) +mt19937-n+)
    (%vm-mt-twist state))
  (let* ((mt (vm-random-state-mt state))
         (y (aref mt (vm-random-state-index state))))
    (incf (vm-random-state-index state))
    (setf y (logxor y (ash y -11)))
    (setf y (logxor y (logand (ash y 7) #x9d2c5680)))
    (setf y (logxor y (logand (ash y 15) #xefc60000)))
    (%vm-u32 (logxor y (ash y -18)))))

(defparameter *random-state* (%vm-mt-seed 5489)
  "Default VM random state used by RANDOM.")

(defparameter *vm-random-state* *random-state*
  "Compatibility alias for the default VM random state.")

(defun %vm-random-time-seed ()
  (%vm-u32 (logxor (get-universal-time)
                   (get-internal-real-time)
                   (sxhash (machine-instance)))))

(defun make-random-state (&optional state)
  "Return a VM random state.
NIL copies *RANDOM-STATE*, T creates a fresh time-seeded state, an existing
random-state is copied, and an integer seed is accepted as a VM extension."
  (cond ((null state) (%vm-mt-copy *random-state*))
        ((eq state t) (%vm-mt-seed (%vm-random-time-seed)))
        ((vm-random-state-p state) (%vm-mt-copy state))
        ((integerp state) (%vm-mt-seed state))
        (t (error "Invalid random state designator: ~S" state))))

(defun %vm-random-unit (state)
  (/ (vm-random-state-next-u32 state) 4294967296.0d0))

(defun random (limit &optional (state *random-state*))
  "Return a pseudo-random number less than LIMIT using STATE.
Integer LIMIT returns an integer in [0,LIMIT). Float LIMIT returns a float in
[0,LIMIT) with the same float format as LIMIT."
  (check-type state random-state)
  (cond ((and (integerp limit) (plusp limit))
         (mod (vm-random-state-next-u32 state) limit))
        ((and (floatp limit) (plusp limit))
         (coerce (* (coerce limit 'double-float) (%vm-random-unit state))
                 (type-of limit)))
        (t (error "Invalid random limit: ~S" limit))))

(defun vm-make-random-state (&optional state)
  "Compatibility wrapper for MAKE-RANDOM-STATE."
  (make-random-state state))

(defun vm-random (limit &optional (state *random-state*))
  "Compatibility wrapper for RANDOM."
  (random limit state))

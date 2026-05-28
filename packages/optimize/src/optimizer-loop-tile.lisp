;;;; optimizer-loop-tile.lisp — FR-515 loop tiling / blocking

(in-package :cl-cc/optimize)

(defparameter *opt-loop-tile-default-l1-elements* 32
  "Default FR-515 L1 tile extent used after a cache-size probe succeeds.")

(defparameter *opt-loop-tile-default-l2-elements* 256
  "Default FR-515 L2 tile extent used after a cache-size probe succeeds.")

(defvar *opt-loop-tile-cache-info* :unprobed
  "Cached plist (:l1 BYTES :l2 BYTES) or NIL when cache probing failed.")

(defun %loop-tile-trim-newline (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun %loop-tile-run-program-line (program args)
  "Return PROGRAM output as a trimmed string, or NIL on failure."
  (handler-case
      (let ((out (make-string-output-stream)))
        (let ((proc #+sbcl (sb-ext:run-program program args :output out :error nil :search t)
                    #-sbcl nil))
          (when #+sbcl (zerop (sb-ext:process-exit-code proc))
                #-sbcl nil
            (%loop-tile-trim-newline (get-output-stream-string out)))))
    (error () nil)))

(defun %loop-tile-parse-positive-integer (string)
  (when (and string (plusp (length string)))
    (handler-case
        (let ((value (parse-integer string :junk-allowed t)))
          (and (plusp value) value))
      (error () nil))))

(defun %loop-tile-read-positive-integer-file (path)
  (handler-case
      (with-open-file (s path :direction :input)
        (%loop-tile-parse-positive-integer (read-line s nil nil)))
    (error () nil)))

(defun %loop-tile-cache-size-sysctl (name)
  (%loop-tile-parse-positive-integer
   (%loop-tile-run-program-line "sysctl" (list "-n" name))))

(defun %loop-tile-probe-cache-info ()
  "Return cache-size plist from CPUID/sysctl-style OS interfaces, or NIL.

On Darwin this uses sysctl hw.l1dcachesize/hw.l2cachesize.  On Linux it reads
the sysfs cache-size files exposed from CPUID/firmware.  NIL deliberately means
unknown and prevents FR-515 from transforming loops."
  (let* ((darwin-l1 (%loop-tile-cache-size-sysctl "hw.l1dcachesize"))
         (darwin-l2 (%loop-tile-cache-size-sysctl "hw.l2cachesize"))
         (linux-l1 (%loop-tile-read-positive-integer-file
                    "/sys/devices/system/cpu/cpu0/cache/index0/size"))
         (linux-l2 (%loop-tile-read-positive-integer-file
                    "/sys/devices/system/cpu/cpu0/cache/index2/size"))
         ;; Linux cache files may be reported in KiB on common kernels.
         (l1 (or darwin-l1 (and linux-l1 (if (< linux-l1 4096) (* linux-l1 1024) linux-l1))))
         (l2 (or darwin-l2 (and linux-l2 (if (< linux-l2 4096) (* linux-l2 1024) linux-l2)))))
    (and l1 l2 (list :l1 l1 :l2 l2))))

(defun opt-loop-cache-info ()
  "Return probed cache information for FR-515, or NIL when unknown."
  (when (eq *opt-loop-tile-cache-info* :unprobed)
    (setf *opt-loop-tile-cache-info* (%loop-tile-probe-cache-info)))
  *opt-loop-tile-cache-info*)

(defun opt-loop-tile-sizes (&optional (cache-info (opt-loop-cache-info)))
  "Return (values L1-ELEMENTS L2-ELEMENTS) when CACHE-INFO is known.

The element counts are conservative defaults bounded by common L1/L2 capacities:
32 for L1-sized tiles and 256 for L2-sized blocking."
  (when cache-info
    (let* ((l1 (getf cache-info :l1))
           (l2 (getf cache-info :l2))
           (l1-elems (and l1 (max 8 (min *opt-loop-tile-default-l1-elements*
                                         (floor l1 1024)))))
           (l2-elems (and l2 (max l1-elems (min *opt-loop-tile-default-l2-elements*
                                                (floor l2 1024))))))
      (values l1-elems l2-elems))))

(defun %loop-tile-copy-inst (inst)
  (handler-case
      (sexp->instruction (instruction->sexp inst))
    (error () inst)))

(defun %loop-tile-fresh-label (base suffix tile-size)
  (intern (format nil "~A__TILE_~A_~D" base suffix tile-size) :keyword))

(defun %loop-tile-find-inner-loop (vec outer)
  "Return the first canonical inner loop wholly contained in OUTER, if any."
  (loop for i from (+ (opt-loop-head-index outer) 3) below (opt-loop-back-index outer)
        for lp = (%loop-fr514-parse-canonical-loop-at vec i)
        when (and lp (< (opt-loop-exit-index lp) (opt-loop-back-index outer)))
          do (return lp)))

(defun %loop-tile-affine-body-p (instructions lp)
  "Return T when LP has matrix/convolution-like affine array accesses."
  (multiple-value-bind (const-env def-env)
      (%loop-fr514-build-envs instructions (opt-loop-head-index lp))
    (multiple-value-bind (core _step) (%loop-fr514-core-and-step lp)
      (declare (ignore _step))
      (some (lambda (inst)
              (%loop-fr514-affine-access inst (opt-loop-iv-reg lp) const-env def-env))
            core))))

(defun %loop-tile-speed3-policy-p (instructions)
  "Best-effort detection for frontend `(declare (optimize (speed 3)))` markers."
  (some (lambda (inst)
          (handler-case
              (let ((sexp (instruction->sexp inst)))
                (and (consp sexp)
                     (search "SPEED" (string-upcase (prin1-to-string sexp)))
                     (search "3" (prin1-to-string sexp))))
            (error () nil)))
        instructions))

(defun %loop-tile-candidate-p (instructions vec outer l1-size l2-size)
  "Return inner loop when OUTER is a conservative 2D/3D tiling candidate."
  (declare (ignore l1-size l2-size))
  (let ((inner (%loop-tile-find-inner-loop vec outer)))
    (and inner
         (%loop-tile-affine-body-p instructions inner)
         (not (some (lambda (inst)
                      (typep inst '(or vm-call vm-tail-call vm-trampoline vm-generic-call
                                       vm-apply vm-set-global vm-slot-write)))
                    (opt-loop-body outer)))
         inner)))

(defun %loop-tile-remake-lt (cmp rhs)
  "Copy CMP as VM-LT with RHS replaced by RHS."
  (make-vm-lt :dst (vm-dst cmp) :lhs (vm-lhs cmp) :rhs rhs))

(defun %loop-tile-const-reg (name tile-size)
  (intern (format nil "~A-TILE-SIZE-~D" name tile-size) :keyword))

(defun %loop-tile-bound-reg (name tile-size)
  (intern (format nil "~A-TILE-END-~D" name tile-size) :keyword))

(defun %loop-tile-safe-2d-p (outer inner)
  "Return T when OUTER/INNER are strict two-deep loops safe to materialize."
  (and inner
       (< (opt-loop-head-index outer) (opt-loop-head-index inner))
       (< (opt-loop-exit-index inner) (opt-loop-back-index outer))
       (notany (lambda (inst)
                 (typep inst '(or vm-label vm-jump vm-jump-zero vm-ret vm-halt)))
               (butlast (opt-loop-body inner)))))

(defun %loop-tile-emit-strip-mined-2d (vec outer inner l1-size l2-size out)
  "Emit concrete 2D strip-mined loops for a canonical nested loop.

Generated shape:
  outer-tile: while i < I-limit
    i-end = min(i + L2, I-limit)
    outer-point: while i < i-end
      initialize j
      inner-tile: while j < J-limit
        j-end = min(j + L1, J-limit)
        inner-point: while j < j-end
          original inner core; j += step
      outer suffix; i += step

This preserves the original induction registers and uses VM-MIN for the bounded
tile end, so non-multiple remainders execute naturally."
  (let* ((outer-head (opt-loop-head-label outer))
         (inner-head (opt-loop-head-label inner))
         (outer-tile-head (%loop-tile-fresh-label outer-head "L2_HEAD" l2-size))
         (outer-point-head (%loop-tile-fresh-label outer-head "POINT" l2-size))
         (outer-point-exit (%loop-tile-fresh-label outer-head "POINT_DONE" l2-size))
         (inner-tile-head (%loop-tile-fresh-label inner-head "L1_HEAD" l1-size))
         (inner-point-head (%loop-tile-fresh-label inner-head "POINT" l1-size))
         (inner-point-exit (%loop-tile-fresh-label inner-head "POINT_DONE" l1-size))
         (outer-tile-reg (%loop-tile-const-reg outer-head l2-size))
         (inner-tile-reg (%loop-tile-const-reg inner-head l1-size))
         (outer-end-reg (%loop-tile-bound-reg outer-head l2-size))
         (inner-end-reg (%loop-tile-bound-reg inner-head l1-size))
         (outer-cmp (aref vec (opt-loop-cmp-index outer)))
         (inner-cmp (aref vec (opt-loop-cmp-index inner)))
         (outer-jz (aref vec (opt-loop-jz-index outer)))
         (inner-jz (aref vec (opt-loop-jz-index inner)))
         (outer-step (car (last (opt-loop-body outer))))
         (inner-step (car (last (opt-loop-body inner))))
         (outer-prefix-start (+ (opt-loop-jz-index outer) 1))
         (outer-prefix-end (1- (opt-loop-head-index inner)))
         (outer-suffix-start (1+ (opt-loop-exit-index inner)))
         (outer-suffix-end (1- (opt-loop-back-index outer))))
    (push (make-vm-const :dst outer-tile-reg :value l2-size) out)
    (push (make-vm-const :dst inner-tile-reg :value l1-size) out)
    (push (make-vm-label :name outer-tile-head) out)
    (push (%loop-tile-copy-inst outer-cmp) out)
    (push (make-vm-jump-zero :reg (vm-reg outer-jz) :label (opt-loop-exit-label outer)) out)
    (push (make-vm-add :dst outer-end-reg :lhs (opt-loop-iv-reg outer) :rhs outer-tile-reg) out)
    (push (make-vm-min :dst outer-end-reg :lhs outer-end-reg :rhs (opt-loop-limit-reg outer)) out)
    (push (make-vm-label :name outer-point-head) out)
    (push (%loop-tile-remake-lt outer-cmp outer-end-reg) out)
    (push (make-vm-jump-zero :reg (vm-reg outer-jz) :label outer-point-exit) out)
    (loop for k from outer-prefix-start to outer-prefix-end
          do (push (%loop-tile-copy-inst (aref vec k)) out))
    (push (make-vm-label :name inner-tile-head) out)
    (push (%loop-tile-copy-inst inner-cmp) out)
    (push (make-vm-jump-zero :reg (vm-reg inner-jz) :label (opt-loop-exit-label inner)) out)
    (push (make-vm-add :dst inner-end-reg :lhs (opt-loop-iv-reg inner) :rhs inner-tile-reg) out)
    (push (make-vm-min :dst inner-end-reg :lhs inner-end-reg :rhs (opt-loop-limit-reg inner)) out)
    (push (make-vm-label :name inner-point-head) out)
    (push (%loop-tile-remake-lt inner-cmp inner-end-reg) out)
    (push (make-vm-jump-zero :reg (vm-reg inner-jz) :label inner-point-exit) out)
    (multiple-value-bind (core _step) (%loop-fr514-core-and-step inner)
      (declare (ignore _step))
      (dolist (inst core) (push (%loop-tile-copy-inst inst) out)))
    (push (%loop-tile-copy-inst inner-step) out)
    (push (make-vm-jump :label inner-point-head) out)
    (push (make-vm-label :name inner-point-exit) out)
    (push (make-vm-jump :label inner-tile-head) out)
    (push (make-vm-label :name (opt-loop-exit-label inner)) out)
    (loop for k from outer-suffix-start to outer-suffix-end
          do (unless (eq (aref vec k) outer-step)
               (push (%loop-tile-copy-inst (aref vec k)) out)))
    (push (%loop-tile-copy-inst outer-step) out)
    (push (make-vm-jump :label outer-point-head) out)
    (push (make-vm-label :name outer-point-exit) out)
    (push (make-vm-jump :label outer-tile-head) out)
    (push (make-vm-label :name (opt-loop-exit-label outer)) out)
    out))

(defun opt-pass-loop-tile (instructions)
  "FR-515: cache-size adaptive loop tiling/blocking for affine nested loops.

Tiling is gated on a successful L1/L2 cache-size probe; when cache information is
unknown this pass is a no-op.  For recognized 2D/3D canonical nested loops with
affine aref/aset-style access patterns, the pass emits tile-plan labels derived
from L1/L2 tile sizes and preserves the original executable loop body."
  (multiple-value-bind (l1-size l2-size) (opt-loop-tile-sizes)
    (if (not (and l1-size l2-size))
        instructions
        (let* ((vec (coerce instructions 'vector))
               (n (length vec))
               (out nil)
               (changed nil)
               (i 0)
               (speed3-p (%loop-tile-speed3-policy-p instructions)))
          (declare (ignore speed3-p))
          (loop while (< i n)
                do (let ((outer (%loop-fr514-parse-canonical-loop-at vec i)))
                     (if (null outer)
                         (progn (push (aref vec i) out) (incf i))
                         (let ((inner (%loop-tile-candidate-p instructions vec outer l1-size l2-size)))
                            (if (and inner (%loop-tile-safe-2d-p outer inner))
                                (progn
                                  (setf out (%loop-tile-emit-strip-mined-2d vec outer inner
                                                                                l1-size l2-size out)
                                        changed t
                                        i (1+ (opt-loop-exit-index outer))))
                               (progn
                                 (loop for k from (opt-loop-head-index outer)
                                       to (opt-loop-exit-index outer)
                                       do (push (aref vec k) out))
                                 (setf i (1+ (opt-loop-exit-index outer)))))))))
          (if changed (nreverse out) instructions)))))

(unless (fboundp 'opt-pass-loop-tile)
  (defun opt-pass-loop-tile (instructions)
    instructions))

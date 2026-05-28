;;;; optimizer-autotune.lisp — FR-582 auto-tuning SIMD tile sizes

(in-package :cl-cc/optimize)

(defparameter *autotune-simd-enabled* nil
  "When true, auto-select SIMD/loop-tile sizes from detected CPU cache geometry.

Default NIL preserves the existing optimizer output and the 7746/0 baseline.")

(defvar *autotune-simd-cache-info* :unprobed
  "Cached CPU cache geometry plist (:L1 bytes :L2 bytes :L3 bytes).")

(defun %autotune-trim (string)
  (and string (string-trim '(#\Space #\Tab #\Newline #\Return) string)))

(defun %autotune-run-program-line (program args)
  "Return a trimmed single-line command result, or NIL on failure."
  (handler-case
      (let ((out (make-string-output-stream)))
        (let ((proc #+sbcl (sb-ext:run-program program args :output out :error nil :search t)
                    #-sbcl nil))
          (when #+sbcl (zerop (sb-ext:process-exit-code proc))
                #-sbcl nil
            (%autotune-trim (get-output-stream-string out)))))
    (error () nil)))

(defun %autotune-parse-cache-size (value)
  "Parse VALUE as bytes, accepting Linux CPUID/sysfs suffixes K/M/G."
  (when (and value (plusp (length value)))
    (handler-case
        (let* ((trimmed (%autotune-trim value))
               (number (parse-integer trimmed :junk-allowed t))
               (suffix (and (plusp (length trimmed))
                            (char-upcase (char trimmed (1- (length trimmed)))))))
          (when (plusp number)
            (case suffix
              (#\K (* number 1024))
              (#\M (* number 1024 1024))
              (#\G (* number 1024 1024 1024))
              (otherwise (if (< number 4096) (* number 1024) number)))))
      (error () nil))))

(defun %autotune-read-cache-file (path)
  (handler-case
      (with-open-file (stream path :direction :input :if-does-not-exist nil)
        (and stream (%autotune-parse-cache-size (read-line stream nil nil))))
    (error () nil)))

(defun %autotune-sysctl-size (name)
  (%autotune-parse-cache-size
   (%autotune-run-program-line "sysctl" (list "-n" name))))

(defun %autotune-detect-cache-info ()
  "Detect CPU cache geometry via sysctl on macOS/ARM or CPUID/sysfs on x86 Linux."
  (let* ((darwin-l1 (%autotune-sysctl-size "hw.l1dcachesize"))
         (darwin-l2 (%autotune-sysctl-size "hw.l2cachesize"))
         (darwin-l3 (%autotune-sysctl-size "hw.l3cachesize"))
         ;; Linux sysfs cache index data is populated from CPUID/firmware.
         (linux-l1 (%autotune-read-cache-file "/sys/devices/system/cpu/cpu0/cache/index0/size"))
         (linux-l2 (%autotune-read-cache-file "/sys/devices/system/cpu/cpu0/cache/index2/size"))
         (linux-l3 (%autotune-read-cache-file "/sys/devices/system/cpu/cpu0/cache/index3/size"))
         (l1 (or darwin-l1 linux-l1 (* 32 1024)))
         (l2 (or darwin-l2 linux-l2 (* 256 1024)))
         (l3 (or darwin-l3 linux-l3 (* 8 1024 1024))))
    (list :l1 l1 :l2 l2 :l3 l3
          :source (cond ((or darwin-l1 darwin-l2 darwin-l3) :sysctl)
                        ((or linux-l1 linux-l2 linux-l3) :cpuid-sysfs)
                        (t :default)))))

(defun autotune-simd-cache-info ()
  "Return cached FR-582 cache geometry."
  (when (eq *autotune-simd-cache-info* :unprobed)
    (setf *autotune-simd-cache-info* (%autotune-detect-cache-info)))
  *autotune-simd-cache-info*)

(defun autotune-simd-tile-sizes (&optional (cache-info (autotune-simd-cache-info)))
  "Return three tile sizes derived from CACHE-INFO.

Defaults and thresholds follow FR-582: L1=32KiB -> 32, L2=256KiB -> 256,
L3=8MiB -> 512.  Larger caches keep the same conservative upper bounds."
  (let ((l1 (getf cache-info :l1))
        (l2 (getf cache-info :l2))
        (l3 (getf cache-info :l3)))
    (values (if (and l1 (>= l1 (* 32 1024))) 32 16)
            (if (and l2 (>= l2 (* 256 1024))) 256 128)
            (if (and l3 (>= l3 (* 8 1024 1024))) 512 256))))

(defun %autotune-clone-simd-op (inst lanes)
  "Clone SIMD marker INST with LANES as the chosen tile extent."
  (make-vm-simd-vector-op :op (vm-simd-vector-op-op inst)
                          :dst-array (vm-simd-vector-op-dst-array inst)
                          :lhs-array (vm-simd-vector-op-lhs-array inst)
                          :rhs-array (vm-simd-vector-op-rhs-array inst)
                          :index-reg (vm-simd-vector-op-index-reg inst)
                          :lanes lanes
                          :element-type (vm-simd-vector-op-element-type inst)))

(defun opt-pass-autotune-simd (instructions)
  "FR-582: auto-tune SIMD tile sizes from CPU cache geometry.

The pass derives L1/L2/L3 tile sizes, runs the existing loop-tiling pass under
those defaults when available, and retunes SIMD vector markers to the L1 tile.
With *AUTOTUNE-SIMD-ENABLED* NIL this is a no-op."
  (if (not *autotune-simd-enabled*)
      instructions
      (multiple-value-bind (l1-tile l2-tile l3-tile) (autotune-simd-tile-sizes)
        (declare (ignore l3-tile))
        (let* ((tiled (if (fboundp 'opt-pass-loop-tile)
                          (let ((*opt-loop-tile-default-l1-elements* l1-tile)
                                (*opt-loop-tile-default-l2-elements* l2-tile))
                            (opt-pass-loop-tile instructions))
                          instructions))
               (changed nil)
               (out (mapcar (lambda (inst)
                              (if (typep inst 'vm-simd-vector-op)
                                  (progn
                                    (setf changed t)
                                    (%autotune-clone-simd-op inst l1-tile))
                                  inst))
                            tiled)))
          (if changed out tiled)))))

(unless (fboundp 'opt-pass-autotune-simd)
  (defun opt-pass-autotune-simd (instructions)
    instructions))

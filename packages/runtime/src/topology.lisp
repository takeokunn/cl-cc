(in-package :cl-cc/runtime)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and sbcl linux) (require :sb-alien))

#+(and sbcl linux)
(sb-alien:define-alien-routine ("sched_getaffinity" %rt-sched-getaffinity) sb-alien:int
  (pid sb-alien:int)
  (cpusetsize sb-alien:unsigned-long)
  (mask (* sb-alien:unsigned-char)))

#+(and sbcl linux)
(sb-alien:define-alien-routine ("sched_setaffinity" %rt-sched-setaffinity) sb-alien:int
  (pid sb-alien:int)
  (cpusetsize sb-alien:unsigned-long)
  (mask (* sb-alien:unsigned-char)))

(defconstant +rt-affinity-mask-bytes+ 128
  "Bytes reserved for Linux cpu_set_t-compatible affinity masks.")

(defvar *rt-detected-cpu-cores* nil
  "Cached runtime-visible CPU core count for this process.")

(defvar *rt-detected-numa-topology* nil
  "Cached NUMA topology for this process.")

(defun %rt-file-lines (path)
  (ignore-errors
    (with-open-file (stream path :direction :input :if-does-not-exist nil)
      (when stream
        (loop for line = (read-line stream nil nil)
              while line
              collect line)))))

(defun %rt-file-string (path)
  (let ((lines (%rt-file-lines path)))
    (when lines
      (format nil "~{~a~^~%~}" lines))))

(defun %rt-trim (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun %rt-split-string (string delimiter)
  (loop with start = 0
        for pos = (position delimiter string :start start)
        collect (subseq string start pos)
        while pos
        do (setf start (1+ pos))))

(defun %rt-parse-positive-integer (string)
  (ignore-errors
    (let ((value (parse-integer (%rt-trim string) :junk-allowed t)))
      (when (and value (plusp value)) value))))

#+darwin
(defun %rt-darwin-run-program-environment ()
  (list (format nil "PATH=~a"
                (or (sb-ext:posix-getenv "PATH")
                    "/usr/bin:/bin:/usr/sbin:/sbin"))))

(defun %rt-run-program-output (program args)
  (ignore-errors
    (let ((out (make-string-output-stream)))
      (let ((process (sb-ext:run-program program args
                                         :search t
                                         :output out
                                         :error nil
                                         :wait t
                                         #+darwin :environment
                                         #+darwin (%rt-darwin-run-program-environment))))
        (when (and process (zerop (sb-ext:process-exit-code process)))
          (%rt-trim (get-output-stream-string out)))))))

(defun %rt-parse-non-negative-integer (string &key start end)
  (ignore-errors
    (let ((value (parse-integer string
                                :start (or start 0)
                                :end end
                                :junk-allowed t)))
      (when (and value (<= 0 value)) value))))

(defun %rt-parse-cpu-token (token)
  (let ((cpu (%rt-parse-non-negative-integer (%rt-trim token))))
    (when cpu (list cpu))))

(defun %rt-parse-cpu-range-token (token dash)
  (let ((start (%rt-parse-non-negative-integer token :end dash))
        (end (%rt-parse-non-negative-integer token :start (1+ dash))))
    (when (and start end (<= start end))
      (loop for cpu from start to end collect cpu))))

(defun %rt-parse-cpulist-token (token)
  (let* ((piece (%rt-trim token))
         (dash (position #\- piece)))
    (cond
      ((zerop (length piece)) nil)
      (dash (%rt-parse-cpu-range-token piece dash))
      (t (%rt-parse-cpu-token piece)))))

(defun %rt-parse-cpulist (text)
  (when text
    (remove-duplicates
     (loop for part in (%rt-split-string (%rt-trim text) #\,)
           append (%rt-parse-cpulist-token part))
     :test #'eql)))

(defun %rt-format-cpulist (cpus)
  (format nil "~{~d~^,~}" (sort (copy-list cpus) #'<)))

(defun %rt-default-cpu-list ()
  (loop for cpu below (detect-cpu-cores) collect cpu))

(defun %rt-linux-cpu-count-from-proc ()
  (let ((processors 0)
        (max-id -1))
    (dolist (line (%rt-file-lines "/proc/cpuinfo"))
      (let ((colon (position #\: line)))
        (when colon
          (let ((key (%rt-trim (subseq line 0 colon)))
                (value (%rt-trim (subseq line (1+ colon)))))
            (when (string= key "processor")
              (incf processors)
              (let ((id (parse-integer value :junk-allowed t)))
                (when id (setf max-id (max max-id id)))))))))
    (cond ((plusp processors) processors)
          ((>= max-id 0) (1+ max-id))
          (t nil))))

(defun %rt-linux-cpu-count-from-getconf ()
  (%rt-parse-positive-integer
   (or (%rt-run-program-output "getconf" '("_NPROCESSORS_ONLN"))
       (%rt-run-program-output "nproc" '()))))

(defun %rt-darwin-cpu-count ()
  (%rt-parse-positive-integer
   (or (%rt-run-program-output "/usr/sbin/sysctl" '("-n" "hw.ncpu"))
       (%rt-run-program-output "sysctl" '("-n" "hw.ncpu")))))

(defun %rt-detect-cpu-cores-uncached ()
  (or #+linux (or (%rt-linux-cpu-count-from-proc)
                  (%rt-linux-cpu-count-from-getconf))
      #+darwin (%rt-darwin-cpu-count)
      1))

(defun detect-cpu-cores ()
  "Detect the number of online CPU cores visible to the runtime.

Linux hosts prefer /proc/cpuinfo and fall back to getconf/nproc. macOS hosts
query sysctl hw.ncpu. Unsupported hosts return 1. The return value is always a
positive integer. The result is cached because host topology is stable during a
compiler process and external detection can be expensive under Nix wrappers."
  (or *rt-detected-cpu-cores*
      (setf *rt-detected-cpu-cores*
            (%rt-detect-cpu-cores-uncached))))

(defun %rt-linux-node-id-from-path (pathname)
  (let* ((directory (pathname-directory pathname))
         (node-name (car (last directory))))
    (when (and node-name
               (>= (length node-name) 5)
               (string= node-name "node" :end1 4))
      (parse-integer node-name :start 4 :junk-allowed t))))

(defun %rt-linux-node-memory-bytes (node-id)
  (let ((path (format nil "/sys/devices/system/node/node~d/meminfo" node-id)))
    (loop for line in (%rt-file-lines path)
          for marker = "MemTotal:"
          when (search marker line)
            do (let* ((after (subseq line (+ (search marker line) (length marker))))
                      (kb (parse-integer after :junk-allowed t)))
                 (return (and kb (* kb 1024)))))))

(defun %rt-topology-node-id (node)
  (getf node :node-id))

(defun %rt-node-memory-bytes-or-zero (node)
  (or (getf node :memory-bytes) 0))

(defun %rt-sort-topology-nodes (nodes)
  (sort nodes #'< :key #'%rt-topology-node-id))

(defun %rt-topology-memory-bytes (nodes)
  (reduce #'+ nodes :key #'%rt-node-memory-bytes-or-zero :initial-value 0))

(defun %rt-linux-numa-from-sysfs ()
  (let ((nodes
          (loop for cpulist-path in (directory "/sys/devices/system/node/node*/cpulist")
                for node-id = (%rt-linux-node-id-from-path cpulist-path)
                for cpus = (%rt-parse-cpulist (%rt-file-string cpulist-path))
                when (and node-id cpus)
                  collect (list :node-id node-id
                                :cpus (sort cpus #'<)
                                :memory-bytes (%rt-linux-node-memory-bytes node-id)
                                :kind :dram))))
    (when nodes (%rt-sort-topology-nodes nodes))))

(defun %rt-linux-numa-from-numactl ()
  (let ((output (%rt-run-program-output "numactl" '("--hardware"))))
    (when output
      (let ((nodes nil))
        (dolist (line (%rt-split-string output #\Newline))
          (when (and (search "node " line) (search " cpus:" line))
            (let* ((tokens (remove "" (%rt-split-string (%rt-trim line) #\Space) :test #'string=))
                   (node-id (parse-integer (second tokens) :junk-allowed t))
                   (cpus (mapcan #'%rt-parse-cpu-token (cdddr tokens))))
              (when (and node-id cpus)
                (push (list :node-id node-id :cpus cpus :memory-bytes nil :kind :dram)
                      nodes)))))
        (when nodes (%rt-sort-topology-nodes nodes))))))

(defun %rt-default-numa-topology ()
  (list (list :node-id 0
              :cpus (%rt-default-cpu-list)
              :memory-bytes nil
              :kind :dram)))

(defun %rt-detect-numa-topology-uncached ()
  (or #+linux (or (%rt-linux-numa-from-sysfs)
                  (%rt-linux-numa-from-numactl))
      (%rt-default-numa-topology)))

(defun detect-numa-topology ()
  "Detect NUMA topology as a list of node property lists.

Each node contains :NODE-ID, :CPUS, :MEMORY-BYTES, and :KIND. Linux reads
/sys/devices/system/node/node*/cpulist and falls back to numactl --hardware.
macOS reports one DRAM node because Darwin exposes no NUMA topology. Unsupported
hosts return a single default node covering all detected CPUs. The detected
topology is cached and returned as a fresh tree so callers cannot mutate the
process cache."
  (copy-tree
   (or *rt-detected-numa-topology*
       (setf *rt-detected-numa-topology*
             (%rt-detect-numa-topology-uncached)))))

#+(and sbcl linux)
(defun %rt-affinity-vector->cpus (mask)
  (loop for byte-index below (length mask)
        append (loop with byte = (aref mask byte-index)
                     for bit below 8
                     for cpu = (+ (* byte-index 8) bit)
                     when (not (zerop (logand byte (ash 1 bit))))
                       collect cpu)))

#+(and sbcl linux)
(defun %rt-cpus->affinity-vector (cpus)
  (let ((mask (make-array +rt-affinity-mask-bytes+
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (dolist (cpu cpus mask)
      (when (and (integerp cpu) (<= 0 cpu) (< cpu (* +rt-affinity-mask-bytes+ 8)))
        (multiple-value-bind (byte-index bit) (floor cpu 8)
          (setf (aref mask byte-index)
                (logior (aref mask byte-index) (ash 1 bit))))))))

#+(and sbcl linux)
(defun %rt-with-affinity-alien-mask (mask function)
  (sb-alien:with-alien ((alien-mask (array sb-alien:unsigned-char #.+rt-affinity-mask-bytes+)))
    (loop for i below +rt-affinity-mask-bytes+
          do (setf (sb-alien:deref alien-mask i) (aref mask i)))
    (funcall function mask (sb-alien:addr alien-mask))))

#+(and sbcl linux)
(defun %rt-copy-affinity-mask-from-alien (mask alien-mask)
  (loop for i below +rt-affinity-mask-bytes+
        do (setf (aref mask i) (sb-alien:deref alien-mask i)))
  mask)

#+(and sbcl linux)
(defun %rt-get-affinity-with-alien-mask (mask alien-mask)
  (when (zerop (%rt-sched-getaffinity 0 +rt-affinity-mask-bytes+ alien-mask))
    (%rt-affinity-vector->cpus
     (%rt-copy-affinity-mask-from-alien mask alien-mask))))

#+(and sbcl linux)
(defun %rt-set-affinity-with-alien-mask (mask alien-mask)
  (declare (ignore mask))
  (when (zerop (%rt-sched-setaffinity 0 +rt-affinity-mask-bytes+ alien-mask))
    (get-cpu-affinity-mask)))

(defun %rt-linux-affinity-from-status ()
  (loop for line in (%rt-file-lines "/proc/self/status")
        when (search "Cpus_allowed_list:" line)
          do (let ((colon (position #\: line)))
               (return (and colon (%rt-parse-cpulist (subseq line (1+ colon))))))))

(defun get-cpu-affinity-mask ()
  "Return the current thread/process CPU affinity as a list of CPU indexes.

On SBCL/Linux this calls sched_getaffinity(2). If that is unavailable, Linux
parses /proc/self/status. macOS and unsupported hosts return NIL and issue a
warning because portable thread affinity is not exposed by the host runtime."
  #+(and sbcl linux)
  (let ((mask (make-array +rt-affinity-mask-bytes+
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (or (ignore-errors
          (%rt-with-affinity-alien-mask
           mask
           #'%rt-get-affinity-with-alien-mask))
        (%rt-linux-affinity-from-status)))
  #-(and sbcl linux)
  (progn
    (warn "CPU affinity query is not supported on this host; returning NIL.")
    nil))

(defun set-cpu-affinity-mask (cpus)
  "Set the current process CPU affinity to CPUS and return the effective mask.

CPUS is a list of zero-based CPU indexes. SBCL/Linux uses sched_setaffinity(2)
without requiring project C code. macOS and unsupported hosts return NIL and
warn because Darwin thread_policy_set affinity tags do not expose a portable
CPU bitmask compatible with this runtime API."
  (check-type cpus list)
  #+(and sbcl linux)
  (let ((mask (%rt-cpus->affinity-vector cpus)))
    (or (ignore-errors
          (%rt-with-affinity-alien-mask
           mask
           #'%rt-set-affinity-with-alien-mask))
        (progn
          (warn "sched_setaffinity failed; CPU affinity was not changed.")
          nil)))
  #-(and sbcl linux)
  (progn
    (warn "CPU affinity changes are not supported on this host; returning NIL.")
    nil))

(defun %rt-linux-memory-total-bytes ()
  (loop for line in (%rt-file-lines "/proc/meminfo")
        when (search "MemTotal:" line)
          do (let ((kb (parse-integer (subseq line (length "MemTotal:")) :junk-allowed t)))
               (return (and kb (* kb 1024))))))

(defun %rt-linux-nvme-tier-info ()
  (loop for size-path in (directory "/sys/block/nvme*/size")
        for sectors = (%rt-parse-positive-integer (%rt-file-string size-path))
        when sectors
          collect (list :tier :nvme
                        :device (let* ((dirs (pathname-directory size-path))
                                       (name (car (last dirs))))
                                  (and name (string name)))
                        :bytes (* sectors 512)
                        :source :sysfs)))

(defun %rt-hbm-line-p (line)
  (search "hbm" line :test #'char-equal))

(defun %rt-linux-hbm-present-p ()
  (or (directory "/sys/devices/system/node/node*/memory_side_cache/index*")
      (some #'%rt-hbm-line-p
            (append (%rt-file-lines "/proc/meminfo") nil))))

(defun memory-tier-info ()
  "Return detected memory/storage tiers as property lists.

The result always includes a DRAM tier when no richer information is available.
Linux reports DRAM from /proc/meminfo or NUMA node meminfo, adds an HBM marker
when HMAT/memory-side-cache hints are visible in sysfs, and reports NVMe block
devices as storage-backed tiers. macOS reports unified DRAM using sysctl-derived
host information when possible."
  (let ((tiers nil))
    #+linux
    (progn
      (push (list :tier :dram
                  :bytes (or (%rt-linux-memory-total-bytes)
                             (%rt-topology-memory-bytes (detect-numa-topology)))
                  :nodes (detect-numa-topology)
                  :source :linux)
            tiers)
      (when (%rt-linux-hbm-present-p)
        (push (list :tier :hbm :bytes nil :source :linux-hmat) tiers))
      (setf tiers (nconc (%rt-linux-nvme-tier-info) tiers)))
    #+darwin
    (push (list :tier :dram
                :bytes (%rt-parse-positive-integer
                        (or (%rt-run-program-output "/usr/sbin/sysctl" '("-n" "hw.memsize"))
                            (%rt-run-program-output "sysctl" '("-n" "hw.memsize"))))
                :nodes (detect-numa-topology)
                :source :darwin)
          tiers)
    (or tiers
        (list (list :tier :dram
                    :bytes nil
                    :nodes (detect-numa-topology)
                    :source :fallback)))))

(defun rt-cpu-topology ()
  "Return runtime CPU and NUMA topology information as a property list."
  (list :cores (detect-cpu-cores)
        :numa-nodes (detect-numa-topology)
        :memory-tiers (memory-tier-info)))

(defun rt-thread-set-affinity (core)
  "Restrict execution to CORE and return the effective CPU mask, or NIL if unsupported."
  (set-cpu-affinity-mask (list core)))

(defun rt-thread-get-affinity ()
  "Return the current CPU affinity list, or NIL if unsupported."
  (get-cpu-affinity-mask))

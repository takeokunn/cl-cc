;;;; Execute-Only Memory support (FR-772)
;;;
;;; JIT code is compiled in a private RW mapping and finalized into an executable
;;; mapping whose read permission is removed when the target can represent XOM.
;;; The implementation deliberately degrades to RX on targets without hardware
;;; support (for example x86-64 without PKU) rather than failing compilation.

(in-package :cl-cc/runtime)

(defparameter *xom-enabled* nil
  "When true, finalize JIT code pages as execute-only where the platform supports
it.  Disabled by default; unsupported hardware falls back gracefully to RX.")

(defparameter *rt-xom-pku-key* nil
  "Best-effort x86 PKU protection key reserved for execute-only code pages.")

(defstruct rt-xom-region
  "Descriptor for a JIT code mapping participating in Execute-Only Memory."
  (mmap nil)
  (size 0 :type integer)
  (finalized-p nil :type boolean)
  (read-protected-p nil :type boolean)
  (pku-key nil))

(defun rt-host-aarch64-p ()
  "Return true when the host machine is AArch64/ARM64."
  (let ((machine (string-upcase (or (machine-type) ""))))
    (or (search "AARCH64" machine)
        (search "ARM64" machine))))

(defun rt-host-x86-64-p ()
  "Return true when the host machine is x86-64/AMD64."
  (let ((machine (string-upcase (or (machine-type) ""))))
    (or (search "X86-64" machine)
        (search "X86_64" machine)
        (search "AMD64" machine))))

(defun %rt-cpu-feature-text ()
  "Return best-effort CPU feature text for capability detection."
  (or (ignore-errors
        (when (and (find-package :uiop) (fboundp 'uiop:run-program))
          (uiop:run-program '("sysctl" "-a")
                            :output :string
                            :ignore-error-status t)))
      (ignore-errors
        (with-open-file (in "/proc/cpuinfo" :direction :input)
          (let ((out (make-string-output-stream)))
            (loop for line = (read-line in nil nil)
                  while line
                  do (progn (write-string line out)
                            (write-char #\Newline out)))
            (get-output-stream-string out))))))

(defun %rt-token-present-p (text token)
  "Return true when TEXT contains TOKEN as a CPU feature token."
  (and (stringp text)
       (let ((lower (string-downcase text))
             (needle (string-downcase token)))
         (search needle lower))))

(defun rt-x86-pku-available-p ()
  "Best-effort x86-64 PKU detection.

Production native runtimes should back this with CPUID leaf 7/ECX bit 3.  The
hosted Lisp runtime has no portable CPUID primitive, so it uses visible OS CPU
feature strings plus CLCC_X86_PKU=1 as an explicit override."
  (and (rt-host-x86-64-p)
       (or (let ((env (sb-ext:posix-getenv "CLCC_X86_PKU")))
             (and env (member (string-downcase env) '("1" "true" "yes" "on")
                              :test #'string=)))
           (%rt-token-present-p (%rt-cpu-feature-text) "pku")
           (%rt-token-present-p (%rt-cpu-feature-text) "ospke"))))

(defun rt-xom-supported-p ()
  "Return true when the active platform can provide execute-only code pages."
  (or (rt-host-aarch64-p)
      (rt-x86-pku-available-p)))

(defun rt-allocate-xom-code-memory (size)
  "Allocate a writable private JIT code region.

The returned RT-XOM-REGION must be finalized with RT-FINALIZE-XOM-CODE-MEMORY
after code emission."
  (check-type size (integer 1 *))
  (make-rt-xom-region
   :mmap (rt-mmap nil size (logior +rt-prot-read+ +rt-prot-write+)
                  +rt-map-anonymous+ nil 0)
   :size (rt-page-align size)))

(defun rt-xom-effective-prot ()
  "Return the final protection mask for code pages under the current hardware."
  (if (and *xom-enabled* (rt-xom-supported-p))
      +rt-prot-exec+
      (logior +rt-prot-read+ +rt-prot-exec+)))

(defun rt-finalize-xom-code-memory (region)
  "Finalize REGION after JIT compilation.

AArch64 uses PROT_EXEC only.  x86-64 uses the same simulated protection when PKU
is available; without PKU the page is executable/readable as a graceful fallback."
  (check-type region rt-xom-region)
  (let* ((mmap (rt-xom-region-mmap region))
         (prot (rt-xom-effective-prot))
         (read-protected-p (and *xom-enabled* (= prot +rt-prot-exec+))))
    (rt-mprotect mmap (rt-xom-region-size region) prot)
    (setf (rt-xom-region-finalized-p region) t
          (rt-xom-region-read-protected-p region) read-protected-p
          (rt-xom-region-pku-key region) (and (rt-x86-pku-available-p)
                                             *rt-xom-pku-key*))
    region))

(defun rt-release-xom-code-memory (region)
  "Release an XOM JIT code region."
  (check-type region rt-xom-region)
  (rt-munmap (rt-xom-region-mmap region) (rt-xom-region-size region)))

(defun rt-allocate-code-memory-xom-aware (size)
  "Allocate code memory through the XOM path when enabled, otherwise use legacy mmap."
  (if *xom-enabled*
      (rt-allocate-xom-code-memory size)
      (rt-allocate-code-memory size)))

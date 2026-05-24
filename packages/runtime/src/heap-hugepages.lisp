;;;; packages/runtime/src/heap-hugepages.lisp — Huge page allocation helpers (FR-623)

(in-package :cl-cc/runtime)

(defconstant +rt-huge-page-size+ (* 2 1024 1024)
  "Huge page size used by cl-cc runtime huge-page probes (2MiB).")

(defconstant +rt-map-hugetlb+ #x40000
  "Linux MAP_HUGETLB flag value used on best-effort huge-page mmap attempts.")

(defconstant +rt-vm-flags-superpage-size-2mb+ #x200000
  "Darwin VM_FLAGS_SUPERPAGE_SIZE_2MB marker used as metadata in the portable backend.")

(defparameter *use-huge-pages* t
  "When true, runtime code/heap mmap helpers try huge pages before falling back.

Huge pages are opt-in and best-effort: allocation must keep working when the
host does not support MAP_HUGETLB, Darwin superpages, or any native equivalent.")

(defvar *rt-huge-pages-active-p* nil
  "True after a huge-page probe/allocation succeeds in this runtime instance.")

(defun rt-huge-page-align (size)
  "Round SIZE in bytes up to a 2MiB huge-page boundary."
  (* +rt-huge-page-size+ (ceiling size +rt-huge-page-size+)))

(defun %rt-platform-huge-page-flags (flags)
  "Return FLAGS augmented with a platform-specific huge-page request marker."
  (let ((software (string-downcase (or (ignore-errors (software-type)) ""))))
    (cond
      ((search "linux" software)
       (logior flags +rt-map-hugetlb+))
      ((or (search "darwin" software) (search "mac" software))
       (logior flags +rt-vm-flags-superpage-size-2mb+))
      (t flags))))

(defun rt-huge-page-mmap (addr length prot flags fd offset)
  "Best-effort huge-page mmap wrapper with graceful fallback to regular pages.

The portable Common Lisp backend records mmap regions as descriptors.  Native
backends can map this function to real mmap/VM_FLAGS_SUPERPAGE_SIZE_2MB calls;
here we align length and logical addresses to 2MiB, try platform flags, and fall
back to RT-MMAP if anything fails."
  (let* ((aligned-length (rt-huge-page-align length))
         (huge-flags (%rt-platform-huge-page-flags flags)))
    (when (and (boundp '*rt-mmap-next-address*)
               (integerp *rt-mmap-next-address*))
      (setf *rt-mmap-next-address*
            (rt-huge-page-align *rt-mmap-next-address*)))
    (handler-case
        (let ((region (rt-mmap addr aligned-length prot huge-flags fd offset)))
          (setf *rt-huge-pages-active-p* t)
          region)
      (error ()
        (setf *rt-huge-pages-active-p* nil)
        (rt-mmap addr (rt-page-align length) prot flags fd offset)))))

(defun try-enable-huge-pages (&key (probe-size +rt-huge-page-size+))
  "Enable best-effort huge pages and return true when a probe allocation works.

Failure is non-fatal: *USE-HUGE-PAGES* remains true so later native backends may
try again, while HUGE-PAGES-ENABLED-P reports whether the current probe/allocation
actually succeeded."
  (setf *use-huge-pages* t)
  (handler-case
      (let ((probe (rt-huge-page-mmap nil probe-size
                                      (logior +rt-prot-read+ +rt-prot-write+)
                                      +rt-map-anonymous+ nil 0)))
        (rt-munmap probe probe-size)
        (setf *rt-huge-pages-active-p* t))
    (error ()
      (setf *rt-huge-pages-active-p* nil))))

(defun huge-pages-enabled-p ()
  "Return true when huge pages are opted in and the latest probe/allocation worked."
  (and *use-huge-pages* *rt-huge-pages-active-p*))

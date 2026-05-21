;;;; Memory-Mapped I/O (FR-349, FR-351)
(in-package :cl-cc/runtime)

(defconstant +rt-page-size+ 4096)
(defconstant +rt-prot-none+ 0)
(defconstant +rt-prot-read+ 1)
(defconstant +rt-prot-write+ 2)
(defconstant +rt-prot-exec+ 4)
(defconstant +rt-map-shared+ 1)
(defconstant +rt-map-private+ 2)
(defconstant +rt-map-fixed+ 16)
(defconstant +rt-map-anonymous+ 32)

(defstruct rt-mmap-region
  (address nil)
  (length 0 :type integer)
  (prot +rt-prot-read+ :type integer)
  (flags +rt-map-private+ :type integer)
  (fd nil)
  (offset 0 :type integer)
  (buffer nil)
  (released-p nil))

(defvar *rt-mmap-registry* (make-hash-table :test #'eq))
(defvar *rt-mmap-next-address* #x100000000)
(defvar *rt-resource-limits* (make-hash-table))

(defun rt-page-align (size)
  (* +rt-page-size+ (ceiling size +rt-page-size+)))

(defun rt-valid-mmap-size-p (size)
  (and (integerp size) (plusp size)))

(defun rt-set-resource-limit (name value)
  (setf (gethash name *rt-resource-limits*) value))

(defun rt-resource-limit (name)
  (gethash name *rt-resource-limits*))

(defun %rt-make-address (length)
  (prog1 *rt-mmap-next-address*
    (incf *rt-mmap-next-address* (rt-page-align length))))

(defun rt-mmap (addr length prot flags fd offset)
  "Create a memory-backed mmap region. Native mmap can replace this descriptor later."
  (declare (ignore addr))
  (unless (rt-valid-mmap-size-p length)
    (error "Invalid mmap length: ~a" length))
  (let* ((aligned (rt-page-align length))
         (address (%rt-make-address aligned))
         (region (make-rt-mmap-region
                  :address address :length aligned :prot prot :flags flags
                  :fd fd :offset offset
                  :buffer (make-array aligned :element-type '(unsigned-byte 8)
                                             :initial-element 0))))
    (setf (gethash address *rt-mmap-registry*) region)
    region))

(defun rt-mmap-raw (addr length prot flags fd offset)
  (rt-mmap addr length prot flags fd offset))

(defun rt-munmap (region-or-address &optional length)
  (let* ((address (if (typep region-or-address 'rt-mmap-region)
                      (rt-mmap-region-address region-or-address)
                      region-or-address))
         (region (gethash address *rt-mmap-registry*)))
    (when (and region length (> length (rt-mmap-region-length region)))
      (error "munmap length ~a exceeds mapped length ~a" length (rt-mmap-region-length region)))
    (when region
      (setf (rt-mmap-region-released-p region) t)
      (remhash address *rt-mmap-registry*))
    t))

(defun rt-munmap-raw (addr length) (rt-munmap addr length))

(defun rt-mprotect (region-or-address length prot)
  (let* ((address (if (typep region-or-address 'rt-mmap-region)
                      (rt-mmap-region-address region-or-address)
                      region-or-address))
         (region (gethash address *rt-mmap-registry*)))
    (unless region (error "No mmap region at address: ~a" address))
    (when (> length (rt-mmap-region-length region))
      (error "mprotect length ~a exceeds mapped length ~a" length (rt-mmap-region-length region)))
    (setf (rt-mmap-region-prot region) prot)
    t))

(defun rt-mmap-buffer (region)
  (check-type region rt-mmap-region)
  (when (rt-mmap-region-released-p region)
    (error "mmap region released"))
  (rt-mmap-region-buffer region))

(defun rt-mmap-ref (region index)
  (aref (rt-mmap-buffer region) index))

(defun rt-mmap-set (region index value)
  (unless (not (zerop (logand (rt-mmap-region-prot region) +rt-prot-write+)))
    (error "mmap region is not writable"))
  (setf (aref (rt-mmap-buffer region) index) value))

(defun rt-allocate-code-memory (size)
  (rt-mmap nil size (logior +rt-prot-read+ +rt-prot-write+ +rt-prot-exec+)
           +rt-map-anonymous+ nil 0))

(defun rt-release-code-memory (region size)
  (rt-munmap region size))

(defun rt-allocate-anonymous-memory (size &key (prot (logior +rt-prot-read+ +rt-prot-write+)))
  (rt-mmap nil size prot +rt-map-anonymous+ nil 0))

(defun rt-mmap-init ()
  (clrhash *rt-mmap-registry*)
  (clrhash *rt-resource-limits*)
  (setf *rt-mmap-next-address* #x100000000)
  t)

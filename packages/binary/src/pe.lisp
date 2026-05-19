;;;; packages/binary/src/pe.lisp - PE/COFF Binary Format Support
;;;
;;; Implements PE32+ (64-bit) Windows executable and DLL image generation.
;;; Supports x86-64 and ARM64 machine types, DOS stub, PE/COFF headers,
;;; section layout, imports, exports, and base relocations.
;;;
;;; Pure Common Lisp implementation - no external dependencies.

(in-package :cl-cc/binary)

;;; ------------------------------------------------------------
;;; PE/COFF constants
;;; ------------------------------------------------------------

(defconstant +pe-dos-signature+ #x5a4d)      ; MZ
(defconstant +pe-signature+ #x00004550)      ; PE\0\0

(defconstant +pe-machine-amd64+ #x8664)
(defconstant +pe-machine-arm64+ #xaa64)

(defconstant +pe-file-executable-image+ #x0002)
(defconstant +pe-file-large-address-aware+ #x0020)
(defconstant +pe-file-dll+ #x2000)

(defconstant +pe-magic-pe32-plus+ #x020b)
(defconstant +pe-subsystem-windows-gui+ 2)
(defconstant +pe-subsystem-windows-cui+ 3)
(defconstant +pe-number-of-rva-and-sizes+ 16)

(defconstant +pe-section-cnt-code+ #x00000020)
(defconstant +pe-section-cnt-initialized-data+ #x00000040)
(defconstant +pe-section-mem-execute+ #x20000000)
(defconstant +pe-section-mem-read+ #x40000000)
(defconstant +pe-section-mem-write+ #x80000000)

(defconstant +pe-directory-export+ 0)
(defconstant +pe-directory-import+ 1)
(defconstant +pe-directory-base-reloc+ 5)
(defconstant +pe-directory-iat+ 12)

(defconstant +pe-reloc-absolute+ 0)
(defconstant +pe-reloc-dir64+ 10)

(defconstant +pe-file-alignment+ 512)
(defconstant +pe-section-alignment+ 4096)
(defconstant +pe-default-image-base-exe+ #x140000000)
(defconstant +pe-default-image-base-dll+ #x180000000)

;;; ------------------------------------------------------------
;;; Structures and builder API
;;; ------------------------------------------------------------

(defstruct pe-section
  "A PE section payload and its computed layout metadata."
  (name "" :type string)
  (data (make-array 0 :element-type '(unsigned-byte 8))
        :type (simple-array (unsigned-byte 8) (*)))
  (characteristics 0 :type (unsigned-byte 32))
  (virtual-address 0 :type (unsigned-byte 32))
  (virtual-size 0 :type (unsigned-byte 32))
  (raw-pointer 0 :type (unsigned-byte 32))
  (raw-size 0 :type (unsigned-byte 32)))

(defstruct pe-import
  "One imported DLL and its function names."
  (dll-name "" :type string)
  (functions nil :type list))

(defstruct pe-export
  "One exported symbol. RVA is the exported function RVA."
  (name "" :type string)
  (rva 0 :type (unsigned-byte 32))
  (ordinal 1 :type (unsigned-byte 16)))

(defstruct pe-builder
  "Accumulates fields for a PE32+ executable or DLL image."
  (machine +pe-machine-amd64+ :type (unsigned-byte 16))
  (dll-p nil :type boolean)
  (subsystem +pe-subsystem-windows-cui+ :type (unsigned-byte 16))
  (image-base +pe-default-image-base-exe+ :type (unsigned-byte 64))
  (entry-point 0 :type (unsigned-byte 32))
  (text (make-array 0 :element-type '(unsigned-byte 8))
        :type (simple-array (unsigned-byte 8) (*)))
  (rdata (make-array 0 :element-type '(unsigned-byte 8))
         :type (simple-array (unsigned-byte 8) (*)))
  (data (make-array 0 :element-type '(unsigned-byte 8))
        :type (simple-array (unsigned-byte 8) (*)))
  (imports (list (make-pe-import :dll-name "kernel32.dll"
                                 :functions '("ExitProcess" "GetStdHandle" "WriteConsoleA")))
           :type list)
  (exports nil :type list)
  (base-relocations nil :type list))

(defun make-pe32+-builder (&key (arch :x86-64) dll-p
                                (subsystem :console)
                                image-base)
  "Create a PE32+ builder for ARCH (:X86-64, :ARM64, or :AARCH64)."
  (let* ((machine (ecase arch
                    (:x86-64 +pe-machine-amd64+)
                    ((:arm64 :aarch64) +pe-machine-arm64+)))
         (default-base (if dll-p +pe-default-image-base-dll+ +pe-default-image-base-exe+)))
    (make-pe-builder :machine machine
                     :dll-p (and dll-p t)
                     :subsystem (ecase subsystem
                                  (:console +pe-subsystem-windows-cui+)
                                  (:gui +pe-subsystem-windows-gui+))
                     :image-base (or image-base default-base))))

(defun pe-add-text-bytes (builder bytes)
  "Set BUILDER's .text payload to BYTES."
  (setf (pe-builder-text builder) (%pe-ub8-vector bytes))
  builder)

(defun pe-add-rdata-bytes (builder bytes)
  "Set BUILDER's .rdata payload to BYTES."
  (setf (pe-builder-rdata builder) (%pe-ub8-vector bytes))
  builder)

(defun pe-add-data-bytes (builder bytes)
  "Set BUILDER's .data payload to BYTES."
  (setf (pe-builder-data builder) (%pe-ub8-vector bytes))
  builder)

(defun pe-add-import (builder dll-name function-names)
  "Add imported FUNCTION-NAMES from DLL-NAME to BUILDER."
  (push (make-pe-import :dll-name dll-name :functions function-names)
        (pe-builder-imports builder))
  builder)

(defun pe-add-export (builder name rva &key ordinal)
  "Add exported NAME at RVA to BUILDER."
  (push (make-pe-export :name name
                        :rva rva
                        :ordinal (or ordinal (1+ (length (pe-builder-exports builder)))))
        (pe-builder-exports builder))
  builder)

(defun pe-add-base-relocation (builder rva)
  "Add one IMAGE_REL_BASED_DIR64 relocation for RVA."
  (push rva (pe-builder-base-relocations builder))
  builder)

;;; ------------------------------------------------------------
;;; Windows x86-64 ABI helpers
;;; ------------------------------------------------------------

(defparameter *pe-x86-64-argument-registers* '(:rcx :rdx :r8 :r9)
  "Windows x86-64 ABI integer/pointer argument registers.")

(defconstant +pe-x86-64-shadow-space-size+ 32
  "Bytes of caller-allocated shadow space required before each call.")

(defun pe-x86-64-stack-adjustment (stack-argument-count)
  "Return bytes to reserve before a Windows x86-64 call.

The reservation includes 32 bytes of shadow space, stack arguments, and padding
to keep RSP 16-byte aligned at the call boundary.  This helper documents the ABI
contract used by native Windows call lowering."
  (let* ((base (+ +pe-x86-64-shadow-space-size+ (* 8 stack-argument-count)))
         (padding (mod (- 16 (mod base 16)) 16)))
    (+ base padding)))

;;; ------------------------------------------------------------
;;; Byte helpers
;;; ------------------------------------------------------------

(defun %pe-ub8-vector (bytes)
  "Return BYTES as a simple unsigned-byte 8 vector."
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun %pe-ascii-bytes (string &key (nul t))
  "Return STRING as ASCII bytes, optionally NUL terminated."
  (let* ((extra (if nul 1 0))
         (bytes (make-array (+ (length string) extra)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (loop for char across string
          for i from 0
          do (setf (aref bytes i) (char-code char)))
    bytes))

(defun %pe-write-fixed-name (buf name)
  "Write an 8-byte COFF section name field."
  (dotimes (i 8)
    (binary-buffer-write-u8 buf (if (< i (length name))
                                    (char-code (char name i))
                                    0))))

(defun %pe-patch-u16le (bytes offset value)
  (setf (aref bytes offset) (logand value #xff)
        (aref bytes (1+ offset)) (logand (ash value -8) #xff)))

(defun %pe-patch-u32le (bytes offset value)
  (setf (aref bytes offset) (logand value #xff)
        (aref bytes (+ offset 1)) (logand (ash value -8) #xff)
        (aref bytes (+ offset 2)) (logand (ash value -16) #xff)
        (aref bytes (+ offset 3)) (logand (ash value -24) #xff)))

(defun %pe-pad-to (buf size)
  (binary-buffer-write-pad buf (max 0 (- size (length buf)))))

(defun %pe-pad-to-align (buf alignment)
  (binary-buffer-write-pad buf (- (align-up (length buf) alignment) (length buf))))

(defun %pe-rva-to-file-offset (rva sections)
  "Translate RVA to file offset using SECTIONS."
  (dolist (section sections (error "RVA #x~x is outside PE sections" rva))
    (let ((start (pe-section-virtual-address section))
          (end (+ (pe-section-virtual-address section)
                  (max (pe-section-virtual-size section)
                       (pe-section-raw-size section)))))
      (when (and (<= start rva) (< rva end))
        (return (+ (pe-section-raw-pointer section) (- rva start)))))))

;;; ------------------------------------------------------------
;;; DOS stub and data directories
;;; ------------------------------------------------------------

(defun pe-build-dos-stub ()
  "Build an MZ DOS header plus a tiny DOS message stub.

The first 64 bytes are the DOS header.  E_LFANEW points at #x80 where the PE
signature is written.  The DOS program prints a Windows-required message when
run as a DOS executable."
  (let ((buf (elf-make-buffer)))
    (binary-buffer-write-u16le buf +pe-dos-signature+)
    (binary-buffer-write-u16le buf #x0090) ; e_cblp
    (binary-buffer-write-u16le buf #x0003) ; e_cp
    (binary-buffer-write-u16le buf 0)      ; e_crlc
    (binary-buffer-write-u16le buf #x0004) ; e_cparhdr
    (binary-buffer-write-u16le buf 0)      ; e_minalloc
    (binary-buffer-write-u16le buf #xffff) ; e_maxalloc
    (binary-buffer-write-u16le buf 0)      ; e_ss
    (binary-buffer-write-u16le buf #x00b8) ; e_sp
    (binary-buffer-write-u16le buf 0)      ; e_csum
    (binary-buffer-write-u16le buf 0)      ; e_ip
    (binary-buffer-write-u16le buf 0)      ; e_cs
    (binary-buffer-write-u16le buf #x0040) ; e_lfarlc
    (binary-buffer-write-u16le buf 0)      ; e_ovno
    (binary-buffer-write-pad buf 8)        ; e_res
    (binary-buffer-write-u16le buf 0)      ; e_oemid
    (binary-buffer-write-u16le buf 0)      ; e_oeminfo
    (binary-buffer-write-pad buf 20)       ; e_res2
    (binary-buffer-write-u32le buf #x80)   ; e_lfanew
    ;; DOS code: push cs; pop ds; mov dx,msg; mov ah,09h; int 21h; mov ax,4c01h; int 21h
    (binary-buffer-write-bytes buf '(#x0e #x1f #xba #x0e #x00 #xb4 #x09 #xcd #x21
                                     #xb8 #x01 #x4c #xcd #x21))
    (binary-buffer-write-bytes buf (%pe-ascii-bytes "This program requires Windows" :nul nil))
    (binary-buffer-write-bytes buf '(#x0d #x0a #x24))
    (%pe-pad-to buf #x80)
    (binary-buffer-to-array buf)))

(defun %pe-empty-directory-table ()
  (make-array +pe-number-of-rva-and-sizes+
              :initial-element (cons 0 0)))

(defun %pe-set-directory (directories index rva size)
  (setf (aref directories index) (cons rva size)))

;;; ------------------------------------------------------------
;;; Import, export, and relocation payloads
;;; ------------------------------------------------------------

(defun pe-build-import-table (imports section-rva)
  "Build an .idata section and return values: bytes, import-dir, iat-dir.

IMPORT-DIR and IAT-DIR are (RVA . SIZE) conses suitable for the optional-header
data directory table.  Imports use IMAGE_IMPORT_DESCRIPTOR, import lookup table,
hint/name entries, and IAT entries."
  (let* ((imports (reverse imports))
         (descriptor-count (length imports))
         (descriptor-size (* 20 (1+ descriptor-count)))
         (buf (elf-make-buffer))
         (descriptors nil)
         (iat-start 0)
         (iat-end 0))
    (binary-buffer-write-pad buf descriptor-size)
    (dolist (import imports)
      (let* ((functions (pe-import-functions import))
             (ilt-offset (length buf)))
        (binary-buffer-write-pad buf (* 8 (1+ (length functions))))
        (let ((iat-offset (length buf)))
          (when (zerop iat-start)
            (setf iat-start iat-offset))
          (binary-buffer-write-pad buf (* 8 (1+ (length functions))))
          (let ((name-rvas nil))
            (dolist (function functions)
              (%pe-pad-to-align buf 2)
              (let ((hint-name-offset (length buf)))
                (binary-buffer-write-u16le buf 0)
                (binary-buffer-write-bytes buf (%pe-ascii-bytes function))
                (push (+ section-rva hint-name-offset) name-rvas)))
            (let ((dll-name-offset (length buf)))
              (binary-buffer-write-bytes buf (%pe-ascii-bytes (pe-import-dll-name import)))
              (let ((name-rvas (nreverse name-rvas)))
                (loop for rva in name-rvas
                      for i from 0
                      do (%pe-patch-u32le buf (+ ilt-offset (* i 8)) rva)
                         (%pe-patch-u32le buf (+ iat-offset (* i 8)) rva))
                (push (list :ilt (+ section-rva ilt-offset)
                            :name (+ section-rva dll-name-offset)
                            :iat (+ section-rva iat-offset))
                      descriptors)
                (setf iat-end (+ iat-offset (* 8 (1+ (length functions)))))))))))
    (loop for descriptor in (nreverse descriptors)
          for offset from 0 by 20
          do (%pe-patch-u32le buf offset (getf descriptor :ilt))
             (%pe-patch-u32le buf (+ offset 12) (getf descriptor :name))
             (%pe-patch-u32le buf (+ offset 16) (getf descriptor :iat)))
    (let ((bytes (binary-buffer-to-array buf)))
      (values bytes
              (cons section-rva descriptor-size)
              (cons (+ section-rva iat-start) (- iat-end iat-start))))))

(defun pe-build-export-table (exports section-rva dll-name)
  "Build an .edata export table for EXPORTS.

EXPORTS is a list of PE-EXPORT structures with function RVAs.  Returns bytes and
an export directory cons (RVA . SIZE)."
  (let* ((exports (sort (copy-list exports) #'string< :key #'pe-export-name))
         (count (length exports))
         (buf (elf-make-buffer)))
    (when (zerop count)
      (return-from pe-build-export-table
        (values (make-array 0 :element-type '(unsigned-byte 8)) (cons 0 0))))
    ;; IMAGE_EXPORT_DIRECTORY placeholder.
    (binary-buffer-write-pad buf 40)
    (let* ((dll-name-offset (length buf))
           (ordinal-base 1)
           (function-table-offset (progn
                                    (binary-buffer-write-bytes buf (%pe-ascii-bytes dll-name))
                                    (%pe-pad-to-align buf 4)
                                    (length buf))))
      (dolist (export exports)
        (binary-buffer-write-u32le buf (pe-export-rva export)))
      (let ((name-pointer-offset (length buf)))
        (binary-buffer-write-pad buf (* 4 count))
        (let ((ordinal-table-offset (length buf)))
          (dolist (export exports)
            (binary-buffer-write-u16le buf (- (pe-export-ordinal export) ordinal-base)))
          (let ((name-rvas nil))
            (dolist (export exports)
              (let ((name-offset (length buf)))
                (binary-buffer-write-bytes buf (%pe-ascii-bytes (pe-export-name export)))
                (push (+ section-rva name-offset) name-rvas)))
            (loop for name-rva in (nreverse name-rvas)
                  for offset from name-pointer-offset by 4
                  do (%pe-patch-u32le buf offset name-rva))
            (%pe-patch-u32le buf 0 0) ; characteristics
            (%pe-patch-u32le buf 4 0) ; timestamp
            (%pe-patch-u16le buf 8 0) ; major version
            (%pe-patch-u16le buf 10 0) ; minor version
            (%pe-patch-u32le buf 12 (+ section-rva dll-name-offset))
            (%pe-patch-u32le buf 16 ordinal-base)
            (%pe-patch-u32le buf 20 count)
            (%pe-patch-u32le buf 24 count)
            (%pe-patch-u32le buf 28 (+ section-rva function-table-offset))
            (%pe-patch-u32le buf 32 (+ section-rva name-pointer-offset))
            (%pe-patch-u32le buf 36 (+ section-rva ordinal-table-offset))))))
    (let ((bytes (binary-buffer-to-array buf)))
      (values bytes (cons section-rva (length bytes))))))

(defun pe-build-base-relocations (relocation-rvas section-rva)
  "Build a .reloc payload with IMAGE_REL_BASED_DIR64 entries."
  (declare (ignore section-rva))
  (let ((buf (elf-make-buffer))
        (pages (make-hash-table :test #'eql)))
    (dolist (rva relocation-rvas)
      (let ((page (logand rva #xfffff000))
            (offset (logand rva #xfff)))
        (push offset (gethash page pages))))
    (maphash
     (lambda (page offsets)
       (let* ((entries (sort (copy-list offsets) #'<))
              (entry-count (+ (length entries) (if (oddp (length entries)) 1 0)))
              (block-size (+ 8 (* 2 entry-count))))
         (binary-buffer-write-u32le buf page)
         (binary-buffer-write-u32le buf block-size)
         (dolist (offset entries)
           (binary-buffer-write-u16le buf (logior (ash +pe-reloc-dir64+ 12) offset)))
         (when (oddp (length entries))
           (binary-buffer-write-u16le buf (ash +pe-reloc-absolute+ 12)))))
     pages)
    (binary-buffer-to-array buf)))

;;; ------------------------------------------------------------
;;; Headers and final image assembly
;;; ------------------------------------------------------------

(defun %pe-make-section (name data characteristics)
  (make-pe-section :name name
                   :data (%pe-ub8-vector data)
                   :virtual-size (length data)
                   :characteristics characteristics))

(defun %pe-layout-sections (sections size-of-headers)
  "Assign RVAs and raw file offsets to SECTIONS."
  (let ((next-rva +pe-section-alignment+)
        (next-file size-of-headers))
    (dolist (section sections)
      (let* ((data-size (length (pe-section-data section)))
             (raw-size (if (zerop data-size) 0 (align-up data-size +pe-file-alignment+))))
        (setf (pe-section-virtual-address section) next-rva
              (pe-section-virtual-size section) data-size
              (pe-section-raw-pointer section) (if (zerop raw-size) 0 next-file)
              (pe-section-raw-size section) raw-size)
        (incf next-rva (align-up (max 1 data-size) +pe-section-alignment+))
        (incf next-file raw-size))))
  sections)

(defun %pe-section-by-name (sections name)
  (or (find name sections :key #'pe-section-name :test #'string=)
      (error "Missing PE section ~A" name)))

(defun %pe-write-coff-header (buf builder section-count size-of-optional-header)
  (binary-buffer-write-u16le buf (pe-builder-machine builder))
  (binary-buffer-write-u16le buf section-count)
  (binary-buffer-write-u32le buf 0) ; timestamp, deterministic
  (binary-buffer-write-u32le buf 0) ; symbol table pointer
  (binary-buffer-write-u32le buf 0) ; symbol count
  (binary-buffer-write-u16le buf size-of-optional-header)
  (binary-buffer-write-u16le buf (logior +pe-file-executable-image+
                                        +pe-file-large-address-aware+
                                        (if (pe-builder-dll-p builder) +pe-file-dll+ 0))))

(defun %pe-write-optional-header (buf builder sections directories size-of-headers)
  (let* ((text (%pe-section-by-name sections ".text"))
         (size-of-code (pe-section-raw-size text))
         (initialized-size (loop for section in sections
                                 unless (string= (pe-section-name section) ".text")
                                   sum (pe-section-raw-size section)))
         (size-of-image (align-up
                         (loop for section in sections
                               maximize (+ (pe-section-virtual-address section)
                                           (max 1 (pe-section-virtual-size section))))
                         +pe-section-alignment+)))
    (binary-buffer-write-u16le buf +pe-magic-pe32-plus+)
    (binary-buffer-write-u8 buf 14) ; linker major
    (binary-buffer-write-u8 buf 0)  ; linker minor
    (binary-buffer-write-u32le buf size-of-code)
    (binary-buffer-write-u32le buf initialized-size)
    (binary-buffer-write-u32le buf 0) ; uninitialized size
    (binary-buffer-write-u32le buf (pe-builder-entry-point builder))
    (binary-buffer-write-u32le buf (pe-section-virtual-address text))
    (binary-buffer-write-u64le buf (pe-builder-image-base builder))
    (binary-buffer-write-u32le buf +pe-section-alignment+)
    (binary-buffer-write-u32le buf +pe-file-alignment+)
    (binary-buffer-write-u16le buf 6) ; OS major
    (binary-buffer-write-u16le buf 0)
    (binary-buffer-write-u16le buf 0) ; image version
    (binary-buffer-write-u16le buf 0)
    (binary-buffer-write-u16le buf 6) ; subsystem version
    (binary-buffer-write-u16le buf 0)
    (binary-buffer-write-u32le buf 0) ; win32 version
    (binary-buffer-write-u32le buf size-of-image)
    (binary-buffer-write-u32le buf size-of-headers)
    (binary-buffer-write-u32le buf 0) ; checksum
    (binary-buffer-write-u16le buf (pe-builder-subsystem builder))
    (binary-buffer-write-u16le buf #x8160) ; NX, dynamic base, high entropy VA, terminal aware
    (binary-buffer-write-u64le buf #x100000) ; stack reserve
    (binary-buffer-write-u64le buf #x1000)   ; stack commit
    (binary-buffer-write-u64le buf #x100000) ; heap reserve
    (binary-buffer-write-u64le buf #x1000)   ; heap commit
    (binary-buffer-write-u32le buf 0)        ; loader flags
    (binary-buffer-write-u32le buf +pe-number-of-rva-and-sizes+)
    (dotimes (i +pe-number-of-rva-and-sizes+)
      (let ((directory (aref directories i)))
        (binary-buffer-write-u32le buf (car directory))
        (binary-buffer-write-u32le buf (cdr directory))))))

(defun %pe-write-section-header (buf section)
  (%pe-write-fixed-name buf (pe-section-name section))
  (binary-buffer-write-u32le buf (pe-section-virtual-size section))
  (binary-buffer-write-u32le buf (pe-section-virtual-address section))
  (binary-buffer-write-u32le buf (pe-section-raw-size section))
  (binary-buffer-write-u32le buf (pe-section-raw-pointer section))
  (binary-buffer-write-u32le buf 0) ; relocations pointer
  (binary-buffer-write-u32le buf 0) ; line numbers pointer
  (binary-buffer-write-u16le buf 0) ; relocation count
  (binary-buffer-write-u16le buf 0) ; line number count
  (binary-buffer-write-u32le buf (pe-section-characteristics section)))

(defun pe-finalize (builder)
  "Assemble BUILDER into a PE32+ image and return a byte vector."
  (let* ((empty (make-array 0 :element-type '(unsigned-byte 8)))
         (text (%pe-make-section ".text" (pe-builder-text builder)
                                (logior +pe-section-cnt-code+
                                        +pe-section-mem-execute+
                                        +pe-section-mem-read+)))
         (rdata (%pe-make-section ".rdata" (pe-builder-rdata builder)
                                 (logior +pe-section-cnt-initialized-data+
                                         +pe-section-mem-read+)))
         (data (%pe-make-section ".data" (pe-builder-data builder)
                                (logior +pe-section-cnt-initialized-data+
                                        +pe-section-mem-read+
                                        +pe-section-mem-write+)))
         (idata (%pe-make-section ".idata" empty
                                 (logior +pe-section-cnt-initialized-data+
                                         +pe-section-mem-read+
                                         +pe-section-mem-write+)))
         (edata (%pe-make-section ".edata" empty
                                 (logior +pe-section-cnt-initialized-data+
                                         +pe-section-mem-read+)))
         (reloc (%pe-make-section ".reloc" empty
                                 (logior +pe-section-cnt-initialized-data+
                                         +pe-section-mem-read+)))
         (sections (list text rdata data idata edata reloc))
         (dos-stub-size #x80)
         (optional-header-size 240)
         (header-size (+ dos-stub-size 4 20 optional-header-size (* 40 (length sections))))
         (size-of-headers (align-up header-size +pe-file-alignment+)))
    ;; First pass gives stable RVAs for data-directory-bearing sections.
    (%pe-layout-sections sections size-of-headers)
    (multiple-value-bind (idata-bytes import-dir iat-dir)
        (pe-build-import-table (pe-builder-imports builder)
                               (pe-section-virtual-address idata))
      (setf (pe-section-data idata) idata-bytes)
      ;; .edata and .reloc RVAs depend on the final .idata size.
      (%pe-layout-sections sections size-of-headers)
      (multiple-value-bind (edata-bytes export-dir)
          (pe-build-export-table (pe-builder-exports builder)
                                 (pe-section-virtual-address edata)
                                 (if (pe-builder-dll-p builder) "cl-cc.dll" "cl-cc.exe"))
        (setf (pe-section-data edata) edata-bytes)
        (let ((reloc-bytes (pe-build-base-relocations (pe-builder-base-relocations builder)
                                                      (pe-section-virtual-address reloc))))
          (setf (pe-section-data reloc) reloc-bytes)
          ;; Re-layout after generated sections receive their final sizes.
          (%pe-layout-sections sections size-of-headers)
          (let ((directories (%pe-empty-directory-table)))
            (%pe-set-directory directories +pe-directory-import+ (car import-dir) (cdr import-dir))
            (%pe-set-directory directories +pe-directory-iat+ (car iat-dir) (cdr iat-dir))
            (when (plusp (length edata-bytes))
              (%pe-set-directory directories +pe-directory-export+ (car export-dir) (cdr export-dir)))
            (when (plusp (length reloc-bytes))
              (%pe-set-directory directories +pe-directory-base-reloc+
                                 (pe-section-virtual-address reloc)
                                 (length reloc-bytes)))
            (let ((out (elf-make-buffer)))
              (binary-buffer-write-bytes out (pe-build-dos-stub))
              (binary-buffer-write-u32le out +pe-signature+)
              (%pe-write-coff-header out builder (length sections) optional-header-size)
              (%pe-write-optional-header out builder sections directories size-of-headers)
              (dolist (section sections)
                (%pe-write-section-header out section))
              (%pe-pad-to out size-of-headers)
              (dolist (section sections)
                (when (plusp (pe-section-raw-size section))
                  (%pe-pad-to out (pe-section-raw-pointer section))
                  (binary-buffer-write-bytes out (pe-section-data section))
                  (%pe-pad-to out (+ (pe-section-raw-pointer section)
                                     (pe-section-raw-size section)))))
              (binary-buffer-to-array out))))))))

(defun write-pe-file (filename bytes)
  "Write PE image BYTES to FILENAME."
  (with-open-file (out filename
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-sequence bytes out))
  filename)

(defun compile-to-pe (code-bytes reloc-entries &key output-file (arch :x86-64)
                                   dll-p (subsystem :console) exports
                                   (rdata-bytes (make-array 0 :element-type '(unsigned-byte 8)))
                                   (data-bytes (make-array 0 :element-type '(unsigned-byte 8))))
  "Create a PE32+ executable or DLL image from CODE-BYTES and RELOC-ENTRIES.

RELOC-ENTRIES may contain .text-relative integer offsets or (OFFSET . SYMBOL)
pairs; PE base relocations use the OFFSET part and are emitted as
IMAGE_REL_BASED_DIR64 entries.  EXPORTS is a list of names exported from .text
at offset zero unless callers add precise RVAs through PE-ADD-EXPORT."
  (let* ((builder (make-pe32+-builder :arch arch :dll-p dll-p :subsystem subsystem))
         (text-rva +pe-section-alignment+))
    (pe-add-text-bytes builder code-bytes)
    (pe-add-rdata-bytes builder rdata-bytes)
    (pe-add-data-bytes builder data-bytes)
    (setf (pe-builder-entry-point builder) text-rva)
    (dolist (reloc reloc-entries)
      (pe-add-base-relocation builder (+ text-rva (if (consp reloc) (car reloc) reloc))))
    (loop for name in exports
          for ordinal from 1
          do (pe-add-export builder name text-rva :ordinal ordinal))
    (let ((bytes (pe-finalize builder)))
      (when output-file
        (write-pe-file output-file bytes))
      bytes)))

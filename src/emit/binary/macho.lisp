;;;; src/binary/macho.lisp - Mach-O Binary Format Support
;;;
;;; Implements Mach-O format for macOS executable generation.
;;; Supports x86-64 and AArch64 architectures.
;;;
;;; Pure Common Lisp implementation - no external dependencies.
;;;
;;; References:
;;; - https://github.com/aidansteele/osx-abi-macho-file-format-reference
;;; - /usr/include/mach-o/loader.h
;;; - /usr/include/mach-o/nlist.h

(in-package :cl-cc/binary)

;;; Constants

;; Magic numbers
(defconstant +mh-magic-64+ #xFEEDFACF
  "64-bit Mach-O magic number (little-endian).")

;; CPU Types
(defconstant +cpu-type-x86-64+ #x01000007
  "x86-64 (AMD64) CPU type.")
(defconstant +cpu-type-arm64+ #x0100000C
  "ARM64 CPU type.")
(defconstant +cpu-subtype-x86-64-all+ #x00000003
  "x86-64 all subtypes.")
(defconstant +cpu-subtype-arm64-all+ #x00000000
  "ARM64 all subtypes.")

;; File Types
(defconstant +mh-execute+ 2
  "Executable file type.")

;; Load Command Types
(defconstant +lc-segment-64+ #x19
  "64-bit segment load command.")
(defconstant +lc-symtab+ #x03
  "Symbol table load command.")
(defconstant +lc-dysymtab+ #x0B
  "Dynamic symbol table load command.")
(defconstant +lc-main+ #x80000028
  "Main entry point load command (LC_MAIN | LC_REQ_DYLD).")

;; Header Flags
(defconstant +mh-noundefs+ 1
  "No undefined references.")
(defconstant +mh-dyldlink+ 4
  "Can be dynamically linked.")
(defconstant +mh-pie+ #x200000
  "Position-independent executable.")

;; Section Flags
(defconstant +s-attr-pure-instructions+ #x80000000
  "Section contains only instructions.")
(defconstant +s-attr-some-instructions+ #x400
  "Section contains some instructions.")

;;; Structures

(defstruct mach-header
  "64-bit Mach-O header structure."
  (magic +mh-magic-64+ :type (unsigned-byte 32))
  (cputype +cpu-type-x86-64+ :type (unsigned-byte 32))
  (cpusubtype +cpu-subtype-x86-64-all+ :type (unsigned-byte 32))
  (filetype +mh-execute+ :type (unsigned-byte 32))
  (ncmds 0 :type (unsigned-byte 32))
  (sizeofcmds 0 :type (unsigned-byte 32))
  (flags +mh-noundefs+ :type (unsigned-byte 32))
  (reserved 0 :type (unsigned-byte 32)))

(defstruct segment-command
  "64-bit segment load command."
  (cmd +lc-segment-64+ :type (unsigned-byte 32))
  (cmdsize 72 :type (unsigned-byte 32))
  (segname "" :type string)
  (vmaddr 0 :type (unsigned-byte 64))
  (vmsize 0 :type (unsigned-byte 64))
  (fileoff 0 :type (unsigned-byte 64))
  (filesize 0 :type (unsigned-byte 64))
  (maxprot 7 :type (unsigned-byte 32))   ; rwx
  (initprot 5 :type (unsigned-byte 32))  ; rx
  (nsects 0 :type (unsigned-byte 32))
  (flags 0 :type (unsigned-byte 32))
  (payload (make-array 0 :element-type '(unsigned-byte 8))
           :type (simple-array (unsigned-byte 8) (*)))
  (sections nil :type list))

(defstruct section
  "64-bit section structure."
  (sectname "" :type string)
  (segname "" :type string)
  (addr 0 :type (unsigned-byte 64))
  (size 0 :type (unsigned-byte 64))
  (offset 0 :type (unsigned-byte 32))
  (align 0 :type (unsigned-byte 32))
  (reloff 0 :type (unsigned-byte 32))
  (nreloc 0 :type (unsigned-byte 32))
  (flags 0 :type (unsigned-byte 32))
  (reserved1 0 :type (unsigned-byte 32))
  (reserved2 0 :type (unsigned-byte 32))
  (reserved3 0 :type (unsigned-byte 32)))

(defstruct symtab-command
  "Symbol table load command."
  (cmd +lc-symtab+ :type (unsigned-byte 32))
  (cmdsize 24 :type (unsigned-byte 32))
  (symoff 0 :type (unsigned-byte 32))
  (nsyms 0 :type (unsigned-byte 32))
  (stroff 0 :type (unsigned-byte 32))
  (strsize 0 :type (unsigned-byte 32)))

(defstruct dysymtab-command
  "Dynamic symbol table load command. Minimal zero-filled serialization."
  (cmd +lc-dysymtab+ :type (unsigned-byte 32))
  (cmdsize 80 :type (unsigned-byte 32))
  (ilocalsym 0 :type (unsigned-byte 32))
  (nlocalsym 0 :type (unsigned-byte 32))
  (iextdefsym 0 :type (unsigned-byte 32))
  (nextdefsym 0 :type (unsigned-byte 32))
  (iundefsym 0 :type (unsigned-byte 32))
  (nundefsym 0 :type (unsigned-byte 32))
  (tocoff 0 :type (unsigned-byte 32))
  (ntoc 0 :type (unsigned-byte 32))
  (modtaboff 0 :type (unsigned-byte 32))
  (nmodtab 0 :type (unsigned-byte 32))
  (extrefsymoff 0 :type (unsigned-byte 32))
  (nextrefsyms 0 :type (unsigned-byte 32))
  (indirectsymoff 0 :type (unsigned-byte 32))
  (nindirectsyms 0 :type (unsigned-byte 32))
  (extreloff 0 :type (unsigned-byte 32))
  (nextrel 0 :type (unsigned-byte 32))
  (locreloff 0 :type (unsigned-byte 32))
  (nlocrel 0 :type (unsigned-byte 32)))

(defstruct entry-point-command
  "LC_MAIN entry point command."
  (cmd +lc-main+ :type (unsigned-byte 32))
  (cmdsize 24 :type (unsigned-byte 32))
  (entryoff 0 :type (unsigned-byte 64))
  (stacksize 0 :type (unsigned-byte 64)))

(defstruct nlist
  "64-bit symbol table entry."
  (n-strx 0 :type (unsigned-byte 32))
  (n-type 0 :type (unsigned-byte 8))
  (n-sect 0 :type (unsigned-byte 8))
  (n-desc 0 :type (unsigned-byte 16))
  (n-value 0 :type (unsigned-byte 64)))

;;; Byte Buffer - Pure Common Lisp Implementation

(defun make-binary-buffer (&optional (initial-size 4096))
  "Create a fresh shared binary buffer as an adjustable byte vector."
  (make-array initial-size
              :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer 0))

(defun binary-buffer-write-u8 (buffer byte)
  (declare (type (array (unsigned-byte 8) (*)) buffer)
           (type (unsigned-byte 8) byte))
  (vector-push-extend byte buffer))

(defun binary-buffer-write-u16le (buffer value)
  (binary-buffer-write-u8 buffer (logand value #xFF))
  (binary-buffer-write-u8 buffer (logand (ash value -8) #xFF)))

(defun binary-buffer-write-u32le (buffer value)
  (binary-buffer-write-u8 buffer (logand value #xFF))
  (binary-buffer-write-u8 buffer (logand (ash value -8) #xFF))
  (binary-buffer-write-u8 buffer (logand (ash value -16) #xFF))
  (binary-buffer-write-u8 buffer (logand (ash value -24) #xFF)))

(defun binary-buffer-write-u64le (buffer value)
  (binary-buffer-write-u32le buffer (logand value #xFFFFFFFF))
  (binary-buffer-write-u32le buffer (logand (ash value -32) #xFFFFFFFF)))

(defun binary-buffer-write-s64le (buffer value)
  (binary-buffer-write-u64le buffer (logand value #xFFFFFFFFFFFFFFFF)))

(defun binary-buffer-write-bytes (buffer bytes)
  (etypecase bytes
    (list (dolist (b bytes) (binary-buffer-write-u8 buffer b)))
    (vector (loop for b across bytes do (binary-buffer-write-u8 buffer b)))))

(defun binary-buffer-write-pad (buffer n)
  (dotimes (_ n) (binary-buffer-write-u8 buffer 0)))

(defun binary-buffer-to-array (buffer)
  (make-array (length buffer)
              :element-type '(unsigned-byte 8)
              :initial-contents buffer))

(defclass byte-buffer ()
  ((data :initarg :data
         :accessor byte-buffer-data
         :type (array (unsigned-byte 8) (*))
         :documentation "The underlying byte array."))
  (:documentation "A simple growable byte buffer for binary output."))

(defun make-byte-buffer (&optional (initial-size 4096))
  "Create a new byte buffer with INITIAL-SIZE capacity."
  (make-instance 'byte-buffer
                 :data (make-binary-buffer initial-size)))

(defun buffer-write-byte (buffer byte)
  "Write a single BYTE to BUFFER."
  (declare (type byte-buffer buffer)
           (type (unsigned-byte 8) byte))
  (binary-buffer-write-u8 (byte-buffer-data buffer) byte))

(defun buffer-write-bytes (buffer bytes)
  "Write a sequence of BYTES to BUFFER."
  (declare (type byte-buffer buffer))
  (binary-buffer-write-bytes (byte-buffer-data buffer) bytes))

(defun buffer-get-bytes (buffer)
  "Get the contents of BUFFER as a simple-array of (unsigned-byte 8)."
  (declare (type byte-buffer buffer))
  (binary-buffer-to-array (byte-buffer-data buffer)))

;;; Utilities

(defun align-up (value alignment)
  "Align VALUE up to ALIGNMENT boundary."
  (declare (type (unsigned-byte 64) value)
           (type (unsigned-byte 64) alignment)
           (optimize (speed 3) (safety 0)))
  (* (ceiling value alignment) alignment))

(defun string-to-ascii-bytes (string)
  "Convert STRING to a vector of ASCII bytes."
  (declare (type string string))
  (let ((result (make-array (length string) :element-type '(unsigned-byte 8))))
    (loop for char across string
          for i from 0
          do (setf (aref result i) (char-code char)))
    result))

;;; Serialization Primitives

(defun serialize-uint32-le (value buffer)
  "Write 32-bit VALUE to BUFFER in little-endian byte order."
  (declare (type (unsigned-byte 32) value)
           (type byte-buffer buffer)
           (optimize (speed 3) (safety 0)))
  (buffer-write-byte buffer (logand value #xFF))
  (buffer-write-byte buffer (logand (ash value -8) #xFF))
  (buffer-write-byte buffer (logand (ash value -16) #xFF))
  (buffer-write-byte buffer (logand (ash value -24) #xFF)))

(defun serialize-uint64-le (value buffer)
  "Write 64-bit VALUE to BUFFER in little-endian byte order."
  (declare (type (unsigned-byte 64) value)
           (type byte-buffer buffer)
           (optimize (speed 3) (safety 0)))
  (serialize-uint32-le (logand value #xFFFFFFFF) buffer)
  (serialize-uint32-le (ash value -32) buffer))

(defun serialize-string-16 (string buffer)
  "Write STRING as 16-byte null-padded field to BUFFER."
  (declare (type string string)
           (type byte-buffer buffer)
           (optimize (speed 3) (safety 1)))
  (let ((bytes (string-to-ascii-bytes string)))
    (dotimes (i 16)
      (buffer-write-byte buffer (if (< i (length bytes)) (aref bytes i) 0)))))

(defun serialize-bytes (bytes buffer)
  "Write byte sequence BYTES to BUFFER."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type byte-buffer buffer)
           (optimize (speed 3) (safety 0)))
  (buffer-write-bytes buffer bytes))

;;; (Structure serialization and builder class definition are in macho-serialize.lisp,
;;;  which loads after this file. Builder API is in macho-build.lisp.)


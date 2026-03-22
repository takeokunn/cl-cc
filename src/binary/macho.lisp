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

;;; ----------------------------------------------------------------------------
;;; Constants
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; Structures
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; Byte Buffer - Pure Common Lisp Implementation
;;; ----------------------------------------------------------------------------

(defclass byte-buffer ()
  ((data :initarg :data
         :accessor byte-buffer-data
         :type (array (unsigned-byte 8) (*))
         :documentation "The underlying byte array."))
  (:documentation "A simple growable byte buffer for binary output."))

(defun make-byte-buffer (&optional (initial-size 4096))
  "Create a new byte buffer with INITIAL-SIZE capacity."
  (make-instance 'byte-buffer
                 :data (make-array initial-size
                                   :element-type '(unsigned-byte 8)
                                   :adjustable t
                                   :fill-pointer 0)))

(defun buffer-write-byte (buffer byte)
  "Write a single BYTE to BUFFER."
  (declare (type byte-buffer buffer)
           (type (unsigned-byte 8) byte))
  (vector-push-extend byte (byte-buffer-data buffer)))

(defun buffer-write-bytes (buffer bytes)
  "Write a sequence of BYTES to BUFFER."
  (declare (type byte-buffer buffer))
  (loop for b across bytes
        do (vector-push-extend b (byte-buffer-data buffer))))

(defun buffer-get-bytes (buffer)
  "Get the contents of BUFFER as a simple-array of (unsigned-byte 8)."
  (declare (type byte-buffer buffer))
  (map '(simple-array (unsigned-byte 8) (*)) #'identity (byte-buffer-data buffer)))

;;; ----------------------------------------------------------------------------
;;; Utilities
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; Serialization Primitives
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; Structure Serialization
;;; ----------------------------------------------------------------------------

(defun serialize-mach-header (header buffer)
  "Serialize MACH-HEADER to BUFFER."
  (declare (type mach-header header)
           (type byte-buffer buffer))
  (serialize-uint32-le (mach-header-magic header) buffer)
  (serialize-uint32-le (mach-header-cputype header) buffer)
  (serialize-uint32-le (mach-header-cpusubtype header) buffer)
  (serialize-uint32-le (mach-header-filetype header) buffer)
  (serialize-uint32-le (mach-header-ncmds header) buffer)
  (serialize-uint32-le (mach-header-sizeofcmds header) buffer)
  (serialize-uint32-le (mach-header-flags header) buffer)
  (serialize-uint32-le (mach-header-reserved header) buffer))

(defun serialize-segment-command (seg buffer)
  "Serialize SEGMENT-COMMAND to BUFFER (without sections)."
  (declare (type segment-command seg)
           (type byte-buffer buffer))
  (serialize-uint32-le (segment-command-cmd seg) buffer)
  (serialize-uint32-le (segment-command-cmdsize seg) buffer)
  (serialize-string-16 (segment-command-segname seg) buffer)
  (serialize-uint64-le (segment-command-vmaddr seg) buffer)
  (serialize-uint64-le (segment-command-vmsize seg) buffer)
  (serialize-uint64-le (segment-command-fileoff seg) buffer)
  (serialize-uint64-le (segment-command-filesize seg) buffer)
  (serialize-uint32-le (segment-command-maxprot seg) buffer)
  (serialize-uint32-le (segment-command-initprot seg) buffer)
  (serialize-uint32-le (segment-command-nsects seg) buffer)
  (serialize-uint32-le (segment-command-flags seg) buffer))

(defun serialize-section (sect buffer)
  "Serialize SECTION to BUFFER."
  (declare (type section sect)
           (type byte-buffer buffer))
  (serialize-string-16 (section-sectname sect) buffer)
  (serialize-string-16 (section-segname sect) buffer)
  (serialize-uint64-le (section-addr sect) buffer)
  (serialize-uint64-le (section-size sect) buffer)
  (serialize-uint32-le (section-offset sect) buffer)
  (serialize-uint32-le (section-align sect) buffer)
  (serialize-uint32-le (section-reloff sect) buffer)
  (serialize-uint32-le (section-nreloc sect) buffer)
  (serialize-uint32-le (section-flags sect) buffer)
  (serialize-uint32-le (section-reserved1 sect) buffer)
  (serialize-uint32-le (section-reserved2 sect) buffer)
  (serialize-uint32-le (section-reserved3 sect) buffer))

(defun serialize-entry-point (entry buffer)
  "Serialize ENTRY-POINT-COMMAND to BUFFER."
  (declare (type entry-point-command entry)
           (type byte-buffer buffer))
  (serialize-uint32-le (entry-point-command-cmd entry) buffer)
  (serialize-uint32-le (entry-point-command-cmdsize entry) buffer)
  (serialize-uint64-le (entry-point-command-entryoff entry) buffer)
  (serialize-uint64-le (entry-point-command-stacksize entry) buffer))

(defun serialize-symtab-command (symtab buffer)
  "Serialize SYMTAB-COMMAND to BUFFER."
  (declare (type symtab-command symtab)
           (type byte-buffer buffer))
  (serialize-uint32-le (symtab-command-cmd symtab) buffer)
  (serialize-uint32-le (symtab-command-cmdsize symtab) buffer)
  (serialize-uint32-le (symtab-command-symoff symtab) buffer)
  (serialize-uint32-le (symtab-command-nsyms symtab) buffer)
  (serialize-uint32-le (symtab-command-stroff symtab) buffer)
  (serialize-uint32-le (symtab-command-strsize symtab) buffer))

(defun serialize-nlist (nlist buffer)
  "Serialize NLIST to BUFFER."
  (declare (type nlist nlist)
           (type byte-buffer buffer))
  (serialize-uint32-le (nlist-n-strx nlist) buffer)
  (buffer-write-byte buffer (nlist-n-type nlist))
  (buffer-write-byte buffer (nlist-n-sect nlist))
  (serialize-uint32-le (nlist-n-desc nlist) buffer)
  (serialize-uint64-le (nlist-n-value nlist) buffer))

;;; ----------------------------------------------------------------------------
;;; Builder
;;; ----------------------------------------------------------------------------

(defclass mach-o-builder ()
  ((header :reader mach-o-builder-header
           :documentation "Mach-O header structure.")
   (segments :initform nil
             :accessor mach-o-builder-segments
             :documentation "List of segment commands.")
   (entry-point :accessor mach-o-builder-entry-point
                :documentation "Entry point command.")
   (string-table :initform (make-array 1024 :element-type '(unsigned-byte 8)
                                              :fill-pointer 1)
                 :reader mach-o-builder-string-table
                 :documentation "String table for symbols.")
   (symbol-table :initform nil
                 :accessor mach-o-builder-symbol-table
                 :documentation "List of nlist entries."))
  (:documentation "Builder class for constructing Mach-O executables."))

(defun make-mach-o-builder (arch)
  "Create a Mach-O builder for the specified architecture.
ARCH should be :X86-64 or :ARM64."
  (declare (type (member :x86-64 :arm64) arch))
  (let ((builder (make-instance 'mach-o-builder)))
    (setf (slot-value builder 'header)
          (make-mach-header
           :cputype (ecase arch
                      (:x86-64 +cpu-type-x86-64+)
                      (:arm64 +cpu-type-arm64+))
           :cpusubtype (ecase arch
                         (:x86-64 +cpu-subtype-x86-64-all+)
                         (:arm64 +cpu-subtype-arm64-all+))))
    (setf (mach-o-builder-entry-point builder)
          (make-entry-point-command))
    builder))

(defun add-text-segment (builder code-bytes &key (base-addr #x100000000))
  "Add __TEXT segment with code to BUILDER.
CODE-BYTES should be a simple-array of (unsigned-byte 8).
BASE-ADDR is the virtual memory address for the segment."
  (declare (type mach-o-builder builder)
           (type (simple-array (unsigned-byte 8) (*)) code-bytes))
  (let* ((code-size (length code-bytes))
         (text-section (make-section
                        :sectname "__text"
                        :segname "__TEXT"
                        :addr base-addr
                        :size code-size
                        :align 4  ; 2^4 = 16-byte alignment
                        :flags (logior +s-attr-pure-instructions+
                                       +s-attr-some-instructions+)))
         (text-segment (make-segment-command
                        :segname "__TEXT"
                        :vmaddr base-addr
                        :vmsize (align-up code-size #x1000)
                        :nsects 1
                        :cmdsize (+ 72 (* 80 1))  ; segment header + 1 section
                        :sections (list text-section))))
    (push text-segment (mach-o-builder-segments builder))
    builder))

(defun add-data-segment (builder data-bytes &key (base-addr #x100001000))
  "Add __DATA segment to BUILDER.
DATA-BYTES should be a simple-array of (unsigned-byte 8).
BASE-ADDR is the virtual memory address for the segment."
  (declare (type mach-o-builder builder)
           (type (simple-array (unsigned-byte 8) (*)) data-bytes))
  (let* ((data-size (length data-bytes))
         (data-section (make-section
                        :sectname "__data"
                        :segname "__DATA"
                        :addr base-addr
                        :size data-size
                        :align 4))
         (data-segment (make-segment-command
                        :segname "__DATA"
                        :vmaddr base-addr
                        :vmsize (align-up data-size #x1000)
                        :nsects 1
                        :maxprot 7    ; rwx
                        :initprot 6   ; rw
                        :cmdsize (+ 72 (* 80 1))
                        :sections (list data-section))))
    (push data-segment (mach-o-builder-segments builder))
    builder))

(defun add-symbol (builder name &key (value 0) (type 0) (sect 1))
  "Add a symbol to the builder's symbol table.
NAME is the symbol name string.
VALUE is the symbol's address/value.
TYPE is the symbol type byte.
SECT is the section number."
  (declare (type mach-o-builder builder)
           (type string name))
  (let ((strx (fill-pointer (mach-o-builder-string-table builder))))
    ;; Add name to string table (null-terminated)
    (loop for char across name
          do (vector-push-extend (char-code char)
                                 (mach-o-builder-string-table builder)))
    (vector-push-extend 0 (mach-o-builder-string-table builder))
    ;; Create nlist entry
    (push (make-nlist :n-strx strx
                      :n-type type
                      :n-sect sect
                      :n-value value)
          (mach-o-builder-symbol-table builder)))
  builder)

(defun add-entry-point (builder offset)
  "Add LC_MAIN entry point with file OFFSET."
  (declare (type mach-o-builder builder)
           (type (unsigned-byte 64) offset))
  (setf (entry-point-command-entryoff (mach-o-builder-entry-point builder))
        offset)
  builder)

(defun build-mach-o (builder code-bytes)
  "Build complete Mach-O executable from BUILDER and CODE-BYTES.
Returns a simple-array of (unsigned-byte 8)."
  (declare (type mach-o-builder builder)
           (type (simple-array (unsigned-byte 8) (*)) code-bytes))
  ;; Create output buffer
  (let* ((buffer (make-byte-buffer 65536))
         ;; Calculate sizes
         (header-size 32)
         (segments (nreverse (mach-o-builder-segments builder)))
         ;; Segment command size: 72 bytes header + 80 bytes per section
         (seg-cmd-sizes (loop for seg in segments
                              sum (+ 72 (* 80 (segment-command-nsects seg)))))
         (main-cmd-size 24)
         (cmds-size (+ seg-cmd-sizes main-cmd-size))
         ;; Align code to page boundary
         (code-offset (align-up (+ header-size cmds-size) 4096)))

    ;; Update header
    (let ((header (mach-o-builder-header builder)))
      (setf (mach-header-ncmds header) (1+ (length segments))
            (mach-header-sizeofcmds header) cmds-size
            (mach-header-flags header) (logior +mh-noundefs+ +mh-pie+)))

    ;; Update segment/section file offsets
    (let ((file-offset code-offset))
      (dolist (seg segments)
        (setf (segment-command-fileoff seg) file-offset
              (segment-command-filesize seg) (length code-bytes))
        (dolist (sect (segment-command-sections seg))
          (setf (section-offset sect) file-offset))
        (incf file-offset (align-up (segment-command-filesize seg) #x1000))))

    ;; Update entry point
    (setf (entry-point-command-entryoff (mach-o-builder-entry-point builder))
          code-offset)

    ;; Write header
    (serialize-mach-header (mach-o-builder-header builder) buffer)

    ;; Write segment commands with sections
    (dolist (seg segments)
      (serialize-segment-command seg buffer)
      (dolist (sect (segment-command-sections seg))
        (serialize-section sect buffer)))

    ;; Write LC_MAIN
    (serialize-entry-point (mach-o-builder-entry-point builder) buffer)

    ;; Pad to code offset
    (let ((current-pos (length (byte-buffer-data buffer))))
      (loop repeat (- code-offset current-pos)
            do (buffer-write-byte buffer 0)))

    ;; Write code
    (serialize-bytes code-bytes buffer)

    ;; Return the result as a simple-array
    (buffer-get-bytes buffer)))

(defun write-mach-o-file (filename mach-o-bytes)
  "Write MACH-O-BYTES to FILENAME as a binary file."
  (declare (type (or pathname string) filename)
           (type (simple-array (unsigned-byte 8) (*)) mach-o-bytes))
  (with-open-file (out filename
                        :direction :output
                        :element-type '(unsigned-byte 8)
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (write-sequence mach-o-bytes out)
    filename))

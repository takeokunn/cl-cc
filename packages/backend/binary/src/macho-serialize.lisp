;;;; packages/backend/binary/src/macho-serialize.lisp — Mach-O Structure Serialization
;;;
;;; Serializes Mach-O structures (mach-header, segment-command, section,
;;; entry-point-command, symtab-command, dysymtab-command, nlist) into
;;; byte-buffer instances using the serialization primitives from macho.lisp.
;;;
;;; Also defines the mach-o-builder CLOS class (fields only; methods are in
;;; macho-build.lisp which loads after this file).
;;;
;;; Load order: after emit/binary/macho.lisp, before emit/binary/macho-build.lisp.

(in-package :cl-cc/binary)

;;; Structure Serialization

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

(defun serialize-dysymtab-command (dysymtab buffer)
  "Serialize DYSYMTAB-COMMAND to BUFFER."
  (declare (type dysymtab-command dysymtab)
           (type byte-buffer buffer))
  (dolist (field (list (dysymtab-command-cmd dysymtab)
                       (dysymtab-command-cmdsize dysymtab)
                       (dysymtab-command-ilocalsym dysymtab)
                       (dysymtab-command-nlocalsym dysymtab)
                       (dysymtab-command-iextdefsym dysymtab)
                       (dysymtab-command-nextdefsym dysymtab)
                       (dysymtab-command-iundefsym dysymtab)
                       (dysymtab-command-nundefsym dysymtab)
                       (dysymtab-command-tocoff dysymtab)
                       (dysymtab-command-ntoc dysymtab)
                       (dysymtab-command-modtaboff dysymtab)
                       (dysymtab-command-nmodtab dysymtab)
                       (dysymtab-command-extrefsymoff dysymtab)
                       (dysymtab-command-nextrefsyms dysymtab)
                       (dysymtab-command-indirectsymoff dysymtab)
                       (dysymtab-command-nindirectsyms dysymtab)
                       (dysymtab-command-extreloff dysymtab)
                       (dysymtab-command-nextrel dysymtab)
                       (dysymtab-command-locreloff dysymtab)
                       (dysymtab-command-nlocrel dysymtab)))
    (serialize-uint32-le field buffer)))

(defun serialize-nlist (nlist buffer)
  "Serialize NLIST to BUFFER."
  (declare (type nlist nlist)
           (type byte-buffer buffer))
  (serialize-uint32-le (nlist-n-strx nlist) buffer)
  (buffer-write-byte buffer (nlist-n-type nlist))
  (buffer-write-byte buffer (nlist-n-sect nlist))
  (serialize-uint32-le (nlist-n-desc nlist) buffer)
  (serialize-uint64-le (nlist-n-value nlist) buffer))

;;; Builder Class

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

;;; (make-mach-o-builder, add-text-segment, add-data-segment,
;;;  add-symbol, add-entry-point, build-mach-o, write-mach-o-file
;;;  are in macho-build.lisp which loads after this file.)

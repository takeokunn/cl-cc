;;;; tests/pbt/generators-macho.lisp - Mach-O Binary Structure Generators
;;;
;;; Generators for Mach-O binary format structures used in binary output testing.
;;; Type expression generators are in generators.lisp.

(in-package :cl-cc/pbt-macho)

(defvar *max-mach-o-sections* 5
  "Maximum number of sections in generated Mach-O segments.")

;;; Mach-O Structure Generators

;; Mach-O Magic Numbers
(defvar +mh-magic+ #xFEEDFACE "32-bit Mach-O")
(defvar +mh-magic-64+ #xFEEDFACF "64-bit Mach-O")
(defvar +mh-cigam+ #xCEFAEDFE "32-bit Mach-O byte-swapped")
(defvar +mh-cigam-64+ #xCFFAEDFE "64-bit Mach-O byte-swapped")

;; CPU Types
(defvar +cpu-type-x86+ 7)
(defvar +cpu-type-x86-64+ #x01000007)
(defvar +cpu-type-arm+ 12)
(defvar +cpu-type-arm64+ #x0100000C)

;; CPU Subtypes
(defvar +cpu-subtype-x86-all+ 3)
(defvar +cpu-subtype-arm-all+ 0)
(defvar +cpu-subtype-arm64-all+ 0)

;; File Types
(defvar +mh-object+ 1 "Relocatable object file")
(defvar +mh-execute+ 2 "Executable file")
(defvar +mh-fvm+ 3 "Fixed VM shared library")
(defvar +mh-core+ 4 "Core file")
(defvar +mh-preload+ 5 "Preloaded executable")
(defvar +mh-dylib+ 6 "Dynamic library")
(defvar +mh-dylinker+ 7 "Dynamic link editor")
(defvar +mh-bundle+ 8 "Dynamic bundle")

;; Header Flags
(defvar +mh-noundefs+ 1 "No undefined references")
(defvar +mh-dyldlink+ 4 "Dyld will link this")
(defvar +mh-pie+ #x200000 "Position-independent executable")

;; Load Command Types
(defvar +lc-segment+ 1)
(defvar +lc-segment-64+ #x19)
(defvar +lc-symtab+ 2)
(defvar +lc-dysymtab+ #x0B)
(defvar +lc-load-dylib+ #x0C)
(defvar +lc-id-dylib+ #x0D)
(defvar +lc-load-weak-dylib+ #x80000018)
(defvar +lc-uuid+ #x1B)
(defvar +lc-rpath+ #x8000001C)
(defvar +lc-code-signature+ #x1D)
(defvar +lc-reexport-dylib+ #x8000001F)
(defvar +lc-version-min-macosx+ #x24)
(defvar +lc-build-version+ #x32)

;; Segment Flags
(defvar +vm-prot-read+ 1)
(defvar +vm-prot-write+ 2)
(defvar +vm-prot-execute+ 4)

(defstruct (mach-header (:constructor make-mach-header-raw))
  "Structure representing a Mach-O header."
  magic cputype cpusubtype filetype ncmds sizeofcmds flags reserved)

(defstruct (mach-segment-command (:constructor make-mach-segment-raw))
  "Structure representing a Mach-O segment command."
  cmd cmdsize segname vmaddr vmsize fileoff filesize maxprot initprot nsects flags sections)

(defstruct (mach-section (:constructor make-mach-section-raw))
  "Structure representing a Mach-O section."
  sectname segname addr size offset align reloff nreloc flags reserved1 reserved2 reserved3)

(defun gen-mach-magic ()
  "Generate valid Mach-O magic numbers."
  (gen-one-of (list +mh-magic+ +mh-magic-64+ +mh-cigam+ +mh-cigam-64+)))

(defun gen-mach-cpu-type ()
  "Generate valid CPU types for Mach-O."
  (gen-one-of (list +cpu-type-x86+ +cpu-type-x86-64+ +cpu-type-arm+ +cpu-type-arm64+)))

(defun gen-mach-cpu-subtype ()
  "Generate valid CPU subtypes for Mach-O."
  (gen-one-of (list +cpu-subtype-x86-all+ +cpu-subtype-arm-all+ +cpu-subtype-arm64-all+)))

(defun gen-mach-file-type ()
  "Generate valid Mach-O file types."
  (gen-one-of (list +mh-object+ +mh-execute+ +mh-dylib+ +mh-bundle+ +mh-preload+ +mh-core+)))

(defun gen-mach-flags ()
  "Generate valid Mach-O header flags."
  (gen-fmap
   (lambda (flag-list) (reduce #'logior flag-list :initial-value 0))
   (gen-list-of (gen-one-of (list +mh-noundefs+ +mh-dyldlink+ +mh-pie+ 0))
                :min-length 0 :max-length 3)))

(defun gen-mach-header ()
  "Generate valid Mach-O header for testing."
  (gen-bind
   (gen-mach-magic)
   (lambda (magic)
     (let ((is-64bit (or (= magic +mh-magic-64+) (= magic +mh-cigam-64+))))
       (gen-bind
        (gen-mach-cpu-type)
        (lambda (cpu)
          (gen-fmap
           (lambda (rest)
             (destructuring-bind (subtype filetype flags ncmds sizeofcmds) rest
               (make-mach-header-raw
                :magic magic :cputype cpu :cpusubtype subtype :filetype filetype
                :ncmds ncmds :sizeofcmds sizeofcmds :flags flags
                :reserved (if is-64bit 0 nil))))
           (gen-tuple (gen-mach-cpu-subtype) (gen-mach-file-type) (gen-mach-flags)
                      (gen-integer :min 1 :max 10) (gen-integer :min 32 :max 4096)))))))))

(defun gen-segment-permissions ()
  "Generate valid segment permissions (rwx)."
  (gen-fmap
   (lambda (perms) (reduce #'logior perms :initial-value 0))
   (gen-list-of (gen-one-of (list +vm-prot-read+ +vm-prot-write+ +vm-prot-execute+))
                :min-length 1 :max-length 3)))

(defun gen-segment-name ()
  "Generate valid Mach-O segment names (16 bytes, uppercase)."
  (gen-one-of '("__TEXT" "__DATA" "__LINKEDIT" "__OBJC" "__IMPORT" "__LC_SEGMENT")))

(defun gen-section-name ()
  "Generate valid Mach-O section names."
  (gen-one-of '("__text" "__data" "__bss" "__const" "__cstring"
                "__literal4" "__literal8" "__mod_init_func"
                "__mod_term_func" "__objc_classlist")))

(defun gen-mach-section ()
  "Generate valid Mach-O section for testing."
  (gen-fmap
   (lambda (data)
     (destructuring-bind (sectname segname addr size offset align flags) data
       (make-mach-section-raw
        :sectname sectname :segname segname :addr addr :size size :offset offset
        :align align :reloff 0 :nreloc 0 :flags flags
        :reserved1 0 :reserved2 0 :reserved3 nil)))
   (gen-tuple (gen-section-name) (gen-segment-name)
              (gen-integer :min 0 :max #xFFFFFF) (gen-integer :min 0 :max #xFFFF)
              (gen-integer :min 512 :max #xFFFFF) (gen-one-of '(0 1 2 3 4))
              (gen-integer :min 0 :max #xFFFFFFFF))))

(defun gen-mach-segment-command ()
  "Generate valid Mach-O segment command for testing."
  (gen-bind
   (gen-segment-permissions)
   (lambda (maxprot)
     (gen-bind
      (gen-segment-permissions)
      (lambda (initprot)
        (gen-fmap
         (lambda (data)
           (destructuring-bind (segname vmaddr vmsize fileoff filesize sections) data
             (make-mach-segment-raw
              :cmd +lc-segment-64+ :cmdsize (+ 72 (* 80 (length sections)))
              :segname segname :vmaddr vmaddr :vmsize vmsize :fileoff fileoff
              :filesize filesize :maxprot maxprot :initprot initprot
              :nsects (length sections) :flags 0 :sections sections)))
         (gen-tuple (gen-segment-name)
                    (gen-integer :min 0 :max #xFFFFFFFFFF)
                    (gen-integer :min 0 :max #xFFFFFFFFFF)
                    (gen-integer :min 0 :max #xFFFFFFFFFF)
                    (gen-integer :min 0 :max #xFFFFF)
                    (gen-list-of (gen-mach-section)
                                 :min-length 0 :max-length *max-mach-o-sections*))))))))

(defun gen-mach-load-command-type ()
  "Generate Mach-O load command types."
  (gen-one-of (list +lc-segment+ +lc-segment-64+ +lc-symtab+
                    +lc-dysymtab+ +lc-uuid+ +lc-load-dylib+
                    +lc-rpath+ +lc-code-signature+)))

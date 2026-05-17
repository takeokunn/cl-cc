;;;; packages/binary/src/package.lisp - Binary Format Package Definition
;;;
;;; Package for Mach-O binary format support in cl-cc.

(defpackage :cl-cc/binary
  (:use :cl)
  (:nicknames :binary)
  (:export
   ;; Mach-O constants
   #:+mh-magic-64+
   #:+cpu-type-x86-64+
   #:+cpu-type-arm64+
   #:+cpu-subtype-x86-64-all+
   #:+cpu-subtype-arm64-all+
   #:+mh-execute+
   #:+lc-segment-64+
   #:+lc-symtab+
   #:+lc-main+
   #:+mh-noundefs+
   #:+mh-dyldlink+
   #:+mh-pie+
   #:+s-attr-pure-instructions+
   #:+s-attr-some-instructions+

   ;; Structures
   #:mach-header
   #:segment-command
   #:section
   #:symtab-command
   #:entry-point-command
   #:nlist

   ;; Accessors
   #:mach-header-magic
   #:mach-header-cputype
   #:mach-header-cpusubtype
   #:mach-header-filetype
   #:mach-header-ncmds
   #:mach-header-sizeofcmds
   #:mach-header-flags
   #:mach-header-reserved
   #:segment-command-cmd
   #:segment-command-cmdsize
   #:segment-command-segname
   #:segment-command-vmaddr
   #:segment-command-vmsize
   #:segment-command-fileoff
   #:segment-command-filesize
   #:segment-command-maxprot
   #:segment-command-initprot
   #:segment-command-nsects
   #:segment-command-flags
   #:segment-command-sections
   #:section-sectname
   #:section-segname
   #:section-addr
   #:section-size
   #:section-offset
   #:section-align
   #:section-reloff
   #:section-nreloc
   #:section-flags
   #:section-reserved1
   #:section-reserved2
   #:section-reserved3
   #:entry-point-command-cmd
   #:entry-point-command-cmdsize
   #:entry-point-command-entryoff
   #:entry-point-command-stacksize

   ;; Builder
   #:mach-o-builder
   #:make-mach-o-builder
   #:add-text-segment
   #:add-data-segment
   #:add-symbol
   #:add-entry-point
   #:build-mach-o
   #:write-mach-o-file

   ;; Utilities
   #:align-up
   #:serialize-uint32-le
   #:serialize-uint64-le
   #:with-output-to-vector

   ;; ELF64 backend
   #:compile-to-elf64
   #:compile-to-elf64-exec
   #:make-elf64-executable
   #:elf64-add-load-segment
   #:elf64-add-gnu-stack-segment
   #:write-elf64-file))

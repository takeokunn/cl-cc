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
    #:+lc-load-dylib+
    #:+lc-code-signature+
    #:+lc-dyld-info-only+
    #:+lc-main+
   #:+mh-noundefs+
   #:+mh-dyldlink+
   #:+mh-pie+
    #:+s-attr-pure-instructions+
    #:+s-attr-some-instructions+
     #:+compact-unwind-encoding-none+
     #:+compact-unwind-x86-64-mode-stack-immd+
     #:+x86-64-reloc-unsigned+
     #:+x86-64-reloc-signed+
     #:+x86-64-reloc-branch+
     #:+x86-64-reloc-got-load+
     #:+arm64-reloc-unsigned+
     #:+arm64-reloc-branch26+
     #:+arm64-reloc-page21+
     #:+arm64-reloc-pageoff12+

   ;; Structures
   #:mach-header
   #:segment-command
   #:section
    #:symtab-command
    #:dysymtab-command
    #:dyld-info-command
    #:dylib-command
    #:linkedit-data-command
    #:relocation-info
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
    #:relocation-info-r-address
    #:relocation-info-r-symbolnum
    #:relocation-info-r-pcrel
    #:relocation-info-r-length
    #:relocation-info-r-extern
    #:relocation-info-r-type
    #:entry-point-command-cmd
   #:entry-point-command-cmdsize
   #:entry-point-command-entryoff
   #:entry-point-command-stacksize

   ;; Builder
   #:mach-o-builder
   #:make-mach-o-builder
    #:add-text-segment
    #:add-data-segment
     #:add-data-const-segment
     #:add-symbol
     #:add-relocation
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
    #:elf64-add-rodata-bytes
    #:compile-to-elf64-exec
    #:make-elf64-executable
    #:elf64-add-load-segment
    #:elf64-add-gnu-stack-segment
    #:write-elf64-file

    ;; PE/COFF backend
    #:+pe-machine-amd64+
    #:+pe-machine-arm64+
    #:+pe-magic-pe32-plus+
    #:+pe-section-alignment+
    #:+pe-file-alignment+
    #:pe-builder
    #:pe-section
    #:pe-import
    #:pe-export
    #:make-pe32+-builder
    #:pe-add-text-bytes
    #:pe-add-rdata-bytes
    #:pe-add-data-bytes
    #:pe-add-import
    #:pe-add-export
    #:pe-add-base-relocation
    #:pe-finalize
    #:compile-to-pe
    #:write-pe-file
    #:*pe-x86-64-argument-registers*
    #:+pe-x86-64-shadow-space-size+
    #:pe-x86-64-stack-adjustment))

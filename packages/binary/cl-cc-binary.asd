;;;; cl-cc-binary.asd — independent ASDF system for binary-format emitters
;;;;
;;;; Phase 2 of the package-by-feature monorepo migration. Files live in the
;;;; :cl-cc/binary package and are accessed by callers via the qualified
;;;; cl-cc/binary: prefix. Truly leaf — no dependencies on other cl-cc systems.

(asdf:defsystem :cl-cc-binary
  :description "cl-cc binary-format emitters — Mach-O, ELF, WebAssembly module bytes"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "macho")
   (:file "macho-buffer")
   (:file "macho-serialize")
   (:file "macho-build")
   (:file "elf")
   (:file "elf-emit")
   (:file "wasm")))

;;;; cl-cc-runtime.asd — independent ASDF system for the runtime library
;;;;
;;;; Phase 2 of the package-by-feature monorepo migration. Files live in the
;;;; :cl-cc/runtime package (rt-* function namespace + GC + heap + frame).
;;;; Leaf system: no dependencies on other cl-cc systems after the rt-eval
;;;; backward dependency on :cl-cc was removed.

(asdf:defsystem :cl-cc-runtime
  :description "cl-cc runtime library — rt-* primitives, GC, heap, frame, value codec"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
   ((:file "package")
    (:file "runtime-region")
    (:file "runtime")
   (:file "runtime-ops")
   (:file "runtime-strings")
   (:file "runtime-math-io")
   (:file "runtime-clos")
   (:file "runtime-io")
   (:file "value")
   (:file "value-codec")
   (:file "frame")
     (:file "heap-data")
     (:file "heap-core")
     (:file "heap-free-list")
     (:file "heap-resize")
     (:file "heap-trace")
     (:file "gc-references")
     (:file "gc-profile")
     (:file "gc-data")
     (:file "gc-safepoints")
      (:file "gc-policy")
      (:file "gc-roots-objects")
      (:file "gc-tlab")
      (:file "gc-minor")
      (:file "gc-write-barrier")
      (:file "gc-major-mark")
      (:file "gc-workers")
      (:file "gc-major-sweep")))

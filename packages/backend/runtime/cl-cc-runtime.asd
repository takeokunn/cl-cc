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
   (:file "runtime")
   (:file "runtime-ops")
   (:file "runtime-math-io")
   (:file "value")
   (:file "value-codec")
   (:file "frame")
   (:file "heap")
   (:file "heap-trace")
   (:file "gc")
   (:file "gc-write-barrier")
   (:file "gc-major")))

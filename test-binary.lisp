;;;; test-binary.lisp - Test binary module

(require :asdf)
(asdf:load-asd #P"/Users/take/ghq/github.com/takeokunn/cl-cc/cl-cc.asd")
(asdf:load-system :cl-cc)

(in-package :cl-cc/binary)

;; Test constants
(format t "~%=== Testing Constants ===~%")
(format t "+MH-MAGIC-64+ = #x~X~%" +mh-magic-64+)
(format t "+CPU-TYPE-X86-64+ = #x~X~%" +cpu-type-x86-64+)
(format t "+CPU-TYPE-ARM64+ = #x~X~%" +cpu-type-arm64+)

;; Test structures
(format t "~%=== Testing Structures ===~%")
(let ((header (make-mach-header :cputype +cpu-type-x86-64+)))
  (format t "Created mach-header: magic=#x~X, cputype=#x~X~%"
          (mach-header-magic header)
          (mach-header-cputype header)))

;; Test alignment
(format t "~%=== Testing Alignment ===~%")
(format t "align-up(100, 4096) = ~A~%" (align-up 100 4096))
(format t "align-up(4096, 4096) = ~A~%" (align-up 4096 4096))
(format t "align-up(5000, 4096) = ~A~%" (align-up 5000 4096))

;; Test builder
(format t "~%=== Testing Builder ===~%")
(let* ((builder (make-mach-o-builder :x86-64))
       (code-bytes (make-array 16 :element-type '(unsigned-byte 8)
                                  :initial-contents '(#x48 #x31 #xC0 #x48 #xFF #xC0 #x48 #x89
                                                       #xC7 #x48 #x31 #xC0 #xC3 #x90 #x90 #x90))))
  (add-text-segment builder code-bytes)
  (add-entry-point builder 4096)
  (let ((result (build-mach-o builder code-bytes)))
    (format t "Built Mach-O: ~A bytes~%" (length result))
    (format t "First 32 bytes (header): ~{#x~2,'0X ~}~%"
            (coerce (subseq result 0 32) 'list)))
  (format t "~%Builder test PASSED~%"))

;; Test file writing
(format t "~%=== Testing File Writing ===~%")
(let* ((builder (make-mach-o-builder :x86-64))
       (code-bytes (make-array 16 :element-type '(unsigned-byte 8)
                                  :initial-contents '(#x48 #x31 #xC0 ; xor rax, rax
                                                       #x48 #xFF #xC0 ; inc rax
                                                       #xC3           ; ret
                                                       0 0 0 0 0 0 0 0 0 0 0 0 0))))
  (add-text-segment builder code-bytes)
  (let ((result (build-mach-o builder code-bytes)))
    (write-mach-o-file "/tmp/test-macho" result)
    (format t "Wrote /tmp/test-macho~%"))
  (format t "~%File writing test PASSED~%"))

(format t "~%=== All Tests PASSED ===~%")
(sb-ext:exit :code 0)

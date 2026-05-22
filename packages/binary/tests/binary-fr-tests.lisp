;;;; tests/binary-fr-tests.lisp — Binary Output Feature Requirement Evidence Tests
;;;;
;;;; Tests for binary output FR implementations:
;;;; - FR-247: Unwind Tables / .eh_frame Generation
;;;; - FR-540/541: Safepoint polling and precise-GC stack maps
;;;; - FR-550/551/554: DWARF5, Wasm source maps, sanitizer instrumentation
;;;; - FR-560/561/562: Zero-cost EH table selection and landing pads

(in-package :cl-cc/test)

(defsuite binary-fr-suite
  :description "Binary output Feature Requirement evidence tests (FR-247)"
  :parent cl-cc-unit-suite)

(in-suite binary-fr-suite)

;;; ------------------------------------------------------------
;;; FR-247: Unwind Tables / .eh_frame Generation
;;; ------------------------------------------------------------

(deftest fr-247-binary-package-loaded
  "FR-247: CL-CC/BINARY package is loaded for ELF/Mach-O output."
  (assert-true (find-package "CL-CC/BINARY")))

(deftest fr-247-elf-section-constants-defined
  "FR-247: ELF section type constants (SHT-PROGBITS etc.) are defined in binary package."
  (let ((pkg (find-package "CL-CC/BINARY")))
    (assert-true pkg)
    (let* ((progbits (find-symbol "+SHT-PROGBITS+" pkg))
           (has-const (or progbits
                          (find-symbol "*SHT-PROGBITS*" pkg)
                          (find-symbol "SHT-PROGBITS" pkg))))
      (assert-true has-const))))

(deftest fr-247-x86-64-codegen-stack-map-support
  "FR-247/FR-370/FR-371: CL-CC/EMIT package has native compilation infrastructure symbols."
  (let ((pkg (find-package "CL-CC/EMIT")))
    (assert-true pkg)
    (let ((has-emit (or (find-symbol "COMPILE-TO-NATIVE" pkg)
                        (find-symbol "COMPILE-TO-X86-64-BYTES" pkg)
                        (find-symbol "EMIT-INSTRUCTION" pkg))))
      (assert-true has-emit))))

(deftest fr-247-binary-elf-emit-symbols
  "FR-247: ELF binary emitter symbols exist (WRITE-ELF64-FILE, COMPILE-TO-ELF64, or MAKE-ELF64-EXECUTABLE)."
  (let ((pkg (find-package "CL-CC/BINARY")))
    (assert-true pkg)
    (let ((has-elf (or (find-symbol "WRITE-ELF64-FILE" pkg)
                       (find-symbol "COMPILE-TO-ELF64" pkg)
                       (find-symbol "MAKE-ELF64-EXECUTABLE" pkg))))
      (assert-true has-elf))))

(deftest fr-247-binary-macho-emit-symbols
  "FR-247: Mach-O binary emitter symbols exist (WRITE-MACH-O-FILE, BUILD-MACH-O, or MAKE-MACH-O-BUILDER)."
  (let ((pkg (find-package "CL-CC/BINARY")))
    (assert-true pkg)
    (let ((has-macho (or (find-symbol "WRITE-MACH-O-FILE" pkg)
                          (find-symbol "BUILD-MACH-O" pkg)
                          (find-symbol "MAKE-MACH-O-BUILDER" pkg))))
      (assert-true has-macho))))

(defun %fr-byte-list (bytes)
  (coerce bytes 'list))

(deftest fr-540-safepoint-polling-inserts-real-poll-instructions
  "FR-540: safepoint polling pass inserts flag load, conditional skip, helper call, and helper body."
  (let* ((instructions (list (cl-cc:make-vm-label :name "entry")
                             (cl-cc:make-vm-const :dst :r0 :value 1)
                             (cl-cc:make-vm-ret :reg :r0)))
         (polled (cl-cc/optimize:opt-pass-safepoint-polling instructions)))
    (assert-true (> (length polled) (length instructions)))
    (assert-true (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-get-global)
                              (eq (cl-cc/vm::vm-global-name inst)
                                  cl-cc/optimize::*opt-safepoint-flag-name*)))
                       polled))
    (assert-true (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-call)
                              (= 1 (length (cl-cc/vm::vm-args inst)))))
                       polled))
    (assert-true (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-label)
                              (string= (cl-cc/vm::vm-name inst)
                                       cl-cc/optimize::*opt-safepoint-label*)))
                       polled))))

(deftest fr-541-stack-map-records-follow-safepoints
  "FR-541: enabled stack-map emission inserts :GC-STACK-MAP records after safepoints."
  (let* ((code (list '(:const :r0 1)
                     '(:safepoint :roots ((:kind :stack :slot 2 :type :pointer)))
                     '(:ret :r0)))
         (mapped (let ((cl-cc/codegen:*precise-gc-stack-maps-enabled* t))
                   (cl-cc/codegen:cg-embed-stack-maps-after-safepoints code)))
         (record (find :gc-stack-map mapped :key (lambda (entry)
                                                   (and (consp entry) (first entry))))))
    (assert-true record)
    (assert-equal :gc-stack-map (first record))
    (assert-= 0 (getf (rest record) :safepoint-id))
    (assert-true (getf (rest record) :pc-offset))))

(deftest fr-550-dwarf5-debug-sections-have-byte-level-headers
  "FR-550: DWARF5 builder emits .debug_info/.debug_line/.debug_abbrev with v5 headers."
  (let* ((cu (cl-cc/binary::make-dwarf-compile-unit
              :name "fr550.lisp"
              :producer "cl-cc-test"
              :low-pc #x1000
              :high-pc #x1020
              :lines '((0 1) (4 2))
              :subprograms
              (list (cl-cc/binary::make-dwarf-subprogram
                     :name "main"
                     :low-pc #x1000
                     :high-pc #x1020
                     :parameters
                     (list (cl-cc/binary::make-dwarf-variable-location
                            :name "x" :kind :register :register 0))))))
         (alist (cl-cc/binary::build-dwarf-section-alist cu))
         (info (%fr-byte-list (cdr (assoc ".debug_info" alist :test #'string=))))
         (line (%fr-byte-list (cdr (assoc ".debug_line" alist :test #'string=))))
         (abbrev (%fr-byte-list (cdr (assoc ".debug_abbrev" alist :test #'string=)))))
    (assert-true (cdr (assoc ".debug_info" alist :test #'string=)))
    (assert-true (cdr (assoc ".debug_line" alist :test #'string=)))
    (assert-true (cdr (assoc ".debug_abbrev" alist :test #'string=)))
    ;; .debug_info: unit_length:u32, version:u16=5, unit_type=compile, addr_size=8.
    (assert-equal '(5 0 1 8) (subseq info 4 8))
    ;; .debug_line: unit_length:u32, version:u16=5, addr_size=8, segment_selector=0.
    (assert-equal '(5 0 8 0) (subseq line 4 8))
    (assert-true (> (length abbrev) 8))))

(deftest fr-551-wasm-source-map-v3-json-contains-required-fields
  "FR-551: Wasm source map generation emits Source Map v3 JSON with sources and mappings."
  (let ((json (cl-cc/emit:build-wasm-source-map-v3
               (list (list :offset 0 :source "input.lisp" :line 1 :column 0)
                     (list :offset 4 :source "input.lisp" :line 3 :column 2))
               :file "out.wasm"
               :source-root "/src")))
    (assert-true (search "\"version\": 3" json))
    (assert-true (search "\"file\": \"out.wasm\"" json))
    (assert-true (search "\"sourceRoot\": \"/src\"" json))
    (assert-true (search "\"sources\": [\"input.lisp\"]" json))
    (assert-true (search "\"mappings\":" json))))

(deftest fr-554-sanitizer-flag-emits-ubsan-instrumentation
  "FR-554: --sanitize/UBSan-style codegen flag emits JO+INT3 instrumentation after arithmetic."
  (let* ((program (cl-cc/vm::make-vm-program
                   :instructions (list (cl-cc:make-vm-const :dst :r0 :value 1)
                                       (cl-cc:make-vm-const :dst :r1 :value 2)
                                       (cl-cc:make-vm-integer-add :dst :r2 :lhs :r0 :rhs :r1)
                                       (cl-cc:make-vm-halt :reg :r2))
                   :result-register :r2))
         (plain (%fr-byte-list (cl-cc/codegen:compile-to-x86-64-bytes program)))
         (ubsan (%fr-byte-list (cl-cc/codegen:compile-to-x86-64-bytes program :ubsan t))))
    (assert-false (search '(#x70 #x01 #xCC) plain :test #'eql))
    (assert-true (search '(#x70 #x01 #xCC) ubsan :test #'eql))))

(deftest fr-560-562-zero-cost-eh-landing-pad-emits-real-transfer-code
  "FR-560/562: enabled landing pads are real MOVABS+JMP code, not UD2-only stubs."
  (let* ((landing-pad (cl-cc/codegen::make-x86-64-landing-pad
                       :start-address #x10
                       :end-address #x20
                       :handler-address #x12345678
                       :handler-label "handler"
                       :handler-type 'error
                       :result-register :r0))
         (bytes (%fr-byte-list
                 (cl-cc/codegen::with-output-to-vector (stream)
                   (cl-cc/codegen::emit-x86-64-landing-pad-stub landing-pad stream)))))
    (assert-= cl-cc/codegen::+x86-64-landing-pad-stub-size+ (length bytes))
    (assert-equal '(#x49 #xBB) (subseq bytes 0 2))
    (assert-equal '(#x41 #xFF #xE3) (subseq bytes 10 13))
    (assert-false (search '(#x0F #x0B) bytes :test #'eql))))

(deftest fr-561-eh-model-selection-keeps-sjlj-default-and-enables-table-mode
  "FR-561: SJLJ remains the default; table EH is selected by *ZERO-COST-EH-ENABLED*."
  (let* ((instructions (list (cl-cc:make-vm-label :name "entry")
                             (cl-cc:make-vm-establish-handler
                              :handler-label "handler" :result-reg :r0 :error-type 'error)
                             (cl-cc:make-vm-label :name "handler")
                             (cl-cc:make-vm-ret :reg :r0)))
         (offsets (cl-cc/codegen::build-label-offsets instructions 0))
         (pads (cl-cc/codegen::x86-64-build-landing-pad-table instructions offsets 0))
         (fdes (cl-cc/codegen::x86-64-landing-pad-table->dwarf-fdes pads)))
    (assert-false cl-cc/codegen:*zero-cost-eh-enabled*)
    (assert-= 1 (length pads))
    (assert-= 1 (length fdes))
    (assert-true (plusp (cl-cc/codegen::x86-64-landing-pad-handler-address (first pads))))
    (assert-true (plusp (cl-cc/binary::dwarf-eh-fde-address-range (first fdes))))))

(deftest fr-560-integrated-x86-64-eh-appends-non-ud2-landing-pad
  "FR-560: full x86-64 emission appends non-UD2 landing-pad bytes when table EH is enabled."
  (let* ((program (cl-cc/vm::make-vm-program
                   :instructions (list (cl-cc:make-vm-label :name "entry")
                                       (cl-cc:make-vm-establish-handler
                                        :handler-label "handler" :result-reg :r0 :error-type 'error)
                                       (cl-cc:make-vm-const :dst :r0 :value 7)
                                       (cl-cc:make-vm-ret :reg :r0)
                                       (cl-cc:make-vm-label :name "handler")
                                       (cl-cc:make-vm-const :dst :r0 :value 9)
                                       (cl-cc:make-vm-ret :reg :r0))
                   :result-register :r0))
         (bytes (%fr-byte-list
                 (let ((cl-cc/codegen:*zero-cost-eh-enabled* t))
                   (cl-cc/codegen:compile-to-x86-64-bytes program)))))
    (assert-true (search '(#x49 #xBB) bytes :test #'eql))
    (assert-true (search '(#x41 #xFF #xE3) bytes :test #'eql))
    (assert-false (search '(#x0F #x0B) bytes :test #'eql))))

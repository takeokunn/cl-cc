;;;; packages/emit/src/ebpf.lisp — verifier-friendly eBPF backend helpers

(in-package :cl-cc/emit)

(defparameter +ebpf-target-flag+ "--target=ebpf"
  "Opt-in target selector for the eBPF backend.  The backend is never enabled by default.")

(defconstant +ebpf-instruction-size+ 8
  "eBPF instructions are fixed-width 64-bit records.")

(defconstant +ebpf-max-verifier-instructions+ 4096
  "Conservative instruction cap for small verifier-friendly programs.")

(defconstant +ebpf-op-exit+ #x95)
(defconstant +ebpf-op-call+ #x85)
(defconstant +ebpf-op-mov64-imm+ #xb7)
(defconstant +ebpf-op-mov64-reg+ #xbf)
(defconstant +ebpf-op-add64-imm+ #x07)
(defconstant +ebpf-op-add64-reg+ #x0f)
(defconstant +ebpf-op-ld-map-fd+ #x18)
(defconstant +ebpf-pseudo-map-fd+ 1)

(defconstant +bpf-helper-map-lookup-elem+ 1)
(defconstant +bpf-helper-map-update-elem+ 2)

(defconstant +bpf-map-type-hash+ 1)
(defconstant +bpf-map-type-array+ 2)

(defstruct (ebpf-program-plan (:constructor make-ebpf-program-plan))
  program-name
  hook-point
  instructions
  maps
  uses-helpers-p
  verifier-safe-p)

(defstruct (ebpf-map-def (:constructor make-ebpf-map-def
                                       (&key name type key-size value-size max-entries flags fd)))
  (name "map" :type string)
  (type +bpf-map-type-array+ :type integer)
  (key-size 4 :type integer)
  (value-size 8 :type integer)
  (max-entries 1 :type integer)
  (flags 0 :type integer)
  (fd 0 :type integer))

(defun ebpf-target-flag-p (target)
  "Return true when TARGET selects the eBPF backend (`:EBPF' or `--target=ebpf')."
  (or (eq target :ebpf)
      (and (stringp target) (string= target +ebpf-target-flag+))
      (and (stringp target) (string= target "ebpf"))))

(defun %u8 (n) (logand n #xff))
(defun %u16 (n) (logand n #xffff))
(defun %u32 (n) (logand n #xffffffff))

(defun %byte-vector (&rest bytes)
  (coerce (mapcar #'%u8 bytes) '(simple-array (unsigned-byte 8) (*))))

(defun %append-bytes (&rest vectors)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0)))
    (dolist (vector vectors)
      (loop for byte across vector do (vector-push-extend byte out)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun %ebpf-emit-insn (opcode dst src off imm)
  "Emit one eBPF instruction: opcode, dst/src nibble, signed offset, immediate.

The resulting vector is exactly eight bytes in the kernel-defined little-endian
layout: opcode | dst:src | off16 | imm32."
  (let ((off16 (%u16 off))
        (imm32 (%u32 imm)))
    (%byte-vector opcode
                  (logior (ash (%u8 src) 4) (%u8 dst))
                  off16 (ash off16 -8)
                  imm32 (ash imm32 -8) (ash imm32 -16) (ash imm32 -24))))

(defun %ebpf-emit-ld-map-fd (dst fd)
  "Emit BPF_LD_MAP_FD pseudo load.  This is a two-instruction 64-bit immediate."
  (%append-bytes
   (%ebpf-emit-insn +ebpf-op-ld-map-fd+ dst +ebpf-pseudo-map-fd+ 0 fd)
   (%ebpf-emit-insn 0 0 0 0 0)))

(defun %ebpf-valid-register-p (reg)
  (and (integerp reg) (<= 0 reg 10)))

(defun %ebpf-check-register (reg inst)
  (unless (%ebpf-valid-register-p reg)
    (error "Invalid eBPF register R~A in ~S; verifier-visible registers are R0..R10"
           reg inst)))

(defun %ebpf-helper-id (helper)
  (etypecase helper
    (integer helper)
    (symbol
     (ecase helper
       (:bpf-map-lookup-elem +bpf-helper-map-lookup-elem+)
       (:bpf-map-update-elem +bpf-helper-map-update-elem+)))))

(defun %ebpf-lower-inst (inst)
  "Lower INST to encoded eBPF bytes.

Supported verifier-friendly forms:
  (:exit)
  (:mov64-imm dst imm)       ; BPF_MOV64 immediate
  (:mov64-reg dst src)       ; BPF_MOV64 register
  (:add64-imm dst imm)       ; BPF_ADD64 immediate
  (:add64-reg dst src)       ; BPF_ADD64 register
  (:ld-map-fd dst fd)        ; BPF_LD_MAP_FD pseudo instruction
  (:call helper-or-id)       ; BPF_CALL, including map lookup/update helpers"
  (unless (consp inst)
    (error "Invalid eBPF instruction: ~S" inst))
  (case (car inst)
    (:exit
     (%ebpf-emit-insn +ebpf-op-exit+ 0 0 0 0))
    (:mov64-imm
     (destructuring-bind (_ dst imm) inst
       (declare (ignore _))
       (%ebpf-check-register dst inst)
       (%ebpf-emit-insn +ebpf-op-mov64-imm+ dst 0 0 imm)))
    (:mov64-reg
     (destructuring-bind (_ dst src) inst
       (declare (ignore _))
       (%ebpf-check-register dst inst)
       (%ebpf-check-register src inst)
       (%ebpf-emit-insn +ebpf-op-mov64-reg+ dst src 0 0)))
    (:add64-imm
     (destructuring-bind (_ dst imm) inst
       (declare (ignore _))
       (%ebpf-check-register dst inst)
       (%ebpf-emit-insn +ebpf-op-add64-imm+ dst 0 0 imm)))
    (:add64-reg
     (destructuring-bind (_ dst src) inst
       (declare (ignore _))
       (%ebpf-check-register dst inst)
       (%ebpf-check-register src inst)
       (%ebpf-emit-insn +ebpf-op-add64-reg+ dst src 0 0)))
    (:ld-map-fd
     (destructuring-bind (_ dst fd) inst
       (declare (ignore _))
       (%ebpf-check-register dst inst)
       (%ebpf-emit-ld-map-fd dst fd)))
    (:call
     (destructuring-bind (_ helper) inst
       (declare (ignore _))
       (%ebpf-emit-insn +ebpf-op-call+ 0 0 0 (%ebpf-helper-id helper))))
    (otherwise
     (error "Unsupported eBPF instruction form: ~S" inst))))

(defun %ebpf-instruction-heap-op-p (insn)
  "Conservative guard: reject obvious heap operations for eBPF lowering."
  (and (consp insn)
       (member (car insn) '(alloc malloc free make-array make-instance cons)
               :test #'eq)))

(defun %ebpf-program-exits-p (instructions)
  (and instructions (eq (caar (last instructions)) :exit)))

(defun validate-ebpf-verifier-constraints (instructions)
  "Validate the subset that this backend promises to emit for the kernel verifier."
  (when (> (length instructions) +ebpf-max-verifier-instructions+)
    (error "eBPF program has ~D instructions, over verifier-friendly cap ~D"
           (length instructions) +ebpf-max-verifier-instructions+))
  (when (find-if #'%ebpf-instruction-heap-op-p instructions)
    (error "eBPF lowering rejects heap allocation and host heap operations"))
  (unless (%ebpf-program-exits-p instructions)
    (error "eBPF programs emitted by this backend must end with BPF_EXIT"))
  t)

(defun plan-ebpf-program (program-name instructions &key (hook-point :xdp)
                                                   (maps nil)
                                                   (uses-helpers-p t))
  "Build an eBPF plan while enforcing basic verifier-oriented constraints."
  (validate-ebpf-verifier-constraints instructions)
  (make-ebpf-program-plan :program-name program-name
                          :hook-point hook-point
                          :instructions instructions
                          :maps maps
                          :uses-helpers-p uses-helpers-p
                          :verifier-safe-p t))

(defun emit-ebpf-bytecode (instructions)
  "Encode INSTRUCTIONS to an eBPF bytecode vector."
  (validate-ebpf-verifier-constraints instructions)
  (apply #'%append-bytes (mapcar #'%ebpf-lower-inst instructions)))

(defun %emit-le (out value bytes)
  (dotimes (i bytes)
    (vector-push-extend (%u8 (ash value (* -8 i))) out)))

(defun %emit-cstr (out string)
  (loop for ch across string do (vector-push-extend (%u8 (char-code ch)) out))
  (vector-push-extend 0 out))

(defun %bytes-of-string (string &key nul)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0)))
    (loop for ch across string do (vector-push-extend (%u8 (char-code ch)) out))
    (when nul (vector-push-extend 0 out))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun %ebpf-map-def-bytes (map)
  "Return libbpf legacy map definition bytes: type/key/value/max_entries/flags."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0)))
    (dolist (value (list (ebpf-map-def-type map)
                         (ebpf-map-def-key-size map)
                         (ebpf-map-def-value-size map)
                         (ebpf-map-def-max-entries map)
                         (ebpf-map-def-flags map)))
      (%emit-le out value 4))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun %align-up (value alignment)
  (* alignment (ceiling value alignment)))

(defun %section-name-offsets (names)
  (let ((offset 1)
        (table (make-hash-table :test #'equal)))
    (dolist (name names table)
      (setf (gethash name table) offset)
      (incf offset (1+ (length name))))))

(defun %string-table (strings)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0))
        (offsets (make-hash-table :test #'equal)))
    (vector-push-extend 0 out)
    (dolist (string strings)
      (unless (gethash string offsets)
        (setf (gethash string offsets) (length out))
        (%emit-cstr out string)))
    (values (coerce out '(simple-array (unsigned-byte 8) (*))) offsets)))

(defun %emit-elf64-symbol (out name-offset info shndx value size)
  (%emit-le out name-offset 4)
  (%emit-le out info 1)
  (%emit-le out 0 1)
  (%emit-le out shndx 2)
  (%emit-le out value 8)
  (%emit-le out size 8))

(defun %emit-elf64-section-header (out name type flags addr offset size link info align entsize)
  (%emit-le out name 4)
  (%emit-le out type 4)
  (%emit-le out flags 8)
  (%emit-le out addr 8)
  (%emit-le out offset 8)
  (%emit-le out size 8)
  (%emit-le out link 4)
  (%emit-le out info 4)
  (%emit-le out align 8)
  (%emit-le out entsize 8))

(defun %ebpf-section-name (hook-point)
  (ecase hook-point
    (:xdp "xdp")
    (:tc "classifier")
    (:socket-filter "socket")
    (:kprobe "kprobe/clcc")
    (:tracepoint "tracepoint/clcc")))

(defun emit-ebpf-elf-object (program-name instructions &key (hook-point :xdp)
                                                       (maps nil)
                                                       (license "GPL"))
  "Generate a minimal ELF64 relocatable eBPF object loadable by libbpf/bpftool.

The object contains a program section (for example `xdp'), a `license' section,
an optional `.maps' section with legacy map definitions, plus symbol/string and
section-name tables.  Map FD relocation is intentionally left to libbpf-style
loaders; direct bytecode can still use (:LD-MAP-FD dst fd) when an FD is known."
  (let* ((code (emit-ebpf-bytecode instructions))
         (prog-section (%ebpf-section-name hook-point))
         (maps-bytes (if maps
                         (apply #'%append-bytes (mapcar #'%ebpf-map-def-bytes maps))
                         #()))
         (license-bytes (%bytes-of-string license :nul t))
         (section-names (if maps
                            (list prog-section "license" ".maps" ".symtab" ".strtab" ".shstrtab")
                            (list prog-section "license" ".symtab" ".strtab" ".shstrtab")))
         (name-offsets (%section-name-offsets section-names))
         (shstr (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                                       :adjustable t :fill-pointer 0)))
                  (vector-push-extend 0 out)
                  (dolist (name section-names)
                    (%emit-cstr out name))
                  (coerce out '(simple-array (unsigned-byte 8) (*)))))
         (symbol-names (append (list "" program-name)
                               (mapcar #'ebpf-map-def-name maps)))
         (strtab nil)
         (strtab-offsets nil))
    (multiple-value-setq (strtab strtab-offsets) (%string-table symbol-names))
    (let* ((symtab (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0))
           (section-index-prog 1)
           (section-index-maps (and maps 3))
           (symtab-index (if maps 4 3))
           (strtab-index (if maps 5 4))
           (shstr-index (if maps 6 5)))
      (%emit-elf64-symbol symtab 0 0 0 0 0)
      (%emit-elf64-symbol symtab (gethash program-name strtab-offsets) #x12
                         section-index-prog 0 (length code))
      (dolist (map maps)
        (%emit-elf64-symbol symtab (gethash (ebpf-map-def-name map) strtab-offsets) #x11
                           section-index-maps 0 20))
      (let* ((sections (if maps
                           (list (list prog-section code 6 8 0 0)       ; SHF_ALLOC|SHF_EXECINSTR
                                 (list "license" license-bytes 3 1 0 0) ; SHF_ALLOC|SHF_WRITE
                                 (list ".maps" maps-bytes 3 8 0 0)
                                 (list ".symtab" (coerce symtab '(simple-array (unsigned-byte 8) (*))) 0 8 strtab-index 1)
                                 (list ".strtab" strtab 0 1 0 0)
                                 (list ".shstrtab" shstr 0 1 0 0))
                           (list (list prog-section code 6 8 0 0)
                                 (list "license" license-bytes 3 1 0 0)
                                 (list ".symtab" (coerce symtab '(simple-array (unsigned-byte 8) (*))) 0 8 strtab-index 1)
                                 (list ".strtab" strtab 0 1 0 0)
                                 (list ".shstrtab" shstr 0 1 0 0))))
             (file-offset 64)
             (layout nil))
        (dolist (section sections)
          (destructuring-bind (_ data flags align link info) section
            (declare (ignore _ flags link info))
            (setf file-offset (%align-up file-offset align))
            (push (append section (list file-offset)) layout)
            (incf file-offset (length data))))
        (setf layout (nreverse layout))
        (let* ((shoff (%align-up file-offset 8))
               (out (make-array 0 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
          ;; ELF64 little-endian relocatable for EM_BPF (247).
          (dolist (byte '(#x7f #x45 #x4c #x46 #x02 #x01 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
            (vector-push-extend byte out))
          (%emit-le out 1 2)       ; ET_REL
          (%emit-le out 247 2)     ; EM_BPF
          (%emit-le out 1 4)
          (%emit-le out 0 8)
          (%emit-le out 0 8)
          (%emit-le out shoff 8)
          (%emit-le out 0 4)
          (%emit-le out 64 2)
          (%emit-le out 0 2)
          (%emit-le out 0 2)
          (%emit-le out 64 2)
          (%emit-le out (1+ (length sections)) 2)
          (%emit-le out shstr-index 2)
          (dolist (entry layout)
            (destructuring-bind (_ data flags align link info offset) entry
              (declare (ignore _ flags align link info))
              (loop while (< (length out) offset) do (vector-push-extend 0 out))
              (loop for byte across data do (vector-push-extend byte out))))
          (loop while (< (length out) shoff) do (vector-push-extend 0 out))
          (%emit-elf64-section-header out 0 0 0 0 0 0 0 0 0 0)
          (dolist (entry layout)
            (destructuring-bind (name data flags align link info offset) entry
              (%emit-elf64-section-header
               out (gethash name name-offsets)
               (if (string= name ".symtab") 2 1)
               flags 0 offset (length data) link info align
               (if (string= name ".symtab") 24 0))))
          (coerce out '(simple-array (unsigned-byte 8) (*))))))))

(defun compile-ebpf-program (program-name instructions &key (hook-point :xdp)
                                                      (maps nil)
                                                      (uses-helpers-p t)
                                                      (emit-elf-p nil))
  "Plan and encode an eBPF program.

Returns PLAN and BYTECODE by default.  When EMIT-ELF-P is true, the second value
is a minimal ELF object instead of raw bytecode."
  (let ((plan (plan-ebpf-program program-name instructions
                                 :hook-point hook-point
                                 :maps maps
                                 :uses-helpers-p uses-helpers-p)))
    (values plan
            (if emit-elf-p
                (emit-ebpf-elf-object program-name instructions
                                      :hook-point hook-point
                                      :maps maps)
                (emit-ebpf-bytecode (ebpf-program-plan-instructions plan))))))

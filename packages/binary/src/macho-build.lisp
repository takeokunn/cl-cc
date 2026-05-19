;;;; packages/binary/src/macho-build.lisp — Mach-O Builder API
;;;
;;; Contains:
;;;   - make-mach-o-builder — factory for :x86-64 / :arm64
;;;   - add-text-segment, %make-pagezero-segment, add-data-segment
;;;   - add-symbol, add-entry-point
;;;   - build-mach-o — assemble all segments → byte array
;;;   - write-mach-o-file — write byte array to file
;;;
;;; Load order: after emit/binary/macho-serialize.lisp.
;;; (mach-o-builder class defined in macho-serialize.lisp;
;;;  constants, structs, buffer helpers, serialization primitives in macho.lisp)
(in-package :cl-cc/binary)


(defparameter *arch-cpu-specs*
  '((:x86-64 #.+cpu-type-x86-64+ #.+cpu-subtype-x86-64-all+)
    (:arm64  #.+cpu-type-arm64+  #.+cpu-subtype-arm64-all+))
  "Alist of (arch cputype cpusubtype) constants for Mach-O header construction.")

(defun make-mach-o-builder (arch)
  "Create a Mach-O builder for the specified architecture.
ARCH should be :X86-64 or :ARM64."
  (declare (type (member :x86-64 :arm64) arch))
  (let* ((cpu-spec (or (assoc arch *arch-cpu-specs*)
                       (error "Unknown Mach-O arch: ~S" arch)))
          (builder (make-instance 'mach-o-builder)))
    (setf (slot-value builder 'header)
          (make-mach-header :cputype    (second cpu-spec)
                            :cpusubtype (third  cpu-spec)))
     (setf (mach-o-builder-entry-point builder)
           (make-entry-point-command))
    (setf (gethash "/usr/lib/libSystem.B.dylib"
                   (mach-o-builder-bind-ordinal-table builder))
          1)
     builder))

(defun add-text-segment (builder code-bytes &key (base-addr #x100000000))
  "Add __TEXT segment with code to BUILDER.
CODE-BYTES should be a simple-array of (unsigned-byte 8).
BASE-ADDR is the virtual memory address for the segment.

The byte order is preserved exactly; native pipeline layout passes such as
PIPELINE-REORDER-FUNCTIONS must run before CODE-BYTES is handed to the Mach-O
builder."
  (declare (type mach-o-builder builder)
           (type (simple-array (unsigned-byte 8) (*)) code-bytes))
  (let* ((code-size (length code-bytes))
         (text-section (make-section
                        :sectname "__text"
                        :segname "__TEXT"
                        :addr base-addr
                        :size code-size
                        :align 4  ; 2^4 = 16-byte alignment
                        :flags (logior +s-attr-pure-instructions+
                                       +s-attr-some-instructions+)))
         (text-segment (make-segment-command
                        :segname "__TEXT"
                        :vmaddr base-addr
                        :vmsize (align-up code-size #x1000)
                        :payload code-bytes
                        :nsects 1
                        :cmdsize (+ 72 (* 80 1))  ; segment header + 1 section
                        :sections (list text-section))))
    (push text-segment (mach-o-builder-segments builder))
    builder))

;;; FR-406: distribution compression support.
;;;
;;; SBCL versions that provide SB-EXT:COMPRESS can produce zlib/deflate payloads
;;; directly.  The current development SBCL used by CI may not expose that
;;; symbol, so compression is a capability probe rather than a reader-time
;;; dependency.  When unavailable or unsuccessful, callers fall back to the
;;; original byte vector and omit compressed section flags.

(defconstant +cl-cc-compression-none+ 0
  "No compression was applied.")

(defconstant +cl-cc-compression-zlib+ 1
  "zlib/deflate compression algorithm identifier.")

(defun %ub8-vector (bytes)
  "Return BYTES as a simple unsigned-byte 8 vector."
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun %find-sb-ext-compress ()
  "Return SB-EXT:COMPRESS when the host SBCL provides it, otherwise NIL."
  (let ((package (find-package "SB-EXT")))
    (when package
      (multiple-value-bind (symbol status) (find-symbol "COMPRESS" package)
        (when (and symbol status (fboundp symbol))
          (symbol-function symbol))))))

(defun maybe-compress-code-bytes (code-bytes &key compress)
  "Optionally compress CODE-BYTES using SB-EXT:COMPRESS.

Returns four values: payload bytes, compression algorithm identifier, original
size, and compressed size.  When compression is disabled or unavailable, the
payload is CODE-BYTES and the algorithm is +CL-CC-COMPRESSION-NONE+."
  (declare (type (simple-array (unsigned-byte 8) (*)) code-bytes))
  (let ((original-size (length code-bytes)))
    (if compress
        (let ((compress-fn (%find-sb-ext-compress)))
          (if compress-fn
              (handler-case
                  (let ((compressed (%ub8-vector (funcall compress-fn code-bytes))))
                    (if (< (length compressed) original-size)
                        (values compressed +cl-cc-compression-zlib+
                                original-size (length compressed))
                        (values code-bytes +cl-cc-compression-none+
                                original-size original-size)))
                (error ()
                  (values code-bytes +cl-cc-compression-none+
                          original-size original-size)))
              (values code-bytes +cl-cc-compression-none+
                      original-size original-size)))
        (values code-bytes +cl-cc-compression-none+ original-size original-size))))

(defun build-compression-metadata (algorithm original-size compressed-size)
  "Build a compact CL-CC compression metadata header.

Header layout, little-endian:
  magic "CLCZ" | version u32 | algorithm u32 | original-size u64 |
  compressed-size u64."
  (let ((buf (elf-make-buffer)))
    (binary-buffer-write-bytes buf (map 'vector #'char-code "CLCZ"))
    (binary-buffer-write-u32le buf 1)
    (binary-buffer-write-u32le buf algorithm)
    (binary-buffer-write-u64le buf original-size)
    (binary-buffer-write-u64le buf compressed-size)
    (binary-buffer-to-array buf)))

(defun build-compressed-code-payload (compressed-bytes algorithm original-size compressed-size)
  "Return metadata followed by COMPRESSED-BYTES for a compressed code section."
  (let* ((metadata (build-compression-metadata algorithm original-size compressed-size))
         (payload (make-array (+ (length metadata) (length compressed-bytes))
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))
    (replace payload metadata :start1 0)
    (replace payload compressed-bytes :start1 (length metadata))
    payload))

(defun %make-pagezero-segment ()
  "Create the __PAGEZERO segment required for Mach-O executables."
  (make-segment-command
   :segname "__PAGEZERO"
   :vmaddr 0
   :vmsize #x100000000
   :fileoff 0
   :filesize 0
   :maxprot 0
   :initprot 0
   :nsects 0
   :cmdsize 72
   :sections nil))

(defun %make-linkedit-segment (fileoff filesize vmaddr)
  "Create the __LINKEDIT segment required for code signing."
  (make-segment-command
   :segname "__LINKEDIT"
   :vmaddr vmaddr
   :vmsize (max #x1000 (align-up filesize #x1000))
   :fileoff fileoff
   :filesize filesize
   :maxprot 1
   :initprot 1
   :nsects 0
   :cmdsize 72
   :sections nil))

(defun add-data-segment (builder data-bytes &key (base-addr #x100001000))
  "Add __DATA segment to BUILDER.
DATA-BYTES should be a simple-array of (unsigned-byte 8).
BASE-ADDR is the virtual memory address for the segment."
  (declare (type mach-o-builder builder)
           (type (simple-array (unsigned-byte 8) (*)) data-bytes))
  (let* ((data-size (length data-bytes))
         (data-section (make-section
                        :sectname "__data"
                        :segname "__DATA"
                        :addr base-addr
                        :size data-size
                        :align 4))
          (data-segment (make-segment-command
                         :segname "__DATA"
                         :vmaddr base-addr
                         :vmsize (align-up data-size #x1000)
                         :payload data-bytes
                         :nsects 1
                         :maxprot 6    ; rw-
                         :initprot 6   ; rw
                         :cmdsize (+ 72 (* 80 1))
                         :sections (list data-section))))
    (push data-segment (mach-o-builder-segments builder))
    builder))

(defun add-data-const-segment (builder const-bytes &key (base-addr #x100002000))
  "Add a read-only __DATA_CONST segment for immutable constants.

String literals and constant pools should use this segment instead of __DATA.
It is emitted with read-only max/init protections (r--) so writes to mapped
constant data are rejected by the operating system."
  (declare (type mach-o-builder builder)
           (type (simple-array (unsigned-byte 8) (*)) const-bytes))
  (let* ((const-size (length const-bytes))
         (const-section (make-section
                         :sectname "__const"
                         :segname "__DATA_CONST"
                         :addr base-addr
                         :size const-size
                         :align 4))
         (const-segment (make-segment-command
                         :segname "__DATA_CONST"
                         :vmaddr base-addr
                         :vmsize (align-up const-size #x1000)
                         :payload const-bytes
                         :nsects 1
                         :maxprot 4    ; r--
                         :initprot 4   ; r--
                         :cmdsize (+ 72 (* 80 1))
                         :sections (list const-section))))
    (push const-segment (mach-o-builder-segments builder))
    builder))

(defun macho-build-unwind-info (code-size &key
                                            (encoding +compact-unwind-x86-64-mode-stack-immd+)
                                            (personality 0))
  "Build a compact __unwind_info payload with one frameless function entry.

The payload starts with a small CL-CC table header followed by a per-function
entry: start address, function length, compact unwind ENCODING, and
PERSONALITY.  The default encoding models frameless/RBP-less x86-64 functions.
This Pure CL backend emits the documented interface without platform mprotect
or linker-private helpers."
  (let ((buf (elf-make-buffer)))
    ;; Signature/version for CL-CC's compact table envelope.
    (binary-buffer-write-bytes buf (map 'vector #'char-code "CLCU"))
    (binary-buffer-write-u32le buf 1)         ; version
    (binary-buffer-write-u32le buf 1)         ; entry count
    (binary-buffer-write-u32le buf 16)        ; first entry offset
    ;; Entry: start address (section-relative), length, encoding, personality.
    (binary-buffer-write-u32le buf 0)
    (binary-buffer-write-u32le buf code-size)
    (binary-buffer-write-u32le buf encoding)
    (binary-buffer-write-u32le buf personality)
    (binary-buffer-to-array buf)))

(defun %mach-o-ensure-text-and-unwind (user-segments code-bytes &key compress)
  "Return USER-SEGMENTS with __TEXT unwind info and read-only constants support."
  (multiple-value-bind (compressed-code algorithm original-size compressed-size)
      (maybe-compress-code-bytes code-bytes :compress compress)
    (let* ((code-size (length code-bytes))
           (compressed-payload (when (/= algorithm +cl-cc-compression-none+)
                                 (build-compressed-code-payload compressed-code algorithm
                                                                original-size compressed-size)))
           (unwind-bytes (macho-build-unwind-info code-size))
           (compressed-offset (and compressed-payload (align-up code-size 4)))
           (unwind-offset (align-up (if compressed-payload
                                        (+ compressed-offset (length compressed-payload))
                                        code-size)
                                    4))
          (payload-size (+ unwind-offset (length unwind-bytes)))
          (payload (make-array payload-size :element-type '(unsigned-byte 8)
                               :initial-element 0))
          (text-seg (find "__TEXT" user-segments
                          :key #'segment-command-segname :test #'string=)))
    (replace payload code-bytes :start1 0)
    (when compressed-payload
      (replace payload compressed-payload :start1 compressed-offset))
    (replace payload unwind-bytes :start1 unwind-offset)
    (unless text-seg
      (setf text-seg (make-segment-command
                      :segname "__TEXT"
                      :vmaddr #x100000000
                      :vmsize (align-up payload-size #x1000)
                      :nsects 0
                      :cmdsize 72
                      :sections nil))
      (push text-seg user-segments))
    (let ((text-section (or (find "__text" (segment-command-sections text-seg)
                                  :key #'section-sectname :test #'string=)
                            (make-section :sectname "__text"
                                          :segname "__TEXT"
                                          :size code-size
                                          :align 4
                                          :flags (logior +s-attr-pure-instructions+
                                                         +s-attr-some-instructions+))))
          (compressed-section (when compressed-payload
                                (or (find "__compressed" (segment-command-sections text-seg)
                                          :key #'section-sectname :test #'string=)
                                    (make-section :sectname "__compressed"
                                                  :segname "__TEXT"
                                                  :size (length compressed-payload)
                                                  :align 4))))
          (unwind-section (or (find "__unwind_info" (segment-command-sections text-seg)
                                     :key #'section-sectname :test #'string=)
                               (make-section :sectname "__unwind_info"
                                             :segname "__TEXT"
                                             :size (length unwind-bytes)
                                             :align 2))))
      (setf (section-size text-section) code-size
            (section-size unwind-section) (length unwind-bytes)
            (segment-command-payload text-seg) payload
            (segment-command-nsects text-seg) (if compressed-payload 3 2)
            (segment-command-cmdsize text-seg) (+ 72 (* 80 (segment-command-nsects text-seg)))
            (segment-command-maxprot text-seg) 5
            (segment-command-initprot text-seg) 5
            (segment-command-sections text-seg) (if compressed-payload
                                                    (list text-section compressed-section unwind-section)
                                                    (list text-section unwind-section)))
      (when compressed-section
        (setf (section-size compressed-section) (length compressed-payload))))
    (unless (find "__DATA_CONST" user-segments
                  :key #'segment-command-segname :test #'string=)
      ;; Emit an empty read-only constant segment by default. Frontends with
      ;; string literals/constant pools can call ADD-DATA-CONST-SEGMENT to fill
      ;; this section; keeping the segment present documents and preserves the
      ;; protection boundary in every binary.
      (let* ((const-bytes (make-array 0 :element-type '(unsigned-byte 8)))
             (const-section (make-section :sectname "__const"
                                          :segname "__DATA_CONST"
                                          :addr #x100002000
                                          :size 0
                                          :align 4))
             (const-segment (make-segment-command :segname "__DATA_CONST"
                                                  :vmaddr #x100002000
                                                  :vmsize #x1000
                                                  :payload const-bytes
                                                  :nsects 1
                                                  :maxprot 4
                                                  :initprot 4
                                                  :cmdsize (+ 72 80)
                                                  :sections (list const-section))))
        (push const-segment user-segments)))
    user-segments)))

(defun add-symbol (builder name &key (value 0) (type 0) (sect 1))
  "Add a symbol to the builder's symbol table.
NAME is the symbol name string.
VALUE is the symbol's address/value.
TYPE is the symbol type byte.
SECT is the section number."
  (declare (type mach-o-builder builder)
           (type string name))
  (let ((strx (fill-pointer (mach-o-builder-string-table builder))))
    ;; Add name to string table (null-terminated)
    (loop for char across name
          do (vector-push-extend (char-code char)
                                 (mach-o-builder-string-table builder)))
    (vector-push-extend 0 (mach-o-builder-string-table builder))
    ;; Create nlist entry
    (setf (gethash name (mach-o-builder-symbol-index builder))
          (length (mach-o-builder-symbol-table builder)))
    (push (make-nlist :n-strx strx
                      :n-type type
                      :n-sect sect
                      :n-value value)
          (mach-o-builder-symbol-table builder)))
  builder)

(defun %mach-o-symbol-index (builder name)
  "Return the Mach-O symbol table index for NAME, or NIL when absent."
  (gethash name (mach-o-builder-symbol-index builder)))

(defun %ensure-undefined-symbol (builder name)
  "Ensure NAME exists as an undefined external symbol and return its index."
  (or (%mach-o-symbol-index builder name)
      (progn
        (add-symbol builder name :type (logior +n-undef+ +n-ext+) :sect 0)
        (%mach-o-symbol-index builder name))))

(defun add-relocation (builder offset symbol-name
                       &key (pcrel 1) (length 2) (extern 1)
                         (type +x86-64-reloc-branch+) (section "__text"))
  "Add a Mach-O relocation against SYMBOL-NAME at section-relative OFFSET.

This is suitable for unresolved external calls by default.  GOT references can
pass TYPE as +X86-64-RELOC-GOT-LOAD+, +ARM64-RELOC-PAGE21+, or
+ARM64-RELOC-PAGEOFF12+ as appropriate."
  (declare (type mach-o-builder builder)
           (type (unsigned-byte 32) offset)
           (type string symbol-name section))
  (%ensure-undefined-symbol builder symbol-name)
  (push (list :section section
              :offset offset
              :symbol symbol-name
              :pcrel pcrel
              :length length
              :extern extern
              :type type)
        (mach-o-builder-relocations builder))
  builder)

(defun %mach-o-relocation-infos (builder)
  "Resolve pending builder relocation references into relocation-info records."
  (loop for reloc in (nreverse (mach-o-builder-relocations builder))
        for symbol = (getf reloc :symbol)
        collect (make-relocation-info
                 :r-address (getf reloc :offset)
                 :r-symbolnum (%ensure-undefined-symbol builder symbol)
                 :r-pcrel (getf reloc :pcrel)
                 :r-length (getf reloc :length)
                 :r-extern (getf reloc :extern)
                 :r-type (getf reloc :type))))

(defun %macho-build-bind-opcodes (symbol-names)
  "Build a minimal dyld bind opcode stream for external SYMBOL-NAMES."
  (let ((buf (elf-make-buffer)))
    (dolist (name symbol-names)
      ;; BIND_OPCODE_SET_DYLIB_ORDINAL_IMM | 1 (libSystem)
      (binary-buffer-write-u8 buf #x11)
      ;; BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM | 0, followed by C string.
      (binary-buffer-write-u8 buf #x40)
      (loop for c across name
            do (binary-buffer-write-u8 buf (char-code c)))
      (binary-buffer-write-u8 buf 0)
      ;; BIND_OPCODE_SET_TYPE_IMM | BIND_TYPE_POINTER.
      (binary-buffer-write-u8 buf #x51)
      ;; BIND_OPCODE_DO_BIND.  Segment/offset binding is supplied by relocation
      ;; entries; this stream records the external symbol resolution intent.
      (binary-buffer-write-u8 buf #x90))
    (when symbol-names
      ;; BIND_OPCODE_DONE.
      (binary-buffer-write-u8 buf 0))
    (binary-buffer-to-array buf)))

(defun %buffer-write-u32be (buffer value)
  "Write VALUE as big-endian u32 to byte BUFFER."
  (binary-buffer-write-u8 buffer (logand (ash value -24) #xFF))
  (binary-buffer-write-u8 buffer (logand (ash value -16) #xFF))
  (binary-buffer-write-u8 buffer (logand (ash value -8) #xFF))
  (binary-buffer-write-u8 buffer (logand value #xFF)))

(defun %macho-build-code-directory (code-limit)
  "Build a compact ad-hoc CodeDirectory for CODE-LIMIT bytes.

The emitted CodeDirectory is intentionally self-contained and leaves page hashes
empty when no host SHA-256 provider is available; WRITE-MACH-O-FILE can invoke
the platform codesign tool to replace it with a system-authored signature."
  (let* ((identifier "cl-cc")
         (ident-offset 44)
         (hash-offset (align-up (+ ident-offset (1+ (length identifier))) 4))
         (length hash-offset)
         (buf (elf-make-buffer)))
    (%buffer-write-u32be buf #xfade0c02)       ; CSMAGIC_CODEDIRECTORY
    (%buffer-write-u32be buf length)
    (%buffer-write-u32be buf #x00020400)       ; version
    (%buffer-write-u32be buf #x00000002)       ; ad-hoc
    (%buffer-write-u32be buf hash-offset)
    (%buffer-write-u32be buf ident-offset)
    (%buffer-write-u32be buf 0)                ; nSpecialSlots
    (%buffer-write-u32be buf 0)                ; nCodeSlots
    (%buffer-write-u32be buf code-limit)
    (binary-buffer-write-u8 buf 32)            ; hashSize: SHA-256
    (binary-buffer-write-u8 buf 2)             ; hashType: SHA-256
    (binary-buffer-write-u8 buf 0)             ; platform
    (binary-buffer-write-u8 buf 12)            ; pageSize: 4 KiB
    (loop for c across identifier
          do (binary-buffer-write-u8 buf (char-code c)))
    (binary-buffer-write-u8 buf 0)
    (binary-buffer-write-pad buf (- hash-offset (length buf)))
    (binary-buffer-to-array buf)))

(defun %macho-build-code-signature (code-limit)
  "Build an embedded code-signature SuperBlob containing a CodeDirectory." 
  (let* ((code-directory (%macho-build-code-directory code-limit))
         (total-size (+ 20 (length code-directory)))
         (buf (elf-make-buffer)))
    (%buffer-write-u32be buf #xfade0cc0)       ; CSMAGIC_EMBEDDED_SIGNATURE
    (%buffer-write-u32be buf total-size)
    (%buffer-write-u32be buf 1)                ; one blob
    (%buffer-write-u32be buf 0)                ; CSSLOT_CODEDIRECTORY
    (%buffer-write-u32be buf 20)
    (binary-buffer-write-bytes buf code-directory)
    (binary-buffer-to-array buf)))

(defun add-entry-point (builder offset)
  "Add LC_MAIN entry point with file OFFSET."
  (declare (type mach-o-builder builder)
           (type (unsigned-byte 64) offset))
  (setf (entry-point-command-entryoff (mach-o-builder-entry-point builder))
        offset)
  builder)

(defun build-mach-o (builder code-bytes &key compress)
  "Build complete Mach-O executable from BUILDER and CODE-BYTES.
Returns a simple-array of (unsigned-byte 8).

Layout: __PAGEZERO (fileoff=0) + __TEXT (fileoff=0, covers header through code)
+ __LINKEDIT (after code) + LC_LOAD_DYLINKER + LC_MAIN.
__TEXT.fileoff=0 is required by macOS strict validation for code signing.
Function order in CODE-BYTES is emitted unchanged so pipeline-level function
reordering directly controls the final text layout."
  (declare (type mach-o-builder builder)
           (type (simple-array (unsigned-byte 8) (*)) code-bytes))
  (let* ((buffer (make-byte-buffer 65536))
         (header-size 32)
          (user-segments (%mach-o-ensure-text-and-unwind
                          (nreverse (mach-o-builder-segments builder))
                          code-bytes
                          :compress compress))
          (relocations (%mach-o-relocation-infos builder))
          (relocation-size (* 8 (length relocations)))
          (symbols (nreverse (mach-o-builder-symbol-table builder)))
          (string-table (subseq (mach-o-builder-string-table builder)
                                0
                                (fill-pointer (mach-o-builder-string-table builder))))
          (has-symbols (plusp (length symbols)))
          (external-symbols (remove-duplicates
                             (loop for reloc in (mach-o-builder-relocations builder)
                                   collect (getf reloc :symbol))
                             :test #'string=))
          (bind-bytes (%macho-build-bind-opcodes external-symbols))
          (has-bind-info (plusp (length bind-bytes)))
          ;; Command sizes: PAGEZERO + user segs + LINKEDIT + DYLINKER + MAIN
          ;; followed by modern dyld/dylib/code-signature commands.  LC_MAIN is
          ;; intentionally kept at its historical offset for existing readers.
          (pagezero-cmd-size 72)
          (user-seg-cmd-sizes (loop for seg in user-segments
                                    sum (+ 72 (* 80 (segment-command-nsects seg)))))
          (linkedit-cmd-size 72)
          (dylinker-cmd-size 32)
          (main-cmd-size 24)
          (dyld-info-cmd-size 48)
          (dylib-cmd-size (align-up (+ 24 (1+ (length "/usr/lib/libSystem.B.dylib"))) 8))
          (code-signature-cmd-size 16)
          (symtab-cmd-size (if has-symbols 24 0))
          (dysymtab-cmd-size (if has-symbols 80 0))
          (cmds-size (+ pagezero-cmd-size user-seg-cmd-sizes linkedit-cmd-size
                        dylinker-cmd-size main-cmd-size
                        dyld-info-cmd-size dylib-cmd-size code-signature-cmd-size
                        symtab-cmd-size dysymtab-cmd-size))
         ;; Code starts at the first page after header+cmds
         (code-offset (align-up (+ header-size cmds-size) 4096))
         ;; __LINKEDIT starts after all user-segment payloads, aligned to 4096
         (linkedit-fileoff (let ((off code-offset))
                             (dolist (seg user-segments off)
                               (let ((payload-len
                                      (if (and (string= (segment-command-segname seg) "__TEXT")
                                               (zerop (length (segment-command-payload seg))))
                                          (length code-bytes)
                                          (length (segment-command-payload seg)))))
                                 (incf off (align-up payload-len #x1000))))))
         ;; Symbol table and string table go into __LINKEDIT
          (nsyms (length symbols))
          (relocoff (if (plusp relocation-size) linkedit-fileoff 0))
          (symoff (if has-symbols (+ linkedit-fileoff relocation-size) 0))
          (stroff (if has-symbols (+ symoff (* nsyms 18)) 0))
          (strsize (length string-table))
          (bind-off (if has-bind-info
                        (+ linkedit-fileoff relocation-size
                           (if has-symbols (+ (* nsyms 18) strsize) 0))
                        0))
          (bind-size (if has-bind-info (length bind-bytes) 0))
          (code-signature-off (+ linkedit-fileoff relocation-size
                                 (if has-symbols (+ (* nsyms 18) strsize) 0)
                                 bind-size))
          (code-signature-bytes (%macho-build-code-signature code-signature-off))
          (code-signature-size (length code-signature-bytes))
          (linkedit-filesize (+ relocation-size
                                (if has-symbols (+ (* nsyms 18) strsize) 0)
                                bind-size
                                code-signature-size))
          (linkedit-seg (%make-linkedit-segment linkedit-fileoff linkedit-filesize
                                                (+ #x100000000 linkedit-fileoff))))

    ;; Update Mach-O header
    (let ((header (mach-o-builder-header builder)))
      (setf (mach-header-ncmds header)
            ;; PAGEZERO + user segs + LINKEDIT + DYLINKER + MAIN + DYLD_INFO +
            ;; LOAD_DYLIB + CODE_SIGNATURE [+ SYMTAB + DYSYMTAB]
            (+ 7 (length user-segments) (if has-symbols 2 0))
            (mach-header-sizeofcmds header) cmds-size
            (mach-header-flags header) (logior +mh-dyldlink+ +mh-pie+)))

    ;; Set entryoff = code-offset (offset within __TEXT, which starts at fileoff=0)
    (setf (entry-point-command-entryoff (mach-o-builder-entry-point builder))
          code-offset)

    ;; Update user segments: __TEXT gets fileoff=0 covering header through code;
    ;; other segments get sequential file offsets after code.
    (let ((next-off code-offset))
      (dolist (seg user-segments)
        (let* ((is-text (and (string= (segment-command-segname seg) "__TEXT")
                             (zerop (length (segment-command-payload seg)))))
               (payload (if is-text code-bytes (segment-command-payload seg)))
               (payload-len (length payload)))
          (cond
            ((string= (segment-command-segname seg) "__TEXT")
              ;; __TEXT: fileoff=0, covers from file start through end of code
              ;; and the compact __unwind_info table.
               (setf (segment-command-fileoff seg) 0
                    (segment-command-filesize seg) (+ code-offset payload-len)
                    (segment-command-vmsize seg) (align-up (+ code-offset payload-len) #x1000))
              (dolist (sect (segment-command-sections seg))
                (let ((section-delta
                        (loop with delta = 0
                              for prior in (segment-command-sections seg)
                              until (eq prior sect)
                              do (setf delta (align-up (+ delta (section-size prior)) 4))
                              finally (return delta))))
                   (setf (section-offset sect) (+ code-offset section-delta)
                         (section-addr sect) (+ (segment-command-vmaddr seg)
                                                code-offset
                                                section-delta))
                   (when (and (string= (section-sectname sect) "__text")
                              (plusp (length relocations)))
                     (setf (section-reloff sect) relocoff
                           (section-nreloc sect) (length relocations)))))
              (setf next-off (+ code-offset (align-up payload-len #x1000))))
            (t
             ;; Other segments (e.g. __DATA): sequential after code
             (setf (segment-command-fileoff seg) next-off
                   (segment-command-filesize seg) payload-len)
             (dolist (sect (segment-command-sections seg))
               (setf (section-offset sect) next-off))
             (incf next-off (align-up payload-len #x1000)))))))

    ;; Serialize Mach-O header
    (serialize-mach-header (mach-o-builder-header builder) buffer)

    ;; Serialize __PAGEZERO (fileoff=0, filesize=0)
    (serialize-segment-command (%make-pagezero-segment) buffer)

    ;; Serialize user segments with their sections
    (dolist (seg user-segments)
      (serialize-segment-command seg buffer)
      (dolist (sect (segment-command-sections seg))
        (serialize-section sect buffer)))

    ;; Serialize __LINKEDIT
    (serialize-segment-command linkedit-seg buffer)

    ;; Serialize LC_LOAD_DYLINKER
    (serialize-lc-load-dylinker buffer)

    ;; Serialize LC_SYMTAB / LC_DYSYMTAB when symbols exist
    (when has-symbols
      (serialize-symtab-command
       (make-symtab-command :symoff symoff :nsyms nsyms :stroff stroff :strsize strsize)
       buffer)
      (serialize-dysymtab-command
       (make-dysymtab-command :iextdefsym 0
                              :nextdefsym 0
                              :iundefsym 0
                              :nundefsym nsyms
                              :extreloff relocoff
                              :nextrel (length relocations))
       buffer))

    ;; Serialize LC_MAIN
    (serialize-entry-point (mach-o-builder-entry-point builder) buffer)

    ;; Serialize LC_DYLD_INFO_ONLY / LC_LOAD_DYLIB / LC_CODE_SIGNATURE.
    (serialize-dyld-info-command
     (make-dyld-info-command :bind-off bind-off :bind-size bind-size)
     buffer)
    (serialize-dylib-command (make-dylib-command :cmdsize dylib-cmd-size) buffer)
    (serialize-linkedit-data-command
     (make-linkedit-data-command :dataoff code-signature-off
                                 :datasize code-signature-size)
     buffer)

    ;; Pad header area to code-offset
    (let ((pos (length (byte-buffer-data buffer))))
      (loop repeat (- code-offset pos)
            do (buffer-write-byte buffer 0)))

    ;; Write user segment payloads
    (dolist (seg user-segments)
      (let* ((is-text (and (string= (segment-command-segname seg) "__TEXT")
                           (zerop (length (segment-command-payload seg)))))
             (payload (if is-text code-bytes (segment-command-payload seg)))
             ;; TEXT code is at code-offset; other segs use their fileoff
             (target-off (if (string= (segment-command-segname seg) "__TEXT")
                             code-offset
                             (segment-command-fileoff seg))))
        (let ((pos (length (byte-buffer-data buffer))))
          (when (> target-off pos)
            (loop repeat (- target-off pos)
                  do (buffer-write-byte buffer 0))))
        (serialize-bytes payload buffer)
        (let ((aligned-end (align-up (+ target-off (length payload)) #x1000)))
          (loop repeat (- aligned-end (length (byte-buffer-data buffer)))
                do (buffer-write-byte buffer 0)))))

    ;; Pad to __LINKEDIT start
    (let ((pos (length (byte-buffer-data buffer))))
      (when (< pos linkedit-fileoff)
        (loop repeat (- linkedit-fileoff pos)
              do (buffer-write-byte buffer 0))))

    ;; Write relocation entries, symbol table, bind info, and code signature
    ;; into __LINKEDIT.
    (dolist (reloc relocations)
      (serialize-relocation-info reloc buffer))

    (when has-symbols
      (dolist (sym symbols)
        (serialize-nlist sym buffer))
      (serialize-bytes (coerce string-table '(simple-array (unsigned-byte 8) (*))) buffer))
    (when has-bind-info
      (serialize-bytes bind-bytes buffer))
    (serialize-bytes code-signature-bytes buffer)

    (buffer-get-bytes buffer)))

(defun write-mach-o-file (filename mach-o-bytes &key (codesign t))
  "Write MACH-O-BYTES to FILENAME as a binary file."
  (declare (type (or pathname string) filename)
           (type (simple-array (unsigned-byte 8) (*)) mach-o-bytes))
  (with-open-file (out filename
                        :direction :output
                        :element-type '(unsigned-byte 8)
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (write-sequence mach-o-bytes out))
  (when codesign
    (let ((codesign-program (probe-file "/usr/bin/codesign")))
      (when codesign-program
        (ignore-errors
          (sb-ext:run-program (namestring codesign-program)
                              (list "-s" "-" "-f" (namestring (pathname filename)))
                              :search nil
                              :output nil
                              :error nil)))))
  filename)

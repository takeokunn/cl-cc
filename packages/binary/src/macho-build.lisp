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
    builder))

(defun add-text-segment (builder code-bytes &key (base-addr #x100000000))
  "Add __TEXT segment with code to BUILDER.
CODE-BYTES should be a simple-array of (unsigned-byte 8).
BASE-ADDR is the virtual memory address for the segment."
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

(defun %mach-o-ensure-text-and-unwind (user-segments code-bytes)
  "Return USER-SEGMENTS with __TEXT unwind info and read-only constants support."
  (let* ((code-size (length code-bytes))
         (unwind-bytes (macho-build-unwind-info code-size))
         (unwind-offset (align-up code-size 4))
         (payload-size (+ unwind-offset (length unwind-bytes)))
         (payload (make-array payload-size :element-type '(unsigned-byte 8)
                              :initial-element 0))
         (text-seg (find "__TEXT" user-segments
                         :key #'segment-command-segname :test #'string=)))
    (replace payload code-bytes :start1 0)
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
          (unwind-section (or (find "__unwind_info" (segment-command-sections text-seg)
                                    :key #'section-sectname :test #'string=)
                              (make-section :sectname "__unwind_info"
                                            :segname "__TEXT"
                                            :size (length unwind-bytes)
                                            :align 2))))
      (setf (section-size text-section) code-size
            (section-size unwind-section) (length unwind-bytes)
            (segment-command-payload text-seg) payload
            (segment-command-nsects text-seg) 2
            (segment-command-cmdsize text-seg) (+ 72 (* 80 2))
            (segment-command-maxprot text-seg) 5
            (segment-command-initprot text-seg) 5
            (segment-command-sections text-seg) (list text-section unwind-section)))
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
    user-segments))

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
    (push (make-nlist :n-strx strx
                      :n-type type
                      :n-sect sect
                      :n-value value)
          (mach-o-builder-symbol-table builder)))
  builder)

(defun add-entry-point (builder offset)
  "Add LC_MAIN entry point with file OFFSET."
  (declare (type mach-o-builder builder)
           (type (unsigned-byte 64) offset))
  (setf (entry-point-command-entryoff (mach-o-builder-entry-point builder))
        offset)
  builder)

(defun build-mach-o (builder code-bytes)
  "Build complete Mach-O executable from BUILDER and CODE-BYTES.
Returns a simple-array of (unsigned-byte 8).

Layout: __PAGEZERO (fileoff=0) + __TEXT (fileoff=0, covers header through code)
+ __LINKEDIT (after code) + LC_LOAD_DYLINKER + LC_MAIN.
__TEXT.fileoff=0 is required by macOS strict validation for code signing."
  (declare (type mach-o-builder builder)
           (type (simple-array (unsigned-byte 8) (*)) code-bytes))
  (let* ((buffer (make-byte-buffer 65536))
         (header-size 32)
          (user-segments (%mach-o-ensure-text-and-unwind
                          (nreverse (mach-o-builder-segments builder))
                          code-bytes))
         (symbols (nreverse (mach-o-builder-symbol-table builder)))
         (string-table (subseq (mach-o-builder-string-table builder)
                               0
                               (fill-pointer (mach-o-builder-string-table builder))))
         (has-symbols (plusp (length symbols)))
         ;; Command sizes: PAGEZERO(72) + user segs + LINKEDIT(72) + DYLINKER(32) + MAIN(24)
         (pagezero-cmd-size 72)
         (user-seg-cmd-sizes (loop for seg in user-segments
                                   sum (+ 72 (* 80 (segment-command-nsects seg)))))
         (linkedit-cmd-size 72)
         (dylinker-cmd-size 32)
         (main-cmd-size 24)
         (symtab-cmd-size (if has-symbols 24 0))
         (dysymtab-cmd-size (if has-symbols 80 0))
         (cmds-size (+ pagezero-cmd-size user-seg-cmd-sizes linkedit-cmd-size
                       dylinker-cmd-size main-cmd-size
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
         (symoff (if has-symbols linkedit-fileoff 0))
         (stroff (if has-symbols (+ symoff (* nsyms 18)) 0))
         (strsize (length string-table))
         (linkedit-filesize (if has-symbols (+ (* nsyms 18) strsize) 0))
         (linkedit-seg (%make-linkedit-segment linkedit-fileoff linkedit-filesize
                                               (+ #x100000000 linkedit-fileoff))))

    ;; Update Mach-O header
    (let ((header (mach-o-builder-header builder)))
      (setf (mach-header-ncmds header)
            ;; PAGEZERO + user segs + LINKEDIT + DYLINKER + MAIN [+ SYMTAB + DYSYMTAB]
            (+ 4 (length user-segments) (if has-symbols 2 0))
            (mach-header-sizeofcmds header) cmds-size
            (mach-header-flags header) (logior +mh-noundefs+ +mh-dyldlink+ +mh-pie+)))

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
                (let ((section-delta (if (string= (section-sectname sect) "__unwind_info")
                                         (align-up (length code-bytes) 4)
                                         0)))
                  (setf (section-offset sect) (+ code-offset section-delta)
                        (section-addr sect) (+ (segment-command-vmaddr seg)
                                               code-offset
                                               section-delta))))
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
       (make-dysymtab-command :iextdefsym 0 :nextdefsym 0 :iundefsym 0 :nundefsym nsyms)
       buffer))

    ;; Serialize LC_MAIN
    (serialize-entry-point (mach-o-builder-entry-point builder) buffer)

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

    ;; Write symbol table and string table into __LINKEDIT
    (when has-symbols
      (dolist (sym symbols)
        (serialize-nlist sym buffer))
      (serialize-bytes (coerce string-table '(simple-array (unsigned-byte 8) (*))) buffer))

    (buffer-get-bytes buffer)))

(defun write-mach-o-file (filename mach-o-bytes)
  "Write MACH-O-BYTES to FILENAME as a binary file."
  (declare (type (or pathname string) filename)
           (type (simple-array (unsigned-byte 8) (*)) mach-o-bytes))
  (with-open-file (out filename
                        :direction :output
                        :element-type '(unsigned-byte 8)
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (write-sequence mach-o-bytes out)
    filename))

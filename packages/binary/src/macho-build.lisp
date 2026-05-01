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
Returns a simple-array of (unsigned-byte 8)."
  (declare (type mach-o-builder builder)
           (type (simple-array (unsigned-byte 8) (*)) code-bytes))
  ;; Create output buffer
  (let* ((buffer (make-byte-buffer 65536))
          ;; Calculate sizes
          (header-size 32)
          (segments (cons (%make-pagezero-segment)
                          (nreverse (mach-o-builder-segments builder))))
          (symbols (nreverse (mach-o-builder-symbol-table builder)))
          (string-table (subseq (mach-o-builder-string-table builder)
                                0
                                (fill-pointer (mach-o-builder-string-table builder))))
          (has-symbols (plusp (length symbols)))
          ;; Segment command size: 72 bytes header + 80 bytes per section
          (seg-cmd-sizes (loop for seg in segments
                               sum (+ 72 (* 80 (segment-command-nsects seg)))))
          (main-cmd-size 24)
          (symtab-cmd-size (if has-symbols 24 0))
          (dysymtab-cmd-size (if has-symbols 80 0))
          (cmds-size (+ seg-cmd-sizes main-cmd-size symtab-cmd-size dysymtab-cmd-size))
          ;; Align code to page boundary
          (code-offset (align-up (+ header-size cmds-size) 4096))
          (payload-end (loop with off = code-offset
                             for seg in segments
                             do (incf off (align-up (length (if (and (string= (segment-command-segname seg) "__TEXT")
                                                                     (zerop (length (segment-command-payload seg))))
                                                                code-bytes
                                                                (segment-command-payload seg)))
                                                      #x1000))
                             finally (return off)))
          (symoff (if has-symbols payload-end 0))
          (nsyms (length symbols))
          (stroff (if has-symbols (+ symoff (* nsyms 18)) 0))
          (strsize (length string-table)))

    ;; Update header
      (let ((header (mach-o-builder-header builder)))
       (setf (mach-header-ncmds header) (+ 1 (length segments) (if has-symbols 2 0))
             (mach-header-sizeofcmds header) cmds-size
             (mach-header-flags header) (logior +mh-noundefs+ +mh-pie+)))

    ;; Update segment/section file offsets
    (let ((file-offset code-offset))
      (dolist (seg segments)
        (let ((payload (if (and (string= (segment-command-segname seg) "__TEXT")
                                (zerop (length (segment-command-payload seg))))
                           code-bytes
                           (segment-command-payload seg))))
        (setf (segment-command-fileoff seg) file-offset
              (segment-command-filesize seg)
              (if (string= (segment-command-segname seg) "__PAGEZERO")
                  0
                  (length payload)))
        (dolist (sect (segment-command-sections seg))
          (setf (section-offset sect) file-offset))
        (incf file-offset (align-up (segment-command-filesize seg) #x1000)))))

    ;; Update entry point
    (setf (entry-point-command-entryoff (mach-o-builder-entry-point builder))
          code-offset)

    ;; Write header
    (serialize-mach-header (mach-o-builder-header builder) buffer)

    ;; Write segment commands with sections
    (dolist (seg segments)
      (serialize-segment-command seg buffer)
      (dolist (sect (segment-command-sections seg))
        (serialize-section sect buffer)))

    ;; Write LC_SYMTAB / LC_DYSYMTAB load commands when symbols exist.
    (when has-symbols
      (serialize-symtab-command
       (make-symtab-command :symoff symoff :nsyms nsyms :stroff stroff :strsize strsize)
       buffer)
      (serialize-dysymtab-command
       (make-dysymtab-command :iextdefsym 0 :nextdefsym 0 :iundefsym 0 :nundefsym nsyms)
       buffer))

    ;; Write LC_MAIN
    (serialize-entry-point (mach-o-builder-entry-point builder) buffer)

    ;; Pad to code offset
    (let ((current-pos (length (byte-buffer-data buffer))))
      (loop repeat (- code-offset current-pos)
            do (buffer-write-byte buffer 0)))

    ;; Write segment payloads
    (dolist (seg segments)
      (unless (string= (segment-command-segname seg) "__PAGEZERO")
        (let* ((payload (if (and (string= (segment-command-segname seg) "__TEXT")
                                 (zerop (length (segment-command-payload seg))))
                            code-bytes
                            (segment-command-payload seg)))
               (target-off (segment-command-fileoff seg))
               (current-pos (length (byte-buffer-data buffer))))
          (when (> target-off current-pos)
            (loop repeat (- target-off current-pos)
                  do (buffer-write-byte buffer 0)))
          (serialize-bytes payload buffer)
          (let ((aligned-end (align-up (+ target-off (length payload)) #x1000)))
            (loop repeat (- aligned-end (length (byte-buffer-data buffer)))
                  do (buffer-write-byte buffer 0))))))

    ;; Write symbol table and string table payloads after segment data.
    (when has-symbols
      (dolist (sym symbols)
        (serialize-nlist sym buffer))
      (serialize-bytes (coerce string-table '(simple-array (unsigned-byte 8) (*))) buffer))

    ;; Return the result as a simple-array
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

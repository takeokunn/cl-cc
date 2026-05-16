;;;; tests/unit/emit/macho-builder-tests.lisp — Mach-O Builder API and Extended Tests
;;;;
;;;; Tests for src/emit/binary/macho.lisp and src/emit/binary/macho-serialize.lisp:
;;;; Builder API, additional structure defaults, extended serialization, and binary output.

(in-package :cl-cc/test)

(in-suite macho-suite)

;;; ─── Builder API ────────────────────────────────────────────────────────

(deftest-each macho-builder-creation
  "make-mach-o-builder creates a non-nil builder for all supported targets."
  :cases (("x86-64" :x86-64)
          ("arm64"  :arm64))
  (target)
  (assert-true (cl-cc/binary:make-mach-o-builder target)))

(deftest macho-add-entry-point-succeeds
  "add-entry-point completes without error."
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64)))
    (cl-cc/binary:add-entry-point b 0)
    (assert-true b)))

(deftest macho-add-text-segment-appends-to-segments
  "add-text-segment adds exactly one segment to the builder."
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 0 0 0))))
    (cl-cc/binary:add-text-segment b code)
    (assert-equal 1 (length (cl-cc/binary::mach-o-builder-segments b)))))


(deftest macho-data-segment-is-not-executable
  "__DATA segments are emitted with rw- protections, not rwx."
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (data (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))))
    (cl-cc/binary:add-data-segment b data)
    (let ((seg (find "__DATA" (cl-cc/binary::mach-o-builder-segments b)
                     :key #'cl-cc/binary:segment-command-segname :test #'string=)))
      (assert-false (null seg))
      (assert-equal 6 (cl-cc/binary:segment-command-maxprot seg))
      (assert-equal 6 (cl-cc/binary:segment-command-initprot seg)))))

(deftest macho-build-binary-is-nonempty-ub8-vector-at-least-4096-bytes
  "build-mach-o produces a non-empty ub8 vector at least 4096 bytes (page-aligned)."
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64))
        (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 0 0 0))))
    (cl-cc/binary:add-entry-point b 0)
    (let ((result (cl-cc/binary:build-mach-o b code)))
      (assert-true (> (length result) 0))
      (assert-true (typep result '(simple-array (unsigned-byte 8) (*))))
      (assert-true (>= (length result) 4096)))))

(deftest macho-build-binary-starts-with-feedfacf-magic
  "build-mach-o starts with the 64-bit Mach-O magic bytes #xCF #xFA #xED #xFE."
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64))
        (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary:add-entry-point b 0)
    (let ((result (cl-cc/binary:build-mach-o b code)))
      (assert-equal #xCF (aref result 0))
      (assert-equal #xFA (aref result 1))
      (assert-equal #xED (aref result 2))
      (assert-equal #xFE (aref result 3)))))


(deftest macho-build-serializes-data-segment-payload
  "build-mach-o includes __DATA payload bytes in the final binary output."
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3)))
         (data (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))))
    (cl-cc/binary:add-text-segment b code)
    (cl-cc/binary:add-data-segment b data)
    (cl-cc/binary:add-entry-point b 0)
    (let* ((result (cl-cc/binary:build-mach-o b code))
           (data-seg (find "__DATA" (cl-cc/binary::mach-o-builder-segments b)
                           :key #'cl-cc/binary:segment-command-segname :test #'string=))
           (off (cl-cc/binary:segment-command-fileoff data-seg)))
      (assert-equal 4 (cl-cc/binary:segment-command-filesize data-seg))
      (assert-equal '(1 2 3 4) (coerce (subseq result off (+ off 4)) 'list)))))

(deftest macho-build-serializes-symbol-table
  "build-mach-o emits LC_SYMTAB and serialized nlist/string table bytes when symbols exist."
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary:add-text-segment b code)
    (cl-cc/binary:add-symbol b "_main" :value 0 :sect 1)
    (cl-cc/binary:add-entry-point b 0)
    (let* ((result (cl-cc/binary:build-mach-o b code))
           (symtab-cmd-off (+ 32 72 (+ 72 80) 72 32))
           (symoff (+ (aref result (+ symtab-cmd-off 8))
                      (ash (aref result (+ symtab-cmd-off 9)) 8)
                      (ash (aref result (+ symtab-cmd-off 10)) 16)
                      (ash (aref result (+ symtab-cmd-off 11)) 24)))
           (nsyms (+ (aref result (+ symtab-cmd-off 12))
                     (ash (aref result (+ symtab-cmd-off 13)) 8)
                     (ash (aref result (+ symtab-cmd-off 14)) 16)
                     (ash (aref result (+ symtab-cmd-off 15)) 24)))
           (stroff (+ (aref result (+ symtab-cmd-off 16))
                      (ash (aref result (+ symtab-cmd-off 17)) 8)
                      (ash (aref result (+ symtab-cmd-off 18)) 16)
                      (ash (aref result (+ symtab-cmd-off 19)) 24))))
      (assert-equal 7 (aref result 16))
      (assert-equal #x02 (aref result symtab-cmd-off))
      (assert-equal 1 nsyms)
      (assert-equal 18 (- stroff symoff))
      (assert-equal (char-code #\_) (aref result (+ stroff 1)))
      (assert-equal (char-code #\m) (aref result (+ stroff 2))))))

(deftest macho-build-x86-64-includes-pagezero-segment
  "x86-64 build includes __PAGEZERO segment name at the expected offset."
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64))
        (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary:add-text-segment b code)
    (cl-cc/binary:add-entry-point b 0)
    (let ((result (cl-cc/binary:build-mach-o b code)))
      (assert-equal 5 (aref result 16))
      (assert-equal 0 (aref result 17))
      (assert-equal 0 (aref result 18))
      (assert-equal 0 (aref result 19))
      (let ((pagezero (map 'string #'code-char (subseq result 40 50))))
        (assert-equal "__PAGEZERO" pagezero)))))

(deftest macho-build-arm64-has-different-cputype-from-x86-64
  "ARM64 build starts with FEEDFACF magic but has a different cputype byte than x86-64."
  (let ((b (cl-cc/binary:make-mach-o-builder :arm64))
        (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xD6 #x5F #x03 #xC0))))
    (cl-cc/binary:add-entry-point b 0)
    (let ((result (cl-cc/binary:build-mach-o b code)))
      (assert-equal #xCF (aref result 0))
      (let ((b2 (cl-cc/binary:make-mach-o-builder :x86-64)))
        (cl-cc/binary:add-entry-point b2 0)
        (let ((x64 (cl-cc/binary:build-mach-o b2 code)))
          (assert-false (= (aref result 4) (aref x64 4))))))))

;;; ─── Additional Structure Tests ─────────────────────────────────────────

(deftest macho-structure-defaults
  "Default nlist (zeroed), symtab-command (LC_SYMTAB, 24-byte cmdsize), and section (empty names) defaults."
  (let ((nl (cl-cc/binary::make-nlist)))
    (assert-equal 0 (cl-cc/binary::nlist-n-strx nl))
    (assert-equal 0 (cl-cc/binary::nlist-n-type nl))
    (assert-equal 0 (cl-cc/binary::nlist-n-sect nl))
    (assert-equal 0 (cl-cc/binary::nlist-n-desc nl))
    (assert-equal 0 (cl-cc/binary::nlist-n-value nl)))
  (let ((sc (cl-cc/binary::make-symtab-command)))
    (assert-equal cl-cc/binary:+lc-symtab+ (cl-cc/binary::symtab-command-cmd sc))
    (assert-equal 24 (cl-cc/binary::symtab-command-cmdsize sc))
    (assert-equal 0 (cl-cc/binary::symtab-command-nsyms sc)))
  (let ((sect (cl-cc/binary::make-section)))
    (assert-equal "" (cl-cc/binary:section-sectname sect))
    (assert-equal "" (cl-cc/binary:section-segname sect))
    (assert-equal 0 (cl-cc/binary:section-size sect))
    (assert-equal 0 (cl-cc/binary:section-addr sect))))

(deftest macho-lc-main-constant-has-req-dyld-bit
  "LC_MAIN constant includes the REQ_DYLD bit (0x80000028)."
  (assert-equal #x80000028 cl-cc/binary:+lc-main+))

(deftest macho-header-flag-and-cpu-subtype-constants
  "Mach-O header flags and CPU subtype constants have the correct values."
  (assert-equal 1 cl-cc/binary:+mh-noundefs+)
  (assert-equal 4 cl-cc/binary:+mh-dyldlink+)
  (assert-equal #x200000 cl-cc/binary:+mh-pie+)
  (assert-equal #x00000003 cl-cc/binary:+cpu-subtype-x86-64-all+)
  (assert-equal #x00000000 cl-cc/binary:+cpu-subtype-arm64-all+))

;;; ─── Additional Serialization Tests ─────────────────────────────────────

(deftest macho-serialize-nlist-produces-18-bytes
  "Serialized nlist is exactly 18 bytes."
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (nl (cl-cc/binary::make-nlist :n-strx 1 :n-type #x0f :n-sect 1 :n-desc 0 :n-value 0)))
    (cl-cc/binary::serialize-nlist nl buf)
    (assert-equal 18 (length (cl-cc/binary::byte-buffer-data buf)))))

(deftest macho-serialize-nlist-strx-in-little-endian
  "n-strx value 5 is stored in bytes[0..1] as little-endian (5, 0)."
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (nl (cl-cc/binary::make-nlist :n-strx #x00000005)))
    (cl-cc/binary::serialize-nlist nl buf)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (assert-equal 5 (aref data 0))
      (assert-equal 0 (aref data 1)))))

(deftest-each macho-command-serialization-sizes
  "Serialized command sizes: entry-point=24, symtab=24, section=80 bytes."
  :cases (("entry-point" 24 (cl-cc/binary::make-entry-point-command)
                            #'cl-cc/binary::serialize-entry-point)
          ("symtab"      24 (cl-cc/binary::make-symtab-command)
                            #'cl-cc/binary::serialize-symtab-command)
          ("section"     80 (cl-cc/binary::make-section :sectname "__text" :segname "__TEXT")
                            #'cl-cc/binary::serialize-section))
  (expected obj serializer)
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (funcall serializer obj buf)
    (assert-equal expected (length (cl-cc/binary::byte-buffer-data buf)))))

(deftest macho-buffer-write-bytes-appends-correctly
  "buffer-write-bytes appends all 3 bytes to the byte-buffer in order."
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (bytes (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(10 20 30))))
    (cl-cc/binary::buffer-write-bytes buf bytes)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (assert-equal 3 (length data))
      (assert-equal 10 (aref data 0))
      (assert-equal 20 (aref data 1))
      (assert-equal 30 (aref data 2)))))

(deftest macho-binary-buffer-writes-little-endian-u16-and-u8
  "binary-buffer-write-u16le and write-u8 produce correct little-endian byte sequence."
  (let ((buf (cl-cc/binary::make-binary-buffer 0)))
    (cl-cc/binary::binary-buffer-write-u16le buf #x1234)
    (cl-cc/binary::binary-buffer-write-u8 buf #x56)
    (assert-equal '(#x34 #x12 #x56)
                  (coerce (cl-cc/binary::binary-buffer-to-array buf) 'list))))

;;; ─── Additional Builder API Tests ───────────────────────────────────────

(deftest-each macho-builder-cputypes
  "make-mach-o-builder sets the correct CPU type constant in the header."
  :cases (("x86-64" :x86-64 cl-cc/binary:+cpu-type-x86-64+)
          ("arm64"  :arm64  cl-cc/binary:+cpu-type-arm64+))
  (target expected-cputype)
  (let* ((b (cl-cc/binary:make-mach-o-builder target))
         (hdr (cl-cc/binary::mach-o-builder-header b)))
    (assert-equal expected-cputype (cl-cc/binary:mach-header-cputype hdr))))

(deftest-each macho-add-symbol-behavior
  "add-symbol inserts into symbol-table; multiple calls accumulate."
  :cases (("one-symbol"  1 '("_main"))
          ("two-symbols" 2 '("_start" "_exit")))
  (expected names)
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64)))
    (dolist (name names)
      (cl-cc/binary:add-symbol b name :value 0 :sect 1))
    (assert-equal expected (length (cl-cc/binary::mach-o-builder-symbol-table b)))))

;;; ─── Bug-fix regression: LC_LOAD_DYLINKER, MH_DYLDLINK, entryoff ───────

(deftest macho-build-has-lc-load-dylinker
  "build-mach-o emits LC_LOAD_DYLINKER (cmd byte #x0E) after the last segment command."
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary:add-text-segment b code)
    (cl-cc/binary:add-entry-point b 0)
    (let* ((result (cl-cc/binary:build-mach-o b code))
           (idx (position #x0E result)))
      (assert-true idx)
      (assert-equal #x00 (aref result (+ idx 1)))
      (assert-equal #x00 (aref result (+ idx 2)))
      (assert-equal #x00 (aref result (+ idx 3))))))

(deftest macho-build-header-has-dyldlink-flag
  "build-mach-o sets MH_DYLDLINK (#x4) in the header flags."
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary:add-entry-point b 0)
    (let* ((result (cl-cc/binary:build-mach-o b code))
           (flags (+ (aref result 24)
                     (ash (aref result 25) 8)
                     (ash (aref result 26) 16)
                     (ash (aref result 27) 24))))
      (assert-true (logbitp 2 flags)))))

(deftest macho-entryoff-is-code-offset
  "build-mach-o sets LC_MAIN entryoff to code-offset (4096) — the offset of code within __TEXT.
__TEXT.fileoff=0 so entryoff equals the absolute file offset of the first instruction."
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 0 0 0))))
    (cl-cc/binary:add-text-segment b code)
    (cl-cc/binary:add-entry-point b 0)
    (let* ((result (cl-cc/binary:build-mach-o b code))
           ;; LC_MAIN starts after: header(32)+pagezero(72)+text+section(152)+linkedit(72)+dylinker(32)
           (main-off (+ 32 72 152 72 32))
           (entryoff (+ (aref result (+ main-off 8))
                        (ash (aref result (+ main-off 9)) 8)
                        (ash (aref result (+ main-off 10)) 16)
                        (ash (aref result (+ main-off 11)) 24))))
      (assert-equal 4096 entryoff))))

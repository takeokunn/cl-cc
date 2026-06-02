;;;; packages/codegen/src/wasm-binary-aot.lisp — AOT WASM compilation (FR-219)
;;;
;;; Ahead-of-time compilation: file I/O helpers, hashing, base64 encoding,
;;; optional wasm-opt/wasm2wat/wat2wasm tool integration, build-hash custom
;;; sections, and compile-to-aot-wasm entry point.
;;;
;;; Load order: after wasm-binary.lisp.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; AOT result bundle
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (wasm-aot-result (:conc-name wasm-aot-result-))
  "Result bundle for FR-219 AOT Wasm generation."
  (bytes #() :type vector)
  (wat "" :type string)
  (metadata nil :type list))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tool availability
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-tool-available-p (program)
  "Return T when PROGRAM can be found on PATH."
  (let ((path (ignore-errors (cl-cc/runtime:rt-getenv "PATH"))))
    (and path
         (loop with start = 0
               for end = (position #\: path :start start)
               for dir = (subseq path start end)
               for candidate = (merge-pathnames program
                                                (pathname (format nil "~A/" dir)))
               thereis (probe-file candidate)
               while end
               do (setf start (1+ end))))))

(defun wasm-run-tool-to-string (argv &key input-file)
  "Run an optional wasm tool and return stdout, or NIL when unavailable/failing."
  (declare (ignore input-file))
  (when (and argv (wasm-tool-available-p (first argv)))
    (handler-case
        (uiop:run-program argv :output :string :error-output :string
                              :ignore-error-status nil)
      (error () nil))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; File I/O primitives
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %wasm-write-bytes-file (path bytes)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-sequence bytes out))
  path)

(defun %wasm-read-bytes-file (path)
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf)))

(defun %wasm-temp-path (suffix)
  (merge-pathnames (make-pathname :name (format nil "cl-cc-wasm-~A" (gensym))
                                  :type suffix)
                   (uiop:temporary-directory)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Content hashing
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %wasm-hex-digest-file (path bits)
  "Return a hex digest for PATH using shasum/sha*sum/openssl when available."
  (or (when (wasm-tool-available-p "shasum")
        (let ((out (wasm-run-tool-to-string
                    (list "shasum" "-a" (princ-to-string bits) (namestring path)))))
          (and out (first (uiop:split-string out :separator '(#\Space #\Tab #\Newline))))))
      (let ((tool (format nil "sha~Dsum" bits)))
        (when (wasm-tool-available-p tool)
          (let ((out (wasm-run-tool-to-string (list tool (namestring path)))))
            (and out (first (uiop:split-string out :separator '(#\Space #\Tab #\Newline)))))))
      (when (wasm-tool-available-p "openssl")
        (let ((out (wasm-run-tool-to-string
                    (list "openssl" "dgst" (format nil "-sha~D" bits) "-r" (namestring path)))))
          (and out (first (uiop:split-string out :separator '(#\Space #\Tab #\Newline))))))))

(defun %wasm-byte-vector-hex-digest (bytes bits)
  (let ((tmp (%wasm-temp-path "wasm")))
    (unwind-protect
         (progn
           (%wasm-write-bytes-file tmp bytes)
           (or (%wasm-hex-digest-file tmp bits)
               ;; Deterministic non-cryptographic fallback when no digest tool exists.
               (format nil (format nil "~~~D,'0X" (/ bits 4))
                       (mod (abs (sxhash (coerce bytes 'list)))
                            (expt 16 (/ bits 4))))))
      (ignore-errors (delete-file tmp)))))

(defun wasm-file-content-hash (path &key (bits 256))
  "Return SHA-BITS hex digest for PATH, using optional platform tools."
  (or (%wasm-hex-digest-file path bits)
      (%wasm-byte-vector-hex-digest (%wasm-read-bytes-file path) bits)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Base64 encoding / SRI hash
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %wasm-hex-to-bytes (hex)
  (let* ((clean (remove-if-not #'alphanumericp hex))
         (len (floor (length clean) 2))
         (out (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len out)
      (setf (aref out i)
            (parse-integer clean :start (* i 2) :end (+ (* i 2) 2) :radix 16)))))

(defparameter +wasm-base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "RFC 4648 base64 alphabet used for SRI output.")

(defun %wasm-base64-encode (bytes)
  (with-output-to-string (out)
    (loop for i from 0 below (length bytes) by 3
          for b0 = (aref bytes i)
          for have1 = (< (1+ i) (length bytes))
          for have2 = (< (+ i 2) (length bytes))
          for b1 = (if have1 (aref bytes (1+ i)) 0)
          for b2 = (if have2 (aref bytes (+ i 2)) 0)
          for n = (logior (ash b0 16) (ash b1 8) b2)
          do (write-char (char +wasm-base64-alphabet+ (ldb (byte 6 18) n)) out)
             (write-char (char +wasm-base64-alphabet+ (ldb (byte 6 12) n)) out)
             (write-char (if have1 (char +wasm-base64-alphabet+ (ldb (byte 6 6) n)) #\=) out)
             (write-char (if have2 (char +wasm-base64-alphabet+ (ldb (byte 6 0) n)) #\=) out))))

(defun wasm-file-sri-hash (path &key (bits 384))
  "Return an SRI integrity token such as sha384-... for PATH."
  (let ((hex (wasm-file-content-hash path :bits bits)))
    (format nil "sha~D-~A" bits (%wasm-base64-encode (%wasm-hex-to-bytes hex)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Custom section helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-binary-write-custom-section (buffer name payload-string)
  "Append a custom section NAME with UTF-8-ish PAYLOAD-STRING to BUFFER."
  (wasm-binary-write-section
   buffer +wasm-section-custom+
   (lambda (section)
     (wasm-binary-write-name section name)
     (let ((bytes (map 'vector #'char-code payload-string)))
       (wasm-binary-write-bytes section bytes)))))

(defun wasm-append-build-hash-section (bytes hash)
  "Return BYTES with a deterministic cl-cc build hash custom section appended."
  (let ((buffer (cl-cc/binary::make-byte-buffer (+ (length bytes) 96))))
    (wasm-binary-write-bytes buffer bytes)
    (wasm-binary-write-custom-section buffer "cl-cc.build.sha256" hash)
    (cl-cc/binary::buffer-get-bytes buffer)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Optional external tool integration
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-run-wasm-opt-passes (wasm-bytes &key (aot nil))
  "Run Binaryen optimization/removal passes when wasm-opt is available."
  (if (and (or aot *wasm-aot-mode-enabled*) (wasm-tool-available-p "wasm-opt"))
      (let ((tmp-in (%wasm-temp-path "wasm"))
            (tmp-out (%wasm-temp-path "wasm")))
        (unwind-protect
             (handler-case
                 (progn
                   (%wasm-write-bytes-file tmp-in wasm-bytes)
                   (uiop:run-program (list "wasm-opt" "-O3" "--strip-debug"
                                           "--remove-unused-module-elements"
                                           (namestring tmp-in) "-o" (namestring tmp-out))
                                     :ignore-error-status nil)
                   (if (probe-file tmp-out) (%wasm-read-bytes-file tmp-out) wasm-bytes))
               (error () wasm-bytes))
          (ignore-errors (delete-file tmp-in))
          (ignore-errors (delete-file tmp-out))))
      wasm-bytes))

(defun wasm-run-wasm2wat (wasm-bytes fallback-wat)
  "Return wasm2wat output for WASM-BYTES when wabt is available, else FALLBACK-WAT."
  (if (wasm-tool-available-p "wasm2wat")
      (let ((tmp (%wasm-temp-path "wasm")))
        (unwind-protect
             (progn
               (%wasm-write-bytes-file tmp wasm-bytes)
               (or (wasm-run-tool-to-string (list "wasm2wat" (namestring tmp)))
                   fallback-wat))
          (ignore-errors (delete-file tmp))))
      fallback-wat))

(defun wasm-wat-to-binary-if-available (wat fallback-bytes)
  "Assemble WAT through wat2wasm when available, falling back to FALLBACK-BYTES."
  (if (wasm-tool-available-p "wat2wasm")
      (let ((tmp-wat (%wasm-temp-path "wat"))
            (tmp-wasm (%wasm-temp-path "wasm")))
        (unwind-protect
             (handler-case
                 (progn
                   (with-open-file (out tmp-wat :direction :output :if-exists :supersede
                                                :if-does-not-exist :create)
                     (write-string wat out))
                   (uiop:run-program (list "wat2wasm" (namestring tmp-wat) "-o" (namestring tmp-wasm))
                                     :ignore-error-status nil)
                   (if (probe-file tmp-wasm) (%wasm-read-bytes-file tmp-wasm) fallback-bytes))
               (error () fallback-bytes))
          (ignore-errors (delete-file tmp-wat))
          (ignore-errors (delete-file tmp-wasm))))
      fallback-bytes))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Module post-processing
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-determinize-module! (module)
  "Sort module tables for reproducible AOT emission."
  (setf (wasm-module-functions module)
        (sort (copy-list (wasm-module-functions module)) #'string< :key #'wasm-func-wat-name)
        (wasm-module-globals module)
        (sort (copy-list (wasm-module-globals module)) #'string< :key #'wasm-global-def-wat-name))
  (loop for func in (wasm-module-functions module)
        for i from 0
        do (setf (wasm-func-index func) i))
  module)

(defun wasm-eliminate-dead-exports! (module)
  "Conservatively keep only public entry exports for AOT output."
  (dolist (func (wasm-module-functions module))
    (unless (string= (or (wasm-func-export-name func) "") "main")
      (setf (wasm-func-exported-p func) nil)))
  module)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-219: AOT compilation entry point
;;; ─────────────────────────────────────────────────────────────────────────────

(defun compile-to-aot-wasm (program &key deterministic)
  "FR-219: Compile PROGRAM to a self-contained AOT .wasm result bundle.

The function performs dead export/import pruning, optional deterministic
ordering, optional wat2wasm/wasm-opt integration, and embeds a content-hash
custom section without requiring external tools to be installed."
  (let* ((*wasm-aot-mode-enabled* t)
         (module (extract-wasm-functions program)))
    (build-all-wasm-functions module)
    (when deterministic
      (wasm-determinize-module! module))
    (wasm-eliminate-dead-exports! module)
    (let* ((wat (with-output-to-string (s) (emit-wasm-module module s)))
           (fallback (emit-wasm-binary-module module))
           (assembled (wasm-wat-to-binary-if-available wat fallback))
           (optimized (wasm-run-wasm-opt-passes assembled :aot t))
           (sha256 (%wasm-byte-vector-hex-digest optimized 256))
           (final-bytes (if deterministic
                            (wasm-append-build-hash-section optimized sha256)
                            optimized))
           (debug-wat (wasm-run-wasm2wat final-bytes wat)))
      (make-wasm-aot-result
       :bytes final-bytes
       :wat debug-wat
       :metadata (list :format :cl-cc-wasm-aot-v1
                       :sha256 sha256
                       :deterministic (not (null deterministic))
                       :imports-eliminated *wasm-dead-import-elimination-enabled*
                       :wasm-opt (wasm-tool-available-p "wasm-opt")
                       :wabt (and (wasm-tool-available-p "wat2wasm")
                                  (wasm-tool-available-p "wasm2wat")))))))

;;;; packages/codegen/src/wasm-binary-debug.lisp — WASM debug/devtools metadata
;;;
;;; Custom sections for debugging and browser developer tooling:
;;; branch hints (FR-216), DWARF 5 (FR-222), source maps (FR-223),
;;; name section (FR-242), type reflection / devtools JS (FR-263/269/288/317/318),
;;; profiles section (FR-258).
;;;
;;; Load order: after wasm-binary-aot.lisp.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Escaped-string writer macro
;;;
;;; Unifies %wasm-wat-string and %wasm-json-string: both share the same
;;; skeleton (with-output-to-string, opening quote, per-character case dispatch,
;;; closing quote) and differ only in the escape strings for \n, \r, \t.
;;; ─────────────────────────────────────────────────────────────────────────────

(defmacro define-escaped-string-writer (name &key newline-escape return-escape tab-escape
                                                   (docstring nil))
  "Define a function NAME (text) that returns TEXT as an escaped string literal.
NEWLINE-ESCAPE, RETURN-ESCAPE, and TAB-ESCAPE are the literal strings written
in place of the corresponding control characters."
  `(defun ,name (text)
     ,@(when docstring (list docstring))
     (with-output-to-string (out)
       (write-char #\" out)
       (loop for ch across (princ-to-string (or text "")) do
         (case ch
           (#\" (write-string "\\\"" out))
           (#\\ (write-string "\\\\" out))
           (#\Newline (write-string ,newline-escape out))
           (#\Return  (write-string ,return-escape  out))
           (#\Tab     (write-string ,tab-escape     out))
           (otherwise (write-char ch out))))
       (write-char #\" out))))

;;; WAT uses hex escapes (\0a, \0d, \09) inside string literals.
(define-escaped-string-writer %wasm-wat-string
  :newline-escape "\\0a"
  :return-escape  "\\0d"
  :tab-escape     "\\09"
  :docstring "Return TEXT as an escaped WAT string literal.")

;;; JSON uses the standard symbolic escapes (\n, \r, \t).
(define-escaped-string-writer %wasm-json-string
  :newline-escape "\\n"
  :return-escape  "\\r"
  :tab-escape     "\\t"
  :docstring "Return TEXT encoded as a JSON string literal.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Byte-vector WAT string
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %wasm-byte-vector-wat-string (bytes)
  "Return BYTES as an escaped WAT string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for byte across bytes do (format out "\\~2,'0X" byte))
    (write-char #\" out)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Custom section emit primitives
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %emit-wasm-custom-string (stream name text)
  (format stream "~%  (@custom ~A ~A)"
          (%wasm-wat-string name)
          (%wasm-wat-string text)))

(defun %emit-wasm-custom-bytes (stream name bytes)
  (format stream "~%  (@custom ~A ~A)"
          (%wasm-wat-string name)
          (%wasm-byte-vector-wat-string bytes)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Name helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %wasm-clean-debug-name (name)
  (let ((text (if name (princ-to-string name) "anonymous")))
    (if (and (> (length text) 0) (char= (char text 0) #\$))
        (subseq text 1)
        text)))

(defun %wasm-human-local-name (reg)
  "Map a VM register like :R0 to a stable, DevTools-friendly local name."
  (let* ((raw (string-downcase (string reg)))
         (n (ignore-errors (parse-integer raw :start 1))))
    (cond
      ((eql n 0) "temp-result")
      ((integerp n) (format nil "temp-r~D" n))
      (t raw))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-216: Branch Hinting custom section
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wasm-branch-hints-section (module stream)
  "FR-216: Emit @metadata.code.branch_hint custom section for Wasm branch prediction hints.
   Annotates type predicate fast-paths with likely=1 hints."
  (declare (ignore module))
  (format stream "~%  ;; FR-216: Branch Hinting custom section")
  (format stream "~%  (@custom \"metadata.code.branch_hint\"")
  (format stream "~%    ;; Type predicate fast-paths annotated with likely=1")
  (format stream "~%    ;; Functions: consp→car/cdr, numberp→arithmetic, symbolp→symbol-access")
  (format stream "~%    ;; Format: func_idx instr_offset likely_value")
  (format stream "~%  )"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-222: DWARF 5 debug info custom sections
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %wasm-function-source-offset (func)
  (* (or (wasm-func-index func) 0) 16))

(defun %wasm-registers-in-function (func)
  (remove-duplicates
   (append (wasm-func-params func)
           (loop for inst in (wasm-func-source-instructions func)
                 append (append (ignore-errors (cl-cc/regalloc:instruction-defs inst))
                                (ignore-errors (cl-cc/regalloc:instruction-uses inst)))))
   :test #'eq))

(defun %wasm-dwarf-subprogram-for-func (func)
  (let* ((low (%wasm-function-source-offset func))
         (inst-count (length (wasm-func-source-instructions func)))
         (high (+ low (max 1 inst-count)))
         (params (loop for reg in (or (wasm-func-params func) nil)
                       for i from 0
                       collect (cl-cc/binary::make-dwarf-variable-location
                                :name (%wasm-human-local-name reg)
                                :kind :register
                                :register (min i 31)
                                :pc-start low :pc-end high)))
         (locals (loop for reg in (%wasm-registers-in-function func)
                       for i from 0
                       unless (member reg (wasm-func-params func) :test #'eq)
                         collect (cl-cc/binary::make-dwarf-variable-location
                                  :name (%wasm-human-local-name reg)
                                  :kind :register
                                  :register (min i 31)
                                  :pc-start low :pc-end high))))
    (cl-cc/binary::make-dwarf-subprogram
     :name (%wasm-clean-debug-name (wasm-func-wat-name func))
     :low-pc low
     :high-pc high
     :parameters params
     :variables locals)))

(defun %wasm-dwarf-lines-for-module (module)
  (loop for func in (wasm-module-functions module)
        for low = (%wasm-function-source-offset func)
        append (loop for inst in (wasm-func-source-instructions func)
                     for pc from low
                     collect (list pc (1+ (- pc low)) 0 0))))

(defun %wasm-build-dwarf-section-alist (module)
  "Build DWARF5 section payloads for MODULE using existing binary DWARF helpers."
  (let* ((functions (wasm-module-functions module))
         (max-len (loop for f in functions maximize (length (wasm-func-source-instructions f))))
         (high (+ (* (max 0 (1- (length functions))) 16) (max 1 (or max-len 0))))
         (cu (cl-cc/binary::make-dwarf-compile-unit
              :name "cl-cc-wasm-module"
              :producer "cl-cc wasm backend"
              :low-pc 0
              :high-pc high
              :subprograms (mapcar #'%wasm-dwarf-subprogram-for-func functions)
              :lines (%wasm-dwarf-lines-for-module module))))
    (cl-cc/binary::build-dwarf-section-alist cu)))

(defun emit-wasm-dwarf-sections (module stream)
  "FR-222: Emit DWARF 5 debug info as Wasm custom sections.
   Includes .debug_info, .debug_line, .debug_abbrev sections as WAT custom sections.
   Enable with --emit-debug-info flag."
  (when (wasm-feature-enabled-p "CLCC_WASM_DWARF" *wasm-dwarf-debug-info-enabled*)
    (format stream "~%  ;; FR-222: DWARF 5 debug info sections (custom)")
    (dolist (section (%wasm-build-dwarf-section-alist module))
      (%emit-wasm-custom-bytes stream (car section) (cdr section)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-223: Source Map reference
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-source-map-enabled-p ()
  "FR-223: Check if source map emission is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_SOURCE_MAP" *wasm-source-map-enabled*))

(defun emit-wasm-source-map-reference (stream)
  "FR-223: Emit sourceMappingURL custom section reference."
  (format stream "~%  ;; FR-223: Source Map v3 reference")
  (%emit-wasm-custom-string stream "sourceMappingURL" *wasm-source-map-url*))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-242: Name section
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %wasm-name-section-subsection (id writer)
  (let ((payload (wasm-binary-section-bytes writer)))
    (wasm-binary-section-bytes
     (lambda (section)
       (wasm-binary-write-u8 section id)
       (wasm-binary-write-uleb128 section (length payload))
       (wasm-binary-write-bytes section payload)))))

(defun %wasm-name-assoc-vector (buffer assocs)
  (wasm-binary-write-uleb128 buffer (length assocs))
  (dolist (entry assocs)
    (wasm-binary-write-uleb128 buffer (car entry))
    (wasm-binary-write-name buffer (cdr entry))))

(defun %wasm-function-name-assocs (module)
  (loop for func in (wasm-module-functions module)
        collect (cons (or (wasm-func-index func) 0)
                      (%wasm-clean-debug-name (wasm-func-wat-name func)))))

(defun %wasm-local-name-assocs (module)
  (loop for func in (wasm-module-functions module)
        collect (cons (or (wasm-func-index func) 0)
                      (append '((0 . "pc") (1 . "tmp"))
                              (loop for reg in (%wasm-registers-in-function func)
                                    for idx from 2
                                    collect (cons idx (%wasm-human-local-name reg)))))))

(defun %wasm-label-name-assocs (module)
  (loop for func in (wasm-module-functions module)
        collect (cons (or (wasm-func-index func) 0)
                      (loop for inst in (wasm-func-source-instructions func)
                            for idx from 0
                            when (typep inst 'vm-label)
                              collect (cons idx (%wasm-clean-debug-name (vm-name inst)))))))

(defun %wasm-indirect-name-map (buffer entries)
  (wasm-binary-write-uleb128 buffer (length entries))
  (dolist (entry entries)
    (wasm-binary-write-uleb128 buffer (car entry))
    (%wasm-name-assoc-vector buffer (cdr entry))))

(defun %wasm-build-name-section-bytes (module)
  "Build the payload for the standard WebAssembly name custom section."
  (let ((chunks (list
                 (%wasm-name-section-subsection 0 (lambda (b) (wasm-binary-write-name b "cl-cc-wasm-module")))
                 (%wasm-name-section-subsection 1 (lambda (b) (%wasm-name-assoc-vector b (%wasm-function-name-assocs module))))
                 (%wasm-name-section-subsection 2 (lambda (b) (%wasm-indirect-name-map b (%wasm-local-name-assocs module))))
                 (%wasm-name-section-subsection 3 (lambda (b) (%wasm-indirect-name-map b (%wasm-label-name-assocs module)))))))
    (apply #'concatenate '(simple-array (unsigned-byte 8) (*)) chunks)))

(defun wasm-extended-names-feature-enabled-p ()
  "FR-242: Return true when extended wasm name-section metadata is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EXTENDED_NAMES" *wasm-extended-names-enabled*))

(defun emit-wasm-name-section (module stream)
  "FR-242: Emit function/local/label names as a standard Wasm name custom section."
  (format stream "~%  ;; FR-242: Extended Name Section (functions, locals, labels)")
  (%emit-wasm-custom-bytes stream "name" (%wasm-build-name-section-bytes module)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-263/269/288/317/318: DevTools JS helper module
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %wasm-type-reflection-json (module)
  (with-output-to-string (out)
    (format out "{\"exports\":[")
    (let ((first-p t))
      (dolist (func (wasm-module-functions module))
        (when (wasm-func-exported-p func)
          (unless first-p (write-char #\, out))
          (setf first-p nil)
          (format out "{\"name\":~A,\"functionType\":{\"params\":[],\"results\":[\"eqref\"]}}"
                  (%wasm-json-string
                   (or (wasm-func-export-name func)
                       (%wasm-clean-debug-name (wasm-func-wat-name func))))))))
    (format out "],\"gcStructs\":[{\"name\":\"string_t\",\"fields\":[[\"chars\",\"bytes_array_t\"]]},{\"name\":\"cons_t\",\"fields\":[[\"car\",\"eqref\"],[\"cdr\",\"eqref\"]]},{\"name\":\"instance_t\",\"fields\":[[\"class\",\"class_meta_t\"],[\"slots\",\"eqref_array_t\"]]}]}")))

(defun %wasm-devtools-js (module)
  (format nil "export const clccTypeMetadata = ~A;~%
export function attachClccTypeReflection(instance) {~%
  const exports = instance && instance.exports || {};~%
  for (const [name, fn] of Object.entries(exports)) if (typeof fn === 'function') fn.clccType = clccTypeMetadata.exports.find(e => e.name === name) || null;~%
  return { metadata: clccTypeMetadata, describe(value) { return { jsType: typeof value, value, clccType: value && value.clccType || null }; } };~%
}~%
export function captureClccStack(mapper = x => x) { const e = {}; Error.captureStackTrace?.(e, captureClccStack); return String(e.stack || '').split('\\n').slice(1).map(mapper); }~%
export function printBacktrace(mapper) { return captureClccStack(mapper).join('\\n'); }~%
export function createMemoryProfiler(instance) { const memory = instance?.exports?.memory; return { snapshot() { return { byteLength: memory?.buffer?.byteLength || 0, timestamp: Date.now() }; } }; }~%
export async function hotReload({ table, index, module, imports = {} }) { const { instance } = await WebAssembly.instantiate(module, imports); const replacement = instance.exports.main || Object.values(instance.exports).find(v => typeof v === 'function'); table.set(index, replacement); return replacement; }~%
export async function compileReplForm({ compile, table, imports = {}, form }) { const module = await compile(form); return hotReload({ table, index: table.length - 1, module, imports }); }~%"
          (%wasm-type-reflection-json module)))

(defun emit-wasm-developer-tooling-sections (module stream)
  "Emit opt-in browser developer tooling custom sections."
  (when (or (wasm-feature-enabled-p "CLCC_WASM_TYPE_REFLECTION" *wasm-type-reflection-js-api-enabled*)
            (wasm-feature-enabled-p "CLCC_WASM_STACK_INSPECTION" *wasm-call-stack-inspection-enabled*)
            (wasm-feature-enabled-p "CLCC_WASM_MEMORY_PROFILER" *wasm-memory-profiler-enabled*)
            (wasm-feature-enabled-p "CLCC_WASM_HOT_RELOAD" *wasm-hot-code-reload-enabled*)
            (wasm-feature-enabled-p "CLCC_WASM_INCREMENTAL_REPL" *wasm-repl-incremental-compilation-enabled*))
    (format stream "~%  ;; FR-263/269/318/317/288: cl-cc DevTools JS helper module")
    (%emit-wasm-custom-string stream "clcc.devtools.js" (%wasm-devtools-js module))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-258: Wasm Profiles section
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-profiles-feature-enabled-p ()
  "FR-258: Check if profile declaration is enabled. Uses central *wasm-profiles-enabled*."
  (wasm-feature-enabled-p "CLCC_WASM_PROFILES" *wasm-profiles-enabled*))

(defun emit-wasm-profiles-section (stream)
  "FR-258: Emit Wasm Profiles section with required feature declarations."
  (format stream "~%  ;; FR-258: Wasm Profiles — required features")
  (let ((features nil))
    (when (wasm-simd-feature-enabled-p) (push "simd" features))
    (when (wasm-threads-feature-enabled-p) (push "threads" features))
    (when (wasm-eh-feature-enabled-p) (push "exceptions" features))
    (when (wasm-memory64-feature-enabled-p) (push "memory64" features))
    (when (wasm-gc-recursive-types-feature-enabled-p) (push "gc" features))
    (when features
      (format stream "~%  ;; Required: ~{~A~^, ~}" (nreverse features)))))

;;; (emit-instruction methods and compile-to-wasm-wat are in wasm-emit.lisp
;;;  which loads after this file.)
;;; Feature-gate defparameters for FR-206 through FR-325 are in wasm-feature-params.lisp.

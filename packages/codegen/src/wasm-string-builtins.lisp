;;;; packages/codegen/src/wasm-string-builtins.lisp — Wasm String/JS FFI helpers
;;;;
;;;; Feature-gated WAT helpers for the Wasm String Builtins proposal and the
;;;; JavaScript integration features that need small, reusable text forms.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Native string builtins (FR-218, FR-295, FR-320)
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter +wasm-string-builtins-encodings+
  '(:utf8 :wtf8 :lossy-utf8 :wtf16)
  "Encoding keywords understood by the Wasm string builtin emitter helpers.")

(defun wasm-string-builtin-encoding-name (encoding)
  "Return the builtin mnemonic suffix for ENCODING."
  (ecase encoding
    (:utf8 "utf8")
    (:wtf8 "wtf8")
    (:lossy-utf8 "lossy_utf8")
    (:wtf16 "wtf16")))

(defun wasm-stringref-from-eqref-wat (value-wat)
  "Return WAT that treats VALUE-WAT as the backend's string value.

When String Builtins are enabled, string literals and string-producing builtins
store native stringref values directly in eqref VM locals.  For legacy staged
$string_t values, callers should keep using the fallback helpers that operate on
the $chars byte array."
  value-wat)

(defun wasm-string-literal-wat (str)
  "Return WAT for string literal STR.

With FR-218 enabled this emits a native string.new_utf8_array instead of a
linear-memory/string-struct byte representation.  The byte array is still a GC
array immediate, not a linear-memory byte buffer."
  (let* ((bytes (map 'list #'char-code str))
         (byte-elems (format nil "~{~A~^ ~}"
                             (mapcar (lambda (b) (format nil "(i32.const ~D)" b)) bytes)))
         (array-wat (format nil "(array.new_fixed $bytes_array_t ~D~@[ ~A~])"
                            (length bytes)
                            (and bytes byte-elems))))
    (if (wasm-string-builtins-feature-enabled-p)
        (emit-wasm-string-new-wat :utf8 array-wat "(i32.const 0)"
                                  (format nil "(i32.const ~D)" (length bytes)))
        (format nil "(struct.new $string_t ~A)" array-wat))))

(defun emit-wasm-string-new-wat (encoding source-wat start-wat end-wat
                                  &key array-p)
  "Return string.new_* WAT for SOURCE-WAT/START-WAT/END-WAT.

ENCODING is one of :UTF8, :WTF8, :LOSSY-UTF8, or :WTF16.  ARRAY-P is accepted
for call-site clarity; current builtins use the *_array spelling for GC arrays."
  (declare (ignore array-p))
  (format nil "(string.new_~A_array ~A ~A ~A)"
          (wasm-string-builtin-encoding-name encoding)
          source-wat start-wat end-wat))

(defun emit-wasm-string-new-utf8-wat (source-wat start-wat end-wat)
  "Return string.new_utf8_array WAT."
  (emit-wasm-string-new-wat :utf8 source-wat start-wat end-wat :array-p t))

(defun emit-wasm-string-new-wtf8-wat (source-wat start-wat end-wat)
  "Return string.new_wtf8_array WAT."
  (emit-wasm-string-new-wat :wtf8 source-wat start-wat end-wat :array-p t))

(defun emit-wasm-string-new-lossy-utf8-wat (source-wat start-wat end-wat)
  "Return string.new_lossy_utf8_array WAT."
  (emit-wasm-string-new-wat :lossy-utf8 source-wat start-wat end-wat :array-p t))

(defun emit-wasm-string-encode-wat (encoding string-wat array-wat start-wat)
  "Return string.encode_*_array WAT."
  (format nil "(string.encode_~A_array ~A ~A ~A)"
          (wasm-string-builtin-encoding-name encoding)
          (wasm-stringref-from-eqref-wat string-wat)
          array-wat
          start-wat))

(defun emit-wasm-string-decode-wat (encoding array-wat start-wat end-wat)
  "Return string.decode_*_array WAT for reverse transcoding helpers."
  (format nil "(string.decode_~A_array ~A ~A ~A)"
          (wasm-string-builtin-encoding-name encoding)
          array-wat start-wat end-wat))

(defun emit-wasm-string-measure-wat (encoding string-wat)
  "Return string.measure_* WAT."
  (format nil "(string.measure_~A ~A)"
          (wasm-string-builtin-encoding-name encoding)
          (wasm-stringref-from-eqref-wat string-wat)))

(defun emit-wasm-string-concat-wat (lhs-wat rhs-wat)
  "Return string.concat WAT."
  (format nil "(string.concat ~A ~A)"
          (wasm-stringref-from-eqref-wat lhs-wat)
          (wasm-stringref-from-eqref-wat rhs-wat)))

(defun emit-wasm-string-eq-wat (lhs-wat rhs-wat)
  "Return string.eq WAT."
  (format nil "(string.eq ~A ~A)"
          (wasm-stringref-from-eqref-wat lhs-wat)
          (wasm-stringref-from-eqref-wat rhs-wat)))

(defun emit-wasm-string-compare-wat (lhs-wat rhs-wat)
  "Return string.compare WAT."
  (format nil "(string.compare ~A ~A)"
          (wasm-stringref-from-eqref-wat lhs-wat)
          (wasm-stringref-from-eqref-wat rhs-wat)))

(defun emit-wasm-string-from-code-point-wat (code-point-wat)
  "Return string.from_code_point WAT."
  (format nil "(string.from_code_point ~A)" code-point-wat))

(defun emit-wasm-string-as-wtf16-wat (string-wat)
  "Return string.as_wtf16 WAT for a zero-copy UTF-16 view."
  (format nil "(string.as_wtf16 ~A)" (wasm-stringref-from-eqref-wat string-wat)))

(defun emit-wasm-stringview-wtf16-get-codeunit-wat (string-wat index-wat)
  "Return WAT for stringview_wtf16.get_codeunit."
  (format nil "(stringview_wtf16.get_codeunit ~A ~A)"
          (emit-wasm-string-as-wtf16-wat string-wat)
          index-wat))

(defun emit-wasm-string-as-iter-wat (string-wat)
  "Return string.as_iter WAT for code point iteration."
  (format nil "(string.as_iter ~A)" (wasm-stringref-from-eqref-wat string-wat)))

(defun emit-wasm-stringview-iter-next-wat (iter-wat)
  "Return stringview_iter.next WAT."
  (format nil "(stringview_iter.next ~A)" iter-wat))

(defun wasm-bool-to-eqref-wat (bool-wat)
  "Box an i32 boolean expression as CL truth/NIL eqref."
  (format nil "(if (result eqref) ~A (then (ref.i31 (i32.const 1))) (else (ref.null eq)))"
          bool-wat))

(defun wasm-string-compare-predicate-wat (lhs-wat rhs-wat predicate)
  "Return boxed CL boolean for string comparison PREDICATE."
  (let ((cmp (emit-wasm-string-compare-wat lhs-wat rhs-wat)))
    (wasm-bool-to-eqref-wat
     (ecase predicate
       (:=  (format nil "(i32.eqz ~A)" cmp))
       (:<  (format nil "(i32.lt_s ~A (i32.const 0))" cmp))
       (:>  (format nil "(i32.gt_s ~A (i32.const 0))" cmp))
       (:<= (format nil "(i32.le_s ~A (i32.const 0))" cmp))
       (:>= (format nil "(i32.ge_s ~A (i32.const 0))" cmp))
       (:/= (format nil "(i32.ne ~A (i32.const 0))" cmp))))))

(defun wasm-string-char-wat (string-wat index-wat)
  "Return boxed character code for (char STRING INDEX)."
  (wasm-fixnum-box
   (format nil "(i64.extend_i32_u ~A)"
           (emit-wasm-stringview-wtf16-get-codeunit-wat string-wat index-wat))))

(defun emit-wasm-string-utf8-roundtrip-wat (string-wat)
  "Return a helper call that re-encodes STRING-WAT through UTF-8 GC arrays."
  (format nil "(call $clcc_string_roundtrip_utf8 ~A)"
          (wasm-stringref-from-eqref-wat string-wat)))

(defun emit-wat-string-builtin-helpers (stream)
  "Emit reusable FR-218 string builtin helpers."
  (when (wasm-string-builtins-feature-enabled-p)
    (format stream "~%  ;; FR-218: UTF-8 array roundtrip helper for staged string transforms")
    (format stream "~%  (func $clcc_string_roundtrip_utf8 (param $s stringref) (result stringref)")
    (format stream "~%    (local $buf (ref $bytes_array_t))")
    (format stream "~%    (local.set $buf (array.new_default $bytes_array_t (string.measure_utf8 (local.get $s))))")
    (format stream "~%    (drop (string.encode_utf8_array (local.get $s) (local.get $buf) (i32.const 0)))")
    (format stream "~%    (string.new_utf8_array (local.get $buf) (i32.const 0) (array.len (local.get $buf))))" )
    (format stream "~%  )")))

(defun maybe-emit-wasm-string-instruction (inst reg-map stream &key (indent 6))
  "Emit a VM string instruction through Wasm String Builtins when enabled.

Returns T when INST was handled, NIL otherwise.  The fallback path is left to the
caller so existing non-string lowering remains unchanged."
  (unless (wasm-string-builtins-feature-enabled-p)
    (return-from maybe-emit-wasm-string-instruction nil))
  (let ((pad (make-string indent :initial-element #\Space)))
    (labels ((line (fmt &rest args)
               (apply #'format stream (concatenate 'string "~%" pad fmt) args))
             (set-reg (dst wat)
               (line "~A" (reg-local-set reg-map dst wat))))
      (typecase inst
        (vm-concatenate
         (let* ((parts (or (vm-parts inst) (list (vm-str1 inst) (vm-str2 inst))))
                (wat (if parts
                         (reduce #'emit-wasm-string-concat-wat
                                 (mapcar (lambda (reg) (reg-local-ref reg-map reg)) parts))
                         (emit-wasm-string-new-utf8-wat
                          "(array.new_fixed $bytes_array_t 0)" "(i32.const 0)" "(i32.const 0)"))))
           (set-reg (vm-dst inst) wat)
           (reg-record-type reg-map (vm-dst inst) :string)
           t))
        (vm-string-length
         (set-reg (vm-dst inst)
                  (wasm-fixnum-box
                   (format nil "(i64.extend_i32_u ~A)"
                           (emit-wasm-string-measure-wat :wtf16
                                                         (reg-local-ref reg-map (vm-src inst))))))
         t)
        (vm-char
         (set-reg (vm-dst inst)
                  (wasm-string-char-wat (reg-local-ref reg-map (vm-string-reg inst))
                                        (wasm-fixnum-unbox reg-map (vm-index inst)
                                                          :result-type :i32)))
         (reg-record-fixnum-range reg-map (vm-dst inst) (cons 0 #xffff))
         t)
        (vm-string=
         (set-reg (vm-dst inst)
                  (wasm-bool-to-eqref-wat
                   (emit-wasm-string-eq-wat (reg-local-ref reg-map (vm-str1 inst))
                                            (reg-local-ref reg-map (vm-str2 inst)))))
         t)
        (vm-string<
         (set-reg (vm-dst inst)
                  (wasm-string-compare-predicate-wat
                   (reg-local-ref reg-map (vm-str1 inst))
                   (reg-local-ref reg-map (vm-str2 inst)) :<))
         t)
        (vm-string>
         (set-reg (vm-dst inst)
                  (wasm-string-compare-predicate-wat
                   (reg-local-ref reg-map (vm-str1 inst))
                   (reg-local-ref reg-map (vm-str2 inst)) :>))
         t)
        (vm-string<=
         (set-reg (vm-dst inst)
                  (wasm-string-compare-predicate-wat
                   (reg-local-ref reg-map (vm-str1 inst))
                   (reg-local-ref reg-map (vm-str2 inst)) :<=))
         t)
        (vm-string>=
         (set-reg (vm-dst inst)
                  (wasm-string-compare-predicate-wat
                   (reg-local-ref reg-map (vm-str1 inst))
                   (reg-local-ref reg-map (vm-str2 inst)) :>=))
         t)
        (vm-string-not-equal
         (set-reg (vm-dst inst)
                  (wasm-string-compare-predicate-wat
                   (reg-local-ref reg-map (vm-str1 inst))
                   (reg-local-ref reg-map (vm-str2 inst)) :/=))
         t)
        (vm-code-char
         ;; code-char returns a character object.  Keep the VM char convention:
         ;; an i31 code point.  string.from_code_point is exposed by
         ;; emit-wasm-string-from-code-point-wat for callers that need a string.
         (set-reg (vm-dst inst)
                  (wasm-fixnum-box (wasm-fixnum-unbox reg-map (vm-src inst))))
         t)
        ((or vm-string-upcase vm-string-downcase vm-string-capitalize)
          (set-reg (vm-dst inst)
                   (emit-wasm-string-utf8-roundtrip-wat
                    (reg-local-ref reg-map (vm-src inst))))
          (reg-record-type reg-map (vm-dst inst) :string)
          t)
        (t nil)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; JS/FFI integration helpers (FR-217, FR-226, FR-236, FR-262, FR-286, FR-316)
;;; ─────────────────────────────────────────────────────────────────────────────

(defmacro js-ref-p (value)
  "CL-side binding predicate for opaque JS refs staged as $js_ref_t objects."
  `(typep ,value 'js-ref))

(defmacro js-ref-get (value)
  "CL-side binding accessor placeholder for an opaque JS reference payload."
  `(slot-value ,value 'value))

(defun wasm-js-ref-box-wat (externref-wat)
  "Box EXTERNREF-WAT in a GC struct so it can live in eqref VM locals."
  (format nil "(struct.new $js_ref_t ~A)" externref-wat))

(defun wasm-js-ref-unbox-wat (boxed-wat)
  "Return externref payload from a boxed JS ref."
  (format nil "(struct.get $js_ref_t 0 (ref.cast (ref $js_ref_t) ~A))" boxed-wat))

(defun wasm-any-convert-extern-wat (externref-wat)
  "Return FR-286 any.convert_extern WAT."
  (format nil "(any.convert_extern ~A)" externref-wat))

(defun wasm-extern-convert-any-wat (anyref-wat)
  "Return FR-286 extern.convert_any WAT."
  (format nil "(extern.convert_any ~A)" anyref-wat))

(defun emit-wat-js-conversion-functions (stream)
  "Emit WAT helpers for externref/anyref conversion and JS ref boxing."
  (when (wasm-extern-to-any-feature-enabled-p)
    (format stream "~%  ;; FR-226/FR-286: externref ↔ anyref conversion helpers")
    (format stream "~%  (func $clcc_any_from_extern (param externref) (result anyref)")
    (format stream "~%    (any.convert_extern (local.get 0))")
    (format stream "~%  )")
    (format stream "~%  (func $clcc_extern_from_any (param anyref) (result externref)")
    (format stream "~%    (extern.convert_any (local.get 0))")
    (format stream "~%  )")))

(defun emit-wat-js-primitive-builtins (stream)
  "Emit WAT helper declarations for JS primitive builtins (FR-316)."
  (when (wasm-feature-enabled-p "CLCC_WASM_JS_PRIMITIVES"
                                *wasm-js-primitive-builtins-enabled*)
    (format stream "~%  ;; FR-316: JS primitive builtin helper shapes")
    (format stream "~%  (func $js_number_to_i32 (param externref) (result i32) (i32.const 0))")
    (format stream "~%  (func $js_i32_to_number (param i32) (result externref) (ref.null extern))")
    (format stream "~%  (func $js_string_to_i32 (param externref) (result i32) (i32.const 0))")
    (format stream "~%  (func $js_string_to_f64 (param externref) (result f64) (f64.const 0))")
    (format stream "~%  (func $js_get_property (param externref externref) (result externref) (ref.null extern))")))

(defun emit-wat-js-promise-glue (stream)
  "Emit JS Promise Integration glue as droppable data for generated JS hosts."
  (when (wasm-js-promise-feature-enabled-p)
    (let ((payload "const suspending = fn => WebAssembly.Suspending ? new WebAssembly.Suspending(fn) : fn;
const promising = fn => WebAssembly.promising ? WebAssembly.promising(fn) : ((...a) => Promise.resolve(fn(...a)));
export const asyncCl = (instance, name) => promising(instance.exports[name]);"))
      (format stream "~%  (data $clcc_js_promise_glue ~S)" payload)
      (format stream "~%  (func $clcc_drop_js_promise_glue (data.drop $clcc_js_promise_glue))"))))

(defun emit-wat-js-exception-bridge-glue (stream)
  "Emit JS-side exception bridge glue as droppable data for FR-262."
  (when (wasm-js-exception-bridge-feature-enabled-p)
    (let ((payload "const clConditionTag = new WebAssembly.Tag({ parameters: ['externref', 'externref'] });
function wrapJsException(e){ return e instanceof WebAssembly.Exception ? e : new WebAssembly.Exception(clConditionTag, [null, e]); }"))
      (format stream "~%  (data $clcc_js_exception_bridge ~S)" payload)
      (format stream "~%  (func $clcc_drop_js_exception_bridge (data.drop $clcc_js_exception_bridge))"))))

(defun emit-wat-js-ffi-helpers (stream)
  "Emit all JS/FFI helper code controlled by existing feature flags."
  (emit-wat-string-builtin-helpers stream)
  (emit-wat-js-conversion-functions stream)
  (emit-wat-js-primitive-builtins stream)
  (emit-wat-js-promise-glue stream)
  (emit-wat-js-exception-bridge-glue stream))

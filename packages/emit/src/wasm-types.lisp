;;;; packages/emit/src/wasm-types.lisp — facade for canonical codegen WASM types
;;;;
;;;; The WebAssembly type constants and binary encoding helpers live in
;;;; packages/codegen/src/wasm-types.lisp.  This file keeps the historical/spec
;;;; documentation path under packages/emit/src/ valid without duplicating the
;;;; implementation.  Loading cl-cc/emit re-exports the codegen symbols that are
;;;; part of the WASM type and opcode surface.

(in-package :cl-cc/emit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %emit-reexport-codegen-symbols (names)
    "Import and export codegen symbols named by NAMES from CL-CC/EMIT.

When CL-CC/EMIT already owns a symbol with the same name (for example one of the
WASM Threads compatibility constants), keep that local symbol and export it to
avoid package conflicts.  Otherwise import the canonical CL-CC/CODEGEN symbol and
export it from this facade package."
    (let ((emit-package (find-package :cl-cc/emit))
          (codegen-package (find-package :cl-cc/codegen)))
      (dolist (name names)
        (multiple-value-bind (codegen-symbol status)
            (find-symbol name codegen-package)
          (declare (ignore status))
          (when codegen-symbol
            (multiple-value-bind (emit-symbol emit-status)
                (find-symbol name emit-package)
              (declare (ignore emit-status))
              (cond
                ((and emit-symbol (not (eq emit-symbol codegen-symbol)))
                 (export emit-symbol emit-package))
                (emit-symbol
                 (export emit-symbol emit-package))
                (t
                 (import codegen-symbol emit-package)
                 (export codegen-symbol emit-package))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%emit-reexport-codegen-symbols
   '("+WASM-SECTION-CUSTOM+" "+WASM-SECTION-TYPE+" "+WASM-SECTION-IMPORT+"
     "+WASM-SECTION-FUNCTION+" "+WASM-SECTION-TABLE+" "+WASM-SECTION-MEMORY+"
     "+WASM-SECTION-GLOBAL+" "+WASM-SECTION-EXPORT+" "+WASM-SECTION-START+"
     "+WASM-SECTION-ELEMENT+" "+WASM-SECTION-CODE+" "+WASM-SECTION-DATA+"
     "+WASM-SECTION-DATA-COUNT+" "+WASM-SECTION-TAG+"
     "+WASM-I32+" "+WASM-I64+" "+WASM-F32+" "+WASM-F64+"
     "+WASM-FUNCREF+" "+WASM-EXTERNREF+" "+WASM-ANYREF+" "+WASM-EQREF+"
     "+WASM-I31REF+" "+WASM-STRUCTREF+" "+WASM-ARRAYREF+" "+WASM-NULLREF+"
     "+WASM-NULLFUNCREF+" "+WASM-NULLEXTERNREF+"
     "+WASM-TYPE-FUNC+" "+WASM-TYPE-STRUCT+" "+WASM-TYPE-ARRAY+"
     "+WASM-TYPE-SUB+" "+WASM-TYPE-SUB-FINAL+" "+WASM-TYPE-REC+"
     "+WASM-FIELD-I8+" "+WASM-FIELD-I16+" "+WASM-IMMUTABLE+" "+WASM-MUTABLE+"
     "+WASM-EXPORT-FUNC+" "+WASM-EXPORT-TABLE+" "+WASM-EXPORT-MEMORY+"
     "+WASM-EXPORT-GLOBAL+" "+WASM-IMPORT-FUNC+" "+WASM-IMPORT-TABLE+"
     "+WASM-IMPORT-MEMORY+" "+WASM-IMPORT-GLOBAL+"
     "+WASM-UNREACHABLE+" "+WASM-NOP+" "+WASM-BLOCK+" "+WASM-LOOP+"
     "+WASM-IF+" "+WASM-ELSE+" "+WASM-TRY+" "+WASM-CATCH+" "+WASM-THROW+"
     "+WASM-END+" "+WASM-BR+" "+WASM-BR-IF+" "+WASM-BR-TABLE+"
     "+WASM-RETURN+" "+WASM-CALL+" "+WASM-CALL-INDIRECT+" "+WASM-CALL-REF+"
     "+WASM-DROP+" "+WASM-SELECT+" "+WASM-LOCAL-GET+" "+WASM-LOCAL-SET+"
     "+WASM-LOCAL-TEE+" "+WASM-GLOBAL-GET+" "+WASM-GLOBAL-SET+"
     "+WASM-I32-CONST+" "+WASM-I64-CONST+" "+WASM-F64-CONST+"
     "+WASM-I32-EQZ+" "+WASM-I32-EQ+" "+WASM-I32-NE+" "+WASM-I32-LT-S+"
     "+WASM-I32-GT-S+" "+WASM-I32-LE-S+" "+WASM-I32-GE-S+"
     "+WASM-I64-EQZ+" "+WASM-I64-EQ+" "+WASM-I64-NE+" "+WASM-I64-LT-S+"
     "+WASM-I64-GT-S+" "+WASM-I64-LE-S+" "+WASM-I64-GE-S+"
     "+WASM-I64-CLZ+" "+WASM-I64-CTZ+" "+WASM-I64-POPCNT+" "+WASM-I64-ADD+"
     "+WASM-I64-SUB+" "+WASM-I64-MUL+" "+WASM-I64-DIV-S+" "+WASM-I64-DIV-U+"
     "+WASM-I64-REM-S+" "+WASM-I64-REM-U+" "+WASM-I64-AND+" "+WASM-I64-OR+"
     "+WASM-I64-XOR+" "+WASM-I64-SHL+" "+WASM-I64-SHR-S+" "+WASM-I64-SHR-U+"
     "+WASM-I64-ROTL+" "+WASM-I64-ROTR+"
     "+WASM-F64-ABS+" "+WASM-F64-NEG+" "+WASM-F64-CEIL+" "+WASM-F64-FLOOR+"
     "+WASM-F64-TRUNC+" "+WASM-F64-NEAREST+" "+WASM-F64-SQRT+"
     "+WASM-F64-ADD+" "+WASM-F64-SUB+" "+WASM-F64-MUL+" "+WASM-F64-DIV+"
     "+WASM-F64-MIN+" "+WASM-F64-MAX+" "+WASM-F64-COPYSIGN+"
     "+WASM-F32-ABS+" "+WASM-F32-NEG+" "+WASM-F32-CEIL+" "+WASM-F32-FLOOR+"
     "+WASM-F32-TRUNC+" "+WASM-F32-NEAREST+" "+WASM-F32-SQRT+"
     "+WASM-F32-ADD+" "+WASM-F32-SUB+" "+WASM-F32-MUL+" "+WASM-F32-DIV+"
     "+WASM-F32-MIN+" "+WASM-F32-MAX+" "+WASM-F32-COPYSIGN+"
     "+WASM-I32-CLZ+" "+WASM-I32-CTZ+" "+WASM-I32-POPCNT+"
     "+WASM-I32-WRAP-I64+" "+WASM-I64-EXTEND-I32-S+" "+WASM-F64-CONVERT-I64-S+"
     "+WASM-I64-TRUNC-F64-S+" "+WASM-MISC-PREFIX+"
     "+WASM-I32-TRUNC-SAT-F32-S+" "+WASM-I32-TRUNC-SAT-F32-U+"
     "+WASM-I32-TRUNC-SAT-F64-S+" "+WASM-I32-TRUNC-SAT-F64-U+"
     "+WASM-I64-TRUNC-SAT-F32-S+" "+WASM-I64-TRUNC-SAT-F32-U+"
     "+WASM-I64-TRUNC-SAT-F64-S+" "+WASM-I64-TRUNC-SAT-F64-U+"
     "+WASM-I32-EXTEND8-S+" "+WASM-I32-EXTEND16-S+"
     "+WASM-I64-EXTEND8-S+" "+WASM-I64-EXTEND16-S+" "+WASM-I64-EXTEND32-S+"
     "+WASM-REF-NULL+" "+WASM-REF-IS-NULL+" "+WASM-REF-FUNC+" "+WASM-REF-EQ+"
     "+WASM-REF-AS-NON-NULL+" "+WASM-BR-ON-NULL+" "+WASM-BR-ON-NON-NULL+"
     "+WASM-RETURN-CALL+" "+WASM-RETURN-CALL-INDIRECT+"
     "+WASM-MEMORY-COPY+" "+WASM-MEMORY-FILL+" "+WASM-MEMORY-INIT+" "+WASM-DATA-DROP+"
     "+WASM-TABLE-INIT+" "+WASM-TABLE-COPY+" "+WASM-TABLE-FILL+" "+WASM-ELEM-DROP+"
     "+WASM-GC-PREFIX+" "+WASM-SIMD-PREFIX+" "+WASM-V128-LOAD+" "+WASM-V128-STORE+"
     "+WASM-V128-CONST+" "+WASM-I8X16-ADD+" "+WASM-I16X8-ADD+"
     "+WASM-I32X4-ADD+" "+WASM-I64X2-ADD+" "+WASM-V128-AND+"
     "+WASM-V128-OR+" "+WASM-V128-XOR+"
     "+WASM-I8X16-RELAXED-SWIZZLE+" "+WASM-I32X4-RELAXED-TRUNC-F32X4-S+"
     "+WASM-I32X4-RELAXED-TRUNC-F32X4-U+" "+WASM-I32X4-RELAXED-TRUNC-F64X2-S-ZERO+"
     "+WASM-I32X4-RELAXED-TRUNC-F64X2-U-ZERO+" "+WASM-F32X4-RELAXED-MADD+"
     "+WASM-F32X4-RELAXED-NMADD+" "+WASM-F64X2-RELAXED-MADD+"
     "+WASM-F64X2-RELAXED-NMADD+" "+WASM-I8X16-RELAXED-LANE-SELECT+"
     "+WASM-I16X8-RELAXED-LANE-SELECT+" "+WASM-I32X4-RELAXED-LANE-SELECT+"
      "+WASM-I64X2-RELAXED-LANE-SELECT+" "+WASM-F32X4-RELAXED-MIN+"
      "+WASM-F32X4-RELAXED-MAX+" "+WASM-F64X2-RELAXED-MIN+" "+WASM-F64X2-RELAXED-MAX+"
      "+WASM-I16X8-RELAXED-Q15MULR-S+"
     "+WASM-GC-STRUCT-NEW+" "+WASM-GC-STRUCT-NEW-DEFAULT+" "+WASM-GC-STRUCT-GET+"
     "+WASM-GC-STRUCT-GET-S+" "+WASM-GC-STRUCT-GET-U+" "+WASM-GC-STRUCT-SET+"
     "+WASM-GC-ARRAY-NEW+" "+WASM-GC-ARRAY-NEW-DEFAULT+" "+WASM-GC-ARRAY-NEW-FIXED+"
     "+WASM-GC-ARRAY-GET+" "+WASM-GC-ARRAY-GET-S+" "+WASM-GC-ARRAY-GET-U+"
     "+WASM-GC-ARRAY-SET+" "+WASM-GC-ARRAY-LEN+" "+WASM-GC-REF-TEST+"
     "+WASM-GC-REF-CAST+" "+WASM-GC-REF-I31+" "+WASM-GC-I31-GET-S+"
     "+WASM-GC-I31-GET-U+" "+WASM-GC-BR-ON-CAST+" "+WASM-GC-BR-ON-CAST-FAIL+"
     "+WASM-GC-ANY-CONVERT-EXTERN+" "+WASM-GC-EXTERN-CONVERT-ANY+"
     "+WASM-VOID+" "+WASM-MEMORY-SIZE64+" "+WASM-MEMORY-GROW64+"
     "+WASM-GC-STRUCT-NEW-IMMUTABLE+" "+WASM-GC-ARRAY-NEW-IMMUTABLE+"
     "+WASM-GC-ARRAY-NEW-DATA+" "+WASM-GC-ARRAY-NEW-ELEM+" "+WASM-GC-ARRAY-FILL+"
     "+WASM-GC-ARRAY-LOAD2-U+" "+WASM-GC-ARRAY-LOAD4-U+"
     "+WASM-GC-ARRAY-STORE2+" "+WASM-GC-ARRAY-STORE4+"
     "+WASM-GC-STRUCT-GET-S-PACKED+" "+WASM-GC-STRUCT-GET-U-PACKED+"
     "+WASM-GC-STRUCT-SET-PACKED+" "+WASM-SELECT-TYPED+" "+WASM-FUNC-BIND+"
     "+WASM-TRY-TABLE+" "+WASM-THROW-REF+"
     "+WASM-CONT-NEW+" "+WASM-CONT-BIND+" "+WASM-SUSPEND+" "+WASM-RESUME+"
     "+WASM-RESUME-THROW+" "+WASM-CONT-THROW+"
     "+WASM-STRING-NEW-UTF8+" "+WASM-STRING-NEW-WTF8+" "+WASM-STRING-NEW-LOSSY-UTF8+"
     "+WASM-STRING-ENCODE-WTF16+" "+WASM-STRING-ENCODE-UTF8+" "+WASM-STRING-ENCODE-WTF8+"
     "+WASM-STRING-MEASURE-WTF16+" "+WASM-STRING-MEASURE-UTF8+" "+WASM-STRING-MEASURE-WTF8+"
     "+WASM-STRING-CONCAT+" "+WASM-STRING-COMPARE+" "+WASM-STRING-FROM-CODE-POINT+"
     "+WASM-I64-ADD128+" "+WASM-I64-SUB128+" "+WASM-I64-MUL-WIDE-S+" "+WASM-I64-MUL-WIDE-U+"
     "+WASM-F16-LOAD+" "+WASM-F16-STORE+" "+WASM-F16-ADD+" "+WASM-F16-SUB+"
     "+WASM-F16-MUL+" "+WASM-F16-DIV+" "+WASM-F16-MIN+" "+WASM-F16-MAX+"
     "WASM-ENCODE-SIMD-OP" "WASM-ENCODE-U32-LEB128" "WASM-ENCODE-OP-U32"
     "WASM-ENCODE-GLOBAL-GET" "WASM-ENCODE-GLOBAL-SET" "WASM-ENCODE-CALL"
     "WASM-ENCODE-CALL-INDIRECT" "WASM-ENCODE-GC-OP-U32" "WASM-ENCODE-STRUCT-NEW"
     "WASM-ENCODE-ARRAY-NEW" "WASM-ENCODE-TRY" "WASM-ENCODE-CATCH" "WASM-ENCODE-THROW")))

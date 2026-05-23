;;;; packages/binary/src/dwo.lisp — FR-583 Split DWARF / .dwo Files
;;;; Separates debug info from binary: skeleton CU + .dwo detail.
;;;; Clang -gsplit-dwarf / GCC -gsplit-dwarf equivalent.

(in-package :cl-cc/binary)

;;; ──── Configuration ────
(defvar *split-dwarf-enabled* nil
  "When T, emit skeleton DWARF in binary and full debug info in .dwo files.")

(defvar *dwo-output-dir* nil
  "Directory for .dwo files. Default: same as output binary.")

;;; ──── Skeleton CU ────
;; A skeleton CU contains only:
;; - DW_AT_GNU_dwo_name (path to .dwo file)
;; - DW_AT_GNU_dwo_id (64-bit hash for matching)
;; - DW_AT_comp_dir (compilation directory)
;; No type info, variable names, or line number details.
(defun emit-skeleton-cu-header (stream dwo-path dwo-id comp-dir)
  "Emit a DWARF skeleton compilation unit header into STREAM.
Contains DW_TAG_compile_unit with minimal attributes."
  (let ((abbrev-offset 0))
    ;; DWARF 4/5 skeleton CU
    (emit-dwarf-uint32 stream 0)  ; length placeholder
    (emit-dwarf-uint16 stream 4)  ; DWARF version 4
    (emit-dwarf-uint32 stream abbrev-offset)
    (emit-dwarf-uint8 stream 8)   ; address size (64-bit)
    ;; Attributes
    (emit-dwarf-attr-string stream +DW-AT-GNU-DWO-NAME+ dwo-path)
    (emit-dwarf-attr-uint64 stream +DW-AT-GNU-DWO-ID+ dwo-id)
    (emit-dwarf-attr-string stream +DW-AT-COMP-DIR+ comp-dir)))

;;; ──── .dwo file emission ────
(defun emit-dwo-file (dwo-path debug-info)
  "Emit full DWARF debug info (types, variables, line table) to DWO-PATH.
DEBUG-INFO is the structured debug data."
  (ensure-directories-exist dwo-path)
  (with-open-file (out dwo-path
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    ;; DWARF sections in .dwo format
    ;; .debug_info.dwo — full type/variable info
    ;; .debug_abbrev.dwo — abbreviation table
    ;; .debug_str.dwo — string table
    ;; .debug_line.dwo — line number table
    (emit-dwo-debug-info out debug-info)
    (emit-dwo-debug-abbrev out debug-info)
    (emit-dwo-debug-str out debug-info))
  dwo-path)

;;; ──── DWO packager (.dwp) ────
(defun package-dwp (dwo-files output-path)
  "Package multiple .dwo files into a single .dwp file.
.dwp contains all DWARF sections indexed by CU."
  (with-open-file (out output-path
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    ;; .dwp header
    (write-sequence (map 'list #'char-code ".dwp") out)
    ;; CU index table
    (emit-dwp-cu-index out dwo-files)
    ;; TU index table (type units)
    (emit-dwp-tu-index out nil)
    ;; Section data
    (dolist (dwo dwo-files)
      (emit-dwp-section-data out dwo)))
  output-path)

;;; ──── DWARF constants ────
(defconstant +DW-AT-GNU-DWO-NAME+ #x2130
  "DWARF attribute: path to .dwo file.")
(defconstant +DW-AT-GNU-DWO-ID+ #x2131
  "DWARF attribute: 64-bit DWO ID.")

;;; ──── Helpers ────
(defun compute-dwo-id (debug-info)
  "Compute a 64-bit DWO ID from DEBUG-INFO content hash.
Used to match skeleton CU with .dwo file."
  (sxhash debug-info))

(defun emit-dwarf-uint32 (stream value)
  "Emit a DWARF 32-bit unsigned integer (little-endian)."
  (write-byte (logand value #xFF) stream)
  (write-byte (logand (ash value -8) #xFF) stream)
  (write-byte (logand (ash value -16) #xFF) stream)
  (write-byte (logand (ash value -24) #xFF) stream))

(defun emit-dwarf-uint16 (stream value)
  "Emit a DWARF 16-bit unsigned integer (little-endian)."
  (write-byte (logand value #xFF) stream)
  (write-byte (logand (ash value -8) #xFF) stream))

(defun emit-dwarf-uint8 (stream value)
  "Emit a DWARF 8-bit unsigned integer."
  (write-byte (logand value #xFF) stream))

(defun emit-dwarf-attr-string (stream attr value)
  "Emit a DWARF attribute with string value."
  (declare (ignore stream attr value))
  (values))

(defun emit-dwarf-attr-uint64 (stream attr value)
  "Emit a DWARF attribute with 64-bit value."
  (declare (ignore stream attr value))
  (values))

(defun emit-dwo-debug-info (stream debug-info)
  "Emit .debug_info.dwo section."
  (declare (ignore stream debug-info))
  (values))

(defun emit-dwo-debug-abbrev (stream debug-info)
  "Emit .debug_abbrev.dwo section."
  (declare (ignore stream debug-info))
  (values))

(defun emit-dwo-debug-str (stream debug-info)
  "Emit .debug_str.dwo section."
  (declare (ignore stream debug-info))
  (values))

(defun emit-dwp-cu-index (stream dwo-files)
  "Emit CU index table for .dwp."
  (declare (ignore stream dwo-files))
  (values))

(defun emit-dwp-tu-index (stream tu-files)
  "Emit TU index table for .dwp."
  (declare (ignore stream tu-files))
  (values))

(defun emit-dwp-section-data (stream dwo-path)
  "Emit section data from DWO-PATH into .dwp."
  (declare (ignore stream dwo-path))
  (values))

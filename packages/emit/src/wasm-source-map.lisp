;;;; packages/emit/src/wasm-source-map.lisp — Source Map v3 support for Wasm

(in-package :cl-cc/emit)

(defparameter +source-map-base64-digits+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "Base64 digit table used by Source Map v3 VLQ segments.")

(defun %source-map-json-string (value)
  "Return VALUE encoded as a JSON string."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for ch across (princ-to-string (or value ""))
          do (case ch
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Backspace (write-string "\\b" out))
               (#\Page (write-string "\\f" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (otherwise (write-char ch out))))
    (write-char #\" out)))

(defun %source-map-json-string-array (values)
  "Return VALUES encoded as a JSON string array."
  (with-output-to-string (out)
    (write-char #\[ out)
    (loop for value in values
          for first-p = t then nil
          do (unless first-p (write-char #\, out))
             (write-string (%source-map-json-string value) out))
    (write-char #\] out)))

(defun %source-map-vlq-signed (value)
  "Convert signed VALUE to Source Map VLQ sign-bit representation."
  (if (minusp value)
      (logior (ash (- value) 1) 1)
      (ash value 1)))

(defun source-map-encode-vlq (value)
  "Encode integer VALUE with Source Map base64 VLQ encoding."
  (let ((vlq (%source-map-vlq-signed value))
        (chars nil))
    (loop
      (let ((digit (logand vlq #x1f)))
        (setf vlq (ash vlq -5))
        (when (plusp vlq)
          (setf digit (logior digit #x20)))
        (push (char +source-map-base64-digits+ digit) chars)
        (unless (plusp vlq)
          (return (coerce (nreverse chars) 'string)))))))

(defun %source-map-source-index (source sources-ref table)
  "Return SOURCE's stable index in SOURCES-REF/TABLE, adding it when needed.
SOURCES-REF is a cons box (list of sources stored in car)."
  (or (gethash source table)
      (let ((index (length (car sources-ref))))
        (setf (gethash source table) index)
        (setf (car sources-ref) (append (car sources-ref) (list source)))
        index)))

(defun %source-map-normalize-entry (entry sources-ref table previous)
  "Return one VLQ segment and updated PREVIOUS state for ENTRY."
  (destructuring-bind (prev-generated-column prev-source prev-line prev-column) previous
    (let* ((generated-column (max 0 (or (getf entry :offset) 0)))
           (source-name (or (getf entry :source) "<unknown>"))
           (source-index (%source-map-source-index source-name sources-ref table))
           (source-line (max 0 (1- (or (getf entry :line) 1))))
           (source-column (max 0 (or (getf entry :column) 0)))
           (fields (list (- generated-column prev-generated-column)
                         (- source-index prev-source)
                         (- source-line prev-line)
                         (- source-column prev-column))))
      (values (apply #'concatenate 'string (mapcar #'source-map-encode-vlq fields))
              (list generated-column source-index source-line source-column)))))

(defun source-map-encode-mappings (entries)
  "Encode ENTRIES into Source Map v3 mappings and return mappings/sources."
  (let ((sorted (sort (copy-list entries) #'< :key (lambda (entry) (getf entry :offset 0))))
        (sources-ref (list nil))   ; mutable cons box for accumulating source names
        (source-table (make-hash-table :test #'equal))
        (previous '(0 0 0 0))
        (segments nil))
    (dolist (entry sorted)
      (multiple-value-bind (segment next-previous)
          (%source-map-normalize-entry entry sources-ref source-table previous)
        (push segment segments)
        (setf previous next-previous)))
    (values (format nil "~{~A~^,~}" (nreverse segments)) (car sources-ref))))

(defun build-wasm-source-map-v3 (entries &key file (source-root ""))
  "Build a Source Map v3 JSON string for Wasm ENTRIES."
  (multiple-value-bind (mappings sources)
      (source-map-encode-mappings entries)
    (with-output-to-string (out)
      (format out "{~%  \"version\": 3,~%")
      (format out "  \"file\": ~A,~%" (%source-map-json-string (or file "")))
      (format out "  \"sourceRoot\": ~A,~%" (%source-map-json-string source-root))
      (format out "  \"sources\": ~A,~%" (%source-map-json-string-array sources))
      (format out "  \"names\": [],~%")
      (format out "  \"mappings\": ~A~%" (%source-map-json-string mappings))
      (format out "}~%"))))

(defun %wasm-source-map-entries-from-asts (asts instruction-count &key fallback-source)
  "Build best-effort Wasm source-map entries from top-level ASTS."
  (let* ((nodes (remove-if-not (lambda (node)
                                 (and (typep node 'cl-cc/ast:ast-node)
                                      (cl-cc/ast:ast-source-line node)))
                               (if (listp asts) asts (list asts))))
         (count (max 1 (length nodes)))
         (span (max 1 (floor (max 1 instruction-count) count))))
    (loop for node in nodes
          for offset from 0 by span
          collect (list :offset offset
                        :source (or (cl-cc/ast:ast-source-file node) fallback-source "<unknown>")
                        :line (or (cl-cc/ast:ast-source-line node) 1)
                        :column (or (cl-cc/ast:ast-source-column node) 0)))))

(defun %source-map-symbol-call (package-name symbol-name object)
  "Call SYMBOL-NAME from PACKAGE-NAME on OBJECT when available."
  (let* ((package (find-package package-name))
         (symbol (and package (find-symbol symbol-name package))))
    (and symbol (fboundp symbol) (funcall symbol object))))

(defun %source-map-result-program (result)
  (%source-map-symbol-call "CL-CC/COMPILE" "COMPILATION-RESULT-PROGRAM" result))

(defun %source-map-result-assembly (result)
  (%source-map-symbol-call "CL-CC/COMPILE" "COMPILATION-RESULT-ASSEMBLY" result))

(defun %source-map-result-ast (result)
  (%source-map-symbol-call "CL-CC/COMPILE" "COMPILATION-RESULT-AST" result))

(defun %source-map-result-vm-instructions (result)
  (%source-map-symbol-call "CL-CC/COMPILE" "COMPILATION-RESULT-VM-INSTRUCTIONS" result))

(defun wasm-source-map-entries-for-result (result &key source-file)
  "Return Wasm instruction-offset source-map entries for compilation RESULT."
  (let* ((program (%source-map-result-program result))
         (instructions (or (%source-map-result-vm-instructions result)
                           (and program
                                (cl-cc/vm:vm-program-instructions
                                 program))))
         (asts (%source-map-result-ast result)))
    (or (%wasm-source-map-entries-from-asts asts (length instructions)
                                           :fallback-source source-file)
        (list (list :offset 0 :source (or source-file "<unknown>") :line 1 :column 0)))))

(defun write-wasm-source-map (result map-path &key wasm-file source-file (source-root ""))
  "Write RESULT's Wasm Source Map v3 JSON to MAP-PATH and return MAP-PATH."
  (let ((json (build-wasm-source-map-v3
               (wasm-source-map-entries-for-result result :source-file source-file)
               :file (or wasm-file (pathname-name map-path))
               :source-root source-root)))
    (ensure-directories-exist map-path)
    (with-open-file (out map-path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
      (write-string json out))
    map-path))

(defun wasm-source-map-path (wasm-path)
  "Return the default .map path for WASM-PATH."
  (parse-namestring (format nil "~A.map" (namestring (pathname wasm-path)))))

(defun write-wasm-with-source-map (result wasm-path &key source-file)
  "Write RESULT assembly to WASM-PATH and its Source Map v3 sidecar."
  (ensure-directories-exist wasm-path)
  (with-open-file (out wasm-path :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (write-string (or (%source-map-result-assembly result) "") out))
  (write-wasm-source-map result (wasm-source-map-path wasm-path)
                         :wasm-file (file-namestring wasm-path)
                         :source-file source-file)
  (values wasm-path (wasm-source-map-path wasm-path)))

;;;; cli/src/handlers-wasm.lisp — Wasm AOT/Toolchain Helpers
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Low-level helpers for Wasm AOT compilation, file output, validation,
;;; streaming JS loader generation, and metadata writing.
;;; FR-219/221/232/265/305/307/322
;;;
;;; Loaded before handlers.lisp; the compile subcommand handler calls
;;; %compile-to-wasm-output from this file.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/cli)

(defun %wasm-target-requested-p (arch-str parsed)
  "Return T when CLI options select the wasm toolchain."
  (or (flag parsed "--aot")
      (member (string-downcase (or arch-str ""))
              '("wasm" "wasm32" "wasm64" "wasm32-wasi")
              :test #'string=)))

(defun %wasm-output-file (input output aot)
  "Return output pathname for wasm compilation."
  (or output
      (namestring (make-pathname :defaults (pathname input)
                                 :type (if aot "wasm" "wat")))))

(defun %write-string-file (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string text out))
  path)

(defun %write-wasm-bytes-file (path bytes)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-sequence bytes out))
  path)

(defun %wasm-sidecar-path (output type)
  (namestring (make-pathname :defaults (pathname output) :type type)))

(defun %wasm-compile-source-to-vm-result (source file language kwargs)
  "Compile SOURCE to a VM compilation-result for wasm sidecar metadata."
  (if (eq (or language :lisp) :lisp)
      (apply #'compile-string source :source-file file :language :lisp :target :vm kwargs)
      (apply #'compile-string source :language language :target :vm kwargs)))

(defun %wasm-generate-streaming-js (output wasm-path content-hash integrity)
  "Write FR-232 instantiateStreaming + IndexedDB cache glue next to OUTPUT."
  (let* ((js-path (%wasm-sidecar-path output "js"))
         (wasm-name (file-namestring (pathname wasm-path)))
         (cache-key (format nil "cl-cc:wasm:~A" content-hash)))
    (%write-string-file
     js-path
     (format nil "// cl-cc FR-232 streaming WebAssembly loader~%
const DB = 'cl-cc-wasm-cache-v1';~%
const STORE = 'modules';~%
const CACHE_KEY = '~A';~%
const WASM_URL = '~A';~%
export const integrity = '~A';~%
function openDb() {~%
  return new Promise((resolve, reject) => {~%
    const req = indexedDB.open(DB, 1);~%
    req.onupgradeneeded = () => req.result.createObjectStore(STORE);~%
    req.onsuccess = () => resolve(req.result);~%
    req.onerror = () => reject(req.error);~%
  });~%
}~%
async function cachedModule() {~%
  const db = await openDb();~%
  const tx = db.transaction(STORE, 'readonly');~%
  const hit = await new Promise(resolve => { const r = tx.objectStore(STORE).get(CACHE_KEY); r.onsuccess = () => resolve(r.result); r.onerror = () => resolve(null); });~%
  if (hit) return hit;~%
  const module = await WebAssembly.compileStreaming(fetch(WASM_URL, { integrity }));~%
  const put = db.transaction(STORE, 'readwrite').objectStore(STORE).put(module, CACHE_KEY);~%
  await new Promise(resolve => { put.onsuccess = put.onerror = resolve; });~%
  return module;~%
}~%
export async function instantiate(imports = {}) {~%
  if (WebAssembly.instantiateStreaming) {~%
    try { return await WebAssembly.instantiateStreaming(fetch(WASM_URL, { integrity }), imports); } catch (_) {}~%
  }~%
  return WebAssembly.instantiate(await cachedModule(), imports);~%
}~%"
             cache-key wasm-name (or integrity "")))
    js-path))

(defun %wasm-validate-file (path)
  "Validate PATH with WebAssembly.validate and wasmtime when available."
  (let ((ok t))
    (when (cl-cc/codegen:wasm-tool-available-p "node")
      (let ((script (format nil "const fs=require('fs');process.exit(WebAssembly.validate(fs.readFileSync(~S))?0:1)" path)))
        (handler-case
            (uiop:run-program (list "node" "-e" script) :ignore-error-status nil)
          (error (e)
            (setf ok nil)
            (format *error-output* "WebAssembly.validate failed: ~A~%" e)))))
    (when (cl-cc/codegen:wasm-tool-available-p "wasmtime")
      (handler-case
          (uiop:run-program (list "wasmtime" "validate" path) :ignore-error-status nil)
        (error (e)
          (setf ok nil)
          (format *error-output* "wasmtime validate failed: ~A~%" e))))
    ok))

(defun %write-wasm-metadata (output metadata &key integrity sha256 sha384 streaming-js)
  "Write build output metadata as a small deterministic JSON file."
  (let ((path (%wasm-sidecar-path output "metadata.json")))
    (%write-string-file
     path
     (format nil "{~%  \"format\": \"cl-cc-wasm-build-metadata-v1\",~%  \"wasm\": ~S,~%  \"sha256\": ~S,~%  \"sha384\": ~S,~%  \"integrity\": ~S,~%  \"htmlAttribute\": ~S,~%  \"streamingJs\": ~S,~%  \"aot\": ~A,~%  \"deterministic\": ~A~%}~%"
             (file-namestring (pathname output))
             (or sha256 (getf metadata :sha256) "")
             (or sha384 "")
             (or integrity "")
             (if integrity (format nil "integrity=\"~A\"" integrity) "")
             (or streaming-js "")
             (if (eq (getf metadata :format) :cl-cc-wasm-aot-v1) "true" "false")
             (if (getf metadata :deterministic) "true" "false")))
    path))

(defun %compile-to-wasm-output (file output language opts parsed kwargs)
  "Compile FILE through the wasm toolchain and write requested artifacts."
  (let* ((source (%read-command-source file))
         (aot (flag parsed "--aot"))
         (deterministic (compile-opts-deterministic opts))
         (out (%wasm-output-file file output aot))
          (map-path (parse-namestring (format nil "~A.map" (namestring (pathname out)))))
          (cl-cc/codegen::*wasm-aot-mode-enabled* (not (null aot)))
          (cl-cc/codegen::*wasm-source-map-enabled* (or (flag parsed "--source-map")
                                                        cl-cc/codegen::*wasm-source-map-enabled*))
          (cl-cc/codegen::*wasm-source-map-url* (file-namestring map-path))
          (cl-cc/codegen::*wasm-dwarf-debug-info-enabled* (or (compile-opts-debug-info opts)
                                                              (flag parsed "--emit-debug-info")
                                                              cl-cc/codegen::*wasm-dwarf-debug-info-enabled*))
          (cl-cc/codegen::*wasm-extended-names-enabled* (or (flag parsed "--emit-names")
                                                            cl-cc/codegen::*wasm-extended-names-enabled*))
          (cl-cc/codegen::*wasm-type-reflection-js-api-enabled* (or (flag parsed "--type-reflection")
                                                                    cl-cc/codegen::*wasm-type-reflection-js-api-enabled*))
          (cl-cc/codegen::*wasm-call-stack-inspection-enabled* (or (flag parsed "--stack-inspection")
                                                                   cl-cc/codegen::*wasm-call-stack-inspection-enabled*))
          (cl-cc/codegen::*wasm-memory-profiler-enabled* (or (flag parsed "--memory-profiler")
                                                             cl-cc/codegen::*wasm-memory-profiler-enabled*))
          (cl-cc/codegen::*wasm-hot-code-reload-enabled* (or (flag parsed "--hot-reload")
                                                             cl-cc/codegen::*wasm-hot-code-reload-enabled*))
          (cl-cc/codegen::*wasm-repl-incremental-compilation-enabled* (or (flag parsed "--incremental-repl")
                                                                          (compile-opts-incremental opts)
                                                                          cl-cc/codegen::*wasm-repl-incremental-compilation-enabled*))
          (cl-cc/codegen::*wasm-memory64-enabled* (or (flag parsed "--memory64")
                                                      cl-cc/codegen::*wasm-memory64-enabled*))
         (cl-cc/codegen::*wasm-table64-enabled* (or (flag parsed "--memory64")
                                                    cl-cc/codegen::*wasm-table64-enabled*))
         (cl-cc/codegen::*wasm-js-bigint-i64-enabled* (or (flag parsed "--bigint")
                                                          cl-cc/codegen::*wasm-js-bigint-i64-enabled*))
          (vm-result (%wasm-compile-source-to-vm-result source file language kwargs))
          (program (compilation-result-program vm-result)))
    (if aot
        (let* ((result (cl-cc/codegen:compile-to-aot-wasm program :deterministic deterministic))
               (bytes (cl-cc/codegen:wasm-aot-result-bytes result)))
          (%write-wasm-bytes-file out bytes)
          (%write-string-file (%wasm-sidecar-path out "wat") (cl-cc/codegen:wasm-aot-result-wat result))
          (when (flag parsed "--validate")
            (unless (%wasm-validate-file out)
              (uiop:quit 1)))
          (let* ((sha256 (cl-cc/codegen:wasm-file-content-hash out :bits 256))
                 (sha384 (cl-cc/codegen:wasm-file-content-hash out :bits 384))
                 (integrity (when (or (flag parsed "--sri") (flag parsed "--streaming"))
                              (cl-cc/codegen:wasm-file-sri-hash out :bits 384)))
                 (streaming-js (when (flag parsed "--streaming")
                                 (%wasm-generate-streaming-js out out sha256 integrity))))
            (when (or (flag parsed "--sri") (flag parsed "--streaming") deterministic)
              (%write-wasm-metadata out (cl-cc/codegen:wasm-aot-result-metadata result)
                                    :integrity integrity :sha256 sha256 :sha384 sha384
                                    :streaming-js streaming-js)))
          out)
        (let ((wat (cl-cc/codegen:compile-to-wasm-wat program)))
          (%write-string-file out wat)
          (when cl-cc/codegen::*wasm-source-map-enabled*
            (cl-cc/emit:write-wasm-source-map vm-result map-path
                                              :wasm-file (file-namestring (pathname out))
                                              :source-file file)
            (format *error-output* "; cl-cc wasm: wrote source map ~A~%" map-path))
          out))))

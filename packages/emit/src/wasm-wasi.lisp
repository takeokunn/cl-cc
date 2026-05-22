;;;; packages/emit/src/wasm-wasi.lisp — WASI Preview 2 component helpers

(in-package :cl-cc/emit)

(defparameter *wasm-wasi-enabled* nil
  "When true, Wasm emission may include WASI p2 imports. Disabled by default.")

(defparameter +wasi-p2-core-world+ "wasi:cli/command@0.2.0")

(defstruct wasi-import
  (world +wasi-p2-core-world+ :type string)
  (interface "wasi:cli/stdout@0.2.0" :type string)
  (name "get-stdout" :type string)
  (kind :func :type keyword)
  (type nil))

(defun make-wasi-p2-imports (&key (stdio t) random clocks filesystem)
  "Return declarative WASI p2 import descriptors for requested capabilities."
  (append
   (when stdio
     (list (make-wasi-import :interface "wasi:cli/stdin@0.2.0" :name "get-stdin")
           (make-wasi-import :interface "wasi:cli/stdout@0.2.0" :name "get-stdout")
           (make-wasi-import :interface "wasi:cli/stderr@0.2.0" :name "get-stderr")))
   (when random
     (list (make-wasi-import :interface "wasi:random/random@0.2.0" :name "get-random-bytes")))
   (when clocks
     (list (make-wasi-import :interface "wasi:clocks/monotonic-clock@0.2.0" :name "now")))
   (when filesystem
     (list (make-wasi-import :interface "wasi:filesystem/preopens@0.2.0" :name "get-directories")))))

(defun wasi-p2-import->wat (import)
  "Render IMPORT as a WAT import stub."
  (format nil ";; WASI p2 ~A ~A~%(import ~S ~S (func $~A))"
          (wasi-import-world import)
          (wasi-import-interface import)
          (wasi-import-interface import)
          (wasi-import-name import)
          (substitute #\_ #\/ (wasi-import-name import))))

(defun inject-wasi-p2-imports-into-wat (wat imports)
  "Insert WASI p2 IMPORTS after the opening `(module` in WAT."
  (let ((payload (format nil "~{~A~%~}" (mapcar #'wasi-p2-import->wat imports))))
    (if (search "(module" wat)
        (let ((pos (+ (search "(module" wat) (length "(module"))))
          (concatenate 'string (subseq wat 0 pos) "\n" payload (subseq wat pos)))
        (concatenate 'string "(module\n" payload wat "\n)"))))

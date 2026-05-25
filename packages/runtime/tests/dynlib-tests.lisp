;;;; packages/runtime/tests/dynlib-tests.lisp — FR-719 dynamic library tests

(in-package :cl-cc/test)

;;; cl-cc/ffi is provided at load-time by the cl-cc umbrella system.
;;; At compile-time in the test build, the package may not exist yet.
;;; Create a placeholder so the reader can intern symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-cc/ffi)
    (defpackage :cl-cc/ffi
      (:use :cl)
      (:export #:dl-lib #:dl-lib-p #:dl-lib-name #:dl-lib-handle #:dl-lib-loaded
               #:load-shared-library #:load-framework #:unload-shared-library
               #:list-loaded-libraries #:find-foreign-symbol))))

(in-suite cl-cc-unit-suite)

(deftest fr-417-pinned-unboxed-array-provides-data-pointer
  "Pinned unboxed arrays expose a stable FFI data pointer/token until released."
  (let* ((array (make-array 4 :element-type '(unsigned-byte 8)
                            :initial-contents '(1 2 3 4)))
         (buffer (cl-cc/runtime:rt-pin-unboxed-array array)))
    (assert-true (cl-cc/runtime:rt-pinned-unboxed-array-buffer-p buffer))
    (assert-eq array (cl-cc/runtime:rt-pinned-unboxed-array-buffer-array buffer))
    (assert-= 4 (cl-cc/runtime:rt-pinned-unboxed-array-buffer-length buffer))
    (assert-true (cl-cc/runtime:rt-pinned-array-data-pointer buffer))
    (cl-cc/runtime:rt-release-pinned-array buffer)
    (assert-true (cl-cc/runtime:rt-pinned-unboxed-array-buffer-released-p buffer))
    (assert-signals error
      (cl-cc/runtime:rt-pinned-array-data-pointer buffer))))

(defun %test-libc-path ()
  #+darwin "/usr/lib/libSystem.B.dylib"
  #+linux "libc.so.6"
  #-(or darwin linux) nil)

(deftest dynlib-load-find-unload-libc-symbol
  "load-shared-library/find-foreign-symbol/unload-shared-library handle a host libc symbol."
  (let ((path (%test-libc-path)))
    (when path
      (let ((library (cl-cc/ffi:load-shared-library path)))
        (unwind-protect
             (progn
               (assert-true (cl-cc/ffi:dl-lib-p library))
               (assert-equal path (cl-cc/ffi:dl-lib-name library))
               (assert-true (cl-cc/ffi:dl-lib-loaded library))
               (assert-true (functionp (cl-cc/ffi:find-foreign-symbol "printf" library)))
               (assert-null (cl-cc/ffi:find-foreign-symbol "cl_cc_symbol_that_does_not_exist" library)))
          (cl-cc/ffi:unload-shared-library library))
        (assert-false (cl-cc/ffi:dl-lib-loaded library))))))

(deftest dynlib-found-symbol-is-callable
  "find-foreign-symbol returns a callable host-backed function object for no-arg int functions."
  (let ((path (%test-libc-path)))
    (when path
      (let ((library (cl-cc/ffi:load-shared-library path)))
        (unwind-protect
             (let ((getpid (cl-cc/ffi:find-foreign-symbol "getpid" library)))
               (assert-true (functionp getpid))
               (assert-true (integerp (funcall getpid))))
          (cl-cc/ffi:unload-shared-library library))))))

#+darwin
(deftest dynlib-load-framework-foundation
  "load-framework resolves and loads a macOS framework by framework name."
  (let ((library (cl-cc/ffi:load-framework "Foundation")))
    (unwind-protect
         (progn
            (assert-true (cl-cc/ffi:dl-lib-p library))
            (assert-equal "/System/Library/Frameworks/Foundation.framework/Foundation"
                          (cl-cc/ffi:dl-lib-name library)))
      (cl-cc/ffi:unload-shared-library library))))

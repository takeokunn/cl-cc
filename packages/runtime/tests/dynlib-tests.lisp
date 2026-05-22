;;;; packages/runtime/tests/dynlib-tests.lisp — FR-719 dynamic library tests

(in-package :cl-cc/test)

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
      (let ((library (cl-cc/runtime:load-shared-library path)))
        (unwind-protect
             (progn
               (assert-true (cl-cc/runtime:rt-shared-library-p library))
               (assert-equal path (cl-cc/runtime:rt-shared-library-path library))
               (assert-true (cl-cc/runtime:find-foreign-symbol library "printf")))
          (cl-cc/runtime:unload-shared-library library))
        (assert-true (cl-cc/runtime:rt-shared-library-closed-p library))))))

#+darwin
(deftest dynlib-load-framework-foundation
  "load-framework resolves and loads a macOS framework by framework name."
  (let ((library (cl-cc/runtime:load-framework "Foundation")))
    (unwind-protect
         (progn
           (assert-true (cl-cc/runtime:rt-shared-library-p library))
           (assert-equal "Foundation"
                         (cl-cc/runtime:rt-shared-library-framework-name library)))
      (cl-cc/runtime:unload-shared-library library))))

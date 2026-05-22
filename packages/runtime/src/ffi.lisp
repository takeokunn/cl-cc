;;;; FFI / Native Bridge (FR-530-533)
;;;;
;;;; Native foreign function interface for the standalone runtime.
;;;; On host SBCL this delegates to CFFI/SB-ALIEN; on native binary
;;;; it uses libffi or direct dlopen/dlsym.
(in-package :cl-cc/runtime)

;; ── Platform detection ──
(defconstant +ffi-platform+
  #+darwin :darwin
  #+linux :linux
  #-(or darwin linux) :unknown)

;; ── Dynamic library loading ──
(defvar *rt-ffi-loaded-libs* (make-hash-table :test #'equal))

(defun rt-ffi-load-library (path)
  "Load a shared library. Returns an opaque handle."
  #+sbcl (sb-alien:load-shared-object path)
  #-sbcl (progn
           (setf (gethash path *rt-ffi-loaded-libs*) t)
           path))

(defun rt-ffi-close-library (handle)
  "Unload a shared library."
  (declare (ignore handle))
  #+sbcl nil  ; SBCL doesn't support unloading
  #-sbcl t)

(defun rt-ffi-find-symbol (handle name)
  "Find a symbol address in a loaded library. Returns a function pointer."
  #+sbcl (sb-alien:extern-alien name (function sb-alien:void))
  #-sbcl (cons handle name))

;; ── Foreign function calling ──
(defvar *rt-ffi-registered-functions* (make-hash-table :test #'equal))

(defstruct rt-ffi-function
  (name "")           ; function name
  (library nil)       ; library handle
  (arg-types nil)     ; list of :int, :float, :pointer, :string
  (return-type :void) ; return type
  (ptr nil))          ; function pointer

(defun rt-define-foreign-function (name library arg-types return-type)
  "Declare a foreign function signature."
  (let ((ptr (rt-ffi-find-symbol library name)))
    (setf (gethash name *rt-ffi-registered-functions*)
          (make-rt-ffi-function :name name :library library
                                :arg-types arg-types :return-type return-type
                                :ptr ptr))
    name))

(defun rt-foreign-funcall (name &rest args)
  "Call a registered foreign function."
  (let ((ff (gethash name *rt-ffi-registered-functions*)))
    (unless ff (error "Unknown foreign function: ~A" name))
    #+sbcl (apply #'sb-alien:alien-funcall
                  (sb-alien:extern-alien name
                    (function sb-alien:void))
                  args)
    #-sbcl (list :ffi-call name args)))

;; ── Callback support ──
(defstruct rt-ffi-callback
  (id 0)
  (fn nil)        ; Lisp function
  (arg-types nil)
  (return-type :void)
  (trampoline nil))  ; native code address

(defvar *rt-ffi-callbacks* (make-hash-table))
(defvar *rt-ffi-next-callback-id* 0)

(defun rt-make-callback (fn arg-types return-type)
  "Create a C-callable function pointer from a Lisp function."
  (let ((id (incf *rt-ffi-next-callback-id*)))
    #+(and sbcl sb-alien-callback) (sb-alien:alien-callback
                                    (function sb-alien:void) fn)
    #-(and sbcl sb-alien-callback) (let ((cb (make-rt-ffi-callback :id id :fn fn
                                           :arg-types arg-types
                                           :return-type return-type)))
             (setf (gethash id *rt-ffi-callbacks*) cb)
             cb)))

(defun rt-ffi-callback-invoke (cb-id &rest args)
  "Invoke a registered callback. Called from native code."
  (let ((cb (gethash cb-id *rt-ffi-callbacks*)))
    (when cb
      (apply (rt-ffi-callback-fn cb) args))))

;; ── Native struct mapping ──
(defstruct rt-ffi-struct-type
  (name "")
  (fields nil)   ; ((field-name type offset size) ...)
  (total-size 0)
  (alignment 8))

(defvar *rt-ffi-struct-types* (make-hash-table :test #'equal))

(defun rt-define-foreign-struct (name &rest fields)
  "Define a native struct layout with ABI-compliant alignment."
  (let* ((aligned-fields nil)
         (offset 0)
         (max-align 1))
    (dolist (f fields)
      (destructuring-bind (fname ftype) f
        (let* ((size (ecase ftype
                       (:int8 1) (:uint8 1)
                       (:int16 2) (:uint16 2)
                       (:int32 4) (:uint32 4) (:float32 4)
                       (:int64 8) (:uint64 8) (:float64 8)
                       (:pointer 8)))
               (align size))
          (setf max-align (max max-align align))
          (setf offset (ceiling offset align))
          (push (list fname ftype offset size) aligned-fields)
          (incf offset size))))
    (setf offset (ceiling offset max-align))
    (setf (gethash name *rt-ffi-struct-types*)
          (make-rt-ffi-struct-type :name name
                                   :fields (nreverse aligned-fields)
                                   :total-size offset
                                   :alignment max-align))
    name))

(defun rt-ffi-struct-size (name)
  (let ((st (gethash name *rt-ffi-struct-types*)))
    (or (rt-ffi-struct-type-total-size st) 0)))

(defun rt-ffi-struct-field-offset (name field-name)
  (let ((st (gethash name *rt-ffi-struct-types*)))
    (when st
      (dolist (f (rt-ffi-struct-type-fields st))
        (when (eq (first f) field-name)
          (return (third f)))))))

;; ── Inline assembly stubs ──
(defmacro rt-asm ((&rest instrs))
  "Inline assembly placeholder. On native backend, emits actual instructions."
  (declare (ignore instrs))
  #+sbcl '(error "Inline assembly not available on host CL")
  #-sbcl `(progn ,@(mapcar (lambda (i) `(list :asm ,@i)) instrs)))

;; ── Initialization ──
(defun rt-ffi-init ()
  (clrhash *rt-ffi-loaded-libs*)
  (clrhash *rt-ffi-registered-functions*)
  (clrhash *rt-ffi-callbacks*)
  (clrhash *rt-ffi-struct-types*)
  (setf *rt-ffi-next-callback-id* 0)
  t)

;;; ── C embedding API (FR-812) ───────────────────────────────────────────────

(defstruct cl-cc-error
  "Last embedding error visible through cl-cc-last-error."
  (code 0 :type integer)
  (message "" :type string))

(defstruct cl-cc-value
  "Host-side representation of a value returned through the embedding API."
  (kind :nil :type keyword)
  (payload nil))

(defstruct cl-cc-state
  "Embeddable VM state owned by a host process."
  (id 0 :type integer)
  (vm nil)
  (package nil)
  (callbacks (make-hash-table :test #'equal))
  (last-error (make-cl-cc-error))
  (closed-p nil :type boolean)
  (lock (sb-thread:make-mutex :name "cl-cc embedding state lock")))

(defvar *cl-cc-next-state-id* 0)

(defun %cl-cc-find-symbol (package-name symbol-name)
  "Find SYMBOL-NAME in PACKAGE-NAME and return NIL when unavailable."
  (let ((package (find-package package-name)))
    (and package (find-symbol symbol-name package))))

(defun %cl-cc-call-symbol (package-name symbol-name &rest args)
  "Call PACKAGE-NAME::SYMBOL-NAME when it is fbound, otherwise return NIL."
  (let ((symbol (%cl-cc-find-symbol package-name symbol-name)))
    (when (and symbol (fboundp symbol))
      (apply (symbol-function symbol) args))))

(defun %cl-cc-make-vm ()
  "Create a cl-cc VM instance when the VM package is loaded."
  (%cl-cc-call-symbol "CL-CC/VM" "MAKE-VM-INSTANCE"))

(defun %cl-cc-set-error (state code message &rest args)
  "Record an embedding error on STATE."
  (let ((error (make-cl-cc-error :code code
                                 :message (apply #'format nil message args))))
    (setf (cl-cc-state-last-error state) error)
    error))

(defun %cl-cc-clear-error (state)
  "Reset STATE's last-error object to success."
  (setf (cl-cc-state-last-error state) (make-cl-cc-error)))

(defun %cl-cc-wrap-value (value)
  "Wrap a host Lisp value as cl-cc-value."
  (make-cl-cc-value
   :kind (cond
           ((null value) :nil)
           ((integerp value) :integer)
           ((floatp value) :float)
           ((stringp value) :string)
           ((symbolp value) :symbol)
           ((functionp value) :function)
           (t :object))
   :payload value))

(defun %cl-cc-state-package-name (id)
  (format nil "CL-CC/EMBED-~D" id))

(defun %cl-cc-read-forms (code)
  "Read all forms from CODE without enabling reader eval."
  (let ((*read-eval* nil)
        (forms nil))
    (with-input-from-string (in code)
      (loop for form = (read in nil in)
            until (eq form in)
            do (push form forms)))
    (nreverse forms)))

(defun %cl-cc-host-eval (state code)
  "Evaluate CODE in STATE's isolated host package."
  (let ((*package* (cl-cc-state-package state)))
    (loop for form in (%cl-cc-read-forms code)
          for result = (eval form)
          finally (return result))))

(defun %cl-cc-vm-eval (state code)
  "Evaluate CODE through cl-cc/vm when compile hooks are installed."
  (let ((hook (%cl-cc-find-symbol "CL-CC/VM" "*VM-COMPILE-STRING-HOOK*"))
        (runner (%cl-cc-find-symbol "CL-CC/VM" "RUN-STRING-WITH-IO")))
    (if (and hook runner (boundp hook) (symbol-value hook) (fboundp runner))
        (funcall (symbol-function runner) code)
        (%cl-cc-host-eval state code))))

(defun cl-cc-init ()
  "Initialize and return an embeddable cl-cc state object."
  (let* ((id (incf *cl-cc-next-state-id*))
         (package (make-package (%cl-cc-state-package-name id) :use '(:cl)))
         (state (make-cl-cc-state :id id :vm (%cl-cc-make-vm) :package package)))
    state))

(defun cl-cc-eval (state code)
  "Evaluate CODE in STATE and return a cl-cc-value."
  (check-type state cl-cc-state)
  (check-type code string)
  (sb-thread:with-mutex ((cl-cc-state-lock state))
    (handler-case
        (progn
          (when (cl-cc-state-closed-p state)
            (error "Embedding state is closed"))
          (%cl-cc-clear-error state)
          (%cl-cc-wrap-value (%cl-cc-vm-eval state code)))
      (error (condition)
        (%cl-cc-set-error state 1 "~A" condition)
        (make-cl-cc-value :kind :error :payload condition)))))

(defun cl-cc-call (state function-name &rest args)
  "Call FUNCTION-NAME in STATE with ARGS and return a cl-cc-value."
  (check-type state cl-cc-state)
  (check-type function-name string)
  (sb-thread:with-mutex ((cl-cc-state-lock state))
    (handler-case
        (progn
          (when (cl-cc-state-closed-p state)
            (error "Embedding state is closed"))
          (%cl-cc-clear-error state)
          (let* ((*package* (cl-cc-state-package state))
                 (symbol (or (find-symbol (string-upcase function-name) *package*)
                             (find-symbol (string-upcase function-name) :cl))))
            (unless (and symbol (fboundp symbol))
              (error "Unknown embedded function: ~A" function-name))
            (%cl-cc-wrap-value (apply (symbol-function symbol) args))))
      (error (condition)
        (%cl-cc-set-error state 2 "~A" condition)
        (make-cl-cc-value :kind :error :payload condition)))))

(defun cl-cc-register-callback (state name function &key (arg-types nil) (return-type :void))
  "Register FUNCTION as a C-callable callback under NAME for STATE."
  (check-type state cl-cc-state)
  (check-type name string)
  (check-type function function)
  (sb-thread:with-mutex ((cl-cc-state-lock state))
    (let ((callback (rt-make-callback function arg-types return-type)))
      (setf (gethash name (cl-cc-state-callbacks state)) callback)
      callback)))

(defun cl-cc-callback (state name)
  "Return a registered C callback pointer/token by NAME."
  (check-type state cl-cc-state)
  (check-type name string)
  (gethash name (cl-cc-state-callbacks state)))

(defun cl-cc-cleanup (state)
  "Release resources associated with STATE."
  (check-type state cl-cc-state)
  (sb-thread:with-mutex ((cl-cc-state-lock state))
    (clrhash (cl-cc-state-callbacks state))
    (setf (cl-cc-state-vm state) nil
          (cl-cc-state-closed-p state) t)
    t))

(defun cl-cc-last-error (state)
  "Return STATE's last embedding error."
  (check-type state cl-cc-state)
  (cl-cc-state-last-error state))

;;; C ABI spelling aliases for generated/exported shared-library entry points.
(setf (symbol-function '|cl_cc_init|) #'cl-cc-init)
(setf (symbol-function '|cl_cc_eval|) #'cl-cc-eval)
(setf (symbol-function '|cl_cc_call|) #'cl-cc-call)
(setf (symbol-function '|cl_cc_cleanup|) #'cl-cc-cleanup)
(setf (symbol-function '|cl_cc_last_error|) #'cl-cc-last-error)
(setf (symbol-function '|cl_cc_register_callback|) #'cl-cc-register-callback)
(setf (symbol-function '|cl_cc_get_callback|) #'cl-cc-callback)

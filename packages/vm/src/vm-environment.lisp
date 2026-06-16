;;;; vm-environment.lisp — VM Runtime Environment Access Instructions
;;;;
;;;; Contains: vm-boundp, vm-fboundp, vm-makunbound, vm-fdefinition
;;;; (FR-1202), vm-random, vm-make-random-state (FR-1205),
;;;; encode-universal-time, %define-nullary-env-query macro (FR-507 hook).
;;;;
;;;; Float operations (vm-float-inst, vm-decode-float, vm-round-inst) are in
;;;; vm-numeric.lisp (loads before this).
;;;;
;;;; Load order: after vm-numeric.lisp, before vm-numeric-ext.lisp.

(in-package :cl-cc/vm)

(export '(lisp-implementation-type
          lisp-implementation-version
          machine-type
          machine-version
          machine-instance
          software-type
          software-version
          room
          apropos
          apropos-list
          vm-boundp
          vm-fboundp
          vm-makunbound
          vm-fdefinition
          vm-random
          vm-make-random-state
          vm-get-universal-time
          vm-get-internal-real-time
          vm-get-internal-run-time
          vm-decode-universal-time))

;;; Phase 3+: FR-1202 Environment Predicates and FR-1205 Random

;; FR-1202: boundp — test if a symbol has a global variable binding
(define-vm-instruction vm-boundp (vm-instruction)
  "Test if symbol has a global variable value. Returns T/nil."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :boundp)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-boundp) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-src inst)))
         (result (nth-value 1 (gethash sym (vm-global-vars state)))))
    (vm-reg-set state (vm-dst inst) (if result t nil))
    (values (1+ pc) nil nil)))

;; FR-1202: fboundp — test if a symbol names a function
(define-vm-instruction vm-fboundp (vm-instruction)
  "Test if symbol names a function in the VM function table. Returns T/nil."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :fboundp)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-fboundp) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-src inst)))
         (result (or (gethash sym (vm-function-registry state))
                     (vm-bridge-callable sym))))
    (vm-reg-set state (vm-dst inst) (if result t nil))
    (values (1+ pc) nil nil)))

;; FR-1202: makunbound — remove a global variable binding
(define-vm-instruction vm-makunbound (vm-instruction)
  "Remove global variable binding for SYM. Returns SYM."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :makunbound)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-makunbound) state pc labels)
  (declare (ignore labels))
  (let ((sym (vm-reg-get state (vm-src inst))))
    (remhash sym (vm-global-vars state))
    (vm-reg-set state (vm-dst inst) sym)
    (values (1+ pc) nil nil)))

;; FR-1202: fdefinition — retrieve a function object by symbol
(define-vm-instruction vm-fdefinition (vm-instruction)
  "Return the function named by SYM from the VM function registry.
Signals a VM-level error (catchable by handler-case/ignore-errors) if undefined."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :fdefinition)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-fdefinition) state pc labels)
  (let* ((sym (vm-reg-get state (vm-src inst)))
         (fn  (gethash sym (vm-function-registry state))))
    (if fn
        (progn (vm-reg-set state (vm-dst inst) fn)
               (values (1+ pc) nil nil))
        (let ((error-value (format nil "The function ~S is undefined." sym))
              (matching-handler nil)
              (handlers-to-skip 0))
          (dolist (entry (vm-handler-stack state))
            (if (vm-error-type-matches-p error-value (third entry))
                (progn (setf matching-handler entry) (return))
                (incf handlers-to-skip)))
          (if matching-handler
              (progn
                (dotimes (i (1+ handlers-to-skip)) (pop (vm-handler-stack state)))
                (destructuring-bind (handler-label result-reg _type saved-call-stack saved-regs
                                     &optional saved-method-call-stack)
                    matching-handler
                  (declare (ignore _type))
                  (%vm-unwind-to-handler state labels handler-label result-reg
                                         saved-call-stack saved-regs saved-method-call-stack
                                         error-value)))
              (error "Unhandled VM error: fdefinition: ~S is undefined" sym))))))

;; FR-1205: random — generate a random number
(define-vm-instruction vm-random (vm-instruction)
  "Generate a random number in [0, LIMIT)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :random)
  (:sexp-slots dst src))

(define-simple-instruction vm-random :unary random)

;; FR-1205: make-random-state
(define-vm-instruction vm-make-random-state (vm-instruction)
  "Create a random state. If SRC is nil, copy current *random-state*; if t, generate fresh."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :make-random-state) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-make-random-state) state pc labels)
  (declare (ignore labels))
  (let ((arg (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (cond ((eq arg t)  (make-random-state t))
                      ((eq arg nil) (make-random-state))
                      (t (make-random-state arg))))
    (values (1+ pc) nil nil)))

;; FR-1204: get-universal-time — seconds since 1900-01-01
(define-vm-instruction vm-get-universal-time (vm-instruction)
  "Return current time as universal time integer."
  (dst nil :reader vm-dst)
  (:sexp-tag :get-universal-time)
  (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-get-universal-time) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (get-universal-time))
  (values (1+ pc) nil nil))

;; FR-1204: get-internal-real-time
(define-vm-instruction vm-get-internal-real-time (vm-instruction)
  "Return internal real time as integer."
  (dst nil :reader vm-dst)
  (:sexp-tag :get-internal-real-time)
  (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-get-internal-real-time) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (get-internal-real-time))
  (values (1+ pc) nil nil))

;; FR-1204: get-internal-run-time
(define-vm-instruction vm-get-internal-run-time (vm-instruction)
  "Return internal run time (CPU time) as integer."
  (dst nil :reader vm-dst)
  (:sexp-tag :get-internal-run-time)
  (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-get-internal-run-time) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (get-internal-run-time))
  (values (1+ pc) nil nil))

;; FR-1204: decode-universal-time — returns 9 values via vm-values-list
(define-vm-instruction vm-decode-universal-time (vm-instruction)
  "Decode universal time into (sec min hour day month year day-of-week dst-p tz)."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :decode-universal-time) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-decode-universal-time) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (sec min hour date month year day dst-p zone)
      (decode-universal-time (vm-reg-get state (vm-src inst)))
    (setf (vm-values-list state) (list sec min hour date month year day dst-p zone))
    (vm-reg-set state (vm-dst inst) sec)
    (values (1+ pc) nil nil)))

;; FR-1204: encode-universal-time — takes 6 required + optional tz in a list
(define-vm-instruction vm-encode-universal-time (vm-instruction)
  "Encode (sec min hour date month year &optional zone) as universal time."
  (dst nil :reader vm-dst) (args-reg nil :reader vm-args-reg)
  (:sexp-tag :encode-universal-time) (:sexp-slots dst args-reg))

(defmethod execute-instruction ((inst vm-encode-universal-time) state pc labels)
  (declare (ignore labels))
  (let* ((args (vm-reg-get state (vm-args-reg inst)))
         (sec   (nth 0 args))
         (min   (nth 1 args))
         (hour  (nth 2 args))
         (date  (nth 3 args))
         (month (nth 4 args))
         (year  (nth 5 args))
         (zone  (nth 6 args))
         (result (if zone
                     (encode-universal-time sec min hour date month year zone)
                     (encode-universal-time sec min hour date month year))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; FR-612: Environment introspection API

(defparameter *cl-cc-implementation-version* "0.1.0"
  "Version string reported by LISP-IMPLEMENTATION-VERSION.")

(defun lisp-implementation-type ()
  "Return the CL-CC implementation type string."
  "cl-cc")

(defun lisp-implementation-version ()
  "Return the CL-CC implementation version string."
  *cl-cc-implementation-version*)

(defun machine-type ()
  "Return the host machine type used by this VM instance."
  (or (cl:machine-type) "unknown"))

(defun machine-version ()
  "Return the host machine version used by this VM instance."
  (or (cl:machine-version) "unknown"))

(defun machine-instance ()
  "Return the host machine instance name used by this VM instance."
  (or (cl:machine-instance) "unknown"))

(defun software-type ()
  "Return the host operating system type."
  (or (cl:software-type) "unknown"))

(defun software-version ()
  "Return the host operating system version."
  (or (cl:software-version) "unknown"))

(defun %room-state-summary (state stream)
  (let* ((heap (and state (ignore-errors (vm-state-heap state))))
         (globals (and state (ignore-errors (vm-global-vars state))))
         (functions (and state (ignore-errors (vm-function-registry state))))
         (classes (and state (ignore-errors (vm-class-registry state)))))
    (format stream "; VM heap objects: ~D~%" (if heap (hash-table-count heap) 0))
    (format stream "; VM globals: ~D~%" (if globals (hash-table-count globals) 0))
    (format stream "; VM functions: ~D~%" (if functions (hash-table-count functions) 0))
    (format stream "; VM classes: ~D~%" (if classes (hash-table-count classes) 0))))

(defun room (&optional x)
  "Print a compact CL-CC VM room report.
When X is a VM state, include heap/global/function/class table usage.  For
compatibility with older CL-CC code, a stream argument is also accepted."
  (let ((stream (if (streamp x) x *standard-output*))
        (state (and (not (streamp x)) (typep x 'vm-state) x)))
    (format stream "; ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
    (format stream "; Host: ~A ~A on ~A (~A)~%"
            (software-type) (software-version) (machine-type) (machine-instance))
    (%room-state-summary (or state *vm-current-state*) stream)
    (values)))

(defun %apropos-package-list (package package-supplied-p)
  (if package-supplied-p
      (let ((pkg (find-package package)))
        (if pkg
            (list pkg)
            (error "No such package: ~S" package)))
      (list-all-packages)))

(defun apropos-list (string-designator &optional (package nil package-supplied-p))
  "Return symbols whose names contain STRING-DESIGNATOR, case-insensitively."
  (let ((needle (string string-designator))
        (result nil))
    (dolist (pkg (%apropos-package-list package package-supplied-p)
             (sort result #'string< :key #'symbol-name))
      (do-symbols (sym pkg)
        (when (search needle (symbol-name sym) :test #'char-equal)
          (pushnew sym result :test #'eq))))))

(defun apropos (string-designator &optional package)
  "Print symbols matching STRING-DESIGNATOR and return no values."
  (dolist (sym (if package
                   (apropos-list string-designator package)
                   (apropos-list string-designator)))
    (format t "~A~%" sym))
  (values))

;;; FR-507: Environment query functions (nullary — return host CL values)
(defmacro %define-nullary-env-query (name tag cl-form doc)
  `(progn
     (define-vm-instruction ,name (vm-instruction)
       ,doc
       (dst nil :reader vm-dst)
       (:sexp-tag ,tag)
       (:sexp-slots dst))
     (defmethod execute-instruction ((inst ,name) state pc labels)
       (declare (ignore labels))
       (vm-reg-set state (vm-dst inst) ,cl-form)
       (values (1+ pc) nil nil))))

;; FR-301 float rounding (ffloor/fceiling/ftruncate/fround),
;; FR-306 rational, FR-307 complex, FR-507 env query instances
;; are in vm-numeric-ext.lisp (loaded next).

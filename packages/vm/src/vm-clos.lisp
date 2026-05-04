(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — CLOS Instruction Definitions and Execution
;;;
;;; Contains: vm-class-def, vm-make-obj, vm-slot-{read,write},
;;; vm-register-method, vm-generic-call, slot predicate instructions,
;;; MRO helpers (collect-inherited-slots, compute-class-precedence-list),
;;; and all corresponding execute-instruction methods.
;;;
;;; Load order: after vm.lisp, before vm-run.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── CLOS VM instruction defstructs ───────────────────────────────────────

(define-vm-instruction vm-class-def (vm-instruction)
  "Define a class. Creates a class descriptor hash table."
  (dst nil :reader vm-dst)
  (class-name nil :reader vm-class-name-sym)
  (superclasses nil :reader vm-superclasses)
  (slot-names nil :reader vm-slot-names)
  (slot-initargs nil :reader vm-slot-initargs)
  (slot-initform-regs nil :reader vm-slot-initform-regs)
  (slot-types nil :reader vm-slot-types)
  (default-initarg-regs nil :reader vm-default-initarg-regs)
  (class-slots nil :reader vm-class-slots)
  (metaclass-reg nil :reader vm-metaclass-reg)
  (:sexp-tag :class-def))

(define-vm-instruction vm-make-obj (vm-instruction)
  "Create an instance of a class."
  (dst nil :reader vm-dst)
  (class-reg nil :reader vm-class-reg)
  (initarg-regs nil :reader vm-initarg-regs)
  (:sexp-tag :make-obj))

(define-vm-instruction vm-slot-read (vm-instruction)
  "Read a slot value from an object."
  (dst nil :reader vm-dst)
  (obj-reg nil :reader vm-obj-reg)
  (slot-name nil :reader vm-slot-name-sym)
  (:sexp-tag :slot-read))

(define-vm-instruction vm-slot-write (vm-instruction)
  "Write a value to an object slot."
  (obj-reg nil :reader vm-obj-reg)
  (slot-name nil :reader vm-slot-name-sym)
  (value-reg nil :reader vm-value-reg)
  (:sexp-tag :slot-write))

(define-vm-instruction vm-register-method (vm-instruction)
  "Register a method closure on a generic function dispatch table."
  (gf-reg nil :reader vm-gf-reg)
  (specializer nil :reader vm-method-specializer)
  (qualifier nil :reader vm-method-qualifier)
  (method-reg nil :reader vm-method-reg)
  (:sexp-tag :register-method))

(define-vm-instruction vm-generic-call (vm-instruction)
  "Dispatch and call a generic function method."
  (dst nil :reader vm-dst)
  (gf-reg nil :reader vm-gf-reg)
  (args nil :reader vm-args)
  (:sexp-tag :generic-call))

;;; ── Slot predicate instructions ──────────────────────────────────────────

(define-vm-instruction vm-slot-boundp (vm-instruction)
  "Test if slot SLOT-NAME is bound in object OBJ."
  (dst nil :reader vm-dst)
  (obj-reg nil :reader vm-obj-reg)
  (slot-name-sym nil :reader vm-slot-name-sym)
  (:sexp-tag :slot-boundp)
  (:sexp-slots dst obj-reg slot-name-sym))

(define-vm-instruction vm-slot-makunbound (vm-instruction)
  "Remove slot SLOT-NAME from object OBJ. Returns OBJ."
  (dst nil :reader vm-dst)
  (obj-reg nil :reader vm-obj-reg)
  (slot-name-sym nil :reader vm-slot-name-sym)
  (:sexp-tag :slot-makunbound)
  (:sexp-slots dst obj-reg slot-name-sym))

(define-vm-instruction vm-slot-exists-p (vm-instruction)
  "Test if class of OBJ has a slot named SLOT-NAME."
  (dst nil :reader vm-dst)
  (obj-reg nil :reader vm-obj-reg)
  (slot-name-sym nil :reader vm-slot-name-sym)
  (:sexp-tag :slot-exists-p)
  (:sexp-slots dst obj-reg slot-name-sym))

;;; ── MRO and inheritance helpers ──────────────────────────────────────────

(defun %cis-walk (class-names registry seen result-cell)
  "Post-order DFS: accumulate unique slot names into (car RESULT-CELL), skipping SEEN."
  (dolist (cname class-names)
    (let ((super-ht (gethash cname registry)))
      (when super-ht
        (%cis-walk (gethash :__superclasses__ super-ht) registry seen result-cell)
        (dolist (slot (gethash :__slots__ super-ht))
          (unless (gethash slot seen)
            (setf (gethash slot seen) t)
            (push slot (car result-cell))))))))

(defun collect-inherited-slots (superclasses registry)
  "Collect slot names from superclasses in MRO order (depth-first, left-to-right).
Returns a list of slot names without duplicates, preserving order."
  (let ((seen        (make-hash-table :test #'eq))
        (result-cell (list nil)))
    (%cis-walk superclasses registry seen result-cell)
    (nreverse (car result-cell))))

(defun %cia-walk (class-names registry result-cell)
  "Post-order DFS: accumulate unique initarg entries into (car RESULT-CELL)."
  (dolist (cname class-names)
    (let ((super-ht (gethash cname registry)))
      (when super-ht
        (%cia-walk (gethash :__superclasses__ super-ht) registry result-cell)
        (dolist (entry (gethash :__initargs__ super-ht))
          (unless (assoc (car entry) (car result-cell))
            (push entry (car result-cell))))))))

(defun collect-inherited-initargs (superclasses registry)
  "Collect initarg mappings from superclasses. Later entries override earlier ones."
  (let ((result-cell (list nil)))
    (%cia-walk superclasses registry result-cell)
    (nreverse (car result-cell))))

(defun %vm-allow-other-keys-p (initarg-regs state)
  "Return true when INITARG-REGS explicitly contains :ALLOW-OTHER-KEYS with a truthy value."
  (let ((entry (assoc :allow-other-keys initarg-regs)))
    (and entry (vm-reg-get state (cdr entry)) t)))

(defun %vm-validate-initargs (initarg-regs initarg-map state)
  "Signal an error for unknown initargs unless :ALLOW-OTHER-KEYS is true.
INITARG-REGS is an alist of keyword to value register. INITARG-MAP maps accepted
keywords to slot names."
  (unless (%vm-allow-other-keys-p initarg-regs state)
    (dolist (entry initarg-regs)
      (let ((key (car entry)))
        (unless (or (eq key :allow-other-keys)
                    (assoc key initarg-map))
          (error "Invalid initarg ~S" key))))))

(defun %c3-merge (linearizations)
  "Merge a list of linearizations using C3 linearization.
A 'good head' is a class that appears only in the head (first position)
of all linearizations where it occurs — never in any tail."
  (let ((result nil))
    (loop
      ;; Remove empty lists
      (setf linearizations (remove nil linearizations))
      (when (null linearizations)
        (return (nreverse result)))
      ;; Find a good head: first element of some list that does not appear
      ;; in the tail (cdr) of any list
      (let ((good-head nil))
        (dolist (lin linearizations)
          (let ((candidate (first lin)))
            (when (notany (lambda (other)
                            (member candidate (rest other) :test #'eq))
                          linearizations)
              (setf good-head candidate)
              (return))))
        (unless good-head
          (error "C3 linearization: inconsistent class precedence for ~{~S~^, ~}"
                 (mapcar #'first linearizations)))
        (push good-head result)
        ;; Remove good-head from all linearizations
        (setf linearizations
              (mapcar (lambda (lin)
                        (if (eq (first lin) good-head)
                            (rest lin)
                            lin))
                      linearizations))))))

(defun %cpl-linearize (name registry)
  "C3 linearization for NAME: list NAME then merge linearizations of its supers.
Algorithm: L(C) = C + merge(L(C1), L(C2), ..., (C1, C2, ...))"
  (let ((class-ht (gethash name registry)))
    (if (null class-ht)
        (list name)
        (let ((supers (gethash :__superclasses__ class-ht)))
          (if (null supers)
              (list name)
              (cons name
                    (%c3-merge
                     (append (mapcar (lambda (s) (%cpl-linearize s registry)) supers)
                             (list (copy-list supers))))))))))

(defun compute-class-precedence-list (class-name registry)
  "Compute the C3 class precedence list for CLASS-NAME using REGISTRY."
  (%cpl-linearize class-name registry))

;;; ── CLOS instruction execution ───────────────────────────────────────────

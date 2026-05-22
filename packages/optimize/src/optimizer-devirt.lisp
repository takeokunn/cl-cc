(in-package :cl-cc/optimize)

;;; FR-503 — Whole-program CLOS devirtualization.
;;;
;;; The older opt-pass-devirtualize already handles local direct-call discovery
;;; and sealed+satiated generic calls.  This file extends it with a small
;;; whole-program Class Hierarchy Analysis (CHA) used after LTO has merged all
;;; module IR.  It refuses open-world class hierarchies and only rewrites when a
;;; sealed concrete receiver has a single primary method target.

(defparameter *opt-legacy-devirtualize-function*
  (and (fboundp 'opt-pass-devirtualize) (symbol-function 'opt-pass-devirtualize))
  "The pre-FR-503 devirtualization pass captured before this file overrides it.")

(defstruct opt-cha
  classes
  subclasses
  sealed-classes
  generic-methods)

(defun opt-build-class-hierarchy-analysis (instructions)
  "Build whole-program CHA facts from merged VM INSTRUCTIONS."
  (let ((classes (make-hash-table :test #'eq))
        (subclasses (make-hash-table :test #'eq))
        (sealed (make-hash-table :test #'eq))
        (reg-symbol (make-hash-table :test #'eq))
        (reg-label (make-hash-table :test #'eq))
        (gf-methods (make-hash-table :test #'eq)))
    (dolist (inst instructions)
      (typecase inst
        (vm-class-def
         (let ((name (vm-class-name-sym inst)))
           (setf (gethash name classes) inst)
           (when (cl-cc/vm::vm-sealed-p inst) (setf (gethash name sealed) t))
           (dolist (super (vm-superclasses inst))
             (pushnew name (gethash super subclasses) :test #'eq))))
        (vm-const
         (if (symbolp (vm-value inst))
             (setf (gethash (vm-dst inst) reg-symbol) (vm-value inst))
             (remhash (vm-dst inst) reg-symbol)))
        ((or vm-closure vm-func-ref)
         (setf (gethash (vm-dst inst) reg-label) (vm-label-name inst)))
        (vm-move
         (multiple-value-bind (sym found-p) (gethash (vm-src inst) reg-symbol)
           (if found-p (setf (gethash (vm-dst inst) reg-symbol) sym) (remhash (vm-dst inst) reg-symbol)))
         (multiple-value-bind (label found-p) (gethash (vm-src inst) reg-label)
           (if found-p (setf (gethash (vm-dst inst) reg-label) label) (remhash (vm-dst inst) reg-label))))
        (vm-register-method
         (let ((gf (gethash (vm-gf-reg inst) reg-symbol))
               (label (gethash (vm-method-reg inst) reg-label)))
           (when (and gf label (null (vm-method-qualifier inst)))
             (push (list :specializer (vm-method-specializer inst) :label label)
                   (gethash gf gf-methods)))))))
    (make-opt-cha :classes classes :subclasses subclasses :sealed-classes sealed :generic-methods gf-methods)))

(defun %opt-cha-closed-concrete-class-p (cha class-name)
  "T when CLASS-NAME is sealed and has no visible subclasses in the merged unit."
  (and (gethash class-name (opt-cha-sealed-classes cha))
       (null (gethash class-name (opt-cha-subclasses cha)))))

(defun opt-cha-single-target-method-label (cha gf-name class-name)
  "Return the unique direct method label for GF-NAME/CLASS-NAME, or NIL."
  (when (%opt-cha-closed-concrete-class-p cha class-name)
    (let ((matches (remove-if-not (lambda (method)
                                    (eq (getf method :specializer) class-name))
                                  (gethash gf-name (opt-cha-generic-methods cha)))))
      (when (= (length matches) 1)
        (getf (first matches) :label)))))

(defun %opt-cha-track-call-facts (inst reg-symbol reg-object-class reg-gf-name)
  (typecase inst
    (vm-const
     (when (symbolp (vm-value inst))
       (setf (gethash (vm-dst inst) reg-symbol) (vm-value inst))))
    (vm-class-def
     (setf (gethash (vm-dst inst) reg-symbol) (vm-class-name-sym inst)))
    (vm-make-obj
     (let ((class (gethash (vm-class-reg inst) reg-symbol)))
       (if class
           (setf (gethash (vm-dst inst) reg-object-class) class)
           (remhash (vm-dst inst) reg-object-class))))
    (vm-move
     (multiple-value-bind (sym found-p) (gethash (vm-src inst) reg-symbol)
       (if found-p (setf (gethash (vm-dst inst) reg-symbol) sym) (remhash (vm-dst inst) reg-symbol)))
     (multiple-value-bind (class found-p) (gethash (vm-src inst) reg-object-class)
       (if found-p (setf (gethash (vm-dst inst) reg-object-class) class) (remhash (vm-dst inst) reg-object-class)))
     (multiple-value-bind (gf found-p) (gethash (vm-src inst) reg-gf-name)
       (if found-p (setf (gethash (vm-dst inst) reg-gf-name) gf) (remhash (vm-dst inst) reg-gf-name))))
    (t
     (let ((dst (opt-inst-dst inst)))
       (when dst
         (remhash dst reg-symbol)
         (remhash dst reg-object-class)
         (remhash dst reg-gf-name))))))

(defun %opt-cha-note-gf-definition (inst reg-symbol reg-gf-name)
  (when (and (typep inst 'vm-register-function)
             (gethash (cl-cc/vm::vm-src inst) reg-symbol))
    (setf (gethash (cl-cc/vm::vm-src inst) reg-gf-name)
          (cl-cc/vm::vm-func-name inst))))

(defun %opt-cha-pass (instructions)
  (let ((cha (opt-build-class-hierarchy-analysis instructions))
        (reg-symbol (make-hash-table :test #'eq))
        (reg-object-class (make-hash-table :test #'eq))
        (reg-gf-name (make-hash-table :test #'eq))
        (fresh (%opt-fresh-register-generator instructions))
        (result nil))
    (dolist (inst instructions (nreverse result))
      (if (typep inst 'vm-generic-call)
          (let* ((gf-name (gethash (vm-gf-reg inst) reg-gf-name))
                 (receiver (first (vm-args inst)))
                 (class (and receiver (gethash receiver reg-object-class)))
                 (label (and gf-name class (opt-cha-single-target-method-label cha gf-name class))))
            (if label
                (let ((fn (funcall fresh)))
                  (push (make-vm-func-ref :dst fn :label label) result)
                  (push (make-vm-call :dst (vm-dst inst) :func fn :args (copy-list (vm-args inst))) result))
                (push inst result)))
          (progn
            (%opt-cha-track-call-facts inst reg-symbol reg-object-class reg-gf-name)
            (%opt-cha-note-gf-definition inst reg-symbol reg-gf-name)
            (push inst result))))))

(defun opt-pass-devirtualize (instructions)
  "Run whole-program CHA devirtualization, then the legacy local devirt pass."
  (let ((after-cha (%opt-cha-pass instructions)))
    (if *opt-legacy-devirtualize-function*
        (funcall *opt-legacy-devirtualize-function* after-cha)
        after-cha)))

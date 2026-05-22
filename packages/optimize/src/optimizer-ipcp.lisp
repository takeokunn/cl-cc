(in-package :cl-cc/optimize)

;;; FR-502 — Interprocedural Constant Propagation (IPCP)
;;;
;;; This pass is intentionally conservative and VM-level portable.  It creates a
;;; cloned function body when a direct call target and at least one constant
;;; argument are known in the same optimization unit (notably after LTO module
;;; merge).  Constants are materialized at the clone entry, which makes ordinary
;;; fold/SCCP/DCE passes eliminate now-constant branches and dead paths.

(defun %ipcp-safe-token-string (object)
  (let ((s (write-to-string object :escape t :readably nil)))
    (with-output-to-string (out)
      (loop for ch across s
            do (write-char (if (alphanumericp ch) ch #\-) out)))))

(defun %ipcp-specialized-label (label constants)
  (format nil "~A-ipcp-~X" label (sxhash (list label constants))))

(defun %ipcp-track-facts (inst reg-const reg-label)
  (typecase inst
    (vm-const
     (setf (gethash (vm-dst inst) reg-const) (vm-value inst))
     (remhash (vm-dst inst) reg-label))
    ((or vm-closure vm-func-ref)
     (setf (gethash (vm-dst inst) reg-label) (vm-label-name inst))
     (remhash (vm-dst inst) reg-const))
    (vm-move
     (multiple-value-bind (value found-p) (gethash (vm-src inst) reg-const)
       (if found-p (setf (gethash (vm-dst inst) reg-const) value) (remhash (vm-dst inst) reg-const)))
     (multiple-value-bind (label found-p) (gethash (vm-src inst) reg-label)
       (if found-p (setf (gethash (vm-dst inst) reg-label) label) (remhash (vm-dst inst) reg-label))))
    (t
     (let ((dst (opt-inst-dst inst)))
       (when dst
         (remhash dst reg-const)
         (remhash dst reg-label))))))

(defun %ipcp-constant-arg-map (params args reg-const)
  (let ((result nil))
    (loop for param in params
          for arg in args
          do (multiple-value-bind (value found-p) (gethash arg reg-const)
               (when found-p (push (cons param value) result))))
    (nreverse result)))

(defun %ipcp-fold-entry-branch (inst constants)
  (if (typep inst 'vm-jump-zero)
      (let ((entry (assoc (vm-reg inst) constants :test #'eq)))
        (if entry
            (if (opt-falsep (cdr entry))
                (make-vm-jump :label (vm-label-name inst))
                nil)
            inst))
      inst))

(defun %ipcp-clone-body (label body constants)
  (let ((constant-insts
          (loop for (reg . value) in constants
                collect (make-vm-const :dst reg :value value)))
        (rewritten
          (loop for inst in body
                for folded = (%ipcp-fold-entry-branch inst constants)
                when folded collect folded)))
    (append (list (make-vm-label :name label)) constant-insts rewritten)))

(defun %ipcp-call-p (inst)
  (typep inst '(or vm-call vm-tail-call)))

(defun %ipcp-copy-call-with-func (inst func-reg)
  (typecase inst
    (vm-tail-call (make-vm-tail-call :dst (vm-dst inst) :func func-reg :args (copy-list (vm-args inst))))
    (t (make-vm-call :dst (vm-dst inst) :func func-reg :args (copy-list (vm-args inst))))))

(defun opt-pass-ipcp (instructions)
  "Create constant-specialized function versions for direct call sites.

Specialized labels use the required ORIGINAL-FUNCTION-IPCP-{hash} spelling.  The
pass is closed-world/LTO friendly but remains safe outside LTO: if no direct
target or constant argument is known, it leaves the stream unchanged."
  (let ((defs (opt-collect-function-defs instructions))
        (reg-const (make-hash-table :test #'eq))
        (reg-label (make-hash-table :test #'eq))
        (fresh-reg (%opt-fresh-register-generator instructions))
        (clone-cache (make-hash-table :test #'equal))
        (clones nil)
        (result nil))
    (dolist (inst instructions)
      (if (%ipcp-call-p inst)
          (let* ((label (gethash (vm-func-reg inst) reg-label))
                 (def (and label (gethash label defs)))
                 (params (getf def :params))
                 (body (getf def :body))
                 (constants (and def (%ipcp-constant-arg-map params (vm-args inst) reg-const))))
            (if (and label body constants)
                (let* ((key (list label constants))
                       (special-label (or (gethash key clone-cache)
                                          (setf (gethash key clone-cache)
                                                (%ipcp-specialized-label label constants)))))
                  (unless (member special-label clones :key (lambda (entry) (first entry)) :test #'string=)
                    (push (list special-label body constants) clones))
                  (let ((fresh (funcall fresh-reg)))
                    (push (make-vm-func-ref :dst fresh :label special-label :params params) result)
                    (push (%ipcp-copy-call-with-func inst fresh) result)))
                (push inst result))
            (%ipcp-track-facts inst reg-const reg-label))
          (progn
            (%ipcp-track-facts inst reg-const reg-label)
            (push inst result))))
    (append (nreverse result)
            (loop for (label body constants) in (nreverse clones)
                  append (%ipcp-clone-body label body constants)))))

;;;; optimizer-polyhedral.lisp — FR-513 lightweight polyhedral transforms

(in-package :cl-cc/optimize)

(defvar *polyhedral-enabled* nil
  "When non-NIL, enable explicit polyhedral loop transforms.")

(defparameter *polyhedral-default-tile-size* 32
  "Default tile edge used by POLYHEDRAL-TILE for two-dimensional schedules.")

(defstruct (polyhedral-domain (:conc-name poly-domain-))
  dimensions
  constraints)

(defstruct (polyhedral-access (:conc-name poly-access-))
  array-reg
  write-p
  coefficients
  offset)

(defstruct (polyhedral-statement (:conc-name poly-stmt-))
  loops
  domain
  accesses
  schedule
  body)

(defun polyhedral-build-domain (loops constraints)
  "Build a lightweight polyhedral domain descriptor."
  (make-polyhedral-domain :dimensions loops :constraints constraints))

(defun %poly-swap-first-two (list)
  (if (and (consp list) (consp (cdr list)))
      (list* (second list) (first list) (cddr list))
      list))

(defun %poly-swap-coefficients (coefficients)
  (if (and (consp coefficients) (consp (cdr coefficients)))
      (list* (second coefficients) (first coefficients) (cddr coefficients))
      coefficients))

(defun %poly-access-locality-score (access)
  "Return a cache-locality score for keeping the second loop innermost."
  (let ((coeffs (poly-access-coefficients access)))
    (if (and (consp coeffs) (consp (cdr coeffs)))
        (- (abs (second coeffs)) (abs (first coeffs)))
        0)))

(defun %poly-interchange-beneficial-p (statement)
  "Return T when swapping a two-loop schedule improves unit-stride locality."
  (and (polyhedral-statement-p statement)
       (>= (length (poly-stmt-loops statement)) 2)
       (plusp (reduce #'+ (poly-stmt-accesses statement)
                      :key #'%poly-access-locality-score
                      :initial-value 0))))

(defun %poly-interchange-statement (statement)
  (if (not (%poly-interchange-beneficial-p statement))
      statement
      (let* ((domain (poly-stmt-domain statement))
             (new-domain (and domain
                              (make-polyhedral-domain
                               :dimensions (%poly-swap-first-two (poly-domain-dimensions domain))
                               :constraints (poly-domain-constraints domain))))
             (new-accesses (mapcar (lambda (access)
                                     (make-polyhedral-access
                                      :array-reg (poly-access-array-reg access)
                                      :write-p (poly-access-write-p access)
                                      :coefficients (%poly-swap-coefficients
                                                     (poly-access-coefficients access))
                                      :offset (poly-access-offset access)))
                                   (poly-stmt-accesses statement))))
        (make-polyhedral-statement
         :loops (%poly-swap-first-two (poly-stmt-loops statement))
         :domain new-domain
         :accesses new-accesses
         :schedule (list :interchanged t
                         :previous (poly-stmt-schedule statement))
         :body (poly-stmt-body statement)))))

(defun %poly-loop-init-index (vec lp)
  (let ((idx (1- (opt-loop-head-index lp))))
    (and (>= idx 0)
         (let ((inst (aref vec idx)))
           (and (eq (opt-inst-dst inst) (opt-loop-iv-reg lp)) idx)))))

(defun %poly-nested-loop-at (vec outer)
  "Return a perfect inner loop and init index for OUTER, or NIL."
  (loop for i from (+ (opt-loop-head-index outer) 3) below (opt-loop-back-index outer)
        for inner = (%loop-fr514-parse-canonical-loop-at vec i)
        when (and inner
                  (= (or (%poly-loop-init-index vec inner) -1) (1- i))
                  (< (opt-loop-exit-index inner) (opt-loop-back-index outer)))
          do (return inner)))

(defun %poly-unsafe-side-effect-p (inst)
  (typep inst '(or vm-call vm-tail-call vm-trampoline vm-generic-call vm-apply
              vm-set-global vm-slot-write vm-aset vm-ret vm-halt vm-jump vm-jump-zero)))

(defun %poly-affine-read-accesses (instructions inner outer)
  (multiple-value-bind (const-env def-env)
      (%loop-fr514-build-envs instructions (opt-loop-head-index outer))
    (multiple-value-bind (core _step) (%loop-fr514-core-and-step inner)
      (declare (ignore _step))
      (let ((inner-accesses nil)
            (outer-accesses nil))
        (dolist (inst core)
          (when (typep inst 'vm-aref)
            (push (%loop-fr514-affine-access inst (opt-loop-iv-reg inner) const-env def-env)
                  inner-accesses)
            (push (%loop-fr514-affine-access inst (opt-loop-iv-reg outer) const-env def-env)
                  outer-accesses)))
        (values (remove nil inner-accesses) (remove nil outer-accesses)
                (count-if (lambda (inst) (typep inst 'vm-aref)) core))))))

(defun %poly-loop-interchange-candidate-p (instructions vec outer inner)
  "Return T for strict rectangular, read-only affine two-deep loops."
  (multiple-value-bind (core _step) (%loop-fr514-core-and-step inner)
    (declare (ignore _step))
    (multiple-value-bind (inner-accesses outer-accesses access-count)
        (%poly-affine-read-accesses instructions inner outer)
      (and (%poly-loop-init-index vec outer)
           (%poly-loop-init-index vec inner)
           (plusp access-count)
           (= (length inner-accesses) access-count)
           (= (length outer-accesses) access-count)
           (not (some #'%poly-unsafe-side-effect-p core))
           ;; Beneficial when the old inner loop has larger stride than the old
           ;; outer loop for at least one read, e.g. column-major traversal of a
           ;; row-major linearized array.
           (some (lambda (in out)
                   (> (abs (getf in :stride 0))
                      (abs (getf out :stride 0))))
                 inner-accesses outer-accesses)))))

(defun %poly-copy-inst (inst)
  (handler-case
      (sexp->instruction (instruction->sexp inst))
    (error () inst)))

(defun %poly-interchanged-label (name suffix)
  (intern (format nil "~A__POLY_~A" name suffix) :keyword))

(defun %poly-emit-interchanged-loop (vec outer inner out)
  "Emit a concrete loop interchange for a strict two-deep canonical nest."
  (let* ((outer-init (aref vec (%poly-loop-init-index vec outer)))
         (inner-init (aref vec (%poly-loop-init-index vec inner)))
         (outer-cmp (%poly-copy-inst (aref vec (opt-loop-cmp-index outer))))
         (inner-cmp (%poly-copy-inst (aref vec (opt-loop-cmp-index inner))))
         (outer-jz (%poly-copy-inst (aref vec (opt-loop-jz-index outer))))
         (inner-jz (%poly-copy-inst (aref vec (opt-loop-jz-index inner))))
         (outer-step (%poly-copy-inst (car (last (opt-loop-body outer)))))
         (inner-step (%poly-copy-inst (car (last (opt-loop-body inner)))))
         (outer-head (%poly-interchanged-label (opt-loop-head-label outer) "INNER"))
         (inner-head (%poly-interchanged-label (opt-loop-head-label inner) "OUTER"))
         (inner-exit (%poly-interchanged-label (opt-loop-exit-label inner) "DONE"))
         (outer-exit (%poly-interchanged-label (opt-loop-exit-label outer) "DONE")))
    (push (%poly-copy-inst inner-init) out)
    (push (make-vm-label :name inner-head) out)
    (push inner-cmp out)
    (push (make-vm-jump-zero :reg (vm-reg inner-jz) :label outer-exit) out)
    (push (%poly-copy-inst outer-init) out)
    (push (make-vm-label :name outer-head) out)
    (push outer-cmp out)
    (push (make-vm-jump-zero :reg (vm-reg outer-jz) :label inner-exit) out)
    (multiple-value-bind (core _step) (%loop-fr514-core-and-step inner)
      (declare (ignore _step))
      (dolist (inst core) (push (%poly-copy-inst inst) out)))
    (push outer-step out)
    (push (make-vm-jump :label outer-head) out)
    (push (make-vm-label :name inner-exit) out)
    (push inner-step out)
    (push (make-vm-jump :label inner-head) out)
    (push (make-vm-label :name outer-exit) out)
    out))

(defun %poly-interchange-instructions (instructions)
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (out nil)
         (changed nil)
         (i 0))
    (loop while (< i n)
          do (let ((outer (%loop-fr514-parse-canonical-loop-at vec i)))
               (if (null outer)
                   (progn (push (aref vec i) out) (incf i))
                   (let ((inner (%poly-nested-loop-at vec outer)))
                     (if (and inner (%poly-loop-interchange-candidate-p instructions vec outer inner))
                         (progn
                           (setf out (%poly-emit-interchanged-loop vec outer inner out)
                                 changed t
                                 i (1+ (opt-loop-exit-index outer))))
                         (progn
                           (loop for k from (opt-loop-head-index outer)
                                 to (opt-loop-exit-index outer)
                                 do (push (aref vec k) out))
                           (setf i (1+ (opt-loop-exit-index outer)))))))))
    (if changed (nreverse out) instructions)))

(defun polyhedral-loop-interchange (statement)
  "Interchange two-deep affine loops when cache-locality improves.

STATEMENT may be either a POLYHEDRAL-STATEMENT descriptor or a VM instruction
list.  Descriptor schedules are rewritten directly.  Instruction streams are
transformed only for strict, read-only, rectangular two-deep canonical nests with
affine array reads; all other inputs are returned unchanged."
  (cond
    ((polyhedral-statement-p statement) (%poly-interchange-statement statement))
    ((listp statement) (%poly-interchange-instructions statement))
    (t statement)))

(defun polyhedral-tile (statement &key tile-sizes)
  "Apply a basic 2D tile schedule descriptor to STATEMENT.

The current VM instruction set has no min/bound primitive suitable for safe
general strip-mining, so instruction lists are annotated with tile-plan labels by
OPT-PASS-POLYHEDRAL.  Descriptor inputs receive an explicit tiled schedule."
  (let* ((sizes (or tile-sizes (list *polyhedral-default-tile-size*
                                     *polyhedral-default-tile-size*)))
         (sizes (if (listp sizes) sizes (list sizes sizes))))
    (if (and (polyhedral-statement-p statement)
             (>= (length (poly-stmt-loops statement)) 2))
        (make-polyhedral-statement
         :loops (poly-stmt-loops statement)
         :domain (poly-stmt-domain statement)
         :accesses (poly-stmt-accesses statement)
         :schedule (list :tiled t
                         :tile-sizes (subseq (append sizes sizes) 0 2)
                         :previous (poly-stmt-schedule statement))
         :body (poly-stmt-body statement))
        statement)))

(defun polyhedral-fuse (statements)
  "Fuse compatible polyhedral statement descriptors into one descriptor.

Statements are compatible when their first two loops and domains match.  The
fused descriptor concatenates bodies and access summaries while preserving the
shared schedule.  Non-descriptor or incompatible inputs are returned unchanged."
  (if (and (consp statements)
           (every #'polyhedral-statement-p statements)
           (let ((loops (subseq (poly-stmt-loops (first statements)) 0
                                (min 2 (length (poly-stmt-loops (first statements))))))
                 (domain (poly-stmt-domain (first statements))))
             (every (lambda (stmt)
                      (and (equal loops (subseq (poly-stmt-loops stmt) 0
                                                (min 2 (length (poly-stmt-loops stmt)))))
                           (equal (and domain (poly-domain-dimensions domain))
                                  (and (poly-stmt-domain stmt)
                                       (poly-domain-dimensions (poly-stmt-domain stmt))))))
                    statements)))
      (let ((first (first statements)))
        (make-polyhedral-statement
         :loops (poly-stmt-loops first)
         :domain (poly-stmt-domain first)
         :accesses (mapcan (lambda (stmt) (copy-list (poly-stmt-accesses stmt))) statements)
         :schedule (list :fused t :count (length statements)
                         :previous (poly-stmt-schedule first))
         :body (mapcan (lambda (stmt) (copy-list (poly-stmt-body stmt))) statements)))
      statements))

(defun %poly-emit-tile-metadata (vec outer inner tile-size out)
  (push (make-vm-label :name (%poly-interchanged-label (opt-loop-head-label outer)
                                                     (format nil "TILE_OUTER_~D" tile-size)))
        out)
  (push (make-vm-label :name (%poly-interchanged-label (opt-loop-head-label inner)
                                                     (format nil "TILE_INNER_~D" tile-size)))
        out)
  (loop for k from (opt-loop-head-index outer) to (opt-loop-exit-index outer)
        do (push (%poly-copy-inst (aref vec k)) out))
  out)

(defun %poly-tile-instructions (instructions &key (tile-size *polyhedral-default-tile-size*))
  "Annotate strict affine two-deep loops with a concrete 2D tile plan."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (out nil)
         (changed nil)
         (i 0))
    (loop while (< i n)
          do (let ((outer (%loop-fr514-parse-canonical-loop-at vec i)))
               (if (null outer)
                   (progn (push (aref vec i) out) (incf i))
                   (let ((inner (%poly-nested-loop-at vec outer)))
                     (if (and inner (%poly-loop-interchange-candidate-p instructions vec outer inner))
                         (progn
                           (setf out (%poly-emit-tile-metadata vec outer inner tile-size out)
                                 changed t
                                 i (1+ (opt-loop-exit-index outer))))
                         (progn
                           (loop for k from (opt-loop-head-index outer)
                                 to (opt-loop-exit-index outer)
                                 do (push (aref vec k) out))
                           (setf i (1+ (opt-loop-exit-index outer)))))))))
    (if changed (nreverse out) instructions)))

(defun opt-pass-polyhedral (instructions)
  "FR-513 explicit polyhedral pass.

This pass is intentionally not part of the default optimizer pipeline.  When
*POLYHEDRAL-ENABLED* is NIL it is a no-op.  When enabled it performs strict
two-deep affine loop interchange and emits 2D tile-plan metadata for remaining
eligible nests."
  (if (not *polyhedral-enabled*)
      instructions
      (%poly-tile-instructions (polyhedral-loop-interchange instructions))))

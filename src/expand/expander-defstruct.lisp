(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — DEFSTRUCT Expansion
;;;
;;; Translates (defstruct name slots...) to (progn defclass constructor predicate).
;;; Supports :conc-name, :constructor with BOA lambda list, :include inheritance.
;;;
;;; All functions here are pure code generators: they return Lisp forms,
;;; never evaluate them.  Only expand-defstruct calls compiler-macroexpand-all
;;; (via the caller in expander.lisp) to recurse into the output.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── BOA (By-Order-of-Arguments) lambda list helpers ──────────────────────

(defun %defstruct-extract-boa-parts (boa-args)
  "Split a BOA lambda list into (normal-params . aux-bindings)."
  (let ((normal nil) (aux nil) (in-aux nil))
    (dolist (arg boa-args)
      (cond ((eq arg '&aux) (setf in-aux t))
            (in-aux (push (if (consp arg) arg (list arg nil)) aux))
            (t       (push arg normal))))
    (cons (nreverse normal) (nreverse aux))))

(defun %defstruct-boa-param-names (normal-params)
  "Extract bound parameter names from a BOA lambda list (excludes &aux)."
  (let ((names nil))
    (dolist (p normal-params)
      (cond ((member p '(&key &optional &rest &body &allow-other-keys)) nil)
            ((consp p)  (push (if (consp (first p)) (second (first p)) (first p)) names))
            ((symbolp p) (push p names))))
    (nreverse names)))

;;; ── Constructor generation ───────────────────────────────────────────────

(defun %defstruct-make-constructor (ctor-name class-name boa-args all-slots)
  "Generate a DEFUN form for a defstruct constructor.
With BOA-ARGS: uses positional parameters.  Without: uses keyword parameters."
  (if boa-args
      ;; BOA constructor: positional params + &aux bindings
      (let* ((parts        (%defstruct-extract-boa-parts boa-args))
             (normal-params (car parts))
             (aux-bindings  (cdr parts))
             (param-names   (%defstruct-boa-param-names normal-params))
             (aux-names     (mapcar #'first aux-bindings))
             (bound-names   (append param-names aux-names))
             (initargs      (loop for (sname default) in all-slots
                                  append (list (intern (symbol-name sname) "KEYWORD")
                                               (if (member sname bound-names :test #'string=)
                                                   sname
                                                   default))))
             (aux-lets      (mapcar (lambda (b) (list (first b) (second b))) aux-bindings)))
        (list 'defun ctor-name normal-params
              (list 'let* aux-lets
                    (list* 'make-instance (list 'quote class-name) initargs))))
      ;; Keyword constructor: all slots become &key params
      (let ((key-params (mapcar (lambda (s) (list (first s) (second s))) all-slots))
            (initargs   (mapcan (lambda (s)
                                  (list (intern (symbol-name (first s)) "KEYWORD")
                                        (first s)))
                                all-slots)))
        (list 'defun ctor-name (cons '&key key-params)
              (list* 'make-instance (list 'quote class-name) initargs)))))

;;; ── List/Vector-based struct expansion (FR-546) ────────────────────────

(defun %defstruct-typed-constructor (ctor-name struct-name struct-type boa-args all-slots)
  "Generate a constructor for :type list or :type vector defstruct.
The first element is the type tag (struct name), followed by slot values."
  (let* ((slot-count (length all-slots))
         (total-len (1+ slot-count)))  ;; tag + slots
    (if boa-args
        ;; BOA constructor
        (let* ((parts (%defstruct-extract-boa-parts boa-args))
               (normal-params (car parts))
               (aux-bindings (cdr parts))
               (param-names (%defstruct-boa-param-names normal-params))
               (aux-names (mapcar #'first aux-bindings))
               (bound-names (append param-names aux-names))
               (aux-lets (mapcar (lambda (b) (list (first b) (second b))) aux-bindings))
               (slot-values (mapcar (lambda (s)
                                      (if (member (first s) bound-names :test #'string=)
                                          (first s)
                                          (second s)))
                                    all-slots)))
          (if (eq struct-type 'list)
              `(defun ,ctor-name ,normal-params
                 (let* ,aux-lets
                   (list ',struct-name ,@slot-values)))
              `(defun ,ctor-name ,normal-params
                 (let* ,aux-lets
                   (vector ',struct-name ,@slot-values)))))
        ;; Keyword constructor
        (let ((key-params (mapcar (lambda (s) (list (first s) (second s))) all-slots))
              (slot-values (mapcar #'first all-slots)))
          (if (eq struct-type 'list)
              `(defun ,ctor-name (&key ,@key-params)
                 (list ',struct-name ,@slot-values))
              `(defun ,ctor-name (&key ,@key-params)
                 (vector ',struct-name ,@slot-values)))))))

(defun %defstruct-typed-accessors (struct-name struct-type conc-name all-slots)
  "Generate accessor functions for :type list or :type vector defstruct.
Each slot is at offset (1+ index) since position 0 is the type tag."
  (loop for (slot-name default read-only-p) in all-slots
        for idx from 1
        for acc-name = (if conc-name
                           (intern (concatenate 'string (symbol-name conc-name) (symbol-name slot-name)))
                           slot-name)
        collect (if (eq struct-type 'list)
                    `(defun ,acc-name (obj) (nth ,idx obj))
                    `(defun ,acc-name (obj) (aref obj ,idx)))))

(defun %defstruct-typed-predicate (pred-name struct-name struct-type slot-count)
  "Generate a predicate for :type list or :type vector defstruct."
  (when pred-name
    (let ((total-len (1+ slot-count)))
      (if (eq struct-type 'list)
          `(defun ,pred-name (obj)
             (and (listp obj) (eq (car obj) ',struct-name)
                  (= (length obj) ,total-len)))
          `(defun ,pred-name (obj)
             (and (vectorp obj) (> (length obj) 0)
                  (eq (aref obj 0) ',struct-name)
                  (= (length obj) ,total-len)))))))

;;; ── Main defstruct expander ──────────────────────────────────────────────

(defun expand-defstruct (form)
  "Expand (defstruct name-or-options slot...) to (progn defclass constructor predicate).

Supported options:
  (:conc-name prefix)   — accessor prefix; defaults to NAME-
  (:constructor name lambda-list?) — constructor name and optional BOA list
  (:include parent)     — inherit parent slots
  (:type list/vector)   — use list/vector representation instead of CLOS"
  (let* ((name-and-options (second form))
         (slots-raw        (cddr form))
         ;; Parse name and option list
         (name    (if (listp name-and-options) (first name-and-options) name-and-options))
         (options (when (listp name-and-options) (rest name-and-options)))
         ;; :conc-name option
         (conc-opt  (find :conc-name options :key (lambda (o) (when (listp o) (first o)))))
         (conc-name (if conc-opt
                        (second conc-opt)
                        (intern (concatenate 'string (symbol-name name) "-"))))
         ;; :constructor option — (:constructor nil) suppresses
         (ctor-opt  (find :constructor options :key (lambda (o) (when (listp o) (first o)))))
         (ctor-name (cond
                      ((and ctor-opt (null (second ctor-opt))) nil)
                      (ctor-opt (second ctor-opt))
                      (t (intern (concatenate 'string "MAKE-" (symbol-name name))))))
         (boa-args  (when (and ctor-opt (cddr ctor-opt)) (third ctor-opt)))
         ;; :include option
         (incl-opt    (find :include options :key (lambda (o) (when (listp o) (first o)))))
         (parent-name (when incl-opt (second incl-opt)))
         (parent-slots (when parent-name (gethash parent-name *defstruct-slot-registry*)))
         ;; :type option — (:type list) or (:type vector)
         (type-opt   (find :type options :key (lambda (o) (when (listp o) (first o)))))
         (struct-type (when type-opt (second type-opt)))
         ;; Slot normalization: (slot-name default &key :read-only :type) or bare slot-name
         ;; Returns (slot-name default read-only-p)
         (own-slots (mapcar (lambda (s)
                              (if (listp s)
                                  (list (first s) (second s)
                                        (getf (cddr s) :read-only nil))
                                  (list s nil nil)))
                            (remove-if #'stringp slots-raw)))
         (all-slots (append (or parent-slots nil) own-slots))
         ;; :predicate option — (:predicate nil) suppresses
         (pred-opt  (find :predicate options :key (lambda (o) (when (listp o) (first o)))))
         (pred-name (cond
                      ((and pred-opt (null (second pred-opt))) nil)
                      (pred-opt (second pred-opt))
                      (t (intern (concatenate 'string (symbol-name name) "-P")))))
         ;; FR-544: :print-function / :print-object option
         (print-fn-opt (find :print-function options :key (lambda (o) (when (listp o) (first o)))))
         (print-obj-opt (find :print-object options :key (lambda (o) (when (listp o) (first o)))))
         (print-fn (or (when print-fn-opt (second print-fn-opt))
                       (when print-obj-opt (second print-obj-opt)))))
    ;; Register slot info for :include inheritance
    (setf (gethash name *defstruct-slot-registry*) all-slots)
    ;; Dispatch: :type list/vector → non-CLOS expansion; otherwise → CLOS expansion
    (if struct-type
        ;; ── :type list or :type vector expansion (FR-546) ──
        (let* ((ctor-form (when ctor-name
                            (%defstruct-typed-constructor ctor-name name struct-type boa-args all-slots)))
               (accessor-forms (%defstruct-typed-accessors name struct-type conc-name all-slots))
               (pred-form (%defstruct-typed-predicate pred-name name struct-type (length all-slots))))
          `(progn ,@(when ctor-form (list ctor-form))
                  ,@accessor-forms
                  ,@(when pred-form (list pred-form))
                  (quote ,name)))
        ;; ── Standard CLOS-based expansion ──
        (flet ((accessor-name (slot-name)
                 (if conc-name
                     (intern (concatenate 'string (symbol-name conc-name) (symbol-name slot-name)))
                     slot-name)))
          ;; Register accessors for setf expansion — only own non-read-only slots
          ;; (parent accessors are already registered under the parent's conc-name; FR-545)
          (dolist (slot own-slots)
            (unless (third slot) ; skip read-only slots
              (setf (gethash (accessor-name (first slot)) *accessor-slot-map*)
                    (cons name (first slot)))))
          ;; Build DEFCLASS slot specs — only own slots; parent slots are inherited
          ;; through the CLOS superclass chain (FR-545 fix)
          ;; Read-only slots get :reader instead of :accessor (no writer)
          (let* ((defclass-slots
                   (mapcar (lambda (slot)
                             (let ((acc-key (if (third slot) :reader :accessor)))
                               (list (first slot)
                                     :initarg  (intern (symbol-name (first slot)) "KEYWORD")
                                     :initform (second slot)
                                     acc-key (accessor-name (first slot)))))
                           own-slots))
                 (superclasses  (when parent-name (list parent-name)))
                 (defclass-form `(defclass ,name ,superclasses ,defclass-slots))
                 (ctor-form     (when ctor-name
                                  (%defstruct-make-constructor ctor-name name boa-args all-slots)))
                 (pred-form     (when pred-name
                                  `(defun ,pred-name (obj) (typep obj ',name))))
                 ;; FR-544: :print-function emits defmethod print-object
                 (print-form    (when print-fn
                                  (let ((o (gensym "OBJ")) (s (gensym "STR")))
                                    (if print-fn-opt
                                        ;; :print-function takes (obj stream depth)
                                        `(defmethod print-object ((,o ,name) ,s)
                                           (funcall (function ,print-fn) ,o ,s 0))
                                        ;; :print-object takes (obj stream)
                                        `(defmethod print-object ((,o ,name) ,s)
                                           (funcall (function ,print-fn) ,o ,s)))))))
            `(progn ,defclass-form ,@(when ctor-form (list ctor-form))
                                   ,@(when pred-form (list pred-form))
                                   ,@(when print-form (list print-form))
                                   (quote ,name)))))))

;;;; types-extended-concurrency.lisp — Concurrency traits, security labels, region lifetimes, capabilities

(in-package :cl-cc/type)

;;; ─── Concrete advanced semantic APIs (merged for Nix flake source) ─────────

(defstruct concurrency-traits
  "Concrete Send/Sync traits for a type designator."
  (type nil)
  (send nil :type boolean)
  (sync nil :type boolean)
  (note nil))

(defvar *concurrency-trait-registry* (make-hash-table :test #'equal)
  "Overrides for Send/Sync trait information keyed by type designator.")

(defparameter +default-concurrency-traits+
  '((integer t t)
    (fixnum t t)
    (float t t)
    (double-float t t)
    (single-float t t)
    (rational t t)
    (character t t)
    (string t t)
    (symbol t t)
    (keyword t t)
    (null t t)
    (boolean t t)
    (pathname nil nil)
    (hash-table nil nil)
    (cons nil nil)
    (vector nil nil)
    (array nil nil)
    (function nil nil))
  "Built-in Send/Sync defaults for common host types.")

(defun %default-concurrency-traits (type)
  "Return the built-in trait entry for TYPE, or NIL when unknown."
  (let ((entry (assoc type +default-concurrency-traits+ :test #'equal)))
    (when entry
      (make-concurrency-traits :type (first entry)
                               :send (second entry)
                               :sync (third entry)
                               :note :default))))

(defun register-concurrency-traits (type &key send sync note)
  "Register concrete Send/Sync information for TYPE."
  (let ((traits (make-concurrency-traits :type type
                                         :send (not (null send))
                                         :sync (not (null sync))
                                         :note note)))
    (setf (gethash type *concurrency-trait-registry*) traits)
    traits))

(defun lookup-concurrency-traits (type)
  "Return registered or built-in traits for TYPE."
  (or (gethash type *concurrency-trait-registry*)
      (%default-concurrency-traits type)))

(defun sendable-type-p (type)
  "Return T when TYPE may be transferred across threads."
  (let ((traits (lookup-concurrency-traits type)))
    (and traits (concurrency-traits-send traits))))

(defun shareable-type-p (type)
  "Return T when TYPE may be shared across threads by reference."
  (let ((traits (lookup-concurrency-traits type)))
    (and traits (concurrency-traits-sync traits))))

(defun validate-send (type)
  "Return T when TYPE is Send."
  (sendable-type-p type))

(defun validate-sync (type)
  "Return T when TYPE is Sync."
  (shareable-type-p type))

(defun validate-spawn-argument (type)
  "Return T when TYPE may cross a spawn boundary."
  (validate-send type))

(defun validate-shared-reference (type)
  "Return T when TYPE may be shared through references."
  (validate-sync type))


(defparameter +security-label-order+
  '(:public :trusted :tainted :secret :top-secret)
  "Concrete information-flow lattice from least to most restrictive.")

(defun normalize-security-label (label)
  "Normalize LABEL into a keyword designator."
  (cond
    ((keywordp label) label)
    ((symbolp label) (intern (symbol-name label) :keyword))
    ((stringp label) (intern (string-upcase label) :keyword))
    (t label)))

(defun security-label-rank (label)
  "Return LABEL's lattice rank, or NIL when unknown."
  (position (normalize-security-label label) +security-label-order+))

(defun security-label-p (label)
  "Return T when LABEL is part of the security lattice."
  (not (null (security-label-rank label))))

(defun security-label<= (source target)
  "Return T when information at SOURCE may flow into TARGET."
  (let ((source-rank (security-label-rank source))
        (target-rank (security-label-rank target)))
    (and source-rank target-rank (<= source-rank target-rank))))

(defun join-security-labels (left right)
  "Return the least upper bound of LEFT and RIGHT."
  (let* ((left-label (normalize-security-label left))
         (right-label (normalize-security-label right))
         (left-rank (security-label-rank left-label))
         (right-rank (security-label-rank right-label)))
    (unless (and left-rank right-rank)
      (error "Unknown security labels ~S and ~S" left right))
    (nth (max left-rank right-rank) +security-label-order+)))

(defun meet-security-labels (left right)
  "Return the greatest lower bound of LEFT and RIGHT."
  (let* ((left-label (normalize-security-label left))
         (right-label (normalize-security-label right))
         (left-rank (security-label-rank left-label))
         (right-rank (security-label-rank right-label)))
    (unless (and left-rank right-rank)
      (error "Unknown security labels ~S and ~S" left right))
    (nth (min left-rank right-rank) +security-label-order+)))

(defstruct (labeled-value (:constructor %make-labeled-value))
  "A concrete security-labeled runtime value."
  (value nil)
  (label :public)
  (tainted-p nil :type boolean)
  (audit-trail nil :type list))

(defun make-labeled-value (value label &key tainted-p audit-trail)
  "Construct a labeled VALUE with LABEL and optional taint/audit data."
  (unless (security-label-p label)
    (error "Unknown security label: ~S" label))
  (%make-labeled-value :value value
                       :label (normalize-security-label label)
                       :tainted-p (not (null tainted-p))
                       :audit-trail (copy-list audit-trail)))

(defun labeled-value-flow-allowed-p (value-or-label target-label)
  "Return T when VALUE-OR-LABEL may flow to TARGET-LABEL."
  (let ((source-label (if (labeled-value-p value-or-label)
                          (labeled-value-label value-or-label)
                          value-or-label)))
    (security-label<= source-label target-label)))

(defun sanitize-labeled-value (labeled-value sanitizer &key audit-entry)
  "Return a sanitized copy of LABELED-VALUE with taint cleared.
SANITIZER may be NIL, in which case the original payload is preserved."
  (unless (labeled-value-p labeled-value)
    (error "Expected labeled-value, got ~S" labeled-value))
  (let ((sanitized (if sanitizer
                       (funcall sanitizer (labeled-value-value labeled-value))
                       (labeled-value-value labeled-value))))
    (make-labeled-value sanitized
                        (labeled-value-label labeled-value)
                        :tainted-p nil
                        :audit-trail (append (labeled-value-audit-trail labeled-value)
                                             (when audit-entry (list audit-entry))))))

(defun declassify-labeled-value (labeled-value target-label reason)
  "Return a copy of LABELED-VALUE declassified to TARGET-LABEL.
REASON must be present and the target must not be more restrictive than the source."
  (unless (labeled-value-p labeled-value)
    (error "Expected labeled-value, got ~S" labeled-value))
  (unless reason
    (error "Declassification requires an explicit reason"))
  (unless (security-label-p target-label)
    (error "Unknown target security label: ~S" target-label))
  (let ((source (labeled-value-label labeled-value))
        (target (normalize-security-label target-label)))
    (unless (security-label<= target source)
      (error "Cannot declassify from ~S to more restrictive label ~S" source target))
    (make-labeled-value (labeled-value-value labeled-value)
                        target
                        :tainted-p (labeled-value-tainted-p labeled-value)
                        :audit-trail (append (labeled-value-audit-trail labeled-value)
                                             (list (list :declassify source target reason))))))


(define-condition region-lifetime-error (error)
  ((reference :initarg :reference :reader region-lifetime-error-reference))
  (:report (lambda (condition stream)
             (format stream "Region reference ~S is no longer valid"
                     (region-lifetime-error-reference condition)))))

(defvar *region-counter* 0
  "Monotone id source for region tokens.")

(defstruct (region-token (:constructor %make-region-token))
  "A concrete region lifetime token."
  (id 0 :type integer)
  (generation 0 :type integer)
  (active-p t :type boolean))

(defstruct region-ref
  "A value allocated inside a region."
  (token nil)
  (generation 0 :type integer)
  (value nil))

(defun make-region-token ()
  "Create a fresh active region token."
  (%make-region-token :id (incf *region-counter*)
                      :generation 0
                      :active-p t))

(defun close-region (region-token)
  "Invalidate REGION-TOKEN and bump its generation."
  (setf (region-token-active-p region-token) nil)
  (incf (region-token-generation region-token))
  region-token)

(defun region-active-p (region-token)
  "Return T when REGION-TOKEN is still active."
  (and (region-token-p region-token)
       (region-token-active-p region-token)))

(defun region-alloc (region-token value)
  "Allocate VALUE inside REGION-TOKEN."
  (unless (region-active-p region-token)
    (error "Cannot allocate in inactive region ~S" region-token))
  (make-region-ref :token region-token
                   :generation (region-token-generation region-token)
                   :value value))

(defun region-ref-valid-p (region-ref)
  "Return T when REGION-REF still points into a live region generation."
  (and (region-ref-p region-ref)
       (region-token-p (region-ref-token region-ref))
       (region-active-p (region-ref-token region-ref))
       (= (region-ref-generation region-ref)
          (region-token-generation (region-ref-token region-ref)))))

(defun region-deref (region-ref)
  "Read REGION-REF or signal REGION-LIFETIME-ERROR when dead."
  (unless (region-ref-valid-p region-ref)
    (error 'region-lifetime-error :reference region-ref))
  (region-ref-value region-ref))

(defmacro with-region ((name) &body body)
  "Evaluate BODY with NAME bound to a fresh region token."
  `(let ((,name (make-region-token)))
     (unwind-protect
          (progn ,@body)
       (close-region ,name))))


(defstruct (capability (:constructor %make-capability))
  "A concrete capability set."
  (permissions nil :type list))

(defun %normalize-permission (permission)
  "Normalize PERMISSION into a keyword."
  (cond
    ((keywordp permission) permission)
    ((symbolp permission) (intern (symbol-name permission) :keyword))
    ((stringp permission) (intern (string-upcase permission) :keyword))
    (t (error "Unsupported permission designator: ~S" permission))))

(defun %suffixp (suffix string)
  "Return T when STRING ends with SUFFIX."
  (let ((suffix-length (length suffix))
        (string-length (length string)))
    (and (<= suffix-length string-length)
         (string= suffix string :start1 0 :start2 (- string-length suffix-length)))))

(defun %permission-read-variant (permission)
  "Return the read-only variant of PERMISSION when one exists."
  (let* ((normalized (%normalize-permission permission))
         (name (symbol-name normalized)))
    (cond
      ((%suffixp "-WRITE" name)
       (intern (concatenate 'string
                            (subseq name 0 (- (length name) 6))
                            "-READ")
               :keyword))
      ((%suffixp "-READ" name) normalized)
      (t nil))))

(defun %permission-closure (permission)
  "Return PERMISSION plus its implied permissions."
  (let* ((normalized (%normalize-permission permission))
         (read-variant (%permission-read-variant normalized)))
    (remove-duplicates (cons normalized (when read-variant (list read-variant)))
                       :test #'eq)))

(defun make-capability (permissions)
  "Construct a normalized capability set from PERMISSIONS."
  (%make-capability
   :permissions
   (sort (remove-duplicates (mapcan #'%permission-closure permissions) :test #'eq)
         #'string<
         :key #'symbol-name)))

(defun capability-allows-p (capability permission)
  "Return T when CAPABILITY allows PERMISSION."
  (member (%normalize-permission permission)
          (capability-permissions capability)
          :test #'eq))

(defun capability-implies-p (left right)
  "Return T when LEFT subsumes RIGHT.
RIGHT may be another capability or a single permission."
  (cond
    ((capability-p right)
     (every (lambda (permission)
              (capability-allows-p left permission))
            (capability-permissions right)))
    (t
     (capability-allows-p left right))))

(defun restrict-capability (capability restriction)
  "Restrict CAPABILITY according to RESTRICTION.
When RESTRICTION is :READ-ONLY, write permissions are downgraded to read permissions."
  (cond
    ((eq restriction :read-only)
     (make-capability
      (remove nil
              (mapcar #'%permission-read-variant
                      (capability-permissions capability)))))
    ((capability-p restriction)
     (make-capability
      (remove-if-not (lambda (permission)
                       (capability-allows-p restriction permission))
                     (capability-permissions capability))))
    ((listp restriction)
     (let ((allowed (make-capability restriction)))
       (restrict-capability capability allowed)))
    (t
     (error "Unsupported capability restriction: ~S" restriction))))


(defun capability-effects (capability)
  "Return effect keywords implied by CAPABILITY."
  (sort (remove-duplicates
         (mapcar (lambda (permission)
                   (let* ((name (symbol-name permission))
                          (split (position #\- name :from-end t)))
                     (if split
                         (intern (concatenate 'string
                                              (subseq name (1+ split))
                                              "-"
                                              (subseq name 0 split))
                                 :keyword)
                         permission)))
                 (capability-permissions capability))
         :test #'eq)
        #'string<
        :key #'symbol-name))



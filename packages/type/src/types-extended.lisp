;;;; types-extended.lisp — Effect, Constraint, and Extended Type Nodes
;;;;
;;;; Effect rows, handler, GADT, constraint, qualified, error, primitive
;;;; singletons, type-equal-p, and type-free-vars.
;;;; Loads after types-core.lisp.

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

(defun delegate-capability (capability delegated-permissions)
  "Delegate the subset of CAPABILITY named by DELEGATED-PERMISSIONS."
  (restrict-capability capability delegated-permissions))

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


(define-condition unit-mismatch-error (error)
  ((left :initarg :left :reader unit-mismatch-error-left)
   (right :initarg :right :reader unit-mismatch-error-right))
  (:report (lambda (condition stream)
             (format stream "Incompatible units: ~S and ~S"
                     (unit-mismatch-error-left condition)
                     (unit-mismatch-error-right condition)))))

(defstruct unit-definition
  "A physical unit with a dimension vector and conversion scale to canonical SI."
  (name nil)
  (dimension nil :type list)
  (scale 1 :type number))

(defstruct (measure (:constructor %make-measure))
  "A concrete numeric value paired with a unit definition."
  (value 0 :type number)
  (unit nil))

(defvar *unit-registry* (make-hash-table :test #'equal)
  "Known physical units keyed by symbol.")

(defun %unit-key (name)
  "Return a stable registry key for NAME."
  (cond
    ((unit-definition-p name) (%unit-key (unit-definition-name name)))
    ((symbolp name) (string-upcase (symbol-name name)))
    ((stringp name) (string-upcase name))
    (t name)))

(defun %normalize-dimension (dimension)
  "Return DIMENSION as a canonical sorted alist without zero exponents."
  (sort (remove-if (lambda (entry) (zerop (cdr entry)))
                   (copy-list dimension))
        #'string<
        :key (lambda (entry) (symbol-name (car entry)))))

(defun define-unit (name dimension scale)
  "Define NAME with DIMENSION and SCALE relative to the canonical base unit."
  (let ((definition (make-unit-definition :name name
                                          :dimension (%normalize-dimension dimension)
                                          :scale scale)))
    (setf (gethash (%unit-key name) *unit-registry*) definition)
    definition))

(defun find-unit (name)
  "Return the unit definition named NAME, or signal an error."
  (or (gethash (%unit-key name) *unit-registry*)
      (error "Unknown unit: ~S" name)))

(defun unit-designator-p (value)
  "Return T when VALUE names a registered unit or is a unit-definition."
  (or (unit-definition-p value)
      (not (null (gethash (%unit-key value) *unit-registry*)))))

(defun %resolve-unit (unit)
  "Resolve UNIT into a unit-definition."
  (cond
    ((unit-definition-p unit) unit)
    ((symbolp unit) (find-unit unit))
    (t (error "Unsupported unit designator: ~S" unit))))

(defun unit-dimension= (left right)
  "Return T when LEFT and RIGHT have the same physical dimension."
  (equal (unit-definition-dimension (%resolve-unit left))
         (unit-definition-dimension (%resolve-unit right))))

(defun unit-compatible-p (left right)
  "Return T when LEFT and RIGHT are dimensionally compatible."
  (unit-dimension= left right))

(defun %combine-dimensions (left right factor)
  "Combine LEFT and RIGHT dimensions, adding FACTOR*RIGHT into LEFT."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (entry (%normalize-dimension left))
      (setf (gethash (car entry) table) (cdr entry)))
    (dolist (entry (%normalize-dimension right))
      (incf (gethash (car entry) table 0) (* factor (cdr entry))))
    (let (result)
      (maphash (lambda (key value)
                 (unless (zerop value)
                   (push (cons key value) result)))
               table)
      (%normalize-dimension result))))

(defun %derived-unit (left right operation)
  "Return a derived unit from LEFT and RIGHT under OPERATION (:* or :/)."
  (let ((left-unit (%resolve-unit left))
        (right-unit (%resolve-unit right)))
    (ecase operation
      (:*
       (make-unit-definition :name nil
                             :dimension (%combine-dimensions (unit-definition-dimension left-unit)
                                                             (unit-definition-dimension right-unit)
                                                             1)
                             :scale (* (unit-definition-scale left-unit)
                                       (unit-definition-scale right-unit))))
      (:/
       (make-unit-definition :name nil
                             :dimension (%combine-dimensions (unit-definition-dimension left-unit)
                                                             (unit-definition-dimension right-unit)
                                                             -1)
                             :scale (/ (unit-definition-scale left-unit)
                                       (unit-definition-scale right-unit)))))))

(defun convert-unit (value from-unit to-unit)
  "Convert VALUE from FROM-UNIT into TO-UNIT and return the converted numeric value."
  (let ((from (%resolve-unit from-unit))
        (to (%resolve-unit to-unit)))
    (unless (unit-compatible-p from to)
      (error 'unit-mismatch-error :left from-unit :right to-unit))
    (* value (/ (unit-definition-scale from)
                (unit-definition-scale to)))))

(defun convert-measure (measure to-unit)
  "Convert MEASURE into TO-UNIT and return a new measure."
  (let ((target (%resolve-unit to-unit)))
    (%make-measure :value (convert-unit (measure-value measure)
                                        (measure-unit measure)
                                        target)
                   :unit target)))

(defun make-measure (value unit)
  "Construct a concrete measure VALUE in UNIT."
  (%make-measure :value value :unit (%resolve-unit unit)))

(defun measure+ (left right)
  "Add LEFT and RIGHT, preserving LEFT's unit."
  (let ((left-unit (measure-unit left))
        (right-unit (measure-unit right)))
    (unless (unit-compatible-p left-unit right-unit)
      (error 'unit-mismatch-error :left left-unit :right right-unit))
    (%make-measure :value (+ (measure-value left)
                             (convert-unit (measure-value right) right-unit left-unit))
                   :unit left-unit)))

(defun measure- (left right)
  "Subtract RIGHT from LEFT, preserving LEFT's unit."
  (let ((left-unit (measure-unit left))
        (right-unit (measure-unit right)))
    (unless (unit-compatible-p left-unit right-unit)
      (error 'unit-mismatch-error :left left-unit :right right-unit))
    (%make-measure :value (- (measure-value left)
                             (convert-unit (measure-value right) right-unit left-unit))
                   :unit left-unit)))

(defun measure* (left right)
  "Multiply LEFT and RIGHT and derive a composite unit."
  (%make-measure :value (* (measure-value left) (measure-value right))
                 :unit (%derived-unit (measure-unit left) (measure-unit right) :*)))

(defun measure/ (left right)
  "Divide LEFT by RIGHT and derive a composite unit."
  (%make-measure :value (/ (measure-value left) (measure-value right))
                 :unit (%derived-unit (measure-unit left) (measure-unit right) :/)))

(eval-when (:load-toplevel :execute)
  (define-unit 'meter '((:length . 1)) 1)
  (define-unit 'centimeter '((:length . 1)) 1/100)
  (define-unit 'kilometer '((:length . 1)) 1000)
  (define-unit 'second '((:time . 1)) 1)
  (define-unit 'minute '((:time . 1)) 60)
  (define-unit 'hour '((:time . 1)) 3600)
  (define-unit 'gram '((:mass . 1)) 1/1000)
  (define-unit 'kilogram '((:mass . 1)) 1))


(define-condition route-validation-error (error)
  ((detail :initarg :detail :reader route-validation-error-detail))
  (:report (lambda (condition stream)
             (format stream "Invalid route: ~A" (route-validation-error-detail condition)))))

(defstruct (route (:constructor %make-route))
  "A concrete typed route descriptor."
  (method :get)
  (path "/" :type string)
  (parameters nil :type list)
  (request-type nil)
  (response-type nil))

(defstruct api-spec
  "A collection of typed routes."
  (routes nil :type list))

(defparameter +route-methods+ '(:get :post :put :patch :delete :head :options)
  "Supported HTTP methods for typed routes.")

(defun normalize-route-method (method)
  "Normalize METHOD into a keyword route method."
  (cond
    ((keywordp method) method)
    ((symbolp method) (intern (symbol-name method) :keyword))
    ((stringp method) (intern (string-upcase method) :keyword))
    (t method)))

(defun %split-path (path)
  "Split PATH into slash-separated segments."
  (let ((start 0)
        (length (length path))
        segments)
    (loop for index from 0 to length do
      (when (or (= index length) (char= (char path index) #\/))
        (let ((segment (subseq path start index)))
          (unless (string= segment "")
            (push segment segments)))
        (setf start (1+ index))))
    (nreverse segments)))

(defun route-template-parameters (path)
  "Extract ordered placeholder names from PATH."
  (let (parameters)
    (dolist (segment (%split-path path))
      (when (and (> (length segment) 2)
                 (char= (char segment 0) #\{)
                 (char= (char segment (1- (length segment))) #\}))
        (push (intern (string-upcase (subseq segment 1 (1- (length segment)))) :keyword)
              parameters)))
    (nreverse parameters)))

(defun %normalize-route-parameters (parameters)
  "Normalize PARAMETERS into an alist of keyword → type-designator."
  (mapcar (lambda (entry)
            (cond
              ((and (consp entry) (consp (rest entry)) (null (cddr entry)))
               (cons (intern (string-upcase (symbol-name (first entry))) :keyword)
                     (second entry)))
              ((consp entry)
               (cons (intern (string-upcase (symbol-name (car entry))) :keyword)
                     (cdr entry)))
              (t
               (error 'route-validation-error :detail (format nil "malformed parameter ~S" entry)))))
          parameters))

(defun route-valid-p (route)
  "Return T when ROUTE is a valid typed route descriptor."
  (and (route-p route)
       (member (normalize-route-method (route-method route)) +route-methods+ :test #'eq)
       (stringp (route-path route))
       (plusp (length (route-path route)))
       (char= (char (route-path route) 0) #\/)
       (let* ((template-parameters (route-template-parameters (route-path route)))
              (normalized-parameters (%normalize-route-parameters (route-parameters route)))
              (parameter-names (mapcar #'car normalized-parameters)))
         (and (= (length template-parameters) (length parameter-names))
              (equal template-parameters parameter-names)))))

(defun make-route (method path &key parameters request-type response-type)
  "Construct and validate a typed route."
  (let ((route (%make-route :method (normalize-route-method method)
                            :path path
                            :parameters (%normalize-route-parameters parameters)
                            :request-type request-type
                            :response-type response-type)))
    (unless (route-valid-p route)
      (error 'route-validation-error :detail (format nil "~S ~A" method path)))
    route))

(defun %parse-route-parameter (raw-value type-designator)
  "Parse RAW-VALUE according to TYPE-DESIGNATOR."
  (cond
    ((eq type-designator 'integer)
     (handler-case
         (multiple-value-bind (value index) (parse-integer raw-value :junk-allowed t)
           (and (= index (length raw-value)) value))
       (error () nil)))
    ((eq type-designator 'string) raw-value)
    ((eq type-designator 'keyword) (intern (string-upcase raw-value) :keyword))
    (t (when (typep raw-value type-designator) raw-value))))

(defun build-route-path (route parameter-values)
  "Render ROUTE using PARAMETER-VALUES, an alist keyed by parameter name."
  (unless (route-valid-p route)
    (error 'route-validation-error :detail "cannot build an invalid route"))
  (let* ((value-alist (%normalize-route-parameters parameter-values))
         (expected (%normalize-route-parameters (route-parameters route)))
         (segments (%split-path (route-path route))))
    (concatenate
     'string
     "/"
     (format nil "~{~A~^/~}"
             (mapcar (lambda (segment)
                       (if (and (> (length segment) 2)
                                (char= (char segment 0) #\{)
                                (char= (char segment (1- (length segment))) #\}))
                           (let* ((name (intern (string-upcase (subseq segment 1 (1- (length segment)))) :keyword))
                                  (expected-type (cdr (assoc name expected :test #'eq)))
                                  (actual (cdr (assoc name value-alist :test #'eq))))
                             (unless (typep actual expected-type)
                               (error 'route-validation-error
                                      :detail (format nil "parameter ~A expected ~S, got ~S"
                                                      name expected-type actual)))
                             (princ-to-string actual))
                           segment))
                     segments)))))

(defun match-route-path (route path)
  "Return (values T params) when PATH matches ROUTE, otherwise (values NIL NIL)."
  (unless (route-valid-p route)
    (return-from match-route-path (values nil nil)))
  (let ((template-segments (%split-path (route-path route)))
        (path-segments (%split-path path))
        (parameters (%normalize-route-parameters (route-parameters route)))
        results)
    (unless (= (length template-segments) (length path-segments))
      (return-from match-route-path (values nil nil)))
    (loop for template in template-segments
          for actual in path-segments do
      (if (and (> (length template) 2)
               (char= (char template 0) #\{)
               (char= (char template (1- (length template))) #\}))
          (let* ((name (intern (string-upcase (subseq template 1 (1- (length template)))) :keyword))
                 (type-designator (cdr (assoc name parameters :test #'eq)))
                 (parsed (%parse-route-parameter actual type-designator)))
            (unless parsed
              (return-from match-route-path (values nil nil)))
            (push (cons name parsed) results))
          (unless (string= template actual)
            (return-from match-route-path (values nil nil)))))
    (values t (nreverse results))))

(defun api-spec-valid-p (api-spec)
  "Return T when API-SPEC contains only valid, non-duplicate routes."
  (and (api-spec-p api-spec)
       (every #'route-valid-p (api-spec-routes api-spec))
       (let ((seen (make-hash-table :test #'equal)))
         (loop for route in (api-spec-routes api-spec)
               always (let ((key (list (route-method route) (route-path route))))
                        (if (gethash key seen)
                            nil
                            (progn
                              (setf (gethash key seen) t)
                              t)))))))

(defun route-form-valid-p (value)
  "Return T when VALUE is a plausible raw FR-3305 route payload form."
  (and (consp value)
       (member (normalize-route-method (first value)) +route-methods+ :test #'eq)
       (>= (length value) 3)
       (stringp (second value))
       (let* ((path (second value))
              (param-count (length (route-template-parameters path)))
              (tail-count (- (length value) 2)))
         (>= tail-count (max 1 param-count)))))


(define-condition ffi-validation-error (error)
  ((detail :initarg :detail :reader ffi-validation-error-detail))
  (:report (lambda (condition stream)
             (format stream "Invalid FFI descriptor: ~A" (ffi-validation-error-detail condition)))))

(defparameter +ffi-scalar-kinds+
  '(:void :char :short :int :long :float :double :string :bool :size-t)
  "Supported scalar FFI kinds.")

(defstruct (ffi-scalar-type (:constructor %make-ffi-scalar-type))
  "A scalar C-side type."
  (kind :int))

(defstruct (ffi-pointer-type (:constructor %make-ffi-pointer-type))
  "A typed pointer into foreign memory."
  (pointee nil)
  (borrowed-p t :type boolean)
  (nullable-p nil :type boolean))

(defstruct (ffi-callback-type (:constructor %make-ffi-callback-type))
  "A type-checked callback signature."
  (argument-types nil :type list)
  (return-type nil))

(defstruct (ffi-function-descriptor (:constructor %make-ffi-function-descriptor))
  "A named foreign function boundary descriptor."
  (name nil)
  (argument-types nil :type list)
  (return-type nil)
  (abi :c))

(defun make-ffi-scalar-type (kind)
  "Construct a scalar FFI type."
  (let ((normalized (if (keywordp kind) kind (intern (string-upcase (symbol-name kind)) :keyword))))
    (unless (member normalized +ffi-scalar-kinds+ :test #'eq)
      (error 'ffi-validation-error :detail (format nil "unknown scalar kind ~S" kind)))
    (%make-ffi-scalar-type :kind normalized)))

(defun make-ffi-pointer-type (pointee &key (borrowed-p t) nullable-p)
  "Construct a typed foreign pointer."
  (%make-ffi-pointer-type :pointee pointee :borrowed-p borrowed-p :nullable-p nullable-p))

(defun make-ffi-callback-type (argument-types return-type)
  "Construct a typed callback signature."
  (%make-ffi-callback-type :argument-types argument-types :return-type return-type))

(defun make-ffi-function-descriptor (name argument-types return-type &key (abi :c))
  "Construct a typed function descriptor."
  (%make-ffi-function-descriptor :name name
                                 :argument-types argument-types
                                 :return-type return-type
                                 :abi abi))

(defun ffi-type-valid-p (descriptor)
  "Return T when DESCRIPTOR is a recursively valid FFI type descriptor."
  (cond
    ((ffi-scalar-type-p descriptor)
     (member (ffi-scalar-type-kind descriptor) +ffi-scalar-kinds+ :test #'eq))
    ((ffi-pointer-type-p descriptor)
     (ffi-type-valid-p (ffi-pointer-type-pointee descriptor)))
    ((ffi-callback-type-p descriptor)
     (and (every #'ffi-type-valid-p (ffi-callback-type-argument-types descriptor))
          (ffi-type-valid-p (ffi-callback-type-return-type descriptor))))
    ((ffi-function-descriptor-p descriptor)
     (and (or (symbolp (ffi-function-descriptor-name descriptor))
              (stringp (ffi-function-descriptor-name descriptor)))
          (every #'ffi-type-valid-p (ffi-function-descriptor-argument-types descriptor))
          (ffi-type-valid-p (ffi-function-descriptor-return-type descriptor))))
    (t nil)))

(defun ffi-lisp-type-compatible-p (lisp-type descriptor)
  "Return T when LISP-TYPE is compatible with DESCRIPTOR."
  (cond
    ((ffi-scalar-type-p descriptor)
     (case (ffi-scalar-type-kind descriptor)
       (:int (member lisp-type '(integer fixnum) :test #'eq))
       (:float (member lisp-type '(single-float float) :test #'eq))
       (:double (member lisp-type '(double-float float) :test #'eq))
       (:string (eq lisp-type 'string))
       (:bool (member lisp-type '(boolean bool) :test #'eq))
       (:void t)
       (t t)))
    ((ffi-pointer-type-p descriptor)
     (member lisp-type '(system-area-pointer foreign-pointer pointer) :test #'eq))
    ((ffi-callback-type-p descriptor)
     (eq lisp-type 'function))
    (t nil)))

(defun ffi-descriptor-form-valid-p (value)
  "Return T when VALUE is a well-formed raw FR-2103 descriptor form."
  (cond
    ((typep value 'type-node) t)
    ((atom value) t)
    ((not (consp value)) nil)
    ((member (string-upcase (symbol-name (first value))) '("C-INT" "C-STRING" "C-PTR") :test #'string=)
     (= (length value) 2))
    ((string= (string-upcase (symbol-name (first value))) "C-CALLBACK")
     (and (= (length value) 2)
          (second value)))
    (t (every #'ffi-descriptor-form-valid-p (rest value)))))

(defun %ffi-symbol-name (value)
  "Return VALUE's uppercase symbolic name, or NIL."
  (cond
    ((symbolp value) (string-upcase (symbol-name value)))
    ((stringp value) (string-upcase value))
    (t nil)))

(defun %ffi-scalar-kind-from-name (name)
  "Map a raw C descriptor NAME to an FFI scalar keyword."
  (let ((normalized (and name
                         (if (and (> (length name) 2)
                                  (string= (subseq name 0 2) "C-"))
                             (subseq name 2)
                             name))))
    (cdr (assoc normalized
                '(("VOID" . :void) ("CHAR" . :char) ("SHORT" . :short)
                  ("INT" . :int) ("INTEGER" . :int) ("LONG" . :long)
                  ("FLOAT" . :float) ("DOUBLE" . :double)
                  ("STRING" . :string) ("BOOL" . :bool) ("BOOLEAN" . :bool)
                  ("SIZE-T" . :size-t))
                :test #'string=))))

(defun ffi-descriptor-from-form (value)
  "Normalize raw FFI descriptor VALUE into structured FFI descriptor objects."
  (cond
    ((or (ffi-scalar-type-p value)
         (ffi-pointer-type-p value)
         (ffi-callback-type-p value)
         (ffi-function-descriptor-p value))
     value)
    ((or (symbolp value) (stringp value))
     (let ((kind (%ffi-scalar-kind-from-name (%ffi-symbol-name value))))
       (unless kind
         (error 'ffi-validation-error :detail (format nil "unknown FFI scalar descriptor ~S" value)))
       (make-ffi-scalar-type kind)))
    ((consp value)
     (let* ((head (first value))
            (head-name (%ffi-symbol-name head)))
       (cond
         ((and head-name
               (member head-name '("C-PTR" "PTR" "POINTER" "FOREIGN-POINTER") :test #'string=))
          (unless (= (length value) 2)
            (error 'ffi-validation-error :detail (format nil "pointer descriptor needs pointee: ~S" value)))
          (make-ffi-pointer-type (ffi-descriptor-from-form (second value))))
         ((and head-name (string= head-name "C-CALLBACK"))
          (unless (>= (length value) 3)
            (error 'ffi-validation-error :detail (format nil "callback descriptor needs args and return: ~S" value)))
          (make-ffi-callback-type (mapcar #'ffi-descriptor-from-form (second value))
                                  (ffi-descriptor-from-form (third value))))
         ((and head-name
               (member head-name '("FOREIGN" "FOREIGN-FUNCTION" "FFI-FUNCTION") :test #'string=))
          (unless (= (length value) 4)
            (error 'ffi-validation-error :detail (format nil "foreign function descriptor needs name, args, return: ~S" value)))
          (make-ffi-function-descriptor (second value)
                                        (mapcar #'ffi-descriptor-from-form (third value))
                                        (ffi-descriptor-from-form (fourth value))))
         ((%ffi-scalar-kind-from-name head-name)
          (ffi-descriptor-from-form head))
         (t
          (error 'ffi-validation-error :detail (format nil "unknown FFI descriptor form ~S" value))))))
    (t
     (error 'ffi-validation-error :detail (format nil "unsupported FFI descriptor ~S" value)))))

(defun ffi-descriptor-lisp-type (descriptor)
  "Return the CL-CC type-node produced by DESCRIPTOR."
  (let ((normalized (ffi-descriptor-from-form descriptor)))
    (cond
      ((ffi-scalar-type-p normalized)
       (case (ffi-scalar-type-kind normalized)
         ((:int :short :long :size-t) type-int)
         ((:float :double) type-float)
         (:string type-string)
         (:bool type-bool)
         (:void type-null)
         (:char type-char)
         (t type-any)))
      ((ffi-pointer-type-p normalized)
       (make-type-primitive :name 'foreign-pointer))
      ((ffi-callback-type-p normalized)
       (make-type-arrow (mapcar #'ffi-descriptor-lisp-type
                                (ffi-callback-type-argument-types normalized))
                        (ffi-descriptor-lisp-type
                         (ffi-callback-type-return-type normalized))))
      ((ffi-function-descriptor-p normalized)
       (make-type-arrow (mapcar #'ffi-descriptor-lisp-type
                                (ffi-function-descriptor-argument-types normalized))
                        (ffi-descriptor-lisp-type
                         (ffi-function-descriptor-return-type normalized))))
      (t type-any))))

(defstruct (type-interface-module (:constructor %make-type-interface-module))
  "A loaded FR-2405 interface summary."
  (name nil)
  (exports nil :type list)
  (fingerprint nil)
  (exported-types nil :type list))

(defvar *type-interface-registry* (make-hash-table :test #'equal)
  "Maps module names to loaded type interface summaries.")

(defvar *type-interface-export-registry* (make-hash-table :test #'equal)
  "Maps exported names to monomorphic imported type schemes.")

(defun %type-interface-key (name)
  (if (symbolp name) (string-upcase (symbol-name name)) name))

(defun %type-interface-export-name (export)
  (if (and (consp export) (symbolp (first export)))
      (first export)
      export))

(defun %type-interface-export-type (export)
  (if (and (consp export) (second export))
      (parse-type-specifier (second export))
      type-any))

(defun register-type-interface (module exports fingerprint)
  "Register MODULE's exported type interface and return its summary."
  (let* ((exported-types (mapcar (lambda (export)
                                   (cons (%type-interface-export-name export)
                                         (%type-interface-export-type export)))
                                 exports))
         (summary (%make-type-interface-module :name module
                                               :exports (mapcar #'%type-interface-export-name exports)
                                               :fingerprint fingerprint
                                               :exported-types exported-types)))
    (setf (gethash (%type-interface-key module) *type-interface-registry*) summary)
    (dolist (entry exported-types)
      (setf (gethash (%type-interface-key (car entry)) *type-interface-export-registry*)
            (type-to-scheme (cdr entry))))
    summary))

(defun lookup-type-interface (module)
  "Return the loaded interface summary for MODULE, if present."
  (gethash (%type-interface-key module) *type-interface-registry*))

(defun lookup-type-interface-export (name)
  "Return (values scheme found-p) for an imported interface export NAME."
  (let ((scheme (gethash (%type-interface-key name) *type-interface-export-registry*)))
    (values scheme (not (null scheme)))))

(defvar *smt-solver-registry* (make-hash-table :test #'equal)
  "Maps SMT solver names to local request handlers.")

(defun register-smt-solver (name function)
  "Register FUNCTION as the SMT request handler for NAME."
  (setf (gethash (%type-interface-key name) *smt-solver-registry*) function))

(defun lookup-smt-solver (name)
  "Return the SMT solver handler for NAME, if present."
  (gethash (%type-interface-key name) *smt-solver-registry*))

(defun solve-smt-constraint (constraint solver theory)
  "Dispatch an SMT constraint request through the registered solver handler."
  (let ((handler (lookup-smt-solver solver)))
    (unless handler
      (error "No SMT solver registered for ~S" solver))
    (funcall handler constraint theory)))

(defun %default-smt-solver (constraint theory)
  "Deterministic local SMT shim used for type-checker integration tests."
  (list :status :unknown :constraint constraint :theory theory :counterexample :none))

(defvar *type-checker-plugin-registry* (make-hash-table :test #'equal)
  "Maps (plugin . phase) to type-checker plugin handlers.")

(defun %type-plugin-key (plugin phase)
  (cons (%type-interface-key plugin) (%type-interface-key phase)))

(defun register-type-checker-plugin (plugin phase function)
  "Register FUNCTION as PLUGIN's PHASE hook."
  (setf (gethash (%type-plugin-key plugin phase) *type-checker-plugin-registry*) function))

(defun lookup-type-checker-plugin (plugin phase)
  "Return PLUGIN's PHASE hook, if present."
  (gethash (%type-plugin-key plugin phase) *type-checker-plugin-registry*))

(defun run-type-checker-plugin (plugin phase ast arg-types env)
  "Run a registered type-checker plugin hook."
  (let ((handler (lookup-type-checker-plugin plugin phase)))
    (unless handler
      (error "No type-checker plugin registered for ~S phase ~S" plugin phase))
    (funcall handler ast arg-types env)))

(defun %default-type-plugin (_ast _arg-types _env)
  "Default plugin hook result for built-in plugin smoke paths."
  (declare (ignore _ast _arg-types _env))
  (list :status :ok :type type-any))

(defvar *type-synthesis-strategy-registry* (make-hash-table :test #'equal)
  "Maps synthesis strategy names to candidate search handlers.")

(defun register-type-synthesis-strategy (strategy function)
  "Register FUNCTION as the handler for synthesis STRATEGY."
  (setf (gethash (%type-interface-key strategy) *type-synthesis-strategy-registry*) function))

(defun lookup-type-synthesis-strategy (strategy)
  "Return the synthesis handler for STRATEGY, if present."
  (gethash (%type-interface-key strategy) *type-synthesis-strategy-registry*))

(defun run-type-synthesis (signature strategy fuel)
  "Run type-directed synthesis for SIGNATURE using STRATEGY and FUEL."
  (let ((handler (lookup-type-synthesis-strategy strategy)))
    (unless handler
      (error "No type synthesis strategy registered for ~S" strategy))
    (funcall handler signature fuel)))

(defun %default-type-synthesis (signature fuel)
  "Deterministic bounded synthesis shim returning a typed candidate summary."
  (list :status (if (plusp fuel) :candidate :exhausted)
        :signature signature
        :fuel fuel
        :candidate (when (plusp fuel) '(lambda (&rest args) (declare (ignore args)) nil))))

(defun %initialize-advanced-dispatch-registries ()
  "Install deterministic local handlers for advanced integration shims."
  (register-smt-solver 'z3 #'%default-smt-solver)
  (register-smt-solver 'cvc5 #'%default-smt-solver)
  (register-type-checker-plugin 'nat-normalise 'solve #'%default-type-plugin)
  (register-type-checker-plugin 'nat-normalise 'rewrite #'%default-type-plugin)
  (register-type-synthesis-strategy 'enumerative #'%default-type-synthesis)
  (register-type-synthesis-strategy 'refinement #'%default-type-synthesis)
  (register-type-synthesis-strategy 'proof-search #'%default-type-synthesis)
  t)


(defstruct (qtt-binding (:constructor %make-qtt-binding))
  "A QTT binding carrying a concrete multiplicity."
  (name nil)
  (type nil)
  (multiplicity :omega))

(defun normalize-multiplicity (value)
  "Normalize VALUE into one of :ZERO, :ONE, or :OMEGA."
  (cond
    ((member value '(0 :zero zero) :test #'equal) :zero)
    ((member value '(1 :one one) :test #'equal) :one)
    ((member value '(:omega :ω omega unrestricted :unrestricted) :test #'equal) :omega)
    (t (error "Unsupported QTT multiplicity: ~S" value))))

(defun valid-multiplicity-p (value)
  "Return T when VALUE is a QTT multiplicity designator."
  (handler-case
      (progn (normalize-multiplicity value) t)
    (error () nil)))

(defun multiplicity<= (left right)
  "Return T when LEFT is no more permissive than RIGHT."
  (mult-leq (normalize-multiplicity left) (normalize-multiplicity right)))

(defun multiplicity+ (left right)
  "Return the additive composition of LEFT and RIGHT in the QTT semiring."
  (mult-add (normalize-multiplicity left) (normalize-multiplicity right)))

(defun multiplicity* (left right)
  "Return the multiplicative composition of LEFT and RIGHT in the QTT semiring."
  (mult-mul (normalize-multiplicity left) (normalize-multiplicity right)))

(defun multiplicity-zero-p (value)
  "Return T when VALUE denotes erased usage."
  (eq (normalize-multiplicity value) :zero))

(defun multiplicity-one-p (value)
  "Return T when VALUE denotes linear usage."
  (eq (normalize-multiplicity value) :one))

(defun multiplicity-unrestricted-p (value)
  "Return T when VALUE denotes unrestricted usage."
  (eq (normalize-multiplicity value) :omega))

(defun usage-satisfies-multiplicity-p (declared actual-uses)
  "Return T when ACTUAL-USES satisfies DECLARED."
  (let ((normalized (normalize-multiplicity declared)))
    (and (integerp actual-uses)
         (not (minusp actual-uses))
         (case normalized
           (:zero (= actual-uses 0))
           (:one (= actual-uses 1))
           (:omega t)))))

(defun make-qtt-binding (name type multiplicity)
  "Construct a QTT binding with normalized multiplicity."
  (%make-qtt-binding :name name :type type :multiplicity (normalize-multiplicity multiplicity)))

(defun qtt-erased-p (binding)
  "Return T when BINDING is erased at runtime."
  (and (qtt-binding-p binding)
       (multiplicity-zero-p (qtt-binding-multiplicity binding))))


(defstruct (finite-semiring (:constructor %make-finite-semiring))
  "A finite semiring with explicit carrier and operations."
  (name nil)
  (elements nil :type list)
  (zero nil)
  (one nil)
  (add nil)
  (multiply nil)
  (preorder nil))

(defstruct (graded-value (:constructor %make-graded-value))
  "A payload annotated with a semiring grade."
  (grade nil)
  (payload nil)
  (semiring nil))

(defun make-finite-semiring (&key name elements zero one add multiply preorder)
  "Construct a finite semiring description."
  (%make-finite-semiring :name name
                         :elements (copy-list elements)
                         :zero zero
                         :one one
                         :add add
                         :multiply multiply
                         :preorder preorder))

(defun grade-designator-p (value)
  "Return T when VALUE is an accepted graded-type grade designator."
  (or (valid-multiplicity-p value)
      (and (integerp value) (not (minusp value)))))

(defun finite-semiring-valid-p (semiring)
  "Check basic semiring and preorder laws over the finite carrier."
  (let ((elements (finite-semiring-elements semiring))
        (add (finite-semiring-add semiring))
        (multiply (finite-semiring-multiply semiring))
        (zero (finite-semiring-zero semiring))
        (one (finite-semiring-one semiring))
        (preorder (finite-semiring-preorder semiring)))
    (and (member zero elements :test #'equal)
         (member one elements :test #'equal)
         (every (lambda (a)
                  (and (member (funcall add a zero) elements :test #'equal)
                       (member (funcall add zero a) elements :test #'equal)
                       (member (funcall multiply a one) elements :test #'equal)
                       (member (funcall multiply one a) elements :test #'equal)
                       (equal (funcall add a zero) a)
                       (equal (funcall add zero a) a)
                       (equal (funcall multiply a one) a)
                       (equal (funcall multiply one a) a)
                       (equal (funcall multiply a zero) zero)
                       (equal (funcall multiply zero a) zero)))
                elements)
         (every (lambda (a)
                  (every (lambda (b)
                           (and (member (funcall add a b) elements :test #'equal)
                                (member (funcall multiply a b) elements :test #'equal)
                                (equal (funcall add a b) (funcall add b a))))
                         elements))
                elements)
         (every (lambda (a)
                  (every (lambda (b)
                           (every (lambda (c)
                                    (and (equal (funcall add a (funcall add b c))
                                                (funcall add (funcall add a b) c))
                                         (equal (funcall multiply a (funcall multiply b c))
                                                (funcall multiply (funcall multiply a b) c))
                                         (equal (funcall multiply a (funcall add b c))
                                                (funcall add (funcall multiply a b)
                                                             (funcall multiply a c)))
                                         (equal (funcall multiply (funcall add a b) c)
                                                (funcall add (funcall multiply a c)
                                                             (funcall multiply b c)))))
                                  elements))
                         elements))
                elements)
         (or (null preorder)
             (and (every (lambda (a) (funcall preorder a a)) elements)
                  (every (lambda (a)
                           (every (lambda (b)
                                    (every (lambda (c)
                                             (or (not (and (funcall preorder a b)
                                                           (funcall preorder b c)))
                                                 (funcall preorder a c)))
                                           elements))
                                  elements))
                         elements))))))

(defun make-graded-value (grade payload semiring)
  "Construct a graded payload after validating GRADE against SEMIRING."
  (unless (member grade (finite-semiring-elements semiring) :test #'equal)
    (error "Grade ~S is not in semiring carrier ~S" grade (finite-semiring-elements semiring)))
  (%make-graded-value :grade grade :payload payload :semiring semiring))

(defun grade<= (left right semiring)
  "Compare LEFT and RIGHT using SEMIRING's preorder."
  (let ((preorder (finite-semiring-preorder semiring)))
    (unless preorder
      (error "Semiring ~S does not define a preorder" (finite-semiring-name semiring)))
    (funcall preorder left right)))

(defun graded-add (left right &optional (combiner #'list))
  "Combine LEFT and RIGHT using semiring addition."
  (unless (eq (graded-value-semiring left) (graded-value-semiring right))
    (error "Cannot add graded values over different semirings"))
  (let ((semiring (graded-value-semiring left)))
    (%make-graded-value :grade (funcall (finite-semiring-add semiring)
                                        (graded-value-grade left)
                                        (graded-value-grade right))
                        :payload (funcall combiner
                                          (graded-value-payload left)
                                          (graded-value-payload right))
                        :semiring semiring)))

(defun graded-compose (left right &optional (combiner #'list))
  "Compose LEFT and RIGHT using semiring multiplication."
  (unless (eq (graded-value-semiring left) (graded-value-semiring right))
    (error "Cannot compose graded values over different semirings"))
  (let ((semiring (graded-value-semiring left)))
    (%make-graded-value :grade (funcall (finite-semiring-multiply semiring)
                                        (graded-value-grade left)
                                        (graded-value-grade right))
                        :payload (funcall combiner
                                          (graded-value-payload left)
                                          (graded-value-payload right))
                        :semiring semiring)))

(defun make-qtt-semiring ()
  "Return the canonical finite semiring underlying QTT."
  (make-finite-semiring
   :name :qtt
   :elements '(:zero :one :omega)
   :zero :zero
   :one :one
   :add #'multiplicity+
   :multiply #'multiplicity*
   :preorder #'multiplicity<=))


(defstruct (universe-sort (:constructor %make-universe-sort))
  "A universe scaffold for Prop/Set/Type."
  (kind :type)
  (level 0))

(defstruct (cic-proposition (:constructor %make-cic-proposition))
  "A proposition or type tagged with its universe."
  (name nil)
  (universe nil)
  (payload nil))

(defstruct (cic-proof (:constructor %make-cic-proof))
  "A concrete proof witness for a proposition."
  (proposition nil)
  (witness nil)
  (premises nil :type list))

(defstruct (cic-inductive (:constructor %make-cic-inductive))
  "A tiny inductive definition scaffold."
  (name nil)
  (universe nil)
  (constructors nil :type list))

(defun make-universe-sort (kind &optional level)
  "Construct a validated universe sort."
  (let ((normalized (if (keywordp kind) kind (intern (string-upcase (symbol-name kind)) :keyword))))
    (unless (member normalized '(:prop :set :type) :test #'eq)
      (error "Unknown universe kind: ~S" kind))
    (when (eq normalized :type)
      (unless (and (integerp level) (not (minusp level)))
        (error "Type universes require a non-negative level, got ~S" level)))
    (%make-universe-sort :kind normalized :level (or level 0))))

(defun valid-universe-sort-p (universe)
  "Return T when UNIVERSE is well formed."
  (and (universe-sort-p universe)
       (member (universe-sort-kind universe) '(:prop :set :type) :test #'eq)
       (or (not (eq (universe-sort-kind universe) :type))
           (and (integerp (universe-sort-level universe))
                (not (minusp (universe-sort-level universe)))))))

(defun %universe-rank (universe)
  "Return a total ordering rank for UNIVERSE."
  (ecase (universe-sort-kind universe)
    (:prop 0)
    (:set 1)
    (:type (+ 2 (universe-sort-level universe)))))

(defun universe<= (left right)
  "Return T when LEFT is no higher than RIGHT."
  (<= (%universe-rank left) (%universe-rank right)))

(defun max-universe (left right)
  "Return the higher of LEFT and RIGHT."
  (if (universe<= left right) right left))

(defun make-cic-proposition (name universe &optional payload)
  "Construct a validated proposition/type scaffold."
  (unless (valid-universe-sort-p universe)
    (error "Invalid proposition universe: ~S" universe))
  (%make-cic-proposition :name name :universe universe :payload payload))

(defun make-cic-proof (proposition witness &optional premises)
  "Construct a concrete proof witness."
  (%make-cic-proof :proposition proposition :witness witness :premises (copy-list premises)))

(defun cic-proof-valid-p (proof)
  "Return T when PROOF witnesses a proposition in Prop."
  (and (cic-proof-p proof)
       (cic-proposition-p (cic-proof-proposition proof))
       (eq (universe-sort-kind (cic-proposition-universe (cic-proof-proposition proof))) :prop)
       (not (null (cic-proof-witness proof)))))

(defun proof-erasable-p (proof)
  "Return T when PROOF lives in Prop and may be erased."
  (and (cic-proof-valid-p proof)
       (eq (universe-sort-kind (cic-proposition-universe (cic-proof-proposition proof))) :prop)))

(defun make-cic-inductive (name universe constructors)
  "Construct a small inductive scaffold."
  (%make-cic-inductive :name name :universe universe :constructors (copy-list constructors)))

(defun cic-inductive-valid-p (inductive)
  "Return T when INDUCTIVE has a valid universe and unique constructor names."
  (and (cic-inductive-p inductive)
       (valid-universe-sort-p (cic-inductive-universe inductive))
       (not (null (cic-inductive-constructors inductive)))
       (= (length (remove-duplicates (mapcar #'car (cic-inductive-constructors inductive)) :test #'equal))
          (length (cic-inductive-constructors inductive)))))

(defun cic-large-elimination-allowed-p (source-universe target-universe)
  "Return T when eliminating from SOURCE-UNIVERSE into TARGET-UNIVERSE is allowed.
This scaffold forbids large elimination from Prop into computational universes."
  (or (not (eq (universe-sort-kind source-universe) :prop))
      (eq (universe-sort-kind target-universe) :prop)))


(defstruct (termination-evidence (:constructor %make-termination-evidence))
  "Concrete evidence for structural or lexicographic decrease."
  (strategy :structural)
  (measures nil :type list)
  (partial-p nil :type boolean))

(defun %measure-size (value)
  "Return a comparable structural size for VALUE."
  (cond
    ((integerp value) value)
    ((consp value) (length value))
    ((stringp value) (length value))
    ((vectorp value) (length value))
    (t nil)))

(defun structural-decrease-p (previous next)
  "Return T when NEXT is structurally smaller than PREVIOUS."
  (let ((previous-size (%measure-size previous))
        (next-size (%measure-size next)))
    (and previous-size next-size (> previous-size next-size))))

(defun lexicographic-decrease-p (previous next)
  "Return T when NEXT is lexicographically smaller than PREVIOUS."
  (when (= (length previous) (length next))
    (loop for left in previous
          for right in next do
      (cond
        ((eql left right) nil)
        ((< right left) (return t))
        (t (return nil)))
      finally (return nil))))

(defun make-termination-evidence (strategy measures &key partial-p)
  "Construct a termination evidence record."
  (%make-termination-evidence :strategy strategy
                              :measures (copy-list measures)
                              :partial-p partial-p))

(defun termination-evidence-valid-p (evidence)
  "Return T when EVIDENCE proves decreasing recursive progress."
  (and (termination-evidence-p evidence)
       (not (termination-evidence-partial-p evidence))
       (case (termination-evidence-strategy evidence)
         (:structural
          (loop for (left right) on (termination-evidence-measures evidence)
                while right
                always (structural-decrease-p left right)))
         (:lexicographic
          (loop for (left right) on (termination-evidence-measures evidence)
                while right
                always (lexicographic-decrease-p left right)))
         (otherwise nil))))

(defun termination-evidence-form-valid-p (value)
  "Return T when VALUE is a plausible raw termination evidence form."
  (and (consp value)
       (let ((head (first value))
             (strategy (second value)))
         (and (or (eq head :termination)
                  (and (symbolp head)
                       (string= (symbol-name head) "TERMINATION")))
              (or (member strategy '(:structural :lexicographic) :test #'eq)
                  (and (symbolp strategy)
                       (member (string-upcase (symbol-name strategy))
                               '("STRUCTURAL" "LEXICOGRAPHIC")
                               :test #'string=)))))
       (consp (cddr value))))


(defstruct (proof-obligation (:constructor %make-proof-obligation))
  "A machine-checkable obligation."
  (name nil)
  (checker nil)
  (description nil))

(defstruct (proof-evidence (:constructor %make-proof-evidence))
  "Evidence for a named proof obligation."
  (obligation-name nil)
  (payload nil))

(defstruct (proof-carrying-code (:constructor %make-proof-carrying-code))
  "Code paired with proof obligations and evidence."
  (artifact nil)
  (obligations nil :type list)
  (evidence nil :type list))

(defun make-proof-obligation (name checker &optional description)
  "Construct a proof obligation named NAME with CHECKER."
  (%make-proof-obligation :name name :checker checker :description description))

(defun make-proof-evidence (obligation-name payload)
  "Construct evidence for OBLIGATION-NAME."
  (%make-proof-evidence :obligation-name obligation-name :payload payload))

(defun make-proof-carrying-code (artifact obligations evidence)
  "Construct a PCC bundle."
  (%make-proof-carrying-code :artifact artifact
                             :obligations (copy-list obligations)
                             :evidence (copy-list evidence)))

(defun make-nonzero-obligation (name)
  "Return an obligation requiring a non-zero numeric witness."
  (make-proof-obligation name
                         (lambda (payload)
                           (and (numberp payload) (not (zerop payload))))
                         "Payload must be a non-zero number."))

(defun make-type-obligation (name expected-type)
  "Return an obligation requiring PAYLOAD to satisfy EXPECTED-TYPE."
  (make-proof-obligation name
                         (lambda (payload)
                           (typep payload expected-type))
                         (format nil "Payload must satisfy ~S" expected-type)))

(defun verify-proof-obligation (obligation payload)
  "Return T when PAYLOAD satisfies OBLIGATION."
  (and (proof-obligation-p obligation)
       (functionp (proof-obligation-checker obligation))
       (funcall (proof-obligation-checker obligation) payload)))

(defun verify-proof-evidence (obligation evidence)
  "Return T when EVIDENCE discharges OBLIGATION."
  (and (proof-obligation-p obligation)
       (proof-evidence-p evidence)
       (equal (proof-obligation-name obligation)
              (proof-evidence-obligation-name evidence))
       (verify-proof-obligation obligation (proof-evidence-payload evidence))))

(defun verify-proof-carrying-code (bundle)
  "Return T when BUNDLE contains valid evidence for every obligation."
  (and (proof-carrying-code-p bundle)
       (every (lambda (obligation)
                (let ((evidence (find (proof-obligation-name obligation)
                                      (proof-carrying-code-evidence bundle)
                                      :key #'proof-evidence-obligation-name
                                      :test #'equal)))
                  (and evidence (verify-proof-evidence obligation evidence))))
              (proof-carrying-code-obligations bundle))))

(defun proof-evidence-form-valid-p (value)
  "Return T when VALUE is a plausible raw proof-evidence form."
  (and (consp value)
       (let ((head (first value)))
         (or (member head '(:proof :assumption :refl) :test #'eq)
             (and (symbolp head)
                  (member (string-upcase (symbol-name head))
                          '("PROOF" "ASSUMPTION" "REFL")
                          :test #'string=))))
       (consp (rest value))))


;;; ─── Advanced feature metadata / representation ───────────────────────────

(defun canonicalize-type-advanced-feature-id (feature-id)
  "Return FEATURE-ID as an uppercase FR identifier string."
  (cond
    ((stringp feature-id) (string-upcase feature-id))
    ((symbolp feature-id) (string-upcase (symbol-name feature-id)))
    (t (error "Advanced feature id must be a string or symbol: ~S" feature-id))))

(defstruct (type-advanced-feature (:constructor %make-type-advanced-feature))
  "Metadata for a tracked advanced type-system feature section."
  (fr-id "" :type string)
  (title "" :type string)
  (heads nil :type list))

(defun make-type-advanced-feature (&key id title heads)
  "Construct metadata for an advanced type feature section."
  (%make-type-advanced-feature :fr-id (canonicalize-type-advanced-feature-id id)
                               :title title
                               :heads (copy-list heads)))

(defparameter +type-advanced-feature-specs+
  '(("FR-1501" "Null Safety")
    ("FR-1502" "Concurrency Safety Types")
    ("FR-1503" "Information Flow Types")
    ("FR-1504" "Region Types")
    ("FR-1505" "Capability Types")
    ("FR-1601" "Typed Holes")
    ("FR-1602" "Datatype-Generic Programming")
    ("FR-1603" "Effect Inference")
    ("FR-1604" "Value Restriction")
    ("FR-1605" "Type Error Quality")
    ("FR-1606" "Incremental Type Checking")
    ("FR-1701" "Type-Level Natural Numbers")
    ("FR-1702" "Type-Level Strings")
    ("FR-1703" "Multi-Stage Programming")
    ("FR-1704" "Typed Metaprogramming")
    ("FR-1705" "Binding-Time Analysis")
    ("FR-1801" "Lens / Optics")
    ("FR-1802" "Free Monads")
    ("FR-1803" "HLists")
    ("FR-1804" "Type-Safe printf")
    ("FR-1805" "Profunctor / Traversal Types")
    ("FR-1806" "Continuation Types")
    ("FR-1901" "Termination Checking")
    ("FR-1902" "Totality Checking")
    ("FR-1903" "Exhaustiveness Checking")
    ("FR-1904" "Positivity Checking")
    ("FR-1905" "Sized Types")
    ("FR-1906" "Guarded Recursion")
    ("FR-2001" "Curry-Howard")
    ("FR-2002" "Proof-Carrying Code")
    ("FR-2003" "Tactic-Based Type Checking")
    ("FR-2004" "Homotopy Type Theory")
    ("FR-2005" "Normalization by Evaluation")
    ("FR-2101" "Type-Directed Test Generation")
    ("FR-2102" "Type-Directed Serialization")
    ("FR-2103" "Type-Safe FFI")
    ("FR-2104" "Type-Based Fusion")
    ("FR-2105" "Type Inference Performance")
    ("FR-2106" "Type-Safe Embedded DSLs")
    ("FR-2107" "Type-Safe Database Queries")
    ("FR-2201" "Async Types")
    ("FR-2202" "Typed Channels")
    ("FR-2203" "Typed Actors")
    ("FR-2204" "STM Types")
    ("FR-2205" "Coroutine / Generator Types")
    ("FR-2206" "Data Parallel Types")
    ("FR-2301" "Numeric Tower Types")
    ("FR-2302" "Units of Measure")
    ("FR-2303" "Interval Arithmetic Types")
    ("FR-2304" "Tensor Types")
    ("FR-2305" "Precision Types")
    ("FR-2401" "Unification")
    ("FR-2402" "Skolemization")
    ("FR-2403" "Constraint Propagation")
    ("FR-2404" "Visible Type Application")
    ("FR-2405" "Type Interface Files")
    ("FR-2406" "SMT Solver Integration")
    ("FR-2501" "Dynamic Type")
    ("FR-2502" "Runtime Type Representations")
    ("FR-2503" "Template Literal Types")
    ("FR-2504" "Type Guards")
    ("FR-2505" "Typed Regular Expressions")
    ("FR-2601" "Quantified Constraints")
    ("FR-2602" "Constraint Kinds")
    ("FR-2603" "Type Class Witnesses")
    ("FR-2604" "DerivingVia")
    ("FR-2605" "Implicit Function Types")
    ("FR-2606" "Match Types")
    ("FR-2701" "Quantum Types")
    ("FR-2702" "Probabilistic Types")
    ("FR-2703" "Differentiable Types")
    ("FR-2704" "Hardware Types")
    ("FR-2705" "Type-Safe Configuration")
    ("FR-2706" "Typed State Machines")
    ("FR-2801" "Axiomatic Semantics")
    ("FR-2802" "Categorical Semantics")
    ("FR-2803" "Game Semantics")
    ("FR-2804" "Abstract Interpretation")
    ("FR-2805" "Behavioral Subtyping")
    ("FR-2901" "Strictness Analysis")
    ("FR-2902" "Type-Based Alias Analysis")
    ("FR-2903" "Boxity Analysis")
    ("FR-2904" "Arity Analysis")
    ("FR-2905" "Usage Analysis")
    ("FR-3001" "Custom Type Errors")
    ("FR-3002" "Type Checking Plugins")
    ("FR-3003" "Type-Directed Program Synthesis")
    ("FR-3004" "Cross-Language Type Mapping")
    ("FR-3005" "Type-Based Documentation")
    ("FR-3101" "Linear Logic Types")
    ("FR-3102" "Bang Types")
    ("FR-3103" "Bounded Linear Logic")
    ("FR-3104" "Structural Rules as Types")
    ("FR-3201" "Algebraic Subtyping")
    ("FR-3202" "Polar Types")
    ("FR-3203" "Path-Dependent Types")
    ("FR-3204" "Self Type")
    ("FR-3205" "Branded Types")
    ("FR-3301" "Mapped Types")
    ("FR-3302" "Conditional Types")
    ("FR-3303" "Readonly Types")
    ("FR-3304" "Utility Types")
    ("FR-3305" "Type-Safe Routing")
    ("FR-3401" "Quantitative Type Theory")
    ("FR-3402" "Graded Types")
    ("FR-3403" "Data Encodings")
    ("FR-3404" "Extensible Effects")
    ("FR-3405" "Intensional vs Extensional Type Theory")
    ("FR-3406" "Calculus of Inductive Constructions"))
  "Exact advanced FR id list represented by the type system.")

(defparameter +type-advanced-head-specs+
  '((async-function . "FR-2201") (future . "FR-2201")
    (channel . "FR-2202") (actor . "FR-2203") (stm . "FR-2204")
    (coroutine . "FR-2205") (generator . "FR-2205")
    (data-parallel . "FR-2206") (simd . "FR-2206")
    (numeric-tower . "FR-2301") (units-of-measure . "FR-2302")
    (interval . "FR-2303") (tensor . "FR-2304") (precision-type . "FR-2305")
    (dynamic . "FR-2501") (type-rep . "FR-2502") (typerep . "FR-2502")
    (template-literal . "FR-2503") (regex-type . "FR-2505")
    (quantified-constraint . "FR-2601") (constraint-kind . "FR-2602")
    (dict . "FR-2603") (type-witness . "FR-2603")
    (implicit-function . "FR-2605") (match-type . "FR-2606") (type-match . "FR-2606")
    (quantum-type . "FR-2701") (distribution . "FR-2702")
    (differentiable . "FR-2703") (hardware-type . "FR-2704")
    (config-type . "FR-2705") (state-machine . "FR-2706")
    (linear-logic . "FR-3101") (bang-type . "FR-3102")
    (complexity-type . "FR-3103") (structural-rules . "FR-3104")
    (algebraic-subtype . "FR-3201") (path-dependent . "FR-3203")
    (self-type . "FR-3204") (this-type . "FR-3204") (brand-type . "FR-3205")
    (mapped-type . "FR-3301") (conditional-type . "FR-3302")
    (deep-readonly . "FR-3303") (readonly . "FR-3303")
    (exclude-type . "FR-3304") (extract-type . "FR-3304")
    (non-nullable-type . "FR-3304") (omit-type . "FR-3304")
    (partial-type . "FR-3304") (pick-type . "FR-3304")
    (required-type . "FR-3304") (return-type-of . "FR-3304")
    (api-type . "FR-3305") (type-safe-routing . "FR-3305")
    (qtt . "FR-3401") (graded . "FR-3402")
    (church-encoding . "FR-3403") (scott-encoding . "FR-3403")
    (parigot-encoding . "FR-3403") (eff . "FR-3404") (open-union . "FR-3404")
    (type-theory-equality . "FR-3405") (cic . "FR-3406")
    (type-safe-ffi . "FR-2103"))
  "Representative surface heads for advanced feature families.")

(defvar *type-advanced-feature-registry* (make-hash-table :test #'equal)
  "Maps canonical advanced FR ids to type-advanced-feature metadata.")

(defvar *type-advanced-head-registry* (make-hash-table :test #'equal)
  "Maps uppercase representative surface heads to canonical advanced FR ids.")

(defun register-type-advanced-feature (feature)
  "Register FEATURE metadata and return FEATURE."
  (let* ((canonical-id (canonicalize-type-advanced-feature-id
                        (type-advanced-feature-fr-id feature)))
         (canonical-feature
           (make-type-advanced-feature
            :id canonical-id
            :title (type-advanced-feature-title feature)
            :heads (type-advanced-feature-heads feature))))
    (setf (gethash canonical-id *type-advanced-feature-registry*) canonical-feature)
    canonical-feature))

(defun lookup-type-advanced-feature (feature-id)
  "Return metadata for FEATURE-ID, or NIL when it is not registered."
  (gethash (canonicalize-type-advanced-feature-id feature-id)
           *type-advanced-feature-registry*))

(defun list-type-advanced-features ()
  "Return all registered advanced feature metadata sorted by FR id."
  (let (features)
    (maphash (lambda (_id feature)
               (declare (ignore _id))
               (push feature features))
             *type-advanced-feature-registry*)
    (sort features #'string< :key #'type-advanced-feature-fr-id)))

(defun list-type-advanced-feature-ids ()
  "Return all registered advanced FR ids in ascending order."
  (mapcar #'type-advanced-feature-fr-id
          (list-type-advanced-features)))

(defun register-type-advanced-head (head feature-id)
  "Associate HEAD with FEATURE-ID in the representative surface-head table."
  (let* ((key (string-upcase (if (symbolp head) (symbol-name head) head)))
         (canonical-id (canonicalize-type-advanced-feature-id feature-id))
         (feature (or (lookup-type-advanced-feature canonical-id)
                      (error "Unknown advanced feature id for head ~S: ~A" head canonical-id))))
    (setf (gethash key *type-advanced-head-registry*) canonical-id)
    (pushnew key (type-advanced-feature-heads feature) :test #'string=)
    canonical-id))

(defun type-advanced-feature-id-for-head (head)
  "Return the registered FR id for HEAD, or NIL when HEAD is not known."
  (when (or (symbolp head) (stringp head))
    (let ((key (string-upcase (if (symbolp head) (symbol-name head) head))))
      (gethash key *type-advanced-head-registry*))))

(defun type-advanced-head-p (head)
  "Return T when HEAD names a registered advanced feature surface form."
  (and (or (symbolp head) (stringp head))
       (let ((name (string-upcase (if (symbolp head) (symbol-name head) head))))
         (or (string= name "ADVANCED")
             (not (null (gethash name *type-advanced-head-registry*)))))))

(defstruct (type-advanced (:include type-node) (:constructor %make-type-advanced))
  "Validated carrier for advanced feature families."
  (feature-id "" :type string)
  (name 'advanced :type symbol)
  (args nil :type list)
  (properties nil :type list)
  (evidence nil))

(defun %type-advanced-property-sort-key (entry)
  "Return a stable textual sort key for an advanced property ENTRY."
  (let ((key (if (consp entry) (car entry) entry)))
    (if (symbolp key)
        (symbol-name key)
        (prin1-to-string key))))

(defun %type-advanced-normalize-properties (properties)
  "Return PROPERTIES in deterministic alist-key order."
  (stable-sort (copy-list properties)
               #'string<
               :key #'%type-advanced-property-sort-key))

(defun %type-advanced-normalize-graded-keyword-arg (feature-id args properties)
  "Turn (graded :grade T) parsed as a keyword property back into positional args."
  (if (and (member feature-id '("FR-3401" "FR-3402") :test #'string=)
           (null args)
           (= (length properties) 1))
      (let ((entry (first properties)))
        (values (list (car entry) (cdr entry)) nil))
      (values args properties)))

(defun type-advanced-property (advanced property &optional default)
  "Return ADVANCED's PROPERTY value, or DEFAULT when absent."
  (let ((cell (assoc property (type-advanced-properties advanced) :test #'equal)))
    (if cell (cdr cell) default)))

(defun type-advanced-property-present-p (advanced property)
  "Return T when ADVANCED contains PROPERTY."
  (not (null (assoc property (type-advanced-properties advanced) :test #'equal))))

(defparameter +type-advanced-security-label-order+
  +security-label-order+
  "Information-flow lattice from least to most confidential.")

(defun %type-advanced-normalize-symbol-keyword (value)
  "Normalize VALUE into a keyword by symbol/string name when possible."
  (cond
    ((keywordp value) value)
    ((symbolp value) (intern (symbol-name value) :keyword))
    ((stringp value) (intern (string-upcase value) :keyword))
    (t value)))

(defun %type-advanced-label-rank (label)
  "Return LABEL's rank in the advanced information-flow lattice."
  (security-label-rank label))

(defun type-advanced-security-label<= (source target)
  "Return T when information at SOURCE may flow to TARGET."
  (security-label<= source target))

(defun %type-advanced-symbol-name-in-p (value names)
  "Return T when VALUE is a symbol whose name is in NAMES."
  (and (symbolp value)
       (member (symbol-name value) names :test #'string=)))

(defun %type-advanced-head-name (value)
  "Return VALUE's list-head symbol name when VALUE is a cons form."
  (and (consp value)
       (symbolp (first value))
       (symbol-name (first value))))

(defun %type-advanced-payload-security-label (value)
  "Return the explicit security label encoded by VALUE, if any."
  (let ((head (%type-advanced-head-name value)))
    (cond
      ((null head) nil)
      ((string= head "PUBLIC") :public)
      ((string= head "TRUSTED") :trusted)
      ((string= head "TAINTED") :tainted)
      ((string= head "SECRET") :secret)
      ((string= head "TOP-SECRET") :top-secret)
      (t nil))))

(defun %type-advanced-multiplicity-p (value)
  "Return T when VALUE is a supported multiplicity/grade designator."
  (grade-designator-p value))

(defun %type-advanced-route-method-p (value)
  "Return T when VALUE names a supported HTTP route method."
  (%type-advanced-symbol-name-in-p value
                                   '("GET" "POST" "PUT" "PATCH" "DELETE" "HEAD" "OPTIONS")))

(defun type-advanced-route-p (value)
  "Return T when VALUE is a well-formed route payload for FR-3305."
  (route-form-valid-p value))

(defstruct (type-advanced-contract (:constructor %make-type-advanced-contract))
  "Explicit per-FR semantic validation contract for an advanced feature."
  (feature-id "" :type string)
  (semantic-domain nil)
  (exact-args nil)
  (min-args nil)
  (required-properties nil :type list)
  (property-predicates nil :type list)
  (requires-evidence-p nil :type boolean)
  (evidence-predicate nil)
  (custom-validator nil)
  (summary "" :type string))

(defvar *type-advanced-contract-registry* (make-hash-table :test #'equal)
  "Maps canonical advanced FR ids to explicit semantic contracts.")

(defun make-type-advanced-contract (&key id semantic-domain exact-args min-args
                                      required-properties property-predicates
                                      requires-evidence-p evidence-predicate
                                      custom-validator summary)
  "Construct an explicit semantic contract for advanced FEATURE-ID."
  (let ((feature-id (canonicalize-type-advanced-feature-id id)))
    (unless semantic-domain
      (error "Advanced contract ~A requires a semantic domain" feature-id))
    (unless (or exact-args min-args required-properties property-predicates
                requires-evidence-p evidence-predicate custom-validator)
      (error "Advanced contract ~A must encode at least one validation rule" feature-id))
    (%make-type-advanced-contract :feature-id feature-id
                                  :semantic-domain semantic-domain
                                  :exact-args exact-args
                                  :min-args min-args
                                  :required-properties (copy-list required-properties)
                                  :property-predicates (copy-tree property-predicates)
                                  :requires-evidence-p requires-evidence-p
                                  :evidence-predicate evidence-predicate
                                  :custom-validator custom-validator
                                  :summary (or summary ""))))

(defun %type-advanced-contract-spec (id semantic-domain &rest options)
  "Build a plist specification for the contract covering FEATURE-ID."
  (list* :id id :semantic-domain semantic-domain options))

(defun %type-advanced-contract-specs (ids semantic-domain &rest options)
  "Build contract plist specs for each id in IDS."
  (mapcar (lambda (id)
            (apply #'%type-advanced-contract-spec id semantic-domain options))
          ids))

(defun register-type-advanced-contract (contract)
  "Register CONTRACT and return it."
  (let ((feature-id (type-advanced-contract-feature-id contract)))
    (unless (lookup-type-advanced-feature feature-id)
      (error "Cannot register a contract for unknown advanced feature id ~A" feature-id))
    (when (gethash feature-id *type-advanced-contract-registry*)
      (error "Duplicate advanced contract for ~A" feature-id))
    (setf (gethash feature-id *type-advanced-contract-registry*) contract)
    contract))

(defun lookup-type-advanced-contract (feature-id)
  "Return the explicit semantic contract registered for FEATURE-ID, or NIL."
  (gethash (canonicalize-type-advanced-feature-id feature-id)
           *type-advanced-contract-registry*))

(defun %type-advanced-invalid (advanced format-control &rest args)
  "Signal a semantic validation error for ADVANCED."
  (error "Invalid advanced type ~A (~A): ~?"
         (type-advanced-feature-id advanced)
         (type-advanced-name advanced)
         format-control
         args))

(defun %type-advanced-require-arg-count (advanced count)
  "Require ADVANCED to have exactly COUNT positional args."
  (unless (= (length (type-advanced-args advanced)) count)
    (%type-advanced-invalid advanced "expected exactly ~D positional arg(s), got ~D"
                            count (length (type-advanced-args advanced)))))

(defun %type-advanced-require-min-args (advanced count)
  "Require ADVANCED to have at least COUNT positional args."
  (unless (>= (length (type-advanced-args advanced)) count)
    (%type-advanced-invalid advanced "expected at least ~D positional arg(s), got ~D"
                            count (length (type-advanced-args advanced)))))

(defun %type-advanced-resolve-checker (designator)
  "Return a callable predicate/validator for DESIGNATOR."
  (etypecase designator
    (null nil)
    (function designator)
    (symbol (symbol-function designator))))

(defun %type-advanced-contract-property-key (entry)
  "Return the property key encoded by contract ENTRY."
  (if (consp entry) (car entry) entry))

(defun %type-advanced-contract-property-predicate (entry)
  "Return the predicate designator encoded by contract ENTRY."
  (if (consp entry) (cdr entry) nil))

(defun %type-advanced-symbol-name-member-p (value names)
  "Return T when VALUE's textual name is a member of NAMES."
  (and (or (symbolp value) (stringp value))
       (member (string-upcase (if (stringp value) value (symbol-name value)))
               names
               :test #'string=)))

(defun %type-advanced-symbolic-designator-p (value)
  "Return T when VALUE is a symbol/string designator used by semantic metadata."
  (or (symbolp value) (stringp value)))

(defun %type-advanced-boolean-value-p (value)
  "Return T when VALUE is an explicit boolean."
  (or (null value)
      (eq value t)
      (and (type-primitive-p value)
           (member (type-primitive-name value) '(t nil) :test #'eq))))

(defun %type-advanced-positive-integer-p (value)
  "Return T when VALUE is a positive integer."
  (and (integerp value) (plusp value)))

(defun %type-advanced-non-empty-symbolic-list-p (value)
  "Return T when VALUE is a non-empty list of unique symbolic designators."
  (and (listp value)
       value
       (every #'%type-advanced-symbolic-designator-p value)
       (= (length value)
          (length (remove-duplicates value :test #'equal)))))

(defun %type-advanced-interface-export-list-p (value)
  "Return T when VALUE is a non-empty list of unique interface export specs."
  (labels ((export-name (entry)
             (if (and (consp entry) (symbolp (first entry)))
                 (first entry)
                 entry))
           (export-entry-p (entry)
             (or (%type-advanced-symbolic-designator-p entry)
                 (and (consp entry)
                      (symbolp (first entry))
                      (second entry)))))
    (and (listp value)
         value
         (every #'export-entry-p value)
         (let ((names (mapcar #'export-name value)))
           (= (length names)
              (length (remove-duplicates names :test #'equal)))))))

(defun %type-advanced-stage-designator-p (value)
  "Return T when VALUE denotes a supported staging level."
  (or (member value '(0 1) :test #'eql)
      (member (%type-advanced-normalize-symbol-keyword value)
              '(:runtime :code)
              :test #'eq)))

(defun %type-advanced-stage-transition-p (value)
  "Return T when VALUE denotes a supported stage transition."
  (member (%type-advanced-normalize-symbol-keyword value)
          '(:quote :splice :run)
          :test #'eq))

(defun %type-advanced-generator-form-p (value)
  "Return T when VALUE is a plausible type-directed generator descriptor."
  (and (consp value)
       (symbolp (first value))
       (member (string-upcase (symbol-name (first value)))
               '("ARBITRARY" "ENUM" "FUZZ" "SIZED" "GENERATOR")
               :test #'string=)
       (>= (length value) 2)))

(defun %type-advanced-fingerprint-p (value)
  "Return T when VALUE is a plausible interface fingerprint token."
  (or (and (stringp value) (> (length value) 0))
      (integerp value)
      (%type-advanced-symbolic-designator-p value)))

(defun %type-advanced-smt-solver-p (value)
  "Return T when VALUE names a supported SMT solver integration target."
  (or (%type-advanced-symbol-name-member-p value '("Z3" "CVC5"))
      (not (null (lookup-smt-solver value)))))

(defun %type-advanced-smt-theory-p (value)
  "Return T when VALUE names a supported SMT theory."
  (%type-advanced-symbol-name-member-p value '("LIA" "BITVEC" "UF" "ARRAY")))

(defun %type-advanced-plugin-phase-p (value)
  "Return T when VALUE names a supported plugin hook phase."
  (%type-advanced-symbol-name-member-p value '("SOLVE" "REWRITE")))

(defun %type-advanced-synthesis-strategy-p (value)
  "Return T when VALUE names a supported synthesis strategy."
  (or (%type-advanced-symbol-name-member-p value '("ENUMERATIVE" "REFINEMENT" "PROOF-SEARCH"))
      (not (null (lookup-type-synthesis-strategy value)))))

(defun %type-advanced-mapped-transform-p (value)
  "Return T when VALUE names a supported mapped-type transform."
  (%type-advanced-symbol-name-member-p value
                                       '("OPTIONAL" "READONLY" "REQUIRED" "PARTIAL"
                                         "PICK" "OMIT" "RECORD" "EXCLUDE"
                                         "EXTRACT" "NON-NULLABLE")))

(defun %type-advanced-encoding-kind-p (value)
  "Return T when VALUE names a supported functional data encoding."
  (%type-advanced-symbol-name-member-p value '("CHURCH" "SCOTT" "PARIGOT")))

(defun %type-advanced-equality-mode-p (value)
  "Return T when VALUE names a supported equality-checking mode."
  (%type-advanced-symbol-name-member-p value '("INTENSIONAL" "EXTENSIONAL" "OBSERVATIONAL")))

(defun %type-advanced-pointerish-form-p (value)
  "Return T when VALUE resembles a typed pointer/reference descriptor."
  (and (consp value)
       (symbolp (first value))
       (member (string-upcase (symbol-name (first value)))
               '("POINTER" "PTR" "FOREIGN-POINTER" "C-PTR" "SLOT-REF" "CAR-REF")
               :test #'string=)
       (>= (length value) 2)))

(defun %type-advanced-effect-label-list-p (value)
  "Return T when VALUE is a non-empty list of unique effect labels."
  (%type-advanced-non-empty-symbolic-list-p value))

(defun %type-advanced-optic-form-p (value)
  "Return T when VALUE is a plausible lens/prism/traversal descriptor."
  (and (consp value)
       (symbolp (first value))
       (let ((head (string-upcase (symbol-name (first value)))))
         (cond
           ((or (string= head "LENS") (string= head "PRISM"))
            (= (length value) 5))
           ((string= head "TRAVERSAL")
            (>= (length value) 3))
           (t nil)))))

(defun %type-advanced-staged-form-p (value)
  "Return T when VALUE is a plausible code/splice/run descriptor."
  (and (consp value)
       (symbolp (first value))
       (member (string-upcase (symbol-name (first value)))
               '("CODE" "QUOTE" "SPLICE" "RUN")
               :test #'string=)
       (>= (length value) 2)))

(defun %type-advanced-validate-information-flow (advanced)
  "Validate FR-1503 information-flow labels and declassification evidence."
  (%type-advanced-require-min-args advanced 1)
  (let* ((source (%type-advanced-payload-security-label (first (type-advanced-args advanced))))
         (target (type-advanced-property advanced :flow)))
    (when (and target (null (%type-advanced-label-rank target)))
      (%type-advanced-invalid advanced "unknown information-flow target label ~S" target))
    (when (and source target
               (not (type-advanced-security-label<= source target))
               (null (type-advanced-evidence advanced)))
      (%type-advanced-invalid advanced "flow from ~S to ~S requires declassification evidence"
                              source target))))

(defun %type-advanced-validate-type-safe-ffi (advanced)
  "Validate FR-2103 typed FFI descriptors at the boundary shape level."
  (%type-advanced-require-min-args advanced 1)
  (dolist (arg (type-advanced-args advanced))
    (unless (ffi-descriptor-form-valid-p arg)
      (%type-advanced-invalid advanced "malformed FFI descriptor ~S" arg))))

(defun %type-advanced-validate-route (advanced)
  "Validate FR-3305 API route payloads."
  (%type-advanced-require-min-args advanced 1)
  (dolist (route (type-advanced-args advanced))
    (unless (type-advanced-route-p route)
      (%type-advanced-invalid advanced "malformed route payload ~S" route))))

(defun %type-advanced-validate-proof-like (advanced)
  "Validate proof/totality features that require machine-checkable evidence."
  (%type-advanced-require-min-args advanced 1)
  (let ((evidence (type-advanced-evidence advanced))
        (feature-id (type-advanced-feature-id advanced)))
    (unless evidence
      (%type-advanced-invalid advanced "requires explicit proof/termination evidence"))
    (when (member feature-id '("FR-1901" "FR-1902" "FR-1903" "FR-1904" "FR-1905" "FR-1906")
                  :test #'string=)
      (unless (or (termination-evidence-p evidence)
                  (termination-evidence-form-valid-p evidence)
                  (proof-evidence-form-valid-p evidence))
        (%type-advanced-invalid advanced "malformed totality evidence ~S" evidence)))
    (when (member feature-id '("FR-2001" "FR-2002" "FR-2003" "FR-2004" "FR-2005" "FR-3406")
                  :test #'string=)
      (unless (or (proof-evidence-form-valid-p evidence)
                  (cic-proof-valid-p evidence))
        (%type-advanced-invalid advanced "malformed proof evidence ~S" evidence)))))

(defun %type-advanced-validate-incremental-checking (advanced)
  "Validate FR-1606 incremental type-checking cache contracts."
  (let ((dependency-graph (type-advanced-property advanced :dependency-graph))
        (cache (type-advanced-property advanced :cache)))
    (when (equal dependency-graph cache)
      (%type-advanced-invalid advanced
                              "dependency graph and cache descriptors must be distinct, got ~S"
                              dependency-graph))))

(defun %type-advanced-validate-staging (advanced)
  "Validate FR-1703 staged payloads and stage transitions."
  (let* ((payload (first (type-advanced-args advanced)))
         (stage (type-advanced-property advanced :stage))
         (transition (%type-advanced-normalize-symbol-keyword
                      (type-advanced-property advanced :transition))))
    (unless (or (typep payload 'type-node)
                (%type-advanced-staged-form-p payload))
      (%type-advanced-invalid advanced
                              "staging payload must be a code/run/splice form or type node, got ~S"
                              payload))
    (when (member transition '(:run :splice) :test #'eq)
      (unless (type-advanced-evidence advanced)
        (%type-advanced-invalid advanced
                                "~S transition requires stage-safety evidence"
                                transition)))
    (when (and (eq transition :run)
               (not (or (eql stage 1)
                        (eq (%type-advanced-normalize-symbol-keyword stage) :code))))
      (%type-advanced-invalid advanced
                              ":run transition requires stage 1 / :code input, got ~S"
                              stage))))

(defun %type-advanced-validate-optics (advanced)
  "Validate FR-1801 optic descriptors."
  (let ((payload (first (type-advanced-args advanced))))
    (unless (%type-advanced-optic-form-p payload)
      (%type-advanced-invalid advanced
                              "optics payload must be a lens/prism/traversal descriptor, got ~S"
                              payload))))

(defun %type-advanced-validate-test-generation (advanced)
  "Validate FR-2101 type-directed generator configuration."
  (let ((coverage-target (type-advanced-property advanced :coverage-target))
        (samples (type-advanced-property advanced :samples)))
    (when (and samples (> samples coverage-target))
      (%type-advanced-invalid advanced
                              ":samples must not exceed :coverage-target (~S > ~S)"
                              samples
                              coverage-target))))

(defun %type-advanced-validate-interface-files (advanced)
  "Validate FR-2405 interface-file cache metadata."
  (let ((exports (type-advanced-property advanced :exports)))
    (unless (= (length exports)
               (length (remove-duplicates exports :test #'equal)))
      (%type-advanced-invalid advanced
                              "interface exports must be unique, got ~S"
                              exports))))

(defun %type-advanced-validate-smt-integration (advanced)
  "Validate FR-2406 SMT integration metadata."
  (unless (or (type-advanced-evidence advanced)
              (type-advanced-property-present-p advanced :counterexample))
    (%type-advanced-invalid advanced
                            "SMT integration requires either proof evidence or a :counterexample payload")))

(defun %type-advanced-validate-abstract-interpretation (advanced)
  "Validate FR-2804 abstract-interpretation descriptors."
  (let ((widening (type-advanced-property advanced :widening))
        (narrowing (type-advanced-property advanced :narrowing)))
    (when (and (type-advanced-property-present-p advanced :narrowing)
               (equal widening narrowing))
      (%type-advanced-invalid advanced
                              "widening and narrowing descriptors must differ, got ~S"
                              widening))))

(defun %type-advanced-validate-alias-analysis (advanced)
  "Validate FR-2902 alias-analysis descriptors."
  (let ((left (first (type-advanced-args advanced)))
        (right (second (type-advanced-args advanced)))
        (disjoint (type-advanced-property advanced :disjoint)))
    (unless (%type-advanced-pointerish-form-p left)
      (%type-advanced-invalid advanced "left alias operand must be pointer-like, got ~S" left))
    (unless (%type-advanced-pointerish-form-p right)
      (%type-advanced-invalid advanced "right alias operand must be pointer-like, got ~S" right))
    (when (and disjoint (equal left right))
      (%type-advanced-invalid advanced
                              "disjoint alias operands must not be structurally identical: ~S"
                              left))))

(defun %type-advanced-validate-plugins (advanced)
  "Validate FR-3002 plugin hook descriptors."
  (unless (%type-advanced-symbolic-designator-p (first (type-advanced-args advanced)))
    (%type-advanced-invalid advanced "plugin descriptor must start with a symbolic plugin name")))

(defun %type-advanced-validate-synthesis (advanced)
  "Validate FR-3003 synthesis strategy descriptors."
  (let ((strategy (%type-advanced-normalize-symbol-keyword
                   (type-advanced-property advanced :search))))
    (when (and (eq strategy :proof-search)
               (null (type-advanced-evidence advanced)))
      (%type-advanced-invalid advanced
                              "proof-search synthesis requires evidence describing the proof search goal"))))

(defun %type-advanced-validate-brand (advanced)
  "Validate FR-3205 branded-type payloads."
  (unless (or (symbolp (first (type-advanced-args advanced)))
              (stringp (first (type-advanced-args advanced))))
    (%type-advanced-invalid advanced "brand-type requires a symbolic brand name")))

(defun %type-advanced-validate-mapped-types (advanced)
  "Validate FR-3301 mapped-type transforms."
  (let ((base (first (type-advanced-args advanced)))
        (filter (type-advanced-property advanced :filter :absent)))
    (unless (or (typep base 'type-node)
                (consp base)
                (%type-advanced-symbolic-designator-p base))
      (%type-advanced-invalid advanced
                              "mapped-type base must be a type node or structured type form, got ~S"
                              base))
    (when (and (not (eq filter :absent))
               (not (or (%type-advanced-symbolic-designator-p filter)
                        (consp filter)
                        (typep filter 'type-node))))
      (%type-advanced-invalid advanced
                              ":filter must be a symbolic predicate or type form, got ~S"
                              filter))))

(defun %type-advanced-validate-conditional-types (advanced)
  "Validate FR-3302 conditional/infer-type descriptors."
  (let ((then-branch (type-advanced-property advanced :then))
        (else-branch (type-advanced-property advanced :else)))
    (when (type-advanced-property-present-p advanced :infer)
      (unless (%type-advanced-symbolic-designator-p (type-advanced-property advanced :infer))
        (%type-advanced-invalid advanced ":infer must name a symbolic type variable")))
    (when (equal then-branch else-branch)
      (%type-advanced-invalid advanced
                              "conditional branches must differ to encode a real type split, got ~S"
                              then-branch))))

(defun %type-advanced-validate-encodings (advanced)
  "Validate FR-3403 functional data encoding descriptors."
  (let* ((encoding (%type-advanced-normalize-symbol-keyword
                    (type-advanced-property advanced :encoding)))
         (head-name (string-upcase (symbol-name (type-advanced-name advanced))))
         (expected (cond
                     ((string= head-name "CHURCH-ENCODING") :church)
                     ((string= head-name "SCOTT-ENCODING") :scott)
                     ((string= head-name "PARIGOT-ENCODING") :parigot)
                     (t nil))))
    (when (and expected (not (eq expected encoding)))
      (%type-advanced-invalid advanced
                              "encoding property ~S must agree with surface head ~S"
                              encoding
                              (type-advanced-name advanced)))))

(defun %type-advanced-validate-extensible-effects (advanced)
  "Validate FR-3404 extensible-effect descriptors."
  (let ((effects (first (type-advanced-args advanced))))
    (unless (%type-advanced-effect-label-list-p effects)
      (%type-advanced-invalid advanced
                              "extensible effects require a non-empty list of unique effect labels, got ~S"
                              effects))))

(defun %type-advanced-validate-type-theory-equality (advanced)
  "Validate FR-3405 equality-mode descriptors."
  (let* ((mode (%type-advanced-normalize-symbol-keyword
                (type-advanced-property advanced :mode)))
         (left (first (type-advanced-args advanced)))
         (right (second (type-advanced-args advanced))))
    (cond
      ((eq mode :intensional)
       (unless (type-advanced-payload-equal-p left right)
         (%type-advanced-invalid advanced
                                 "intensional equality requires computationally identical payloads, got ~S and ~S"
                                 left
                                 right)))
      ((member mode '(:extensional :observational) :test #'eq)
       (unless (type-advanced-evidence advanced)
         (%type-advanced-invalid advanced
                                 "~S equality requires supporting evidence"
                                 mode))))))

(defun %type-advanced-validate-qtt (advanced)
  "Validate FR-3401 quantitative multiplicity payloads."
  (unless (valid-multiplicity-p (first (type-advanced-args advanced)))
    (%type-advanced-invalid advanced
                            "unsupported QTT multiplicity ~S"
                            (first (type-advanced-args advanced)))))

(defun %type-advanced-validate-graded (advanced)
  "Validate FR-3402 graded-type multiplicity payloads."
  (unless (%type-advanced-multiplicity-p (first (type-advanced-args advanced)))
    (%type-advanced-invalid advanced
                            "unsupported multiplicity/grade ~S"
                            (first (type-advanced-args advanced)))))

(defparameter +type-advanced-contract-specs+
  (append
   (%type-advanced-contract-specs '("FR-1501" "FR-1502" "FR-1504" "FR-1505")
                                  :safety
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-1503" :safety
                                       :min-args 1
                                       :custom-validator '%type-advanced-validate-information-flow))
   (%type-advanced-contract-specs '("FR-1601" "FR-1602" "FR-1603" "FR-1604" "FR-1605")
                                  :inference
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-1606" :inference
                                       :min-args 1
                                       :required-properties '(:dependency-graph :cache)
                                       :property-predicates '((:dependency-graph . %type-advanced-symbolic-designator-p)
                                                              (:cache . %type-advanced-symbolic-designator-p)
                                                              (:lsp . %type-advanced-boolean-value-p))
                                       :custom-validator '%type-advanced-validate-incremental-checking))
   (%type-advanced-contract-specs '("FR-1701" "FR-1702" "FR-1704" "FR-1705")
                                  :type-level-programming
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-1703" :type-level-programming
                                       :exact-args 1
                                       :required-properties '(:stage :transition)
                                       :property-predicates '((:stage . %type-advanced-stage-designator-p)
                                                              (:transition . %type-advanced-stage-transition-p))
                                       :custom-validator '%type-advanced-validate-staging))
   (%type-advanced-contract-specs '("FR-1802" "FR-1803" "FR-1804" "FR-1805" "FR-1806")
                                  :functional-abstractions
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-1801" :functional-abstractions
                                       :exact-args 1
                                       :required-properties '(:lawful)
                                       :property-predicates '((:lawful . %type-advanced-boolean-value-p))
                                       :custom-validator '%type-advanced-validate-optics))
   (%type-advanced-contract-specs '("FR-1901" "FR-1902" "FR-1903" "FR-1904" "FR-1905" "FR-1906")
                                  :totality
                                  :min-args 1
                                  :requires-evidence-p t
                                  :custom-validator '%type-advanced-validate-proof-like)
   (%type-advanced-contract-specs '("FR-2001" "FR-2002" "FR-2003" "FR-2004" "FR-2005")
                                  :proofs
                                  :min-args 1
                                  :requires-evidence-p t
                                  :custom-validator '%type-advanced-validate-proof-like)
   (%type-advanced-contract-specs '("FR-2102" "FR-2104" "FR-2105" "FR-2106" "FR-2107")
                                  :type-directed-runtime
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2101" :type-directed-runtime
                                       :min-args 1
                                       :required-properties '(:generator :coverage-target)
                                       :property-predicates '((:generator . %type-advanced-generator-form-p)
                                                              (:coverage-target . %type-advanced-positive-integer-p)
                                                              (:samples . %type-advanced-positive-integer-p))
                                       :custom-validator '%type-advanced-validate-test-generation)
         (%type-advanced-contract-spec "FR-2103" :type-directed-runtime
                                       :min-args 1
                                       :property-predicates '((:abi . %type-advanced-symbolic-designator-p))
                                       :custom-validator '%type-advanced-validate-type-safe-ffi))
   (%type-advanced-contract-specs '("FR-2201" "FR-2202" "FR-2203" "FR-2204" "FR-2205" "FR-2206")
                                  :concurrency
                                  :min-args 1)
   (%type-advanced-contract-specs '("FR-2301" "FR-2303" "FR-2304" "FR-2305")
                                  :numeric
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2302" :numeric
                                       :exact-args 1
                                       :required-properties '(:unit)
                                       :property-predicates '((:unit . unit-designator-p))))
   (%type-advanced-contract-specs '("FR-2401" "FR-2402" "FR-2403" "FR-2404")
                                  :constraints
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2405" :constraints
                                       :min-args 1
                                       :required-properties '(:exports :fingerprint)
                                        :property-predicates '((:exports . %type-advanced-interface-export-list-p)
                                                              (:fingerprint . %type-advanced-fingerprint-p))
                                       :custom-validator '%type-advanced-validate-interface-files)
         (%type-advanced-contract-spec "FR-2406" :constraints
                                       :min-args 1
                                       :required-properties '(:solver :theory)
                                       :property-predicates '((:solver . %type-advanced-smt-solver-p)
                                                              (:theory . %type-advanced-smt-theory-p))
                                       :custom-validator '%type-advanced-validate-smt-integration))
   (%type-advanced-contract-specs '("FR-2503" "FR-2504" "FR-2505")
                                  :gradual-runtime
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2501" :gradual-runtime :exact-args 1)
         (%type-advanced-contract-spec "FR-2502" :gradual-runtime :exact-args 1))
   (%type-advanced-contract-specs '("FR-2601" "FR-2602" "FR-2604" "FR-2605" "FR-2606")
                                  :typeclasses
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2603" :typeclasses :exact-args 1))
   (%type-advanced-contract-specs '("FR-2701" "FR-2702" "FR-2703" "FR-2704" "FR-2705" "FR-2706")
                                  :domain-specific
                                  :min-args 1)
   (%type-advanced-contract-specs '("FR-2801" "FR-2802" "FR-2803" "FR-2805")
                                  :semantics
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2804" :semantics
                                       :exact-args 1
                                       :required-properties '(:domain :widening)
                                       :property-predicates '((:domain . %type-advanced-symbolic-designator-p)
                                                              (:widening . %type-advanced-symbolic-designator-p)
                                                              (:narrowing . %type-advanced-symbolic-designator-p))
                                       :custom-validator '%type-advanced-validate-abstract-interpretation))
   (%type-advanced-contract-specs '("FR-2901" "FR-2903" "FR-2904" "FR-2905")
                                  :analysis
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2902" :analysis
                                       :exact-args 2
                                       :required-properties '(:disjoint :alias-class)
                                       :property-predicates '((:disjoint . %type-advanced-boolean-value-p)
                                                              (:alias-class . %type-advanced-symbolic-designator-p))
                                       :custom-validator '%type-advanced-validate-alias-analysis))
   (%type-advanced-contract-specs '("FR-3001" "FR-3004" "FR-3005")
                                  :tooling
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-3002" :tooling
                                       :min-args 1
                                       :required-properties '(:hook :phase)
                                       :property-predicates '((:hook . %type-advanced-symbolic-designator-p)
                                                              (:phase . %type-advanced-plugin-phase-p))
                                       :custom-validator '%type-advanced-validate-plugins)
         (%type-advanced-contract-spec "FR-3003" :tooling
                                       :min-args 1
                                       :required-properties '(:search :fuel)
                                       :property-predicates '((:search . %type-advanced-synthesis-strategy-p)
                                                              (:fuel . %type-advanced-positive-integer-p))
                                       :custom-validator '%type-advanced-validate-synthesis))
   (%type-advanced-contract-specs '("FR-3102" "FR-3103" "FR-3104")
                                  :linear-logic
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-3101" :linear-logic :min-args 3))
   (%type-advanced-contract-specs '("FR-3201" "FR-3202" "FR-3203" "FR-3204")
                                  :subtyping
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-3205" :subtyping
                                       :exact-args 2
                                       :custom-validator '%type-advanced-validate-brand))
   (list (%type-advanced-contract-spec "FR-3301" :structural-programming
                                       :exact-args 1
                                       :required-properties '(:transform)
                                       :property-predicates '((:transform . %type-advanced-mapped-transform-p))
                                       :custom-validator '%type-advanced-validate-mapped-types)
         (%type-advanced-contract-spec "FR-3302" :structural-programming
                                       :exact-args 1
                                       :required-properties '(:extends :then :else)
                                       :property-predicates '((:infer . %type-advanced-symbolic-designator-p))
                                       :custom-validator '%type-advanced-validate-conditional-types)
         (%type-advanced-contract-spec "FR-3303" :structural-programming :exact-args 1)
         (%type-advanced-contract-spec "FR-3304" :structural-programming :exact-args 1)
         (%type-advanced-contract-spec "FR-3305" :structural-programming
                                       :min-args 1
                                       :custom-validator '%type-advanced-validate-route))
   (list (%type-advanced-contract-spec "FR-3401" :dependent-foundations
                                       :exact-args 2
                                       :custom-validator '%type-advanced-validate-qtt)
         (%type-advanced-contract-spec "FR-3402" :dependent-foundations
                                       :exact-args 2
                                       :custom-validator '%type-advanced-validate-graded)
         (%type-advanced-contract-spec "FR-3403" :dependent-foundations
                                       :exact-args 1
                                       :required-properties '(:encoding)
                                       :property-predicates '((:encoding . %type-advanced-encoding-kind-p))
                                       :custom-validator '%type-advanced-validate-encodings)
         (%type-advanced-contract-spec "FR-3404" :dependent-foundations
                                       :exact-args 2
                                       :custom-validator '%type-advanced-validate-extensible-effects)
         (%type-advanced-contract-spec "FR-3405" :dependent-foundations
                                       :exact-args 2
                                       :required-properties '(:mode)
                                       :property-predicates '((:mode . %type-advanced-equality-mode-p))
                                       :custom-validator '%type-advanced-validate-type-theory-equality)
         (%type-advanced-contract-spec "FR-3406" :dependent-foundations
                                       :min-args 1
                                       :requires-evidence-p t
                                       :custom-validator '%type-advanced-validate-proof-like)))
  "Explicit per-FR semantic contracts for every tracked advanced feature id.")

(defun %make-type-advanced-contract-from-spec (spec)
  "Construct a semantic contract object from plist SPEC."
  (apply #'make-type-advanced-contract spec))

(defun type-advanced-semantic-domain (feature-id)
  "Return the explicit semantic domain attached to FEATURE-ID's contract."
  (let ((contract (lookup-type-advanced-contract feature-id)))
    (and contract (type-advanced-contract-semantic-domain contract))))

(defun type-advanced-semantics-implemented-p (feature-id)
  "Return T when FEATURE-ID has an explicit semantic contract."
  (not (null (lookup-type-advanced-contract feature-id))))

(defun %type-advanced-validate-contract (advanced contract)
  "Validate ADVANCED against the explicit per-FR CONTRACT."
  (let ((exact-args (type-advanced-contract-exact-args contract))
        (min-args (type-advanced-contract-min-args contract)))
    (when exact-args
      (%type-advanced-require-arg-count advanced exact-args))
    (when (and min-args (null exact-args))
      (%type-advanced-require-min-args advanced min-args)))
  (dolist (property (type-advanced-contract-required-properties contract))
    (unless (type-advanced-property-present-p advanced property)
      (%type-advanced-invalid advanced "missing required property ~S" property)))
  (dolist (entry (type-advanced-contract-property-predicates contract))
    (let ((property (%type-advanced-contract-property-key entry))
          (predicate (%type-advanced-contract-property-predicate entry)))
      (when (type-advanced-property-present-p advanced property)
        (let ((value (type-advanced-property advanced property)))
          (unless (funcall (%type-advanced-resolve-checker predicate) value)
            (%type-advanced-invalid advanced
                                    "property ~S failed contract predicate ~S with value ~S"
                                    property
                                    predicate
                                    value))))))
  (let ((evidence (type-advanced-evidence advanced))
        (evidence-predicate (type-advanced-contract-evidence-predicate contract)))
    (when (and (type-advanced-contract-requires-evidence-p contract)
               (null evidence))
      (%type-advanced-invalid advanced "requires explicit semantic evidence"))
    (when (and evidence evidence-predicate
               (not (funcall (%type-advanced-resolve-checker evidence-predicate) evidence)))
      (%type-advanced-invalid advanced
                              "evidence failed contract predicate ~S with value ~S"
                              evidence-predicate
                              evidence)))
  (let ((custom-validator (type-advanced-contract-custom-validator contract)))
    (when custom-validator
      (funcall (%type-advanced-resolve-checker custom-validator) advanced)))
  advanced)

(defun %type-advanced-validate-by-feature (advanced)
  "Apply the explicit semantic contract registered for ADVANCED."
  (let ((contract (lookup-type-advanced-contract (type-advanced-feature-id advanced))))
    (unless contract
      (%type-advanced-invalid advanced "no semantic contract is implemented"))
    (%type-advanced-validate-contract advanced contract)))

(defun validate-type-advanced (advanced)
  "Validate ADVANCED against its registered feature and explicit semantic contract."
  (unless (type-advanced-p advanced)
    (error "Expected type-advanced node, got ~S" advanced))
  (unless (lookup-type-advanced-feature (type-advanced-feature-id advanced))
    (%type-advanced-invalid advanced "unregistered feature id"))
  (%type-advanced-validate-by-feature advanced)
  advanced)

(defun type-advanced-valid-p (advanced)
  "Return T when ADVANCED passes semantic validation."
  (handler-case
      (progn (validate-type-advanced advanced) t)
    (error () nil)))

(defun make-type-advanced (&key feature-id (name 'advanced) args properties evidence)
  "Construct a type-advanced node for FEATURE-ID."
  (let ((feature (or (lookup-type-advanced-feature feature-id)
                     (error "Unknown advanced feature id: ~S" feature-id))))
    (multiple-value-bind (normalized-args normalized-properties)
        (%type-advanced-normalize-graded-keyword-arg
         (type-advanced-feature-fr-id feature)
         args
         (%type-advanced-normalize-properties properties))
      (validate-type-advanced
       (%make-type-advanced :feature-id (type-advanced-feature-fr-id feature)
                            :name name
                            :args (copy-list normalized-args)
                            :properties normalized-properties
                            :evidence evidence)))))

(defun make-type-dynamic (value-type &key properties evidence)
  "Construct a Dynamic feature node (FR-2501) around VALUE-TYPE."
  (make-type-advanced :feature-id "FR-2501"
                      :name 'dynamic
                      :args (list value-type)
                      :properties properties
                      :evidence evidence))

(defun make-type-type-rep (represented-type &key properties evidence)
  "Construct a TypeRep feature node (FR-2502) around REPRESENTED-TYPE."
  (make-type-advanced :feature-id "FR-2502"
                      :name 'typerep
                      :args (list represented-type)
                      :properties properties
                      :evidence evidence))

(defun type-advanced-payload-children (value)
  "Return all type-node values nested inside VALUE."
  (cond
    ((typep value 'type-node) (list value))
    ((consp value)
     (append (type-advanced-payload-children (car value))
             (type-advanced-payload-children (cdr value))))
    (t nil)))

(defun type-advanced-payload-map (fn value)
  "Map FN over each type-node nested inside VALUE, preserving outer shape."
  (cond
    ((typep value 'type-node) (funcall fn value))
    ((consp value)
     (cons (type-advanced-payload-map fn (car value))
           (type-advanced-payload-map fn (cdr value))))
    (t value)))

(defun type-advanced-payload-equal-p (left right)
  "Return T when LEFT and RIGHT are structurally equal advanced payloads."
  (cond
    ((and (typep left 'type-node) (typep right 'type-node))
     (type-equal-p left right))
    ((or (typep left 'type-node) (typep right 'type-node)) nil)
    ((and (consp left) (consp right))
     (and (type-advanced-payload-equal-p (car left) (car right))
          (type-advanced-payload-equal-p (cdr left) (cdr right))))
    (t (equal left right))))

(defun %initialize-type-advanced-contract-registry ()
  "Populate the explicit advanced semantic contract registry."
  (clrhash *type-advanced-contract-registry*)
  (dolist (spec +type-advanced-contract-specs+)
    (register-type-advanced-contract
     (%make-type-advanced-contract-from-spec spec)))
  t)

(defun %ensure-type-advanced-contract-coverage ()
  "Ensure every tracked advanced feature id has exactly one explicit contract."
  (let* ((feature-ids (mapcar #'first +type-advanced-feature-specs+))
         (missing (remove-if #'lookup-type-advanced-contract feature-ids))
         (orphan nil))
    (maphash (lambda (feature-id _contract)
               (declare (ignore _contract))
               (unless (member feature-id feature-ids :test #'string=)
                 (push feature-id orphan)))
             *type-advanced-contract-registry*)
    (when missing
      (error "Missing advanced semantic contract(s) for ~{~A~^, ~}" missing))
    (when orphan
      (error "Orphan advanced semantic contract(s): ~{~A~^, ~}" (sort orphan #'string<)))
    (unless (= (hash-table-count *type-advanced-contract-registry*)
               (length feature-ids))
      (error "Advanced contract registry count (~D) does not match feature registry count (~D)"
             (hash-table-count *type-advanced-contract-registry*)
             (length feature-ids)))
    t))

(defun %initialize-type-advanced-feature-registry ()
  "Populate the advanced feature and surface-head registries."
  (clrhash *type-advanced-feature-registry*)
  (clrhash *type-advanced-head-registry*)
  (dolist (spec +type-advanced-feature-specs+)
    (register-type-advanced-feature
     (make-type-advanced-feature :id (first spec) :title (second spec))))
  (dolist (spec +type-advanced-head-specs+)
    (register-type-advanced-head (car spec) (cdr spec)))
  (%initialize-type-advanced-contract-registry)
  (%ensure-type-advanced-contract-coverage)
  t)

(%initialize-type-advanced-feature-registry)

;;; ─── type-effect-row ──────────────────────────────────────────────────────

(defstruct (type-effect-row (:include type-node))
  "An effect row <ε₁, …, εₙ | ρ>.
EFFECTS: list of type-effect-op (concrete effect labels/applications).
ROW-VAR: an open row variable (type-var) or nil for closed rows."
  (effects nil :type list)
  (row-var nil))

(defvar +pure-effect-row+ (make-type-effect-row :effects nil :row-var nil)
  "The empty (pure) closed effect row.")

;;; ─── type-effect-op ───────────────────────────────────────────────────────

(defstruct (type-effect-op (:include type-node))
  "A single (possibly applied) effect label.
NAME: the effect name symbol (e.g., 'IO, 'STATE).
ARGS: type arguments (e.g., (list type-int) for (State Int))."
  (name nil :type symbol)
  (args nil :type list))

;;; ─── type-handler ─────────────────────────────────────────────────────────

(defstruct (type-handler (:include type-node))
  "The type of an effect handler.
EFFECT:   the effect being handled (type-effect-op).
INPUT:    the type of the handled computation's result.
OUTPUT:   the overall return type after handling."
  (effect nil)
  (input  nil)
  (output nil))

;;; ─── type-gadt-con ────────────────────────────────────────────────────────

(defstruct (type-gadt-con (:include type-node))
  "A GADT constructor type — carries its index constraint.
NAME:       the constructor name symbol.
ARG-TYPES:  list of argument types.
INDEX-TYPE: the index type (the return type's type parameter)."
  (name       nil :type symbol)
  (arg-types  nil :type list)
  (index-type nil))

;;; ─── type-constraint ──────────────────────────────────────────────────────

(defstruct (type-constraint (:include type-node))
  "A typeclass constraint C T — the constraint that T has an instance of class C.
CLASS-NAME: the typeclass symbol (e.g., 'EQ, 'NUM, 'SHOW).
TYPE-ARG:   the constrained type (often a type-var)."
  (class-name nil :type symbol)
  (type-arg   nil))

;;; ─── type-qualified ───────────────────────────────────────────────────────

(defstruct (type-qualified (:include type-node)
                           (:constructor %make-type-qualified-raw))
  "A type qualified by typeclass constraints: (C₁ a, C₂ b) => T.
CONSTRAINTS: list of type-constraint.
BODY:        the underlying type."
  (constraints nil :type list)
  (body        nil))

(defun make-type-qualified (&key constraints body)
  "Constructor for type-qualified using the canonical :body slot."
  (%make-type-qualified-raw :constraints constraints :body body))

;;; ─── type-error ───────────────────────────────────────────────────────────

(defstruct (type-error (:include type-node))
  "Error sentinel produced during type inference for error recovery.
MESSAGE: a human-readable description of the type error."
  (message "" :type string))

;;; ─── Well-known primitive singletons ──────────────────────────────────────

(defvar type-int     (make-type-primitive :name 'fixnum)  "fixnum type.")
(defvar type-float   (make-type-primitive :name 'float)   "float type.")
(defvar type-string  (make-type-primitive :name 'string)  "string type.")
(defvar type-bool    (make-type-primitive :name 'boolean) "boolean type.")
(defvar type-symbol  (make-type-primitive :name 'symbol)  "symbol type.")
(defvar type-cons    (make-type-primitive :name 'cons)    "cons type.")
(defvar type-null    (make-type-primitive :name 'null)    "null/nil type.")
(defvar type-any     (make-type-primitive :name 't)       "top type (any).")
(defvar type-char    (make-type-primitive :name 'character) "character type.")
(defvar type-unit    (make-type-primitive :name 'null)    "unit type (alias null).")

(%initialize-advanced-dispatch-registries)

;;; ─── Effect row singletons ────────────────────────────────────────────────

(defvar +io-effect-row+
  (make-type-effect-row
   :effects (list (make-type-effect-op :name 'io))
   :row-var nil)
  "Closed IO effect row.")

;;; ─── Type equality (structural) ───────────────────────────────────────────

(defun %type-list-equal-p (ls1 ls2)
  "True iff LS1 and LS2 have equal length and pairwise type-equal-p elements."
  (and (= (length ls1) (length ls2))
       (every #'type-equal-p ls1 ls2)))

(defun type-equal-p (t1 t2)
  "Structural equality of two types (no substitution applied).
Type-error nodes are never considered equal (each represents a distinct error point),
even if they happen to be the same object."
  (cond
    ((or (type-error-p t1) (type-error-p t2)) nil)   ; errors always distinct
    ((eq t1 t2) t)
    ((and (type-primitive-p t1) (type-primitive-p t2))
     (eq (type-primitive-name t1) (type-primitive-name t2)))
    ((and (type-var-p t1)   (type-var-p t2))   (type-var-equal-p t1 t2))
    ((and (type-rigid-p t1) (type-rigid-p t2)) (type-rigid-equal-p t1 t2))
    ((and (type-arrow-p t1) (type-arrow-p t2))
     (and (%type-list-equal-p (type-arrow-params t1) (type-arrow-params t2))
          (type-equal-p (type-arrow-return t1) (type-arrow-return t2))
          (eq (type-arrow-mult t1) (type-arrow-mult t2))))
    ((and (type-product-p t1)      (type-product-p t2))
     (%type-list-equal-p (type-product-elems t1) (type-product-elems t2)))
    ((and (type-union-p t1)        (type-union-p t2))
     (%type-list-equal-p (type-union-types t1) (type-union-types t2)))
    ((and (type-intersection-p t1) (type-intersection-p t2))
     (%type-list-equal-p (type-intersection-types t1) (type-intersection-types t2)))
    ((and (type-forall-p t1) (type-forall-p t2))
     (and (type-var-equal-p (type-forall-var t1) (type-forall-var t2))
          (type-equal-p (type-forall-body t1) (type-forall-body t2))))
    ((and (type-exists-p t1) (type-exists-p t2))
     (and (type-var-equal-p (type-exists-var t1) (type-exists-var t2))
          (type-equal-p (type-exists-body t1) (type-exists-body t2))))
    ((and (type-app-p t1) (type-app-p t2))
     (and (type-equal-p (type-app-fun t1) (type-app-fun t2))
          (type-equal-p (type-app-arg t1) (type-app-arg t2))))
    ((and (type-mu-p t1) (type-mu-p t2))
     (and (type-var-equal-p (type-mu-var t1) (type-mu-var t2))
          (type-equal-p (type-mu-body t1) (type-mu-body t2))))
    ((and (type-linear-p t1) (type-linear-p t2))
     (and (eq (type-linear-grade t1) (type-linear-grade t2))
          (type-equal-p (type-linear-base t1) (type-linear-base t2))))
    ((and (type-refinement-p t1) (type-refinement-p t2))
     (and (type-equal-p (type-refinement-base t1) (type-refinement-base t2))
          (equal (type-refinement-predicate t1) (type-refinement-predicate t2))))
    ((and (type-effect-row-p t1) (type-effect-row-p t2))
     (and (%type-list-equal-p (type-effect-row-effects t1) (type-effect-row-effects t2))
          (let ((rv1 (type-effect-row-row-var t1))
                (rv2 (type-effect-row-row-var t2)))
            (if (and rv1 rv2) (type-equal-p rv1 rv2) (eq rv1 rv2)))))
    ((and (type-effect-op-p t1) (type-effect-op-p t2))
     (and (eq (type-effect-op-name t1) (type-effect-op-name t2))
          (%type-list-equal-p (type-effect-op-args t1) (type-effect-op-args t2))))
    ((and (type-advanced-p t1) (type-advanced-p t2))
     (and (string= (type-advanced-feature-id t1) (type-advanced-feature-id t2))
          (type-advanced-payload-equal-p (type-advanced-args t1)
                                         (type-advanced-args t2))
          (type-advanced-payload-equal-p (type-advanced-properties t1)
                                         (type-advanced-properties t2))
          (type-advanced-payload-equal-p (type-advanced-evidence t1)
                                         (type-advanced-evidence t2))))
    ((and (type-constraint-p t1) (type-constraint-p t2))
     (and (eq (type-constraint-class-name t1) (type-constraint-class-name t2))
          (type-equal-p (type-constraint-type-arg t1) (type-constraint-type-arg t2))))
    ((and (type-qualified-p t1) (type-qualified-p t2))
     (and (%type-list-equal-p (type-qualified-constraints t1) (type-qualified-constraints t2))
          (type-equal-p (type-qualified-body t1) (type-qualified-body t2))))
    (t nil)))

;;; ─── Structural traversal helpers ──────────────────────────────────────────

(defun type-bound-var (ty)
  "Return the type variable bound by TY, or NIL when TY is not a binder.
This is the shared data-layer hook used by traversal code like type-free-vars."
  (typecase ty
    (type-forall (type-forall-var ty))
    (type-exists (type-exists-var ty))
    (type-lambda (type-lambda-var ty))
    (type-mu     (type-mu-var ty))
    (t nil)))

(defun type-children (ty)
  "Return TY's immediate child types as a flat list.
The function is intentionally structural and side-effect free so traversal logic
can live elsewhere (free vars, occurs check, zonking, printers, etc.)."
  (cond
     ((or (null ty)
          (type-primitive-p ty)
          (type-var-p ty)
          (type-rigid-p ty)
          (type-error-p ty)
          (type-unknown-p ty))
      nil)
    ((type-arrow-p ty)
     (append (type-arrow-params ty)
             (list (type-arrow-return ty))
             (when (type-arrow-effects ty)
               (list (type-arrow-effects ty)))))
    ((type-product-p ty)
     (copy-list (type-product-elems ty)))
    ((type-record-p ty)
     (append (mapcar #'cdr (type-record-fields ty))
             (when (type-record-row-var ty)
               (list (type-record-row-var ty)))))
    ((type-variant-p ty)
     (append (mapcar #'cdr (type-variant-cases ty))
             (when (type-variant-row-var ty)
               (list (type-variant-row-var ty)))))
    ((type-union-p ty)
     (copy-list (type-union-types ty)))
    ((type-intersection-p ty)
     (copy-list (type-intersection-types ty)))
    ((type-forall-p ty)
     (list (type-forall-body ty)))
    ((type-exists-p ty)
     (list (type-exists-body ty)))
    ((type-app-p ty)
     (list (type-app-fun ty) (type-app-arg ty)))
    ((type-lambda-p ty)
     (list (type-lambda-body ty)))
    ((type-mu-p ty)
     (list (type-mu-body ty)))
    ((type-refinement-p ty)
     (list (type-refinement-base ty)))
    ((type-linear-p ty)
     (list (type-linear-base ty)))
    ((type-capability-p ty)
     (list (type-capability-base ty)))
    ((type-effect-row-p ty)
     (append (copy-list (type-effect-row-effects ty))
             (when (type-effect-row-row-var ty)
               (list (type-effect-row-row-var ty)))))
    ((type-effect-op-p ty)
     (copy-list (type-effect-op-args ty)))
    ((type-advanced-p ty)
     (append (mapcan #'type-advanced-payload-children (type-advanced-args ty))
             (mapcan #'type-advanced-payload-children (type-advanced-properties ty))
             (type-advanced-payload-children (type-advanced-evidence ty))))
    ((type-handler-p ty)
     (list (type-handler-effect ty)
           (type-handler-input ty)
           (type-handler-output ty)))
    ((type-gadt-con-p ty)
     (append (copy-list (type-gadt-con-arg-types ty))
             (when (type-gadt-con-index-type ty)
               (list (type-gadt-con-index-type ty)))))
    ((type-constraint-p ty)
     (list (type-constraint-type-arg ty)))
    ((type-qualified-p ty)
     (append (copy-list (type-qualified-constraints ty))
             (list (type-qualified-body ty))))
    (t nil)))

;;; ─── Free type variables ──────────────────────────────────────────────────

(defun %type-free-vars-list (t0)
  "Collect the (possibly-duplicated) list of free type-var nodes in T0."
  (cond
    ((type-var-p t0) (list t0))
    (t
     (let* ((bound-var  (type-bound-var t0))
            (child-vars (mapcan #'%type-free-vars-list (type-children t0))))
       (if bound-var
           (remove-if (lambda (v) (type-var-equal-p v bound-var)) child-vars)
           child-vars)))))

(defun type-free-vars (ty)
  "Return the deduplicated list of free type-var nodes in TY."
  (remove-duplicates (%type-free-vars-list ty) :test #'type-var-equal-p))

;;;; types-extended-units.lisp — Unit definitions, measures, SI registrations
(in-package :cl-cc/type)

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



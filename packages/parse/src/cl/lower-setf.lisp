;;;; lower-setf.lisp — Setf Compound Place Lowering
;;;;
;;;; %lower-setf-place: dispatches slot-value/gethash/getf/fdefinition/simple-rewrite
;;;; places; falls back to error for unsupported compound places.
;;;; define-list-lowerer for (setf) integrates with the main lowerer table.
;;;;
;;;; Load order: after lower.lisp (needs lower-sexp-to-ast, *setf-place-simple-rewrites*).

(in-package :cl-cc/parse)

;;; ── Setf ─────────────────────────────────────────────────────────────────────

(defparameter *setf-nth-list-accessors*
  '(("second" . 1)
    ("third" . 2)
    ("fourth" . 3)
    ("fifth" . 4)
    ("sixth" . 5)
    ("seventh" . 6)
    ("eighth" . 7)
    ("ninth" . 8)
    ("tenth" . 9))
  "Mapping from list accessor names to their zero-based NTHCDR offset.")

(defun %setf-simple-rewrite-rule (head)
  (or (assoc head *setf-place-simple-rewrites*)
      (when (symbolp head)
        (find (symbol-name head) *setf-place-simple-rewrites*
              :key (lambda (rule) (symbol-name (first rule)))
              :test #'string-equal))))

(defun %setf-nth-list-accessor-index (head)
  (when (symbolp head)
    (cdr (assoc (symbol-name head) *setf-nth-list-accessors*
                :test #'string-equal))))

(defun %setf-cdr-chain-form (count base-form)
  (loop with form = base-form
        repeat count
        do (setf form (list 'cdr form))
        finally (return form)))

(defun %setf-cxr-accessor-letters (head)
  "Return the A/D letter string for a CXR accessor symbol, or NIL."
  (when (symbolp head)
    (let* ((name (string-downcase (symbol-name head)))
           (len (length name)))
      (when (and (>= len 3)
                 (<= len 6)
                 (char= (char name 0) #\c)
                 (char= (char name (1- len)) #\r)
                 (loop for i from 1 below (1- len)
                       always (member (char name i) '(#\a #\d))))
        (subseq name 1 (1- len))))))

(defun %setf-cxr-container-form (letters base-form)
  "Build the container form mutated by SETF of a CXR place."
  (loop with form = base-form
        for i from (1- (length letters)) downto 1
        for letter = (char letters i)
        do (setf form (list (if (char= letter #\a) 'car 'cdr) form))
        finally (return form)))

(defun %setf-cxr-rewrite-form (head base-form value-form)
  (let* ((letters (%setf-cxr-accessor-letters head))
         (setter (if (char= (char letters 0) #\a) 'rplaca 'rplacd)))
    (list setter (%setf-cxr-container-form letters base-form) value-form)))

(defun %lower-setf-place (place value-form sf sl sc)
  "Lower a (setf (PLACE ...) VALUE-FORM) compound place.
Simple places dispatch via *setf-place-simple-rewrites*; complex places
(slot-value, gethash, getf) are handled specially."
  (let* ((head (car place))
         (place-args (cdr place))
         (rule (%setf-simple-rewrite-rule head)))
    (if rule
        (let* ((fn-sym  (second rule))
               (n-args  (third rule))
               (used-args (subseq place-args 0 n-args)))
          (lower-sexp-to-ast (append (list fn-sym) used-args (list value-form))))
        (case head
          ((second third fourth fifth sixth seventh eighth ninth tenth)
           (unless (= (length place-args) 1)
             (error "~S setf place requires one argument" head))
           (lower-sexp-to-ast
            (list 'rplaca
                  (%setf-cdr-chain-form (%setf-nth-list-accessor-index head)
                                        (first place-args))
                  value-form)))
          (slot-value
           (let ((obj-form (second place)) (slot-form (third place)))
             (if (and (consp slot-form) (eq (car slot-form) 'quote))
                 (make-ast-set-slot-value
                  :object (lower-sexp-to-ast obj-form)
                  :slot   (second slot-form)
                  :value  (lower-sexp-to-ast value-form)
                  :source-file sf :source-line sl :source-column sc)
                 (lower-sexp-to-ast (list 'rt-slot-set obj-form slot-form value-form)))))
          (gethash
           (make-ast-set-gethash
            :key   (lower-sexp-to-ast (second place))
            :table (lower-sexp-to-ast (third place))
            :value (lower-sexp-to-ast value-form)
            :source-file sf :source-line sl :source-column sc))
          (getf
           (let ((plist-var (second place)) (indicator (third place)))
             (if (symbolp plist-var)
                 (let ((val-ast   (lower-sexp-to-ast value-form))
                       (plist-ast (lower-sexp-to-ast plist-var))
                       (ind-ast   (lower-sexp-to-ast indicator)))
                   (make-ast-progn
                    :forms (list (make-ast-setq
                                  :var plist-var
                                  :value (make-ast-call
                                          :func (lower-sexp-to-ast 'rt-plist-put)
                                          :args (list plist-ast ind-ast val-ast)))
                                 val-ast)))
                 (lower-sexp-to-ast (list 'rt-plist-put (second place) (third place) value-form)))))
          (fdefinition
           (lower-sexp-to-ast (list 'set-fdefinition value-form (second place))))
          (otherwise
            (let ((nth-index (%setf-nth-list-accessor-index head))
                  (letters (%setf-cxr-accessor-letters head)))
              (cond
                ((and nth-index (= (length place-args) 1))
                 (lower-sexp-to-ast
                  (list 'rplaca
                        (%setf-cdr-chain-form nth-index (first place-args))
                        value-form)))
                ((and letters (= (length place-args) 1))
                 (lower-sexp-to-ast
                  (%setf-cxr-rewrite-form head (first place-args) value-form)))
                (t
                 (error "setf only supports symbol, slot-value, gethash, getf, aref, svref, car/cdr, first/rest, second..tenth, cxr, bit, char, elt, nth places")))))))))

(defun %lower-setf-pair (place value-form sf sl sc)
  (if (symbolp place)
      (make-ast-setq :var place :value (lower-sexp-to-ast value-form)
                     :source-file sf :source-line sl :source-column sc)
      (%lower-setf-place place value-form sf sl sc)))

(defun %lower-place-update (operator place delta-form sf sl sc)
  "Lower INCF/DECF-style updates through the existing SETF place machinery."
  (%lower-setf-pair place (list operator place delta-form) sf sl sc))

(define-list-lowerer (setf) (node sf sl sc)
  (let ((args (cdr node)))
    (unless (evenp (length args))
      (error "setf requires pairs of place and value"))
    (cond
      ((null args)
       (make-ast-quote :value nil
                       :source-file sf :source-line sl :source-column sc))
      ((= (length args) 2)
       (%lower-setf-pair (first args) (second args) sf sl sc))
      (t
       (make-ast-progn
        :forms (loop for (place value-form) on args by #'cddr
                     collect (%lower-setf-pair place value-form sf sl sc))
        :source-file sf :source-line sl :source-column sc)))))

(define-list-lowerer (incf decf) (node sf sl sc)
  (unless (member (length node) '(2 3))
    (error "~S requires a place and optional delta" (car node)))
  (%lower-place-update (if (eq (car node) 'incf) '+ '-)
                       (second node)
                       (if (third node) (third node) 1)
                       sf sl sc))

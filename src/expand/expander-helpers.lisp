(in-package :cl-cc)

(defun expand-make-array-form (size rest-args)
  "Expand (make-array size &rest keyword-args).
Promotes to make-adjustable-vector when :fill-pointer or :adjustable is given.
Handles :initial-contents (FR-654) and :initial-element (FR-687) via loop expansion."
  (let (fp adj init-elem init-contents)
    (loop for (key val) on rest-args by #'cddr
          do (case key
               (:fill-pointer    (setf fp val))
               (:adjustable      (setf adj val))
               (:initial-element  (setf init-elem val))
               (:initial-contents (setf init-contents val))
               ;; :element-type, :displaced-to, :displaced-index-offset — silently ignored
               ))
    (cond
      ;; :initial-contents — build array from sequence
      (init-contents
       (let ((arr-g  (gensym "ARR"))
             (cont-g (gensym "CONT"))
             (i-g    (gensym "I")))
         (compiler-macroexpand-all
          `(let* ((,cont-g ,init-contents)
                  (,arr-g  (make-array (length ,cont-g))))
             (dotimes (,i-g (length ,cont-g) ,arr-g)
               (setf (aref ,arr-g ,i-g) (elt ,cont-g ,i-g)))))))
      ;; :fill-pointer / :adjustable — promote to adjustable vector
      ;; When :fill-pointer has a specific non-t value, set it after construction
      ((or fp adj)
       (if (and fp (not (eq fp t)))
           ;; Specific fill-pointer value: create vector then set fill-pointer
           (let ((arr-g (gensym "ARR"))
                 (fp-g  (gensym "FP")))
             (compiler-macroexpand-all
              `(let* ((,fp-g  ,fp)
                      (,arr-g (make-adjustable-vector ,size)))
                 (setf (fill-pointer ,arr-g) ,fp-g)
                 ,arr-g)))
           ;; Boolean fill-pointer (t) or :adjustable only
           (compiler-macroexpand-all `(make-adjustable-vector ,size))))
      ;; :initial-element — fill array with init value
      (init-elem
       (let ((arr-g (gensym "ARR"))
             (ie-g  (gensym "IE"))
             (i-g   (gensym "I")))
         (compiler-macroexpand-all
          `(let* ((,ie-g  ,init-elem)
                  (,arr-g (make-array ,size)))
             (dotimes (,i-g ,size ,arr-g)
               (setf (aref ,arr-g ,i-g) ,ie-g))))))
      ;; plain make-array
      (t
       (compiler-macroexpand-all `(make-array ,size))))))

(defun expand-setf-accessor (place value)
  "Expand (setf (ACCESSOR OBJ) VAL) via *accessor-slot-map* for known struct accessors,
or fall back to generic (setf (slot-value obj 'accessor-name) val)."
  (let ((mapping (gethash (car place) *accessor-slot-map*)))
    (if mapping
        (compiler-macroexpand-all
         `(setf (slot-value ,(second place) ',(cdr mapping)) ,value))
        (compiler-macroexpand-all
         `(setf (slot-value ,(second place) ',(car place)) ,value)))))

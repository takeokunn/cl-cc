;;;; types-extended-qtt.lisp — QTT bindings, graded types
(in-package :cl-cc/type)

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



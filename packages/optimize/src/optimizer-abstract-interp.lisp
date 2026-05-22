;;;; optimizer-abstract-interp.lisp — FR-751 Abstract Interpretation

(in-package :cl-cc/optimize)

(defvar *abstract-interp-enabled* nil
  "When non-NIL, run the FR-751 abstract interpretation pass.")

(defparameter +ai-bottom+ :bottom)
(defparameter +ai-top+ :top)

(defstruct (ai-state (:constructor make-ai-state))
  "Abstract state mapping VM registers to product-domain facts."
  (facts (make-hash-table :test #'eq)))

(defun ai-sign-abstract (value)
  "Map concrete integer VALUE into the sign abstract domain."
  (cond ((not (integerp value)) +ai-top+)
        ((zerop value) :zero)
        ((plusp value) :positive)
        (t :negative)))

(defun ai-sign-join (a b)
  "Least upper bound in the sign domain."
  (cond ((eq a +ai-bottom+) b)
        ((eq b +ai-bottom+) a)
        ((eq a b) a)
        ((and (member a '(:positive :zero) :test #'eq)
              (member b '(:positive :zero) :test #'eq)) :non-negative)
        ((and (member a '(:negative :zero) :test #'eq)
              (member b '(:negative :zero) :test #'eq)) :non-positive)
        (t +ai-top+)))

(defun ai-interval (lo hi)
  "Construct an interval abstract value."
  (cons lo hi))

(defun ai-interval-abstract (value)
  "Map concrete integer VALUE to a singleton interval, otherwise TOP."
  (if (integerp value)
      (ai-interval value value)
      (ai-interval +opt-range-negative-infinity+ +opt-range-positive-infinity+)))

(defun ai-interval-join (a b)
  "Least upper bound for intervals."
  (ai-interval (min (car a) (car b)) (max (cdr a) (cdr b))))

(defun ai-interval-widen (previous next)
  "Widen PREVIOUS with NEXT for fixed-point acceleration."
  (ai-interval (if (< (car next) (car previous)) +opt-range-negative-infinity+ (car previous))
               (if (> (cdr next) (cdr previous)) +opt-range-positive-infinity+ (cdr previous))))

(defun ai-interval-narrow (previous next)
  "Narrow a widened interval using NEXT facts."
  (ai-interval (if (= (car previous) +opt-range-negative-infinity+) (car next) (car previous))
               (if (= (cdr previous) +opt-range-positive-infinity+) (cdr next) (cdr previous))))

(defun ai-congruence (mod residue)
  "Construct congruence X ≡ RESIDUE (mod MOD)."
  (cons (max 0 mod) residue))

(defun ai-congruence-abstract (value)
  "Map concrete integer VALUE into the congruence domain."
  (if (integerp value) (ai-congruence 0 value) (ai-congruence 1 0)))

(defun ai-congruence-join (a b)
  "Conservative congruence join."
  (if (equal a b) a (ai-congruence 1 0)))

(defun ai-alpha (value)
  "Galois abstraction α: concrete VALUE → product abstract value."
  (list :sign (ai-sign-abstract value)
        :interval (ai-interval-abstract value)
        :congruence (ai-congruence-abstract value)
        :nullness (if (null value) :null :non-null)))

(defun ai-gamma (abstract)
  "Galois concretization γ: return a conservative descriptor."
  (list :values (getf abstract :interval)
        :sign (getf abstract :sign)
        :congruence (getf abstract :congruence)
        :nullness (getf abstract :nullness)))

(defun %ai-abstract-join (a b)
  "Join product-domain abstract values."
  (cond ((null a) b)
        ((null b) a)
        (t (list :sign (ai-sign-join (getf a :sign) (getf b :sign))
                 :interval (ai-interval-join (getf a :interval) (getf b :interval))
                 :congruence (ai-congruence-join (getf a :congruence) (getf b :congruence))
                 :nullness (if (eq (getf a :nullness) (getf b :nullness))
                               (getf a :nullness)
                               :maybe-null)))))

(defun %ai-state-copy (state)
  "Return a shallow copy of abstract STATE."
  (let ((copy (make-ai-state)))
    (maphash (lambda (k v) (setf (gethash k (ai-state-facts copy)) v))
             (ai-state-facts state))
    copy))

(defun %ai-state-join-into (dst src &key widen)
  "Join SRC facts into DST. Return T when DST changed."
  (let ((changed nil))
    (maphash
     (lambda (reg value)
       (let* ((old (gethash reg (ai-state-facts dst)))
              (joined (%ai-abstract-join old value))
              (next (if (and widen old joined)
                        (list :sign (getf joined :sign)
                              :interval (ai-interval-widen (getf old :interval)
                                                           (getf joined :interval))
                              :congruence (getf joined :congruence)
                              :nullness (getf joined :nullness))
                        joined)))
         (unless (equal old next)
           (setf (gethash reg (ai-state-facts dst)) next
                 changed t))))
     (ai-state-facts src))
    changed))

(defun %ai-state-get (state reg)
  (and reg (gethash reg (ai-state-facts state))))

(defun %ai-state-set (state reg value)
  (when reg (setf (gethash reg (ai-state-facts state)) value))
  state)

(defun %ai-transfer-binop (inst state op)
  "Transfer simple integer binary OP through STATE."
  (let* ((lhs (%ai-state-get state (vm-lhs inst)))
         (rhs (%ai-state-get state (vm-rhs inst)))
         (li (and lhs (getf lhs :interval)))
         (ri (and rhs (getf rhs :interval))))
    (when (and li ri)
      (%ai-state-set state (opt-inst-dst inst)
                     (list :sign +ai-top+
                           :interval (funcall op li ri)
                           :congruence (ai-congruence 1 0)
                           :nullness :non-null))))
  state)

(defun %ai-transfer-inst (state inst)
  "Apply one instruction transfer function to STATE."
  (typecase inst
    (vm-const (%ai-state-set state (vm-dst inst) (ai-alpha (vm-value inst))))
    (vm-move (%ai-state-set state (vm-dst inst) (%ai-state-get state (vm-src inst))))
    ((or vm-add vm-integer-add vm-add-checked)
     (%ai-transfer-binop inst state (lambda (a b) (ai-interval (+ (car a) (car b))
                                                               (+ (cdr a) (cdr b))))))
    ((or vm-sub vm-integer-sub vm-sub-checked)
     (%ai-transfer-binop inst state (lambda (a b) (ai-interval (- (car a) (cdr b))
                                                               (- (cdr a) (car b))))))
    (t (let ((dst (opt-inst-dst inst)))
         (when dst (%ai-state-set state dst (ai-alpha :unknown))))))
  state)

(defun ai-compute-fixed-point (instructions &key (max-iterations 24))
  "Compute a forward abstract fixed point over INSTRUCTIONS with widening/narrowing."
  (let ((state (make-ai-state)))
    (loop for iteration from 0 below max-iterations
          for previous = (%ai-state-copy state)
          do (dolist (inst instructions) (%ai-transfer-inst state inst))
          until (not (%ai-state-join-into state previous :widen (>= iteration 3))))
    (let ((narrowed (%ai-state-copy state)))
      (dolist (inst instructions) (%ai-transfer-inst narrowed inst))
      (maphash (lambda (reg value)
                 (let ((old (gethash reg (ai-state-facts state))))
                   (when old
                     (setf (gethash reg (ai-state-facts state))
                           (list :sign (getf value :sign)
                                 :interval (ai-interval-narrow (getf old :interval)
                                                              (getf value :interval))
                                 :congruence (getf value :congruence)
                                 :nullness (getf value :nullness))))))
               (ai-state-facts narrowed)))
    state))

(defvar *abstract-interp-last-state* nil
  "Latest FR-751 abstract interpretation result for range/null/bounds consumers.")

(defun opt-pass-abstract-interpretation (instructions)
  "FR-751 analysis-only pass for bounds, null pointer, and range analysis facts."
  (when *abstract-interp-enabled*
    (setf *abstract-interp-last-state* (ai-compute-fixed-point instructions)))
  instructions)

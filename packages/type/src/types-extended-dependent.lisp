;;;; types-extended-dependent.lisp — CIC, universe sorts, proof evidence
(in-package :cl-cc/type)

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



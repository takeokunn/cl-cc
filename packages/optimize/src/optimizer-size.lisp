;;;; optimizer-size.lisp — size-oriented outlining and safepoint polling passes

(in-package :cl-cc/optimize)

(defparameter *opt-outlining-min-length* 3
  "Minimum duplicate straight-line instruction sequence length for outlining.")

(defparameter *opt-outlined-label-prefix* "__clcc_outlined_"
  "Prefix used for generated outlined helper labels.")

(defparameter *opt-safepoint-label* "__clcc_safepoint_poll"
  "Generated no-op safepoint polling helper label.")

(defparameter *opt-safepoint-prefix* "__clcc_safepoint_poll_"
  "Prefix used for generated safepoint poll temporary labels/registers.")

(defparameter *opt-safepoint-flag-name* '*clcc-safepoint-request*
  "Global flag read by inserted safepoint polls.")

(defun %opt-generated-label-present-p (instructions prefix)
  "Return T when INSTRUCTIONS already contain a generated label with PREFIX."
  (some (lambda (inst)
          (and (typep inst 'vm-label)
               (let ((name (vm-name inst)))
                 (and (stringp name)
                      (<= (length prefix) (length name))
                      (string= prefix name :end2 (length prefix))))))
        instructions))

(defun %opt-max-register-index (instructions)
  "Return the largest numeric :R register index mentioned in INSTRUCTIONS."
  (labels ((scan (x best)
             (cond
               ((and (keywordp x)
                     (let ((name (symbol-name x)))
                       (and (>= (length name) 2)
                            (char= (char name 0) #\R)
                            (every #'digit-char-p (subseq name 1)))))
                (max best (parse-integer (subseq (symbol-name x) 1))))
               ((consp x) (scan (cdr x) (scan (car x) best)))
               (t best))))
    (loop for inst in instructions
          maximize (handler-case (scan (instruction->sexp inst) -1)
                     (error () -1)))))

(defun %opt-fresh-register-generator (instructions)
  "Return a closure yielding fresh VM register keywords for INSTRUCTIONS."
  (let ((next (1+ (%opt-max-register-index instructions))))
    (lambda ()
      (prog1 (intern (format nil "R~D" next) :keyword)
        (incf next)))))

(defun %opt-control-or-label-p (inst)
  "Return T when INST should not be part of a straight-line outlined sequence."
  (or (typep inst 'vm-label)
      (typep inst 'vm-jump)
      (typep inst 'vm-jump-zero)
      (typep inst 'vm-ret)
      (typep inst 'vm-call)
      (typep inst 'vm-tail-call)
      (typep inst 'vm-apply)))

(defun %opt-outlinable-subseq-p (seq)
  "Return T when SEQ can be outlined as a nullary helper returning one value."
  (and (>= (length seq) *opt-outlining-min-length*)
       (every (lambda (inst)
                (and (not (%opt-control-or-label-p inst))
                     (opt-inst-pure-p inst)))
              seq)
       (let* ((last-inst (car (last seq)))
              (result-dst (opt-inst-dst last-inst))
              (defs (remove nil (mapcar #'opt-inst-dst seq))))
         (and result-dst
              ;; The helper is nullary; no reads may escape the sequence.
              (loop for inst in seq
                    for pos from 0
                    always (loop for reg in (opt-inst-read-regs inst)
                                 always (member reg (subseq defs 0 pos) :test #'eq)))
              ;; Earlier definitions are internal temporaries only.
              (loop for def in (butlast defs)
                    always (loop for inst in (cdr (member def seq :key #'opt-inst-dst :test #'eq))
                                 thereis (member def (opt-inst-read-regs inst) :test #'eq)))))))

(defun %opt-subseq-key (seq)
  "Return a structural duplicate key for SEQ, ignoring concrete destination names."
  (mapcar #'instruction->sexp seq))

(defun %opt-find-outline-candidate (instructions)
  "Find one duplicate outlinable subsequence. Returns (values start length seq)."
  (let ((seen (make-hash-table :test #'equal))
        (n (length instructions)))
    (loop for len from (min 8 n) downto *opt-outlining-min-length*
          do (clrhash seen)
             (loop for start from 0 to (- n len)
                   for seq = (subseq instructions start (+ start len))
                   when (%opt-outlinable-subseq-p seq)
                     do (let ((key (%opt-subseq-key seq)))
                          (multiple-value-bind (first-start found-p) (gethash key seen)
                            (if found-p
                                (return-from %opt-find-outline-candidate
                                  (values first-start len seq))
                                 (setf (gethash key seen) start))))))))

(defun %opt-replace-outline-occurrences (instructions seq label fresh-reg)
  "Replace all occurrences of SEQ with calls to LABEL."
  (let* ((len (length seq))
         (key (%opt-subseq-key seq))
         (result nil)
         (i 0)
         (n (length instructions)))
    (loop while (< i n)
          do (if (and (<= (+ i len) n)
                      (equal key (%opt-subseq-key (subseq instructions i (+ i len)))))
                 (let ((func-reg (funcall fresh-reg))
                       (dst (opt-inst-dst (nth (1- len) seq))))
                   (push (make-vm-func-ref :dst func-reg :label label) result)
                   (push (make-vm-call :dst dst :func func-reg :args nil) result)
                   (incf i len))
                 (progn
                   (push (nth i instructions) result)
                   (incf i))))
    (nreverse result)))

(defun opt-pass-function-outlining (instructions)
  "FR-294: outline duplicate pure straight-line sequences into shared helpers.

This conservative -Os pass handles nullary pure sequences with one returned value.
It is intentionally one-shot per instruction stream to keep convergence stable."
  (if (%opt-generated-label-present-p instructions *opt-outlined-label-prefix*)
      instructions
      (multiple-value-bind (start len seq) (%opt-find-outline-candidate instructions)
        (declare (ignore start len))
        (if seq
            (let* ((label (format nil "~A~D" *opt-outlined-label-prefix* (sxhash (%opt-subseq-key seq))))
                   (fresh-reg (%opt-fresh-register-generator instructions))
                   (rewritten (%opt-replace-outline-occurrences instructions seq label fresh-reg)))
              (append rewritten
                      (list (make-vm-label :name label))
                      seq
                      (list (make-vm-ret :reg (opt-inst-dst (car (last seq)))))))
            instructions))))

(defun %opt-backedge-targets (instructions)
  "Return an EQUAL table of labels that are loop back-edge targets."
  (let ((positions (make-hash-table :test #'equal))
        (targets (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
            do (setf (gethash (vm-name inst) positions) i))
    (loop for inst in instructions
          for i from 0
          when (typep inst '(or vm-jump vm-jump-zero))
            do (let ((target (gethash (vm-label-name inst) positions)))
                 (when (and target (< target i))
                   (setf (gethash (vm-label-name inst) targets) t))))
    targets))

(defun %opt-safepoint-poll (fresh-reg skip-label)
  "Build an actual safepoint flag check that calls the helper when requested."
  (let ((flag-reg (funcall fresh-reg))
        (func-reg (funcall fresh-reg))
        (dst-reg (funcall fresh-reg)))
    (list (make-vm-get-global :dst flag-reg :name *opt-safepoint-flag-name*)
          (make-vm-jump-zero :reg flag-reg :label skip-label)
          (make-vm-func-ref :dst func-reg :label *opt-safepoint-label*)
          (make-vm-call :dst dst-reg :func func-reg :args (list flag-reg))
          (make-vm-label :name skip-label))))

(defun opt-pass-safepoint-polling (instructions)
  "FR-233: insert safepoint flag checks at function entries and loop backedges."
  (if (%opt-generated-label-present-p instructions *opt-safepoint-prefix*)
      instructions
      (let ((backedges (%opt-backedge-targets instructions))
            (fresh-reg (%opt-fresh-register-generator instructions))
            (result nil)
            (counter 0)
            (entry-seen-p nil))
        (dolist (inst instructions)
          (push inst result)
          (when (and (typep inst 'vm-label)
                     (or (not entry-seen-p)
                         (gethash (vm-name inst) backedges)))
            (let ((skip-label (format nil "~A~D" *opt-safepoint-prefix* counter)))
              (push (make-vm-label :name (format nil "~Asite_~D" *opt-safepoint-prefix* counter)) result)
              (dolist (poll (reverse (%opt-safepoint-poll fresh-reg skip-label)))
                (push poll result)))
            (incf counter)
            (setf entry-seen-p t)))
        (let ((ret-reg (funcall fresh-reg)))
          (append (nreverse result)
                  (list (make-vm-label :name *opt-safepoint-label*)
                        (make-vm-const :dst ret-reg :value 0)
                        (make-vm-ret :reg ret-reg)))))))

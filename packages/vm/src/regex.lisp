(in-package :cl-cc/vm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :regex
    (:use :cl)
    (:shadow #:compile)
    (:export
     #:scan
     #:all-matches
     #:regex-replace
     #:regex-replace-all
     #:regex-split
     #:match-start
     #:match-string
     #:match-group
     #:match-groups
     #:compile
     #:compiled-regex-pattern
     #:compiled-regex-ast
     #:compiled-regex-nfa
     #:compiled-regex-dfa)))

(defstruct regex-nfa start accept (states nil) (capture-groups 0))
(defstruct regex-state (id 0) (transitions nil) (epsilon nil) (capture nil))
(defstruct regex-program pattern tokens (capture-count 0) ignore-case-p)

(defun %re-mk-state (nfa)
  (let ((s (make-regex-state :id (length (regex-nfa-states nfa)))))
    (push s (regex-nfa-states nfa))
    s))

(defun %re-add-trans (from to char)
  (push (cons char to) (regex-state-transitions from)))

(defun %re-code-category (ch)
  (sb-unicode:general-category ch))

(defun %re-decimal-char-p (ch)
  (or (digit-char-p ch)
      (eq (%re-code-category ch) :nd)))

(defun %re-connector-char-p (ch)
  (member (%re-code-category ch) '(:pc :connector-punctuation)
          :test #'eq))

(defun %re-word-char-p (ch)
  (or (alphanumericp ch)
      (%re-decimal-char-p ch)
      (%re-connector-char-p ch)))

(defun %re-space-char-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun %re-property-char-p (property ch)
  (let ((category (%re-code-category ch)))
    (cond
      ((member property '("L" "LETTER" "LETTERS") :test #'string-equal)
       (or (alpha-char-p ch)
           (member category '(:lu :ll :lt :lm :lo :letter
                              :uppercase-letter :lowercase-letter
                              :titlecase-letter :modifier-letter :other-letter)
                   :test #'eq)))
      ((member property '("ND" "DECIMAL_NUMBER" "DECIMAL-DIGIT-NUMBER")
               :test #'string-equal)
       (%re-decimal-char-p ch))
      ((member property '("PC" "CONNECTOR_PUNCTUATION") :test #'string-equal)
       (%re-connector-char-p ch))
      (t nil))))

(defun %re-char= (a b ignore-case-p)
  (if ignore-case-p
      (char-equal a b)
      (char= a b)))

(defun %re-normalize-atom (atom)
  (etypecase atom
    (character (list :literal atom))
    (cons atom)))

(defun %re-parse-property (pattern pos end)
  (unless (and (< pos end) (char= (char pattern pos) #\{))
    (error "Regex parse error: expected { after \\p"))
  (let ((close (position #\} pattern :start (1+ pos) :end end)))
    (unless close
      (error "Regex parse error: unterminated property escape"))
    (values (list :property (subseq pattern (1+ pos) close))
            (1+ close))))

(defun %re-parse-escape (pattern pos end)
  (when (>= pos end)
    (error "Regex parse error: trailing escape"))
  (let ((ch (char pattern pos)))
    (case ch
      (#\d (values '(:digit) (1+ pos)))
      (#\w (values '(:word) (1+ pos)))
      (#\s (values '(:space) (1+ pos)))
      (#\p (%re-parse-property pattern (1+ pos) end))
      (t (values (list :literal ch) (1+ pos))))))

(defun %re-parse-class (pattern pos end)
  (let ((negated nil)
        (items nil))
    (when (and (< pos end) (char= (char pattern pos) #\^))
      (setf negated t
            pos (1+ pos)))
    (loop while (< pos end)
          for ch = (char pattern pos)
          do (cond
               ((char= ch #\])
                (return-from %re-parse-class
                  (values (list :class negated (nreverse items))
                          (1+ pos))))
               ((char= ch #\\)
                (multiple-value-bind (atom next) (%re-parse-escape pattern (1+ pos) end)
                  (push (%re-normalize-atom atom) items)
                  (setf pos next)))
               ((and (< (+ pos 2) end)
                     (char= (char pattern (1+ pos)) #\-)
                     (not (char= (char pattern (+ pos 2)) #\])))
                (push (list :range ch (char pattern (+ pos 2))) items)
                (setf pos (+ pos 3)))
               (t
                (push (list :literal ch) items)
                (incf pos))))
    (error "Regex parse error: unterminated character class")))

(defun %re-parse-quantifier (pattern pos end)
  (when (>= pos end)
    (return-from %re-parse-quantifier (values 1 1 pos)))
  (let ((ch (char pattern pos)))
    (case ch
      (#\* (values 0 nil (1+ pos)))
      (#\+ (values 1 nil (1+ pos)))
      (#\? (values 0 1 (1+ pos)))
      (#\{
       (let ((close (position #\} pattern :start (1+ pos) :end end)))
         (unless close
           (error "Regex parse error: unterminated quantifier"))
         (let* ((body (subseq pattern (1+ pos) close))
                (comma (position #\, body)))
           (if comma
               (values (parse-integer body :start 0 :end comma)
                       (if (= comma (1- (length body)))
                           nil
                           (parse-integer body :start (1+ comma)))
                       (1+ close))
               (let ((n (parse-integer body)))
                 (values n n (1+ close)))))))
      (t (values 1 1 pos)))))

(defun %re-parse-sequence (pattern pos end capture-index)
  (let ((tokens nil)
        (ignore-case-p nil))
    (loop while (< pos end)
          for ch = (char pattern pos)
          do (cond
               ((char= ch #\))
                (return))
               ((and (char= ch #\()
                     (< (+ pos 3) end)
                     (char= (char pattern (1+ pos)) #\?)
                     (char= (char pattern (+ pos 2)) #\i)
                     (char= (char pattern (+ pos 3)) #\)))
                (setf ignore-case-p t
                      pos (+ pos 4)))
               (t
                (let ((atom nil))
                  (cond
                    ((char= ch #\\)
                     (multiple-value-bind (parsed next) (%re-parse-escape pattern (1+ pos) end)
                       (setf atom parsed
                             pos next)))
                    ((char= ch #\[)
                     (multiple-value-bind (parsed next) (%re-parse-class pattern (1+ pos) end)
                       (setf atom parsed
                             pos next)))
                    ((char= ch #\()
                     (multiple-value-bind (inner next new-index inner-ignore)
                         (%re-parse-sequence pattern (1+ pos) end (1+ capture-index))
                       (declare (ignore inner-ignore))
                       (unless (and (< next end) (char= (char pattern next) #\)))
                         (error "Regex parse error: unterminated group"))
                       (setf atom (list :group (1+ capture-index) inner)
                             capture-index new-index
                             pos (1+ next))))
                    ((char= ch #\.)
                     (setf atom '(:any)
                           pos (1+ pos)))
                    ((char= ch #\^)
                     (setf atom '(:start-anchor)
                           pos (1+ pos)))
                    ((char= ch #\$)
                     (setf atom '(:end-anchor)
                           pos (1+ pos)))
                    (t
                     (setf atom (list :literal ch)
                           pos (1+ pos))))
                  (multiple-value-bind (min max next)
                      (%re-parse-quantifier pattern pos end)
                    (push (list :token atom min max) tokens)
                    (setf pos next))))))
    (values (nreverse tokens) pos capture-index ignore-case-p)))

(defun %re-parse (pattern &key (start 0) (end (length pattern)))
  (multiple-value-bind (tokens pos capture-count ignore-case-p)
      (%re-parse-sequence pattern start end 0)
    (unless (= pos end)
      (error "Regex parse error: unexpected token at ~D" pos))
    (make-regex-program
     :pattern pattern
     :tokens tokens
     :capture-count capture-count
     :ignore-case-p ignore-case-p)))

(defun %re-atom-matches-char-p (atom ch ignore-case-p)
  (ecase (first atom)
    (:literal (%re-char= (second atom) ch ignore-case-p))
    (:range (let ((start (second atom))
                  (end (third atom)))
              (if ignore-case-p
                  (let ((probe (char-downcase ch)))
                    (char<= (char-downcase start) probe (char-downcase end)))
                  (char<= start ch end))))
    (:any t)
    (:digit (%re-decimal-char-p ch))
    (:word (%re-word-char-p ch))
    (:space (%re-space-char-p ch))
    (:property (%re-property-char-p (second atom) ch))
    (:class
     (let ((matched nil))
       (dolist (item (third atom))
         (when (%re-atom-matches-char-p item ch ignore-case-p)
           (setf matched t)))
       (if (second atom) (not matched) matched)))))

(defun %re-copy-captures (captures)
  (let ((copy (make-array (length captures) :initial-element nil)))
    (replace copy captures)
    copy))

(defun %re-match-sequence (tokens index string pos limit captures ignore-case-p)
  (if (>= index (length tokens))
      (list (cons pos captures))
      (destructuring-bind (_ atom min max) (nth index tokens)
        (declare (ignore _))
        (let ((states (list (cons pos captures)))
              (levels (list (list (cons pos captures)))))
          (loop for count from 1
                while (and states (or (null max) (<= count max)))
                do (let ((next nil)
                         (progressed nil))
                     (dolist (state states)
                       (destructuring-bind (state-pos . state-captures) state
                         (dolist (matched (%re-match-atom atom string state-pos limit
                                                          state-captures ignore-case-p))
                           (unless (= (car matched) state-pos)
                             (setf progressed t))
                           (push matched next))))
                     (setf states (nreverse next))
                     (when states
                       (push states levels))
                     (unless progressed
                       (setf states nil))))
          (let ((results nil))
            (loop for count downfrom (1- (length levels)) to min
                  for states-at-count = (nth (- (1- (length levels)) count) levels)
                  do (dolist (state states-at-count)
                       (setf results
                             (nconc results
                                    (%re-match-sequence tokens (1+ index) string
                                                        (car state) limit (cdr state)
                                                        ignore-case-p)))))
            results)))))

(defun %re-match-atom (atom string pos limit captures ignore-case-p)
  (case (first atom)
    (:start-anchor
     (when (zerop pos)
       (list (cons pos captures))))
    (:end-anchor
     (when (= pos limit)
       (list (cons pos captures))))
    (:group
     (let ((group-index (second atom))
           (group-tokens (third atom))
           (results nil))
       (dolist (state (%re-match-sequence group-tokens 0 string pos limit
                                          captures ignore-case-p))
         (let ((copy (%re-copy-captures (cdr state))))
           (setf (aref copy group-index) (subseq string pos (car state)))
           (push (cons (car state) copy) results)))
       (nreverse results)))
    (t
     (when (and (< pos limit)
                (%re-atom-matches-char-p atom (char string pos) ignore-case-p))
       (list (cons (1+ pos) captures))))))

(defun %re-search (program string &key (start 0) end)
  (let* ((limit (or end (length string)))
         (tokens (regex-program-tokens program))
         (anchored-p (and tokens (eq (first (second (first tokens))) :start-anchor)))
         (capture-count (regex-program-capture-count program)))
    (loop for i from start to limit
          while (or (not anchored-p) (= i start))
          do (let ((captures (make-array (1+ capture-count) :initial-element nil)))
               (dolist (state (%re-match-sequence tokens 0 string i limit captures
                                                  (regex-program-ignore-case-p program)))
                 (let ((match-end (car state))
                       (groups (%re-copy-captures (cdr state))))
                   (setf (aref groups 0) (subseq string i match-end))
                   (return-from %re-search (values i match-end groups))))))
    nil))

(defun regex-scan (pattern string &key (start 0) end)
  (%re-search (%re-parse pattern) string :start start :end end))

(defun regex-all-matches (pattern string &key (start 0) end)
  (let ((pos start)
        (limit (or end (length string)))
        (program (%re-parse pattern))
        (ranges nil))
    (loop
      (multiple-value-bind (match-start match-end groups)
          (%re-search program string :start pos :end limit)
        (declare (ignore groups))
        (unless match-start
          (return))
        (push (cons match-start match-end) ranges)
        (setf pos (if (= match-start match-end)
                      (1+ match-end)
                      match-end))
        (when (> pos limit)
          (return))))
    (nreverse ranges)))

(defun regex-replace (p s r)
  (multiple-value-bind (ms me) (regex-scan p s)
    (if ms (concatenate 'string (subseq s 0 ms) r (subseq s me)) s)))

(defun regex-replace-all (p s r)
  (with-output-to-string (out)
    (let ((pos 0))
      (loop
        (multiple-value-bind (ms me) (regex-scan p s :start pos)
          (unless ms
            (write-string s out :start pos)
            (return))
          (write-string s out :start pos :end ms)
          (write-string r out)
          (setf pos (if (= ms me) (1+ me) me)))))))

(defun regex-split (p s)
  (let ((parts nil) (pos 0))
    (loop
      (multiple-value-bind (ms me) (regex-scan p s :start pos)
        (unless ms
          (push (subseq s pos) parts)
          (return))
        (push (subseq s pos ms) parts)
        (setf pos (if (= ms me) (1+ me) me))))
    (nreverse parts)))

(in-package :regex)

(defstruct (match (:constructor make-match (start end source &optional groups)))
  start
  end
  source
  (groups #()))

(defstruct compiled-regex
  pattern
  ast
  nfa
  dfa)

(defun match-string (match)
  (subseq (match-source match) (match-start match) (match-end match)))

(defun match-group (match index)
  (if (zerop index)
      (match-string match)
      (let ((groups (match-groups match)))
        (when (< index (length groups))
          (aref groups index)))))

(defun scan (pattern string &key (start 0) end)
  (multiple-value-bind (match-start match-end groups)
      (cl-cc/vm::regex-scan pattern string :start start :end end)
    (when match-start
      (make-match match-start match-end string groups))))

(defun all-matches (pattern string &key (start 0) end)
  (let ((limit (or end (length string)))
        (pos start)
        (program (cl-cc/vm::%re-parse pattern))
        (matches nil))
    (loop
      (multiple-value-bind (match-start match-end groups)
          (cl-cc/vm::%re-search program string :start pos :end limit)
        (unless match-start
          (return))
        (push (make-match match-start match-end string groups) matches)
        (setf pos (if (= match-start match-end)
                      (1+ match-end)
                      match-end))
        (when (> pos limit)
          (return))))
    (nreverse matches)))

(defun %replacement-string (replacement match)
  (with-output-to-string (out)
    (loop for pos from 0 below (length replacement)
          for ch = (char replacement pos)
          do (if (and (char= ch #\\)
                      (< (1+ pos) (length replacement))
                      (digit-char-p (char replacement (1+ pos))))
                 (let* ((index (digit-char-p (char replacement (1+ pos))))
                        (group (match-group match index)))
                   (when group
                     (write-string group out))
                   (incf pos))
                 (write-char ch out)))))

(defun regex-replace (pattern replacement string &key (start 0) end)
  (let ((match (scan pattern string :start start :end end)))
    (if match
        (concatenate 'string
                     (subseq string 0 (match-start match))
                     (%replacement-string replacement match)
                     (subseq string (match-end match)))
        string)))

(defun regex-replace-all (pattern replacement string &key (start 0) end)
  (let ((limit (or end (length string)))
        (pos start))
    (with-output-to-string (out)
      (write-string string out :start 0 :end start)
      (loop
        (let ((match (scan pattern string :start pos :end limit)))
          (unless match
            (write-string string out :start pos)
            (return))
          (write-string string out :start pos :end (match-start match))
          (write-string (%replacement-string replacement match) out)
          (setf pos (match-end match)))))))

(defun regex-split (pattern string)
  (cl-cc/vm::regex-split pattern string))

(defun compile (pattern)
  (let ((program (cl-cc/vm::%re-parse pattern)))
    (make-compiled-regex
     :pattern pattern
     :ast (cl-cc/vm::regex-program-tokens program)
     :nfa program
     :dfa t)))

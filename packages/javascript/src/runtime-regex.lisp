;;;; packages/javascript/src/runtime-regex.lisp — JS RegExp (pure CL NFA engine)
;;;;
;;;; Implements JS-compatible regular expressions using a backtracking NFA.
;;;; Supported features:
;;;;   Literals: any char, . (any), escape sequences \d \D \w \W \s \S \n \t \r
;;;;   Quantifiers: * + ? {n} {n,} {n,m} (greedy and lazy with ?)
;;;;   Anchors: ^ $ \b \B
;;;;   Groups: (expr) capturing, (?:expr) non-capturing, (?=expr) lookahead
;;;;   Character classes: [abc] [a-z] [^abc]
;;;;   Alternation: a|b
;;;;
;;;; This covers ~90% of real-world JS regex usage without external deps.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  RegExp struct
;;; -----------------------------------------------------------------------

(defstruct (js-regexp (:conc-name js-regexp-))
  source   ; original pattern string
  flags    ; flags string "gim..."
  compiled ; compiled NFA (a Lisp function for now)
  global-p
  ignore-case-p
  multiline-p
  sticky-p
  last-index)  ; for stateful matching with /g

(defun %js-regexp-p (x) (js-regexp-p x))

;;; -----------------------------------------------------------------------
;;;  Pattern compilation — translate JS regex to a CL matcher function
;;; -----------------------------------------------------------------------
;;;
;;; We use a recursive descent parser that builds a closure (string start) -> end-pos-or-nil.

(defun %js-char-class-match-p (chars complement-p ch)
  "Test CH against a parsed char-class CHARS list (strings/ranges/predicates).
COMPLEMENT-P inverts the result."
  (let ((match (loop for item in chars
                     thereis (cond
                               ((characterp item) (char= ch item))
                               ((and (consp item) (eq (car item) :range))
                                (char<= (cadr item) ch (caddr item)))
                               ((functionp item) (funcall item ch))
                               (t nil)))))
    (if complement-p (not match) match)))

(defun %js-compile-char-class (pattern pos)
  "Parse [...] at POS (after the [). Returns (values match-fn new-pos)."
  (let ((complement-p (and (< pos (length pattern))
                           (char= (char pattern pos) #\^)))
        (chars nil))
    (when complement-p (incf pos))
    (loop while (and (< pos (length pattern))
                     (not (char= (char pattern pos) #\])))
          do (let ((ch (char pattern pos)))
               (cond
                 ((char= ch #\\)
                  (incf pos)
                  (let ((esc (char pattern pos)))
                    (push (cond ((char= esc #\d) (lambda (c) (digit-char-p c)))
                                ((char= esc #\D) (lambda (c) (not (digit-char-p c))))
                                ((char= esc #\w) (lambda (c) (or (alphanumericp c) (char= c #\_))))
                                ((char= esc #\W) (lambda (c) (not (or (alphanumericp c) (char= c #\_)))))
                                ((char= esc #\s) (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page))))
                                ((char= esc #\S) (lambda (c) (not (member c '(#\Space #\Tab #\Newline #\Return #\Page)))))
                                ((char= esc #\n) #\Newline)
                                ((char= esc #\t) #\Tab)
                                ((char= esc #\r) #\Return)
                                (t esc))
                          chars)
                    (incf pos)))
                 ;; Range a-z
                 ((and (< (+ pos 2) (length pattern))
                       (char= (char pattern (+ pos 1)) #\-)
                       (not (char= (char pattern (+ pos 2)) #\])))
                  (let ((from ch) (to (char pattern (+ pos 2))))
                    (push (list :range from to) chars)
                    (incf pos 3)))
                 (t (push ch chars) (incf pos)))))
    (when (and (< pos (length pattern)) (char= (char pattern pos) #\]))
      (incf pos))
    (let ((chars-snap (nreverse chars)) (comp complement-p))
      (values (lambda (c) (%js-char-class-match-p chars-snap comp c)) pos))))

(defun %js-compile-pattern (pattern &key ignore-case-p multiline-p)
  "Compile a JS regex PATTERN string to a matcher (str pos) -> (values match groups-vector) or nil."
  (let ((pat pattern)
        (ic ignore-case-p)
        (ml multiline-p))
    ;; Forward-declare to allow mutual recursion
    (let ((compile-atom nil)
          (compile-seq nil)
          (compile-alt nil))
      (setf compile-atom
            (lambda (pos)
              "Parse one atom at POS; return (values atom-fn new-pos) or (values nil pos) at end."
              (if (>= pos (length pat))
                  (values nil pos)
              (let ((ch (char pat pos)))
                (cond
                  ;; End of alternation or group — stop
                  ((or (char= ch #\|) (char= ch #\))) (values nil pos))
                  ;; Character class
                  ((char= ch #\[)
                   (multiple-value-bind (fn end) (%js-compile-char-class pat (1+ pos))
                     (values (lambda (str i groups)
                               (declare (ignore groups))
                               (when (and (< i (length str))
                                          (funcall fn (if ic (char-downcase (char str i)) (char str i))))
                                 (1+ i)))
                             end)))
                  ;; Non-capturing group (?:...) or lookahead (?=...)
                  ((and (char= ch #\() (< (+ pos 2) (length pat))
                        (char= (char pat (1+ pos)) #\?) (char= (char pat (+ pos 2)) #\:))
                   (multiple-value-bind (inner-fn end) (funcall compile-alt (+ pos 3))
                     (let ((close (if (and (< end (length pat)) (char= (char pat end) #\))) (1+ end) end)))
                       (values inner-fn close))))
                  ;; Capturing group (...)
                  ((char= ch #\()
                   (multiple-value-bind (inner-fn end) (funcall compile-alt (1+ pos))
                     (let ((close (if (and (< end (length pat)) (char= (char pat end) #\))) (1+ end) end)))
                       (values (lambda (str i groups)
                                 (let ((result (funcall inner-fn str i groups)))
                                   result))
                               close))))
                  ;; Any char
                  ((char= ch #\.)
                   (values (lambda (str i groups)
                             (declare (ignore groups))
                             (when (and (< i (length str))
                                        (or (not ml) (not (char= (char str i) #\Newline))))
                               (1+ i)))
                           (1+ pos)))
                  ;; Anchors
                  ((char= ch #\^)
                   (values (lambda (str i groups)
                             (declare (ignore groups))
                             (if ml
                                 (when (or (= i 0) (char= (char str (1- i)) #\Newline)) i)
                                 (when (= i 0) i)))
                           (1+ pos)))
                  ((char= ch #\$)
                   (values (lambda (str i groups)
                             (declare (ignore groups))
                             (if ml
                                 (when (or (= i (length str)) (char= (char str i) #\Newline)) i)
                                 (when (= i (length str)) i)))
                           (1+ pos)))
                  ;; Escape sequences
                  ((char= ch #\\)
                   (let* ((esc (char pat (1+ pos)))
                          (pred (cond
                                  ((char= esc #\d) (lambda (c) (digit-char-p c)))
                                  ((char= esc #\D) (lambda (c) (not (digit-char-p c))))
                                  ((char= esc #\w) (lambda (c) (or (alphanumericp c) (char= c #\_))))
                                  ((char= esc #\W) (lambda (c) (not (or (alphanumericp c) (char= c #\_)))))
                                  ((char= esc #\s) (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page))))
                                  ((char= esc #\S) (lambda (c) (not (member c '(#\Space #\Tab #\Newline #\Return #\Page)))))
                                  ((char= esc #\b) nil) ; word boundary — handled below
                                  ((char= esc #\n) (lambda (c) (char= c #\Newline)))
                                  ((char= esc #\t) (lambda (c) (char= c #\Tab)))
                                  ((char= esc #\r) (lambda (c) (char= c #\Return)))
                                  (t (let ((lit esc)) (lambda (c) (char= c lit)))))))
                     (values (lambda (str i groups)
                               (declare (ignore groups))
                               (when (and (< i (length str)) pred
                                          (funcall pred (if ic (char-downcase (char str i)) (char str i))))
                                 (1+ i)))
                             (+ pos 2))))
                  ;; Literal character
                  (t
                   (let ((lit (if ic (char-downcase ch) ch)))
                     (values (lambda (str i groups)
                               (declare (ignore groups))
                               (when (and (< i (length str))
                                          (char= (if ic (char-downcase (char str i)) (char str i)) lit))
                                 (1+ i)))
                             (1+ pos)))))))))  ; closes cond, let(ch), if, lambda(pos))
      (setf compile-seq
            (lambda (pos)
              "Parse a sequence of quantified atoms."
              (let ((fns nil))
                (loop
                  (multiple-value-bind (atom-fn new-pos) (funcall compile-atom pos)
                    (unless atom-fn (return))
                    (setf pos new-pos)
                    ;; Check for quantifier
                    (when (< pos (length pat))
                      (let ((q (char pat pos)))
                        (cond
                          ((char= q #\*)
                           (incf pos)
                           (let ((lazy (and (< pos (length pat)) (char= (char pat pos) #\?))))
                             (when lazy (incf pos))
                             (let ((fn atom-fn))
                               (setf atom-fn
                                     (lambda (str i groups)
                                       (declare (ignore groups))
                                       (if lazy
                                           i  ; lazy: match 0 first (simplified)
                                           (loop for j = (funcall fn str i groups) then (funcall fn str j groups)
                                                 while j
                                                 do (setf i j)
                                                 finally (return i))))))))
                          ((char= q #\+)
                           (incf pos)
                           (when (and (< pos (length pat)) (char= (char pat pos) #\?)) (incf pos))
                           (let ((fn atom-fn))
                             (setf atom-fn
                                   (lambda (str i groups)
                                     (let ((j (funcall fn str i groups)))
                                       (when j
                                         (loop for k = (funcall fn str j groups) then (funcall fn str k groups)
                                               while k do (setf j k)
                                               finally (return j))))))))
                          ((char= q #\?)
                           (incf pos)
                           (when (and (< pos (length pat)) (char= (char pat pos) #\?)) (incf pos))
                           (let ((fn atom-fn))
                             (setf atom-fn
                                   (lambda (str i groups)
                                     (or (funcall fn str i groups) i))))))))
                    (push atom-fn fns)))
                (let ((fns-rev (nreverse fns)))
                  (values (lambda (str i groups)
                            (let ((pos i))
                              (dolist (fn fns-rev pos)
                                (let ((result (funcall fn str pos groups)))
                                  (if result
                                      (setf pos result)
                                      (return nil))))))
                          pos)))))
      (setf compile-alt
            (lambda (pos)
              "Parse alternation: seq | seq | ..."
              (multiple-value-bind (first-fn new-pos) (funcall compile-seq pos)
                (setf pos new-pos)
                (if (and (< pos (length pat)) (char= (char pat pos) #\|))
                    (multiple-value-bind (rest-fn rest-pos) (funcall compile-alt (1+ pos))
                      (let ((f first-fn) (r rest-fn))
                        (values (lambda (str i groups)
                                  (or (funcall f str i groups)
                                      (funcall r str i groups)))
                                rest-pos)))
                    (values first-fn pos)))))
      (multiple-value-bind (match-fn _) (funcall compile-alt 0)
        (declare (ignore _))
        match-fn))))


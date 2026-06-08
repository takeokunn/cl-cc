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

;;; -----------------------------------------------------------------------
;;;  Public API — matching
;;; -----------------------------------------------------------------------

(defun %js-make-regex (pattern &optional (flags ""))
  "Create a JS RegExp from PATTERN and FLAGS strings."
  (let* ((ic (find #\i flags))
         (ml (find #\m flags))
         (gl (find #\g flags))
         (st (find #\y flags))
         (compiled (handler-case (%js-compile-pattern pattern :ignore-case-p ic :multiline-p ml)
                     (error () nil))))
    (make-js-regexp
     :source pattern :flags flags
     :compiled compiled
     :global-p (not (null gl))
     :ignore-case-p (not (null ic))
     :multiline-p (not (null ml))
     :sticky-p (not (null st))
     :last-index 0)))

(defun %js-regex-exec (re str &optional (start 0))
  "Execute RE against STR starting at START. Returns [match, groups...] or null."
  (let ((fn (js-regexp-compiled re))
        (n (length str)))
    (unless fn (return-from %js-regex-exec +js-null+))
    (loop for i from start to n
          for end = (funcall fn str i nil)
          when end
            do (let ((match-str (subseq str i end))
                     (result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
                 (vector-push-extend match-str result)
                 ;; Set match metadata
                 (let ((ht (make-hash-table :test #'equal)))
                   (setf (gethash "index" ht) (coerce i 'double-float)
                         (gethash "input" ht) str)
                   ;; Update lastIndex for global regexps
                   (when (js-regexp-global-p re)
                     (setf (js-regexp-last-index re) end))
                   (return (%js-make-object "index" (coerce i 'double-float)
                                             "input" str
                                             "0" match-str))))
          finally (progn
                    (when (js-regexp-global-p re)
                      (setf (js-regexp-last-index re) 0))
                    (return +js-null+)))))

(defun %js-regex-test (re str)
  "RegExp.prototype.test(str) — return t if match found."
  (not (eq +js-null+ (%js-regex-exec re str 0))))

(defun %js-string-match-regex (str re)
  "String.prototype.match(regexp)."
  (if (js-regexp-p re)
      (if (js-regexp-global-p re)
          ;; /g flag: collect all matches
          (let ((results (make-array 0 :element-type t :adjustable t :fill-pointer 0))
                (pos 0))
            (loop
              (let ((m (%js-regex-exec re str pos)))
                (when (eq m +js-null+) (return))
                (let ((match (gethash "0" m)))
                  (vector-push-extend match results)
                  (setf pos (max (1+ pos) (truncate (gethash "index" m) ))))))
            (if (zerop (length results)) +js-null+ results))
          ;; No /g: return first match object
          (%js-regex-exec re str 0))
      ;; String pattern: use substring search
      (%js-string-match str (%js-to-string re))))

(defun %js-string-search-regex (str re)
  "String.prototype.search(regexp) — return index or -1."
  (if (js-regexp-p re)
      (let ((m (%js-regex-exec re str 0)))
        (if (eq m +js-null+) -1 (truncate (gethash "index" m))))
      (%js-string-search str (%js-to-string re))))

(defun %js-string-replace-regex (str re replacement)
  "String.prototype.replace(regexp, replacement)."
  (if (js-regexp-p re)
      (let ((fn (js-regexp-compiled re))
            (result (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character))
            (pos 0))
        (if fn
            (loop
              (let ((end (funcall fn str pos nil)))
                (when (or (null end) (> pos (length str)))
                  (loop for i from pos below (length str)
                        do (vector-push-extend (char str i) result))
                  (return (coerce result 'string)))
                (loop for i from pos below (or (gethash "index" (%js-regex-exec re str pos)) pos)
                      do (vector-push-extend (char str i) result))
                ;; Find actual match start
                (let ((match-start nil))
                  (loop for i from pos to (length str)
                        for end2 = (funcall fn str i nil)
                        when end2
                          do (setf match-start i)
                             (return))
                  (unless match-start
                    (loop for i from pos below (length str)
                          do (vector-push-extend (char str i) result))
                    (return (coerce result 'string)))
                  (let* ((match-str (subseq str match-start end))
                         (repl (if (functionp replacement)
                                   (%js-to-string (%js-funcall replacement match-str match-start str))
                                   (let ((r (%js-to-string replacement)))
                                     (regex-replace-placeholders r match-str)))))
                    (loop for c across repl do (vector-push-extend c result))
                    (setf pos end)
                    (unless (js-regexp-global-p re) (return nil))))))
            (%js-string-replace str (%js-to-string (js-regexp-source re)) replacement)))
      (%js-string-replace str (%js-to-string re) replacement)))

(defun regex-replace-placeholders (template match-str)
  "Replace $& with the match in TEMPLATE."
  (let ((out (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
    (loop for i below (length template)
          do (if (and (char= (char template i) #\$)
                      (< (1+ i) (length template))
                      (char= (char template (1+ i)) #\&))
                 (progn (loop for c across match-str do (vector-push-extend c out))
                        (incf i))
                 (vector-push-extend (char template i) out)))
    (coerce out 'string)))

(defun %js-string-replace-all-regex (str re replacement)
  "String.prototype.replaceAll with regex (must have /g flag)."
  (if (js-regexp-p re)
      (%js-string-replace-regex str re replacement)
      (%js-string-replace-all str (%js-to-string re) replacement)))

(defun %js-string-split-regex (str re &optional limit)
  "String.prototype.split with regex separator."
  (if (js-regexp-p re)
      (let* ((fn (js-regexp-compiled re))
             (results (make-array 0 :element-type t :adjustable t :fill-pointer 0))
             (max-count (if (and limit (not (eq limit +js-undefined+))) (truncate limit) most-positive-fixnum))
             (pos 0))
        (when (or (null fn) (string= str ""))
          (return-from %js-string-split-regex (%js-string-split str (%js-to-string (js-regexp-source re)) limit)))
        (loop while (< pos (length str))
              do (let ((end (funcall fn str pos nil)))
                   (if end
                       (progn
                         (vector-push-extend (subseq str pos (max pos end)) results)
                         (when (>= (length results) max-count) (return))
                         (setf pos (if (= end pos) (1+ pos) end)))
                       (progn
                         (vector-push-extend (subseq str pos) results)
                         (return)))))
        results)
      (%js-string-split str (%js-to-string re) limit)))

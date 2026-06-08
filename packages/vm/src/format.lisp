(in-package :cl-cc/vm)

;;; VM Formatted Output and Reader Instructions
;;;
;;; This file provides VM instructions for formatted output (princ, prin1,
;;; print, terpri, format, write-to-string) and reader operations
;;; (read-from-string, read-sexp) using cl-cc's own lexer/parser.
;;;

;;; ─── Print/Format Instructions ───────────────────────────────────────────

(define-vm-instruction vm-princ (vm-instruction)
  "Print object readably (no escaping) to *standard-output*."
  (src nil :reader vm-src)
  (:sexp-tag :princ)
  (:sexp-slots src))

(define-vm-instruction vm-prin1 (vm-instruction)
  "Print object with escaping to *standard-output*."
  (src nil :reader vm-src)
  (:sexp-tag :prin1)
  (:sexp-slots src))

(define-vm-instruction vm-print-inst (vm-instruction)
  "Print object with newline prefix and space suffix to *standard-output*."
  (src nil :reader vm-src)
  (:sexp-tag :print)
  (:sexp-slots src))

(define-vm-instruction vm-terpri-inst (vm-instruction)
  "Output a newline to *standard-output*."
  (:sexp-tag :terpri))

(define-vm-instruction vm-fresh-line-inst (vm-instruction)
  "Output a newline if not at start of line to *standard-output*."
  (:sexp-tag :fresh-line))

(define-vm-instruction vm-write-to-string-inst (vm-instruction)
  "Convert object to its printed representation as a string (prin1 style, with escaping)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :write-to-string)
  (:sexp-slots dst src))

(define-vm-instruction vm-princ-to-string-inst (vm-instruction)
  "Convert object to its printed representation without escaping (princ style)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :princ-to-string)
  (:sexp-slots dst src))

;; Custom sexp: uses list* with variadic arg-regs
(define-vm-instruction vm-format-inst (vm-instruction)
  "Format string with arguments. Result string stored in DST."
  (dst nil :reader vm-dst)
  (fmt nil :reader vm-fmt)
  (arg-regs nil :reader vm-arg-regs))

(defmethod instruction->sexp ((inst vm-format-inst))
  (list* :format (vm-dst inst) (vm-fmt inst) (vm-arg-regs inst)))

(setf (gethash :format *instruction-constructors*)
      (lambda (sexp)
        (make-vm-format-inst :dst (second sexp)
                             :fmt (third sexp)
                             :arg-regs (cdddr sexp))))

;;; ─── String Stream Instructions ──────────────────────────────────────────

(define-vm-instruction vm-make-string-output-stream-inst (vm-instruction)
  "Create a string output stream, store in DST."
  (dst nil :reader vm-dst)
  (:sexp-tag :make-string-output-stream)
  (:sexp-slots dst))

(define-vm-instruction vm-get-output-stream-string-inst (vm-instruction)
  "Extract accumulated string from string output stream in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :get-output-stream-string)
  (:sexp-slots dst src))

(define-vm-instruction vm-stream-write-string-inst (vm-instruction)
  "Write string in SRC to stream in STREAM-REG."
  (stream-reg nil :reader vm-stream-reg)
  (src nil :reader vm-src)
  (:sexp-tag :stream-write-string)
  (:sexp-slots stream-reg src))

;;; ─── Execute print/format instructions ──────────────────────────────────

(defmethod execute-instruction ((inst vm-princ) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (write-string (vm-write-object-to-string val :escape nil
                                             :circle (%vm-read-print-var state '*print-circle* nil))
                  (vm-output-stream state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-prin1) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (write-string (vm-write-object-to-string val :escape t
                                             :circle (%vm-read-print-var state '*print-circle* nil))
                  (vm-output-stream state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-print-inst) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (terpri (vm-output-stream state))
    (write-string (vm-write-object-to-string val :escape t
                                             :circle (%vm-read-print-var state '*print-circle* nil))
                  (vm-output-stream state))
    (write-char #\Space (vm-output-stream state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-terpri-inst) state pc labels)
  (declare (ignore labels))
  (terpri (vm-output-stream state))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-fresh-line-inst) state pc labels)
  (declare (ignore labels))
  (fresh-line (vm-output-stream state))
  (values (1+ pc) nil nil))

(defun %vm-read-print-var (state sym default)
  "Read a print-control variable from VM global state, returning DEFAULT if unbound."
  (multiple-value-bind (v found) (gethash sym (vm-global-vars state))
    (let ((value (if found v default)))
      ;; Some compile-time paths store singleton lists (e.g. (10)); normalize
      ;; those back to the scalar value expected by CL print-control variables.
      (if (and (consp value) (null (cdr value)))
          (car value)
          value))))

(defmethod execute-instruction ((inst vm-write-to-string-inst) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    ;; Bind ANSI print-control variables from VM global state so that
    ;; (setq *print-base* 16) etc. actually affects write-to-string output.
    ;; Bind the cl-cc/vm print-control specials (the bare symbols) — these are
    ;; what vm-write-object-to-string reads, so binding cl:*print-*  alone is
    ;; overridden by it. Sourcing from VM global state lets both
    ;; (setq *print-base* 16) AND (write-to-string x :base 16) take effect.
    (let ((*print-base*    (%vm-read-print-var state '*print-base*   10))
          (*print-radix*   (%vm-read-print-var state '*print-radix*  nil))
          (*print-escape*  (%vm-read-print-var state '*print-escape* t))
          (*print-level*   (%vm-read-print-var state '*print-level*  nil))
          (*print-length*  (%vm-read-print-var state '*print-length* nil))
          (*print-circle*  (%vm-read-print-var state '*print-circle* nil))
          (*print-readably* (%vm-read-print-var state '*print-readably* nil))
          (*print-pretty*  (%vm-read-print-var state '*print-pretty* nil))
          (cl:*print-pprint-dispatch*
            (%pprint-dispatch-host
             (%vm-read-print-var state '*print-pprint-dispatch*
                                 *print-pprint-dispatch*)))
          (*print-case*    (%vm-read-print-var state '*print-case*   :upcase)))
      (vm-reg-set state (vm-dst inst)
                  (vm-write-object-to-string val :escape t :circle *print-circle*)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-princ-to-string-inst) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (vm-write-object-to-string val :escape nil :circle *print-circle*))
    (values (1+ pc) nil nil)))

;;; ─── Native FORMAT Directive Processor ───────────────────────────────────

(defstruct (%vm-format-context (:constructor %make-vm-format-context (args)))
  (args #() :type vector)
  (index 0 :type fixnum)
  (last-arg nil)
  (column 0 :type fixnum))

(defun %vm-format-remaining-count (ctx)
  (- (cl:length (%vm-format-context-args ctx))
     (%vm-format-context-index ctx)))

(defun %vm-format-next-arg (ctx)
  (let ((index (%vm-format-context-index ctx)))
    (when (>= index (cl:length (%vm-format-context-args ctx)))
      (error "FORMAT argument exhausted"))
    (prog1 (aref (%vm-format-context-args ctx) index)
      (setf (%vm-format-context-last-arg ctx)
            (aref (%vm-format-context-args ctx) index))
      (setf (%vm-format-context-index ctx) (1+ index)))))

(defun %vm-format-plural (ctx colonp atsignp stream)
  "Implement FORMAT ~P pluralization. ~:P reuses the previous arg; ~@P emits y/ies."
  (let ((value (if colonp
                   (%vm-format-context-last-arg ctx)
                   (%vm-format-next-arg ctx))))
    (%vm-format-write stream
                      (if atsignp
                          (if (eql value 1) "y" "ies")
                          (if (eql value 1) "" "s")))))

(defun %vm-format-write-directive (ctx colonp atsignp stream)
  "Implement FORMAT ~W through WRITE, honoring the common pretty/readable flags."
  (let ((value (%vm-format-next-arg ctx)))
    (let ((*print-pretty* (or colonp *print-pretty*))
          (*print-circle* (or atsignp *print-circle*))
          (cl:*print-pretty* (or colonp *print-pretty*))
          (cl:*print-circle* (or atsignp *print-circle*)))
      (%vm-format-write stream
                        (vm-write-object-to-string value
                                                   :escape *print-escape*
                                                   :circle *print-circle*)))))

(defun %vm-format-peek-arg (ctx)
  (let ((index (%vm-format-context-index ctx)))
    (when (>= index (cl:length (%vm-format-context-args ctx)))
      (error "FORMAT argument exhausted"))
    (aref (%vm-format-context-args ctx) index)))

(defun %vm-format-note-output (ctx string)
  "Update CTX's best-effort output column after emitting STRING."
  (loop for ch across string
        do (if (char= ch #\Newline)
               (setf (%vm-format-context-column ctx) 0)
               (incf (%vm-format-context-column ctx)))))

(defun %vm-format-write (stream string &optional ctx)
    (cl:write-string string stream)
  (when ctx
    (%vm-format-note-output ctx string)))

(defun %vm-format-pad (stream string mincol padchar atsignp &optional ctx)
  (let* ((text (princ-to-string string))
         (needed (max 0 (- (or mincol 0) (cl:length text)))))
    (if atsignp
        (progn
          (%vm-format-write stream text ctx)
          (dotimes (_ needed) (declare (ignore _))
        (cl:write-char padchar stream)
            (when ctx (if (char= padchar #\Newline)
                          (setf (%vm-format-context-column ctx) 0)
                          (incf (%vm-format-context-column ctx))))))
        (progn
          (dotimes (_ needed) (declare (ignore _))
        (cl:write-char padchar stream)
            (when ctx (if (char= padchar #\Newline)
                          (setf (%vm-format-context-column ctx) 0)
                          (incf (%vm-format-context-column ctx)))))
          (%vm-format-write stream text ctx)))))

(defun %vm-format-param (params index &optional default)
  (let ((value (nth index params)))
    (if (null value) default value)))

(defun %vm-format-group-digits (string comma-char comma-interval)
  (let* ((signp (and (> (cl:length string) 0) (cl:find (cl:char string 0) "+-")))
         (start (if signp 1 0))
         (digits (cl:subseq string start))
         (interval (or comma-interval 3))
         (comma (or comma-char #\,)))
    (with-output-to-string (out)
      (when signp (write-char (cl:char string 0) out))
      (loop for i from 0 below (cl:length digits)
            when (and (> i 0)
                      (zerop (mod (- (cl:length digits) i) interval)))
              do (write-char comma out)
            do (write-char (cl:char digits i) out)))))

(defun %vm-format-integer (value radix colonp atsignp params stream)
  (let* ((mincol (%vm-format-param params 0 nil))
         (padchar (%vm-format-param params 1 #\Space))
         (comma-char (%vm-format-param params 2 #\,))
         (comma-interval (%vm-format-param params 3 3))
         (text (let ((*print-base* radix) (*print-radix* nil))
                 (write-to-string value :base radix :radix nil)))
         (signed (if (and atsignp (numberp value) (not (minusp value)))
                     (concatenate 'string "+" text)
                     text))
         (grouped (if colonp
                      (%vm-format-group-digits signed comma-char comma-interval)
                      signed)))
    (%vm-format-pad stream grouped mincol padchar nil)))

(defparameter +vm-format-small-cardinals+
  #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
    "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen"
    "seventeen" "eighteen" "nineteen"))

(defparameter +vm-format-small-ordinals+
  #("zeroth" "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"
    "tenth" "eleventh" "twelfth" "thirteenth" "fourteenth" "fifteenth" "sixteenth"
    "seventeenth" "eighteenth" "nineteenth"))

(defparameter +vm-format-tens+
  #("" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))

(defun %vm-format-english-under-100 (n ordinalp)
  (cond
    ((< n 20) (aref (if ordinalp +vm-format-small-ordinals+ +vm-format-small-cardinals+) n))
    (t (let* ((tens (floor n 10))
              (ones (mod n 10))
              (base (aref +vm-format-tens+ tens)))
         (cond
           ((zerop ones) (if ordinalp
                              (concatenate 'string (cl:subseq base 0 (max 0 (- (cl:length base) 1))) "ieth")
                             base))
           (t (concatenate 'string base "-" (%vm-format-english-under-100 ones ordinalp))))))))

(defun %vm-format-english (n ordinalp)
  (cond
    ((minusp n) (concatenate 'string "minus " (%vm-format-english (- n) ordinalp)))
    ((< n 100) (%vm-format-english-under-100 n ordinalp))
    ((< n 1000)
     (let ((hundreds (floor n 100))
           (rest (mod n 100)))
       (if (zerop rest)
           (concatenate 'string (%vm-format-english-under-100 hundreds nil)
                        (if ordinalp " hundredth" " hundred"))
           (concatenate 'string (%vm-format-english-under-100 hundreds nil)
                        " hundred " (%vm-format-english-under-100 rest ordinalp)))))
    (t (princ-to-string n))))

(defun %vm-format-roman (n oldp)
  (declare (ignore oldp))
  (unless (and (integerp n) (< 0 n 4000))
    (return-from %vm-format-roman (princ-to-string n)))
  (let ((pairs '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
                 (100 . "C") (90 . "XC") (50 . "L") (40 . "XL")
                 (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I"))))
    (with-output-to-string (out)
      (dolist (pair pairs)
        (loop while (>= n (car pair))
              do (write-string (cdr pair) out)
                 (decf n (car pair)))))))

(defun %vm-format-radix (value colonp atsignp params stream)
  (let ((radix (%vm-format-param params 0 nil)))
    (cond
      (radix (%vm-format-integer value radix colonp atsignp (cdr params) stream))
      (atsignp (%vm-format-write stream (%vm-format-roman value colonp)))
      (t (%vm-format-write stream (%vm-format-english value colonp))))))

(defun %vm-format-float (directive value params stream)
  (declare (ignore params))
  (let ((mode (case directive
                (#\F :fixed)
                (#\E :exponential)
                (#\G :shortest)
                (#\$ :fixed)
                (otherwise :shortest))))
    (%vm-format-write stream
                      (if (fboundp 'vm-float-to-string)
                          (vm-float-to-string value :mode mode)
                          (princ-to-string value)))))

(defun %vm-format-character-name (ch)
  (or (char-name ch) (string ch)))

(defun %vm-format-character-readable (ch)
  (concatenate 'string "#\\" (%vm-format-character-name ch)))

(defun %vm-format-character (char colonp atsignp stream)
  (let ((ch (if (characterp char) char (code-char char))))
    (cond
      ((and colonp atsignp) (%vm-format-write stream (%vm-format-character-readable ch)))
      (colonp (%vm-format-write stream (%vm-format-character-name ch)))
      (atsignp (%vm-format-write stream (%vm-format-character-readable ch)))
    (t (cl:write-char ch stream)))))

(defun %vm-format-directive-char-p (char)
  (or (cl:alpha-char-p char)
      (cl:find char "%&~*?[]{}<>^/;$|_I")))

(defun %vm-format-parse-param-token (token ctx)
  (cond
    ((string= token "") nil)
    ((string= token "#") (%vm-format-remaining-count ctx))
    ((string-equal token "V") (if (plusp (%vm-format-remaining-count ctx))
                                  (%vm-format-next-arg ctx)
                                  nil))
    ((and (> (cl:length token) 1) (char= (cl:char token 0) #\'))
     (cl:char token 1))
    (t (parse-integer token))))

(defun %vm-format-parse-directive (format-string start ctx)
  (let ((len (cl:length format-string))
        (pos start)
        (params nil)
        (token "")
        (colonp nil)
        (atsignp nil))
    (labels ((push-param ()
               (push (%vm-format-parse-param-token token ctx) params)
               (setf token "")))
      (loop while (< pos len)
            for ch = (cl:char format-string pos)
            do (cond
                 ((char= ch #\,)
                  (push-param)
                  (incf pos))
                 ((char= ch #\:)
                   (when (> (cl:length token) 0) (push-param))
                  (setf colonp t)
                  (incf pos))
                 ((char= ch #\@)
                   (when (> (cl:length token) 0) (push-param))
                  (setf atsignp t)
                  (incf pos))
                 ((and (char= ch #\Space)
                       (or params (> (cl:length token) 0)))
                   (when (> (cl:length token) 0) (push-param))
                  (incf pos))
                  ((%vm-format-directive-char-p ch)
                    (when (> (cl:length token) 0) (push-param))
                   (return-from %vm-format-parse-directive
                     (values (nreverse params) colonp atsignp ch (1+ pos))))
                 (t (setf token (concatenate 'string token (string ch)))
                    (incf pos))))
      (error "Unterminated FORMAT directive in ~S" format-string))))

(defun %vm-format-matching-close (open)
  (case open
    (#\[ #\])
    (#\{ #\})
    (#\< #\>)
    (t (error "No matching close directive for ~A" open))))

(defun %vm-format-find-section-end (format-string start open)
  (let ((close (%vm-format-matching-close open))
        (depth 0)
        (pos start)
        (len (cl:length format-string)))
    (loop while (< pos len)
          do (if (char= (cl:char format-string pos) #\~)
                 (multiple-value-bind (_params _colonp _atsignp dir next)
                     (%vm-format-parse-directive format-string (1+ pos)
                                                 (%make-vm-format-context #()))
                   (declare (ignore _params _colonp _atsignp))
                   (cond
                     ((char= dir open) (incf depth))
                      ((char= dir close)
                       (if (zerop depth)
                           (return-from %vm-format-find-section-end
                             (values pos next))
                           (decf depth))))
                   (setf pos next))
                 (incf pos)))
    (error "Unterminated FORMAT section ~A in ~S" open format-string)))

(defun %vm-format-split-clauses (string)
  (let ((clauses nil)
        (else-index nil)
        (start 0)
        (pos 0)
        (len (cl:length string)))
    (loop while (< pos len)
          do (if (char= (cl:char string pos) #\~)
                 (multiple-value-bind (_params colonp _atsignp dir next)
                     (%vm-format-parse-directive string (1+ pos)
                                                 (%make-vm-format-context #()))
                   (declare (ignore _params _atsignp))
                   (if (char= dir #\;)
                        (progn
                          (push (cl:subseq string start pos) clauses)
                          (when colonp (setf else-index (cl:length clauses)))
                          (setf start next
                                pos next))
                       (setf pos next)))
                 (incf pos)))
    (push (cl:subseq string start) clauses)
    (values (nreverse clauses) else-index)))

(defun %vm-format-call-user-function (name arg colonp atsignp params stream)
  (let* ((slash (cl:position #\: name :from-end t))
         (pkg-name (and slash (cl:subseq name 0 slash)))
         (sym-name (if slash (cl:subseq name (1+ slash)) name))
         (package (if pkg-name (find-package (string-upcase pkg-name)) *package*)))
    (unless package (error "Unknown FORMAT function package: ~A" pkg-name))
    (let ((symbol (find-symbol (string-upcase sym-name) package)))
      (unless (and symbol (fboundp symbol))
        (error "Unknown FORMAT function: ~A" name))
      (apply (symbol-function symbol) stream arg colonp atsignp params))))

(defun %vm-format-render (format-string ctx stream &key (start 0) end)
  (let ((pos start)
        (limit (or end (cl:length format-string))))
    (loop while (< pos limit)
          for ch = (cl:char format-string pos)
          do (if (char= ch #\~)
                 (multiple-value-bind (params colonp atsignp directive next)
                     (%vm-format-parse-directive format-string (1+ pos) ctx)
                   (let ((dir (char-upcase directive)))
                     (case dir
                        (#\A (%vm-format-pad stream (princ-to-string (%vm-format-next-arg ctx))
                                             (%vm-format-param params 0 nil)
                                             (%vm-format-param params 1 #\Space)
                                             atsignp ctx))
                        (#\S (%vm-format-pad stream (write-to-string (%vm-format-next-arg ctx))
                                             (%vm-format-param params 0 nil)
                                             (%vm-format-param params 1 #\Space)
                                             atsignp ctx))
                        (#\% (dotimes (_ (or (%vm-format-param params 0 1) 1))
                               (declare (ignore _))
              (cl:terpri stream)
                               (setf (%vm-format-context-column ctx) 0)))
                        (#\& (dotimes (_ (or (%vm-format-param params 0 1) 1))
                               (declare (ignore _))
              (cl:fresh-line stream)
                               (setf (%vm-format-context-column ctx) 0)))
                       (#\~ (dotimes (_ (or (%vm-format-param params 0 1) 1))
                              (declare (ignore _))
              (cl:write-char #\~ stream)
                               (incf (%vm-format-context-column ctx))))
                        (#\D (%vm-format-integer (%vm-format-next-arg ctx) 10 colonp atsignp params stream))
                        (#\B (%vm-format-integer (%vm-format-next-arg ctx) 2 colonp atsignp params stream))
                        (#\O (%vm-format-integer (%vm-format-next-arg ctx) 8 colonp atsignp params stream))
                        (#\X (%vm-format-integer (%vm-format-next-arg ctx) 16 colonp atsignp params stream))
                       (#\R (%vm-format-radix (%vm-format-next-arg ctx) colonp atsignp params stream))
                       ((#\F #\E #\G #\$) (%vm-format-float dir (%vm-format-next-arg ctx) params stream))
                        (#\C (%vm-format-character (%vm-format-next-arg ctx) colonp atsignp stream))
                        (#\P (%vm-format-plural ctx colonp atsignp stream))
                        (#\W (%vm-format-write-directive ctx colonp atsignp stream))
                        (#\T (let* ((colnum (or (%vm-format-param params 0 1) 1))
                                     (colinc (or (%vm-format-param params 1 1) 1))
                                     (current (%vm-format-context-column ctx))
                                      (target (if atsignp (+ current colnum) (max 0 (1- colnum))))
                                     (spaces (if (>= current target) 0 (- target current))))
                                  (dotimes (_ spaces) (declare (ignore _))
                     (cl:write-char #\Space stream))
                                  (setf (%vm-format-context-column ctx) (+ current spaces))))
                        (#\| (dotimes (_ (or (%vm-format-param params 0 1) 1))
                               (declare (ignore _))
              (cl:write-char #\Page stream)
                                (incf (%vm-format-context-column ctx))))
                       (#\* (let ((n (or (%vm-format-param params 0 1) 1)))
                              (cond
                                (colonp (decf (%vm-format-context-index ctx) n))
                                (atsignp (setf (%vm-format-context-index ctx) n))
                                (t (incf (%vm-format-context-index ctx) n)))
                              (setf (%vm-format-context-index ctx)
                                    (max 0 (min (%vm-format-context-index ctx)
                                                 (cl:length (%vm-format-context-args ctx)))))))
                       (#\? (let ((subfmt (%vm-format-next-arg ctx)))
                              (if atsignp
                                  (%vm-format-render subfmt ctx stream)
                                  (let ((subargs (%vm-format-next-arg ctx)))
                                    (%vm-format-write
                                     stream
                                     (%vm-format-render-to-string
                                       subfmt
                                       (cond
                                         ((stringp subargs) (list subargs))
                                         ((vectorp subargs) (coerce subargs 'list))
                                         ((listp subargs) subargs)
                                         (t (list subargs))))
                                     ctx)))))
                       (#\[ (multiple-value-bind (section-end after-section)
                                (%vm-format-find-section-end format-string next #\[)
                              (multiple-value-bind (clauses else-index)
                                  (%vm-format-split-clauses (cl:subseq format-string next section-end))
                                (let* ((selector (cond
                                                   (colonp (if (%vm-format-next-arg ctx) 1 0))
                                                   (atsignp (let ((arg (%vm-format-peek-arg ctx)))
                                                              (if arg 0 nil)))
                                                   (t (%vm-format-next-arg ctx))))
                                       (selected (cond
                                                   ((null selector) nil)
                                                   ((and (integerp selector)
                                                          (< -1 selector (cl:length clauses)))
                                                    (nth selector clauses))
                                                   (else-index (nth else-index clauses)))))
                                  (when selected
                                    (%vm-format-render selected ctx stream))))
                              (setf next after-section)))
                       (#\{ (multiple-value-bind (section-end after-section)
                                (%vm-format-find-section-end format-string next #\{)
                               (let* ((body (cl:subseq format-string next section-end))
                                     (max-iterations (%vm-format-param params 0 nil))
                                      (items (cond
                                               (atsignp nil)
                                               (t (%vm-format-next-arg ctx))))
                                      (caretp (and (not atsignp)
                                                   (not colonp)
                                                   (search "~^" body :test #'char-equal))))
                                 (loop with count = 0
                                       while (or (null max-iterations) (< count max-iterations))
                                       do (cond
                                            (atsignp
                                             (when (zerop (%vm-format-remaining-count ctx)) (return))
                                             (%vm-format-render body ctx stream))
                                            ((null items) (return))
                                            ((and caretp (plusp count) (null (cdr items)))
                                             (return))
                                            (colonp
                                             (%vm-format-write stream
                                                               (%vm-format-render-to-string body (pop items))))
                                           (t (%vm-format-write stream
                                                                (%vm-format-render-to-string body (list (pop items))))))
                                         (incf count)))
                              (setf next after-section)))
                       (#\< (multiple-value-bind (section-end after-section)
                                (%vm-format-find-section-end format-string next #\<)
                               (let* ((body (cl:subseq format-string next section-end))
                                     (text (with-output-to-string (out)
                                             (%vm-format-render body ctx out))))
                                (%vm-format-pad stream text (%vm-format-param params 0 0)
                                                (%vm-format-param params 1 #\Space)
                                                atsignp))
                              (setf next after-section)))
                       (#\^ (when (or (null params)
                                      (zerop (%vm-format-remaining-count ctx)))
                              (return-from %vm-format-render (values ctx t))))
                         (#\/ (let ((slash (cl:position #\/ format-string :start next :end limit)))
                                (unless slash (error "Unterminated ~~/ FORMAT directive"))
                                (%vm-format-call-user-function (cl:subseq format-string next slash)
                                                             (%vm-format-next-arg ctx)
                                                             colonp atsignp params stream)
                               (setf next (1+ slash))))
                        ;; ─── ~I (indent) ────────────────────────────────────────
                        ;; ~nI → indent relative. ~n:I → indent to column n.
                        ;; ~n@I → indent relative to current column + n.
                        (#\I (let ((n (or (%vm-format-param params 0 0) 0)))
                               (cond
                                 (colonp
                                  ;; ~n:I — indent to absolute column n
                                  (let* ((current (%vm-format-context-column ctx))
                                         (spaces (if (> n current) (- n current) 0)))
                                    (dotimes (_ spaces) (declare (ignore _))
                  (cl:write-char #\Space stream))
                                    (setf (%vm-format-context-column ctx)
                                          (max (%vm-format-context-column ctx) n))))
                                 (atsignp
                                  ;; ~n@I — newline then indent n relative to current position
              (cl:terpri stream)
                                  (setf (%vm-format-context-column ctx) 0)
                                  (dotimes (_ n) (declare (ignore _))
                  (cl:write-char #\Space stream))
                                  (incf (%vm-format-context-column ctx) n))
                                 (t
                                  ;; ~nI — indent n spaces relative to current position (no newline)
                                  (dotimes (_ n) (declare (ignore _))
                  (cl:write-char #\Space stream))
                                  (incf (%vm-format-context-column ctx) n)))))
                        ;; ─── ~_ (conditional newline) ───────────────────────────
                        ;; ~_ → newline. ~n_ → n newlines.
                        ;; ~:_ → process like ~% but at section start (pprint).
                        ;; ~@_ → call pprint-newline :fill.
                        (#\_ (let ((n (or (%vm-format-param params 0 1) 1)))
                               (cond
                                 (colonp
                                  ;; ~:_ — like ~% for pprint (basic: just newline)
                                  (dotimes (_ n) (declare (ignore _))
                  (cl:terpri stream)
                                    (setf (%vm-format-context-column ctx) 0)))
                                 (atsignp
                                  ;; ~@_ — pprint-newline :fill (basic: conditional newline)
                                  (when (plusp n)
                   (cl:terpri stream)
                                    (setf (%vm-format-context-column ctx) 0)))
                                 (t
                                  ;; ~_ — emit newline
                                  (dotimes (_ n) (declare (ignore _))
                   (cl:terpri stream)
                                    (setf (%vm-format-context-column ctx) 0))))))
                       (otherwise (error "Unsupported FORMAT directive: ~A" directive)))
                     (setf pos next)))
                  (progn
              (cl:write-char ch stream)
                    (if (char= ch #\Newline)
                        (setf (%vm-format-context-column ctx) 0)
                        (incf (%vm-format-context-column ctx)))
                    (incf pos))))
    (values ctx nil)))

(defun %vm-format-render-to-string (format-string arg-vals)
  (let ((ctx (%make-vm-format-context (coerce arg-vals 'vector))))
    (values (with-output-to-string (out)
              (%vm-format-render format-string ctx out))
            (%vm-format-context-index ctx))))

(defun %vm-format-native (format-string arg-vals &optional stream)
  "Render FORMAT-STRING with ARG-VALS using cl-cc's native VM FORMAT processor.
When STREAM is NIL, return the produced string.  When STREAM is non-NIL, write to
it via runtime stream functions and return NIL."
  (check-type format-string string)
  (if stream
      (progn
        (%vm-format-render format-string
                           (%make-vm-format-context (coerce arg-vals 'vector))
                           stream)
        nil)
      (with-output-to-string (out)
        (%vm-format-render format-string
                           (%make-vm-format-context (coerce arg-vals 'vector))
                           out))))

(defmethod execute-instruction ((inst vm-format-inst) state pc labels)
  (declare (ignore labels))
  (let* ((fmt-str (vm-reg-get state (vm-fmt inst)))
         (arg-vals (mapcar (lambda (r) (vm-reg-get state r)) (vm-arg-regs inst)))
         (result (handler-case
                     (%vm-format-native fmt-str arg-vals)
                   (error ()
                     (if *vm-self-host-mode*
                         (error "FORMAT self-hosting required: native renderer failed on ~S"
                                fmt-str)
                         ;; Host fallback for unit-test/bootstrap contexts only.
                         (apply #'format nil fmt-str arg-vals))))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-make-string-output-stream-inst) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (make-string-output-stream))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-get-output-stream-string-inst) state pc labels)
  (declare (ignore labels))
  (let ((stream (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (get-output-stream-string stream))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-stream-write-string-inst) state pc labels)
  (declare (ignore labels))
  (let* ((stream-val (vm-reg-get state (vm-stream-reg inst)))
         (stream (if (streamp stream-val)
                     stream-val
                     (vm-get-stream state stream-val)))
         (str (vm-reg-get state (vm-src inst))))
    (write-string str stream)
    (values (1+ pc) nil nil)))

;;; ─── Reader Instructions (use cl-cc's own lexer/parser) ─────────────────

(defun %vm-global-value-by-symbol-name (state name &optional default)
  "Return the first VM global whose symbol-name is NAME."
  (let ((value default)
        (foundp nil))
    (maphash (lambda (key candidate)
               (when (and (not foundp)
                          (symbolp key)
                          (string= (symbol-name key) name))
                 (setf value candidate
                       foundp t)))
             (vm-global-vars state))
    (values value foundp)))

(defun %vm-read-eval-enabled-p (state)
  (multiple-value-bind (flag foundp)
      (%vm-global-value-by-symbol-name state "*READ-EVAL*" t)
    (if foundp flag t)))

(defun %vm-host-readtable-for-state (state)
  "Build a host readtable reflecting supported VM readtable controls."
  (multiple-value-bind (readtable foundp)
      (%vm-global-value-by-symbol-name state "*READTABLE*" nil)
    (when (and foundp
               (hash-table-p readtable)
               (gethash :readtable readtable))
      (let ((host-readtable (cl:copy-readtable (%vm-readtable-host readtable))))
        (when (member (readtable-case readtable) '(:upcase :downcase :preserve :invert))
          (setf (cl:readtable-case host-readtable) (readtable-case readtable)))
        host-readtable))))

(define-vm-instruction vm-read-from-string-inst (vm-instruction)
  "Read an S-expression from a string using cl-cc's own parser."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :read-from-string)
  (:sexp-slots dst src))

(define-vm-instruction vm-read-sexp-inst (vm-instruction)
  "Read an S-expression from a stream handle using cl-cc's own parser."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :read-sexp)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-read-from-string-inst) state pc labels)
  (declare (ignore labels))
  (let* ((str (vm-reg-get state (vm-src inst))))
    ;; Use CL's read-from-string to get both the value and the end position (FR-617)
    (multiple-value-bind (value end-pos)
        (if (stringp str)
            (let ((*read-eval* (%vm-read-eval-enabled-p state))
                  (*readtable* (or (%vm-host-readtable-for-state state)
                                   *readtable*)))
              (cl:read-from-string str nil nil))
            (values nil 0))
      (vm-reg-set state (vm-dst inst) value)
      ;; Store both values so (multiple-value-bind (obj pos) (read-from-string ...) ...) works
      (setf (vm-values-list state) (list value end-pos))
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-read-sexp-inst) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-src inst)))
         (stream (vm-get-stream state handle))
         (line (read-line stream nil nil))
         (value (when line
                  (let ((*read-eval* (%vm-read-eval-enabled-p state))
                        (*readtable* (or (%vm-host-readtable-for-state state)
                                         *readtable*)))
                    (if *vm-parse-forms-hook*
                        (first (funcall *vm-parse-forms-hook* line))
                        (cl:read-from-string line nil nil))))))
    (vm-reg-set state (vm-dst inst) value)
    (values (1+ pc) nil nil)))

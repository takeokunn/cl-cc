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
    (princ val (vm-output-stream state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-prin1) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (prin1 val (vm-output-stream state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-print-inst) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (print val (vm-output-stream state))
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
    (let ((cl:*print-base*    (%vm-read-print-var state '*print-base*   10))
          (cl:*print-radix*   (%vm-read-print-var state '*print-radix*  nil))
          (cl:*print-escape*  (%vm-read-print-var state '*print-escape* t))
          (cl:*print-level*   (%vm-read-print-var state '*print-level*  nil))
          (cl:*print-length*  (%vm-read-print-var state '*print-length* nil))
          (cl:*print-circle*  (%vm-read-print-var state '*print-circle* nil))
          (cl:*print-readably* (%vm-read-print-var state '*print-readably* nil))
          (cl:*print-pretty*  (%vm-read-print-var state '*print-pretty* nil))
          (cl:*print-pprint-dispatch*
            (%pprint-dispatch-host
             (%vm-read-print-var state '*print-pprint-dispatch*
                                 *print-pprint-dispatch*)))
          (cl:*print-case*    (%vm-read-print-var state '*print-case*   :upcase)))
      (vm-reg-set state (vm-dst inst) (write-to-string val)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-princ-to-string-inst) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (princ-to-string val))
    (values (1+ pc) nil nil)))

;;; ─── Native FORMAT Directive Processor ───────────────────────────────────

(defstruct (%vm-format-context (:constructor %make-vm-format-context (args)))
  (args #() :type vector)
  (index 0 :type fixnum))

(defun %vm-format-remaining-count (ctx)
  (- (length (%vm-format-context-args ctx))
     (%vm-format-context-index ctx)))

(defun %vm-format-next-arg (ctx)
  (let ((index (%vm-format-context-index ctx)))
    (when (>= index (length (%vm-format-context-args ctx)))
      (error "FORMAT argument exhausted"))
    (prog1 (aref (%vm-format-context-args ctx) index)
      (setf (%vm-format-context-index ctx) (1+ index)))))

(defun %vm-format-peek-arg (ctx)
  (let ((index (%vm-format-context-index ctx)))
    (when (>= index (length (%vm-format-context-args ctx)))
      (error "FORMAT argument exhausted"))
    (aref (%vm-format-context-args ctx) index)))

(defun %vm-format-write (stream string)
  (rt-write-string string stream))

(defun %vm-format-pad (stream string mincol padchar atsignp)
  (let* ((text (princ-to-string string))
         (needed (max 0 (- (or mincol 0) (length text)))))
    (if atsignp
        (progn
          (%vm-format-write stream text)
          (dotimes (_ needed) (declare (ignore _)) (rt-write-char padchar stream)))
        (progn
          (dotimes (_ needed) (declare (ignore _)) (rt-write-char padchar stream))
          (%vm-format-write stream text)))))

(defun %vm-format-param (params index &optional default)
  (let ((value (nth index params)))
    (if (null value) default value)))

(defun %vm-format-group-digits (string comma-char comma-interval)
  (let* ((signp (and (> (length string) 0) (find (char string 0) "+-")))
         (start (if signp 1 0))
         (digits (subseq string start))
         (interval (or comma-interval 3))
         (comma (or comma-char #\,)))
    (with-output-to-string (out)
      (when signp (write-char (char string 0) out))
      (loop for i from 0 below (length digits)
            when (and (> i 0)
                      (zerop (mod (- (length digits) i) interval)))
              do (write-char comma out)
            do (write-char (char digits i) out)))))

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
                             (concatenate 'string (subseq base 0 (max 0 (- (length base) 1))) "ieth")
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
  ;; Float printing is intentionally simple; it uses CL's numeric conversion for
  ;; one directive while the VM processor owns directive parsing and dispatch.
  (let ((control (with-output-to-string (out)
                   (write-char #\~ out)
                   (loop for p on params
                         for v = (car p)
                         for firstp = t then nil
                         unless firstp do (write-char #\, out)
                         when v do (princ v out))
                   (write-char directive out))))
    (%vm-format-write stream (format nil control value))))

(defun %vm-format-character (char colonp atsignp stream)
  (let ((ch (if (characterp char) char (code-char char))))
    (cond
      ((and colonp atsignp) (%vm-format-write stream (format nil "#\\~A" (or (char-name ch) ch))))
      (colonp (%vm-format-write stream (or (char-name ch) (string ch))))
      (atsignp (%vm-format-write stream (format nil "#\\~A" ch)))
      (t (rt-write-char ch stream)))))

(defun %vm-format-directive-char-p (char)
  (or (alpha-char-p char)
      (find char "%&~*?[]{}<>^/;$")))

(defun %vm-format-parse-param-token (token ctx)
  (cond
    ((string= token "") nil)
    ((string= token "#") (%vm-format-remaining-count ctx))
    ((string-equal token "V") (if (plusp (%vm-format-remaining-count ctx))
                                  (%vm-format-next-arg ctx)
                                  nil))
    ((and (> (length token) 1) (char= (char token 0) #\'))
     (char token 1))
    (t (parse-integer token))))

(defun %vm-format-parse-directive (format-string start ctx)
  (let ((len (length format-string))
        (pos start)
        (params nil)
        (token "")
        (colonp nil)
        (atsignp nil))
    (labels ((push-param ()
               (push (%vm-format-parse-param-token token ctx) params)
               (setf token "")))
      (loop while (< pos len)
            for ch = (char format-string pos)
            do (cond
                 ((char= ch #\,)
                  (push-param)
                  (incf pos))
                 ((char= ch #\:)
                  (when (> (length token) 0) (push-param))
                  (setf colonp t)
                  (incf pos))
                 ((char= ch #\@)
                  (when (> (length token) 0) (push-param))
                  (setf atsignp t)
                  (incf pos))
                 ((and (char= ch #\Space)
                       (or params (> (length token) 0)))
                  (when (> (length token) 0) (push-param))
                  (incf pos))
                 ((%vm-format-directive-char-p ch)
                  (when (> (length token) 0) (push-param))
                  (return (values (nreverse params) colonp atsignp ch (1+ pos))))
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
        (len (length format-string)))
    (loop while (< pos len)
          do (if (char= (char format-string pos) #\~)
                 (multiple-value-bind (_params _colonp _atsignp dir next)
                     (%vm-format-parse-directive format-string (1+ pos)
                                                (%make-vm-format-context nil))
                   (declare (ignore _params _colonp _atsignp))
                   (cond
                     ((char= dir open) (incf depth))
                     ((char= dir close)
                      (if (zerop depth)
                          (return (values pos next))
                          (decf depth))))
                   (setf pos next))
                 (incf pos)))
    (error "Unterminated FORMAT section ~A in ~S" open format-string)))

(defun %vm-format-split-clauses (string)
  (let ((clauses nil)
        (else-index nil)
        (start 0)
        (pos 0)
        (len (length string)))
    (loop while (< pos len)
          do (if (char= (char string pos) #\~)
                 (multiple-value-bind (_params colonp _atsignp dir next)
                     (%vm-format-parse-directive string (1+ pos)
                                                (%make-vm-format-context nil))
                   (declare (ignore _params _atsignp))
                   (if (char= dir #\;)
                       (progn
                         (push (subseq string start pos) clauses)
                         (when colonp (setf else-index (length clauses)))
                         (setf start next))
                       (setf pos next)))
                 (incf pos)))
    (push (subseq string start) clauses)
    (values (nreverse clauses) else-index)))

(defun %vm-format-call-user-function (name arg colonp atsignp params stream)
  (let* ((slash (position #\: name :from-end t))
         (pkg-name (and slash (subseq name 0 slash)))
         (sym-name (if slash (subseq name (1+ slash)) name))
         (package (if pkg-name (find-package (string-upcase pkg-name)) *package*)))
    (unless package (error "Unknown FORMAT function package: ~A" pkg-name))
    (let ((symbol (find-symbol (string-upcase sym-name) package)))
      (unless (and symbol (fboundp symbol))
        (error "Unknown FORMAT function: ~A" name))
      (apply (symbol-function symbol) stream arg colonp atsignp params))))

(defun %vm-format-render (format-string ctx stream &key (start 0) end)
  (let ((pos start)
        (limit (or end (length format-string))))
    (loop while (< pos limit)
          for ch = (char format-string pos)
          do (if (char= ch #\~)
                 (multiple-value-bind (params colonp atsignp directive next)
                     (%vm-format-parse-directive format-string (1+ pos) ctx)
                   (let ((dir (char-upcase directive)))
                     (case dir
                       (#\A (%vm-format-pad stream (princ-to-string (%vm-format-next-arg ctx))
                                            (%vm-format-param params 0 nil)
                                            (%vm-format-param params 1 #\Space)
                                            atsignp))
                       (#\S (%vm-format-pad stream (write-to-string (%vm-format-next-arg ctx))
                                            (%vm-format-param params 0 nil)
                                            (%vm-format-param params 1 #\Space)
                                            atsignp))
                       (#\% (dotimes (_ (or (%vm-format-param params 0 1) 1))
                              (declare (ignore _))
                              (rt-terpri stream)))
                       (#\& (dotimes (_ (or (%vm-format-param params 0 1) 1))
                              (declare (ignore _))
                              (rt-fresh-line stream)))
                       (#\~ (dotimes (_ (or (%vm-format-param params 0 1) 1))
                              (declare (ignore _))
                              (rt-write-char #\~ stream)))
                       (#\D (%vm-format-integer (%vm-format-next-arg ctx) 10 colonp atsignp params stream))
                       (#\B (%vm-format-integer (%vm-format-next-arg ctx) 2 colonp atsignp params stream))
                       (#\O (%vm-format-integer (%vm-format-next-arg ctx) 8 colonp atsignp params stream))
                       (#\X (%vm-format-integer (%vm-format-next-arg ctx) 16 colonp atsignp params stream))
                       (#\R (%vm-format-radix (%vm-format-next-arg ctx) colonp atsignp params stream))
                       ((#\F #\E #\G #\$) (%vm-format-float dir (%vm-format-next-arg ctx) params stream))
                       (#\C (%vm-format-character (%vm-format-next-arg ctx) colonp atsignp stream))
                       (#\T (let ((colnum (or (%vm-format-param params 0 1) 1)))
                              (dotimes (_ colnum) (declare (ignore _)) (rt-write-char #\Space stream))))
                       (#\* (let ((n (or (%vm-format-param params 0 1) 1)))
                              (cond
                                (colonp (decf (%vm-format-context-index ctx) n))
                                (atsignp (setf (%vm-format-context-index ctx) n))
                                (t (incf (%vm-format-context-index ctx) n)))
                              (setf (%vm-format-context-index ctx)
                                    (max 0 (min (%vm-format-context-index ctx)
                                                (length (%vm-format-context-args ctx)))))))
                       (#\? (let ((subfmt (%vm-format-next-arg ctx)))
                              (if atsignp
                                  (%vm-format-render subfmt ctx stream)
                                  (let ((subargs (%vm-format-next-arg ctx)))
                                    (%vm-format-write stream (%vm-format-render-to-string subfmt subargs))))))
                       (#\[ (multiple-value-bind (section-end after-section)
                                (%vm-format-find-section-end format-string next #\[)
                              (multiple-value-bind (clauses else-index)
                                  (%vm-format-split-clauses (subseq format-string next section-end))
                                (let* ((selector (cond
                                                   (colonp (if (%vm-format-next-arg ctx) 1 0))
                                                   (atsignp (let ((arg (%vm-format-peek-arg ctx)))
                                                              (if arg 0 nil)))
                                                   (t (%vm-format-next-arg ctx))))
                                       (selected (cond
                                                   ((null selector) nil)
                                                   ((and (integerp selector)
                                                         (< -1 selector (length clauses)))
                                                    (nth selector clauses))
                                                   (else-index (nth else-index clauses)))))
                                  (when selected
                                    (%vm-format-render selected ctx stream))))
                              (setf next after-section)))
                       (#\{ (multiple-value-bind (section-end after-section)
                                (%vm-format-find-section-end format-string next #\{)
                              (let* ((body (subseq format-string next section-end))
                                     (max-iterations (%vm-format-param params 0 nil))
                                     (items (cond
                                              (atsignp nil)
                                              (t (%vm-format-next-arg ctx)))))
                                (loop with count = 0
                                      while (or (null max-iterations) (< count max-iterations))
                                      do (cond
                                           (atsignp
                                            (when (zerop (%vm-format-remaining-count ctx)) (return))
                                            (%vm-format-render body ctx stream))
                                           ((null items) (return))
                                           (colonp
                                            (%vm-format-write stream
                                                              (%vm-format-render-to-string body (pop items))))
                                           (t (%vm-format-write stream
                                                                (%vm-format-render-to-string body (list (pop items))))))
                                         (incf count)))
                              (setf next after-section)))
                       (#\< (multiple-value-bind (section-end after-section)
                                (%vm-format-find-section-end format-string next #\<)
                              (let* ((body (subseq format-string next section-end))
                                     (text (with-output-to-string (out)
                                             (%vm-format-render body ctx out))))
                                (%vm-format-pad stream text (%vm-format-param params 0 0)
                                                (%vm-format-param params 1 #\Space)
                                                atsignp))
                              (setf next after-section)))
                       (#\^ (when (or (null params)
                                      (zerop (%vm-format-remaining-count ctx)))
                              (return-from %vm-format-render (values ctx t))))
                       (#\/ (let ((slash (position #\/ format-string :start next :end limit)))
                              (unless slash (error "Unterminated ~~/ FORMAT directive"))
                              (%vm-format-call-user-function (subseq format-string next slash)
                                                            (%vm-format-next-arg ctx)
                                                            colonp atsignp params stream)
                              (setf next (1+ slash))))
                       (otherwise (error "Unsupported FORMAT directive: ~A" directive)))
                     (setf pos next)))
                 (progn
                   (rt-write-char ch stream)
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
                     ;; Keep the host fallback for directives outside the native
                     ;; subset and for compatibility while FR-626 matures.
                     (apply #'format nil fmt-str arg-vals)))))
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

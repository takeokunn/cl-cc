;;;; diagnostics.lisp — Rust-style diagnostic messages for the parser

(in-package :cl-cc/parse)

;;; Diagnostic structs

(defstruct fix-it
  "A suggested source edit associated with a diagnostic."
  (text "" :type string)              ; replacement / suggestion text
  (span (cons 0 0)))                  ; (start-byte . end-byte)

(defstruct diagnostic
  "A diagnostic message with source location."
  (severity    :error :type keyword)  ; :error :warning :note
  (span        (cons 0 0))           ; (start-byte . end-byte)
  (source-file nil)
  (message     "" :type string)
  (hints       nil :type list)       ; ((label . (start . end)) ...)
  (notes       nil :type list)       ; list of strings
  (error-code  nil)                  ; machine-readable code, e.g. "E0001"
  (fix-it      nil))                 ; optional FIX-IT suggestion

(defstruct diag-source-location
  "A resolved source location for diagnostics."
  (file nil)
  (line 1 :type integer)
  (column 1 :type integer)
  (byte-offset 0 :type integer))

;;; Location helpers

(defun byte-offset-to-line-col (source byte-offset)
  "Convert a byte offset to (values line column) in SOURCE. Both 1-based."
  (let ((line 1) (col 1))
    (loop for i from 0 below (min byte-offset (length source))
          for ch = (char source i)
          do (if (char= ch #\Newline)
                 (progn (incf line) (setf col 1))
                 (incf col)))
    (values line col)))

(defun source-line-at (source byte-offset)
  "Return the full source line containing BYTE-OFFSET."
  (let* ((start (let ((pos (position #\Newline source :end (min byte-offset (length source)) :from-end t)))
                  (if pos (1+ pos) 0)))
         (end (or (position #\Newline source :start (min byte-offset (length source)))
                  (length source))))
    (subseq source start end)))

;;; Formatting

(defun format-diagnostic (diag source &optional (stream *standard-output*))
  "Format DIAG in Rust-style diagnostic format."
  (let* ((sev (diagnostic-severity diag))
         (msg (diagnostic-message diag))
         (span (diagnostic-span diag))
         (file (or (diagnostic-source-file diag) "<input>"))
         (start-byte (car span)))
    (multiple-value-bind (line col) (byte-offset-to-line-col source start-byte)
      ;; severity: message
      (format stream "~&~(~A~): ~A~%" sev msg)
      ;; --> file:line:col
      (format stream "  --> ~A:~D:~D~%" file line col)
      ;; source line
      (let ((src-line (source-line-at source start-byte)))
        (format stream "   |~%")
        (format stream "~2D | ~A~%" line src-line)
        ;; caret underline
        (format stream "   | ~A^~%" (make-string (max 0 (1- col)) :initial-element #\Space))))
    ;; hints
    (dolist (h (diagnostic-hints diag))
      (format stream "   = hint: ~A~%" (car h)))
    ;; notes
    (dolist (n (diagnostic-notes diag))
      (format stream "   = note: ~A~%" n))
    ;; structured fields
    (when (diagnostic-error-code diag)
      (format stream "   = code: ~A~%" (diagnostic-error-code diag)))
    (when (diagnostic-fix-it diag)
      (let ((fix-it (diagnostic-fix-it diag)))
        (format stream "   = fix-it: replace ~S with ~S~%"
                (fix-it-span fix-it)
                (fix-it-text fix-it))))))

(defun format-diagnostic-list (diags source &optional (stream *standard-output*))
  "Format a list of diagnostics with summary.
When *WERROR-P* is non-NIL, warnings are promoted to errors before output."
  (let ((diags (if *werror-p*
                   (mapcar (lambda (d)
                             (if (eq (diagnostic-severity d) :warning)
                                 (make-diagnostic :severity :error
                                                  :span (diagnostic-span d)
                                                  :source-file (diagnostic-source-file d)
                                                  :message (format nil "[upgraded from warning] ~A"
                                                                   (diagnostic-message d))
                                                  :hints (diagnostic-hints d)
                                                  :notes (diagnostic-notes d)
                                                  :error-code (diagnostic-error-code d)
                                                  :fix-it (diagnostic-fix-it d))
                                 d))
                           diags)
                   diags)))
    (dolist (d diags)
      (format-diagnostic d source stream)
      (terpri stream))
    (let ((nerr (count :error diags :key #'diagnostic-severity))
          (nwarn (count :warning diags :key #'diagnostic-severity)))
      (when *werror-p*
        (format stream "~&note: warnings treated as errors (-Werror)~%"))
      (format stream "~&~D error~:P, ~D warning~:P~%" nerr nwarn))))

;;; Warnings-as-errors (FR-485)

(defvar *werror-p* nil
  "When non-NIL, all diagnostics with :warning severity are promoted to :error.")

;;; Did You Mean? suggestion engine (FR-484)

(defun levenshtein-distance (s1 s2)
  "Compute the Levenshtein (edit) distance between strings S1 and S2."
  (let* ((n (length s1))
         (m (length s2))
         (d (make-array (list (1+ n) (1+ m)) :initial-element 0)))
    (dotimes (i (1+ n)) (setf (aref d i 0) i))
    (dotimes (j (1+ m)) (setf (aref d 0 j) j))
    (dotimes (i n)
      (dotimes (j m)
        (setf (aref d (1+ i) (1+ j))
              (min (1+ (aref d i (1+ j)))
                   (1+ (aref d (1+ i) j))
                   (+ (aref d i j)
                      (if (char= (char s1 i) (char s2 j)) 0 1))))))
    (aref d n m)))

(defun did-you-mean (target candidates &key (max-distance 3) (max-results 3))
  "Return the top MAX-RESULTS candidates closest to TARGET by Levenshtein distance.
Candidates with distance > MAX-DISTANCE are filtered out."
  (let ((scored (loop for c in candidates
                      for d = (levenshtein-distance (string-downcase target)
                                                     (string-downcase (string c)))
                      when (<= d max-distance)
                      collect (cons c d))))
    (mapcar #'car (stable-sort scored #'< :key #'cdr))))

;;; Convenience constructors

(defun make-parse-error (message span &key source-file hints notes error-code fix-it)
  (make-diagnostic :severity :error :message message :span span
                   :source-file source-file :hints hints :notes notes
                   :error-code error-code :fix-it fix-it))

(defun make-parse-warning (message span &key source-file hints notes error-code fix-it)
  (make-diagnostic :severity :warning :message message :span span
                   :source-file source-file :hints hints :notes notes
                   :error-code error-code :fix-it fix-it))

;;; Condition

(define-condition parse-failure (error)
  ((diagnostics :initarg :diagnostics :reader parse-failure-diagnostics :type list))
  (:report (lambda (condition stream)
             (format stream "Parse failed with ~D error~:P"
                     (length (parse-failure-diagnostics condition))))))

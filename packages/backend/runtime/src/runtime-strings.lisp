;;;; packages/backend/runtime/src/runtime-strings.lisp - CL-CC Runtime: Strings and Characters
;;;
;;; Contains: rt-make-string, rt-string-*, rt-char-*, rt-digit-char, rt-char-name,
;;; rt-name-char, rt-parse-integer.
;;;
;;; Depends on runtime.lisp (define-rt-predicate, define-rt-binary-predicate).
;;; Load order: after runtime-ops.lisp.

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Strings
;;; ------------------------------------------------------------

(defun rt-make-string (len &optional (char #\Space))
  (make-string len :initial-element char))
(defun rt-string-length (s) (length s))
(defun rt-string-ref (s idx) (char s idx))
(defun rt-string-set (s idx c) (setf (char s idx) c))
(define-rt-binary-predicate rt-string=              string=)
(define-rt-binary-predicate rt-string<              string<)
(define-rt-binary-predicate rt-string>              string>)
(define-rt-binary-predicate rt-string<=             string<=)
(define-rt-binary-predicate rt-string>=             string>=)
(define-rt-binary-predicate rt-string-equal-ci      string-equal)
(define-rt-binary-predicate rt-string-lessp         string-lessp)
(define-rt-binary-predicate rt-string-greaterp      string-greaterp)
(define-rt-binary-predicate rt-string-not-equal     string-not-equal)
(define-rt-binary-predicate rt-string-not-greaterp  string-not-greaterp)
(define-rt-binary-predicate rt-string-not-lessp     string-not-lessp)
(defun rt-string-upcase (s) (string-upcase s))
(defun rt-string-downcase (s) (string-downcase s))
(defun rt-string-capitalize (s) (string-capitalize s))
(defun rt-string-trim (bag s) (string-trim bag s))
(defun rt-string-left-trim (bag s) (string-left-trim bag s))
(defun rt-string-right-trim (bag s) (string-right-trim bag s))
(defun rt-search-string (pattern target) (search pattern target))
(defun rt-subseq (s start &optional end)
  (if end (subseq s start end) (subseq s start)))
(defun rt-concatenate-seqs (type &rest seqs)
  (apply #'concatenate type seqs))

;;; ------------------------------------------------------------
;;; Characters
;;; ------------------------------------------------------------

(defun rt-char (s idx) (char s idx))
(defun rt-char-code (c) (char-code c))
(defun rt-code-char (n) (code-char n))
(define-rt-binary-predicate rt-char-equal-cs        char=)
(define-rt-binary-predicate rt-char-lt-cs           char<)
(define-rt-binary-predicate rt-char-gt-cs           char>)
(define-rt-binary-predicate rt-char-le-cs           char<=)
(define-rt-binary-predicate rt-char-ge-cs           char>=)
(define-rt-binary-predicate rt-char-ne-cs           char/=)
(define-rt-binary-predicate rt-char-equal-ci        char-equal)
(define-rt-binary-predicate rt-char-not-equal-ci    char-not-equal)
(define-rt-binary-predicate rt-char-lessp-ci        char-lessp)
(define-rt-binary-predicate rt-char-greaterp-ci     char-greaterp)
(define-rt-binary-predicate rt-char-not-lessp-ci    char-not-lessp)
(define-rt-binary-predicate rt-char-not-greaterp-ci char-not-greaterp)
(defun rt-char-upcase (c) (char-upcase c))
(defun rt-char-downcase (c) (char-downcase c))
(define-rt-predicate rt-alpha-char-p    alpha-char-p)
(define-rt-predicate rt-digit-char-p    digit-char-p)
(define-rt-predicate rt-alphanumericp   alphanumericp)
(define-rt-predicate rt-upper-case-p    upper-case-p)
(define-rt-predicate rt-lower-case-p    lower-case-p)
(define-rt-predicate rt-both-case-p     both-case-p)
(define-rt-predicate rt-graphic-char-p  graphic-char-p)
(define-rt-predicate rt-standard-char-p standard-char-p)
(defun rt-digit-char (w &optional (radix 10)) (digit-char w radix))
(defun rt-char-name (c) (char-name c))
(defun rt-name-char (s) (name-char s))
(defun rt-parse-integer (s &key (start 0) end (radix 10) junk-allowed)
  (parse-integer s :start start :end end :radix radix :junk-allowed junk-allowed))

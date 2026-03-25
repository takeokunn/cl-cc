;;;; tests/unit/expand/lambda-list-tests.lisp — Lambda list parser unit tests
;;;;
;;;; Tests for parse-lambda-list which parses macro lambda lists into
;;;; lambda-list-info structures (&optional, &rest, &body, &key,
;;;; &allow-other-keys, &aux).

(in-package :cl-cc/test)

(defsuite lambda-list-suite :description "Lambda list parser unit tests")

;;; ─── Required-only ────────────────────────────────────────────────────────

(deftest parse-ll-required-only
  "Required-only lambda list returns params in order."
  (let ((info (cl-cc::parse-lambda-list '(a b c))))
    (assert-equal '(a b c) (cl-cc::lambda-list-info-required info))
    (assert-equal nil (cl-cc::lambda-list-info-optional info))
    (assert-equal nil (cl-cc::lambda-list-info-rest info))
    (assert-equal nil (cl-cc::lambda-list-info-key-params info))))

(deftest parse-ll-empty
  "Empty lambda list returns empty required."
  (let ((info (cl-cc::parse-lambda-list '())))
    (assert-equal nil (cl-cc::lambda-list-info-required info))))

;;; ─── &optional ────────────────────────────────────────────────────────────

(deftest parse-ll-optional-symbol
  "Symbol &optional param gets nil default and nil supplied-p."
  (let ((info (cl-cc::parse-lambda-list '(x &optional y))))
    (assert-equal '(x) (cl-cc::lambda-list-info-required info))
    (let ((opt (cl-cc::lambda-list-info-optional info)))
      (assert-equal 1 (length opt))
      (assert-equal 'y (first (first opt)))
      (assert-equal nil (second (first opt))))))

(deftest parse-ll-optional-with-default
  "&optional param with default and supplied-p."
  (let* ((info (cl-cc::parse-lambda-list '(&optional (x 42 x-p))))
         (opt (first (cl-cc::lambda-list-info-optional info))))
    (assert-equal 'x (first opt))
    (assert-equal 42 (second opt))
    (assert-equal 'x-p (third opt))))

;;; ─── &rest / &body ───────────────────────────────────────────────────────

(deftest parse-ll-rest
  "&rest captures rest parameter."
  (let ((info (cl-cc::parse-lambda-list '(x &rest args))))
    (assert-equal '(x) (cl-cc::lambda-list-info-required info))
    (assert-equal 'args (cl-cc::lambda-list-info-rest info))))

(deftest parse-ll-body
  "&body captures body parameter (same as &rest semantically)."
  (let ((info (cl-cc::parse-lambda-list '(x &body body))))
    (assert-equal 'body (cl-cc::lambda-list-info-body info))
    (assert-equal nil (cl-cc::lambda-list-info-rest info))))

;;; ─── &key ─────────────────────────────────────────────────────────────────

(deftest parse-ll-key-symbol
  "&key symbol param gets auto-generated keyword."
  (let* ((info (cl-cc::parse-lambda-list '(&key verbose)))
         (kp (first (cl-cc::lambda-list-info-key-params info))))
    ;; kp is ((keyword-name name) default supplied-p)
    (assert-equal :verbose (first (first kp)))
    (assert-equal 'verbose (second (first kp)))
    (assert-equal nil (second kp))))

(deftest parse-ll-key-with-default
  "&key param with default value."
  (let* ((info (cl-cc::parse-lambda-list '(&key (level 3))))
         (kp (first (cl-cc::lambda-list-info-key-params info))))
    (assert-equal :level (first (first kp)))
    (assert-equal 3 (second kp))))

(deftest parse-ll-allow-other-keys
  "&allow-other-keys sets flag when in :key state."
  (let ((info (cl-cc::parse-lambda-list '(&key x &allow-other-keys))))
    (assert-true (cl-cc::lambda-list-info-allow-other-keys info))))

;;; ─── &aux ─────────────────────────────────────────────────────────────────

(deftest parse-ll-aux-symbol
  "&aux symbol gets nil init."
  (let* ((info (cl-cc::parse-lambda-list '(&aux temp)))
         (aux (first (cl-cc::lambda-list-info-aux info))))
    (assert-equal 'temp (first aux))
    (assert-equal nil (second aux))))

(deftest parse-ll-aux-with-init
  "&aux param with init form."
  (let* ((info (cl-cc::parse-lambda-list '(&aux (count 0))))
         (aux (first (cl-cc::lambda-list-info-aux info))))
    (assert-equal 'count (first aux))
    (assert-equal 0 (second aux))))

;;; ─── Combined ─────────────────────────────────────────────────────────────

(deftest parse-ll-full-lambda-list
  "Full lambda list with all sections."
  (let ((info (cl-cc::parse-lambda-list
               '(a b &optional (c 1) &rest args &key (verbose nil) &aux (i 0)))))
    (assert-equal '(a b) (cl-cc::lambda-list-info-required info))
    (assert-equal 1 (length (cl-cc::lambda-list-info-optional info)))
    (assert-equal 'args (cl-cc::lambda-list-info-rest info))
    (assert-equal 1 (length (cl-cc::lambda-list-info-key-params info)))
    (assert-equal 1 (length (cl-cc::lambda-list-info-aux info)))))

;;; ─── Error cases ──────────────────────────────────────────────────────────

(deftest parse-ll-error-after-rest
  "Parameter after &rest signals error."
  (assert-true
   (handler-case
       (progn (cl-cc::parse-lambda-list '(&rest args extra)) nil)
     (error () t))))

(in-package :cl-cc/test)

(defsuite macro-lambda-list-suite
  :description "Lambda list parser unit tests"
  :parent cl-cc-unit-suite)

(in-suite macro-lambda-list-suite)

(deftest macro-lambda-list-required-only
  "parse-lambda-list preserves required parameters in order."
  (let ((info (cl-cc/expand:parse-lambda-list '(a b c))))
    (assert-equal '(a b c) (cl-cc/expand::lambda-list-info-required info))))

(deftest macro-lambda-list-key-and-aux
  "parse-lambda-list records &key and &aux sections."
  (let ((info (cl-cc/expand:parse-lambda-list '(&key verbose &aux (count 0)))))
    (assert-true (cl-cc/expand::lambda-list-info-key-params info))
    (assert-equal '((count 0)) (cl-cc/expand::lambda-list-info-aux info))))

(deftest macro-lambda-list-bindings-shape
  "generate-lambda-bindings and destructure-lambda-list return bindings."
  (assert-true (assoc 'args (cl-cc/expand:generate-lambda-bindings '(&rest args) 'form)))
  (assert-true (assoc 'x (cl-cc/expand:destructure-lambda-list '(x &optional y) 'form))))

(deftest macro-lambda-list-optional-rest-body-environment
  "parse-lambda-list records optional/rest/body/environment sections correctly."
  (let ((info (cl-cc/expand:parse-lambda-list '(a &optional (b 10 b-p) &body body &environment env))))
    (assert-equal '(a) (cl-cc/expand::lambda-list-info-required info))
    (assert-equal '((b 10 b-p)) (cl-cc/expand::lambda-list-info-optional info))
    (assert-eq 'body (cl-cc/expand::lambda-list-info-body info))
    (assert-eq 'env (cl-cc/expand:lambda-list-info-environment info))))

(deftest macro-lambda-list-allow-other-keys-and-key-spec
  "parse-lambda-list preserves explicit keyword specs and &allow-other-keys." 
  (let ((info (cl-cc/expand:parse-lambda-list '(&key ((:size n) 3 supplied-p) &allow-other-keys))))
    (assert-true (cl-cc/expand::lambda-list-info-allow-other-keys info))
    (assert-equal '(((:size n) 3 supplied-p))
                  (mapcar (lambda (spec) (list (first spec) (second spec) (third spec)))
                          (cl-cc/expand::lambda-list-info-key-params info)))))

(deftest macro-lambda-list-generate-bindings-covers-key-and-aux
  "generate-lambda-bindings emits bindings for &key supplied-p and &aux init forms." 
  (let ((bindings (cl-cc/expand:generate-lambda-bindings
                   '(x &key ((:size n) 3 n-p) &aux (count 0))
                   'form)))
    (assert-true (assoc 'x bindings))
    (assert-true (assoc 'n bindings))
    (assert-true (assoc 'n-p bindings))
    (assert-true (assoc 'count bindings))))

(deftest macro-lambda-list-destructure-covers-nested-required-and-key
  "destructure-lambda-list handles nested required patterns and keyword parameters." 
  (let ((bindings (cl-cc/expand:destructure-lambda-list
                   '((head tail) &key ((:limit lim) 5 lim-p) &aux (done nil))
                   'form)))
    (assert-true (assoc 'head bindings))
    (assert-true (assoc 'tail bindings))
    (assert-true (assoc 'lim bindings))
    (assert-true (assoc 'lim-p bindings))
    (assert-true (assoc 'done bindings))))

;;; ── %push-required-bindings ──────────────────────────────────────────────

(deftest push-required-bindings-single
  "%push-required-bindings emits one (temp (car arg)) + (name temp) pair per required param."
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (bindings nil)
         (result-arg nil))
    (multiple-value-setq (result-arg bindings)
      (cl-cc/expand::%push-required-bindings '(x) 'args bindings gsl))
    (assert-= 2 (length bindings))
    (assert-true (assoc 'x bindings))))

(deftest push-required-bindings-advances-cursor
  "%push-required-bindings advances the current-arg cursor with (cdr arg) after each param."
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (bindings nil)
         (result-arg nil))
    (multiple-value-setq (result-arg bindings)
      (cl-cc/expand::%push-required-bindings '(x y) 'args bindings gsl))
    (assert-equal '(cdr (cdr args)) result-arg)))

;;; ── %push-optional-bindings ──────────────────────────────────────────────

(deftest push-optional-bindings-with-supplied-p
  "%push-optional-bindings emits temp + name + supplied-p binding triples."
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (bindings nil)
         (result-arg nil))
    (multiple-value-setq (result-arg bindings)
      (cl-cc/expand::%push-optional-bindings
       '((b 10 b-p)) 'remaining bindings gsl))
    (assert-true (assoc 'b bindings))
    (assert-true (assoc 'b-p bindings))))

(deftest push-optional-bindings-without-supplied-p
  "%push-optional-bindings omits supplied-p binding when slot is nil."
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (bindings nil)
         (result-arg nil))
    (multiple-value-setq (result-arg bindings)
      (cl-cc/expand::%push-optional-bindings
       '((b 10 nil)) 'remaining bindings gsl))
    (assert-true (assoc 'b bindings))
    (assert-= 2 (length bindings))))

;;; ── %push-key-bindings ───────────────────────────────────────────────────

(deftest push-key-bindings-basic
  "%push-key-bindings emits (getf ...) val binding and name binding."
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (bindings nil)
         (result (cl-cc/expand::%push-key-bindings
                  '(((:size n) 3 nil)) 'kwargs bindings gsl)))
    (assert-true (assoc 'n result))))

(deftest push-key-bindings-with-supplied-p
  "%push-key-bindings emits supplied-p binding when supplied-p name is given."
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (bindings nil)
         (result (cl-cc/expand::%push-key-bindings
                  '(((:size n) 3 n-p)) 'kwargs bindings gsl)))
    (assert-true (assoc 'n result))
    (assert-true (assoc 'n-p result))))

;;; ── %push-aux-bindings ───────────────────────────────────────────────────

(deftest push-aux-bindings-single
  "%push-aux-bindings emits one (name init) binding per aux spec."
  (let* ((bindings nil)
         (result (cl-cc/expand::%push-aux-bindings '((count 0)) bindings)))
    (assert-= 1 (length result))
    (assert-equal '(count 0) (first result))))

(deftest push-aux-bindings-multiple
  "%push-aux-bindings accumulates multiple aux specs in order."
  (let* ((bindings nil)
         (result (cl-cc/expand::%push-aux-bindings '((x 1) (y 2)) bindings)))
    (assert-= 2 (length result))
    (assert-true (assoc 'x result))
    (assert-true (assoc 'y result))))

;;; ── %push-destructured-required-bindings ─────────────────────────────────

(deftest push-destructured-required-simple-name
  "%push-destructured-required-bindings binds plain symbols directly."
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (result (cl-cc/expand::%push-destructured-required-bindings '(x) 'args nil gsl)))
    (assert-true (assoc 'x result))))

(deftest push-destructured-required-nested-list
  "%push-destructured-required-bindings recurses into sub-patterns."
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (result (cl-cc/expand::%push-destructured-required-bindings '((a b)) 'args nil gsl)))
    (assert-true (assoc 'a result))
    (assert-true (assoc 'b result))))

;;; ── %push-destructured-key-bindings ──────────────────────────────────────

(deftest push-destructured-key-bindings-emits-getf
  "%push-destructured-key-bindings emits a (getf ...) binding per key param."
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (result (cl-cc/expand::%push-destructured-key-bindings
                  '(((:count n) 0 nil)) 'kw-args nil gsl)))
    (assert-true (assoc 'n result))))

(deftest push-destructured-key-bindings-with-supplied-p
  "%push-destructured-key-bindings emits a supplied-p binding when given."
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (result (cl-cc/expand::%push-destructured-key-bindings
                  '(((:count n) 0 n-p)) 'kw-args nil gsl)))
    (assert-true (assoc 'n result))
    (assert-true (assoc 'n-p result))))

;;; ── *lambda-list-keyword-transitions* data table ─────────────────────────

(deftest-each lambda-list-keyword-transitions-completeness
  "*lambda-list-keyword-transitions* maps every standard lambda list keyword to a state."
  :cases (("&optional"    '&optional    :optional)
          ("&rest"        '&rest        :rest)
          ("&body"        '&body        :body)
          ("&key"         '&key         :key)
          ("&aux"         '&aux         :aux)
          ("&environment" '&environment :environment))
  (keyword expected-state)
  (assert-eq expected-state
             (cdr (assoc keyword cl-cc/expand::*lambda-list-keyword-transitions*))))

(deftest lambda-list-keyword-transitions-excludes-allow-other-keys
  "*lambda-list-keyword-transitions* does NOT contain &allow-other-keys (handled separately)."
  (assert-null (assoc '&allow-other-keys cl-cc/expand::*lambda-list-keyword-transitions*)))

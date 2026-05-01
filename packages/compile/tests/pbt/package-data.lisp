(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf cl-user::*cl-cc-pbt-test-imports*
        '(#:cl-cc-suite
          #:cl-cc-integration-suite
          #:cl-cc-integration-serial-suite
          #:defsuite
          #:in-suite
          #:deftest
          #:run-suite
          #:%fail-test
          #:test-failure
          #:*test-registry*
          #:*current-suite*
          #:*suite-registry*
          #:assert-true
          #:assert-false
          #:assert-=
          #:assert-eq
          #:assert-equal
          #:assert-null
          #:assert-type
          #:assert-signals
          #:run-string))

  (setf cl-user::*cl-cc-pbt-core-imports*
        '(:ast-node :ast-int :ast-var :ast-binop :ast-if :ast-progn :ast-print
          :ast-let :ast-lambda :ast-function :ast-flet :ast-labels :ast-block
          :ast-return-from :ast-tagbody :ast-go :ast-setq :ast-multiple-value-call
          :ast-multiple-value-prog1 :ast-catch :ast-throw :ast-unwind-protect
          :ast-call :ast-quote :ast-the
          :make-ast-int :make-ast-var :make-ast-binop :make-ast-if :make-ast-progn
          :make-ast-print :make-ast-let :make-ast-lambda :make-ast-function
          :make-ast-flet :make-ast-labels :make-ast-block :make-ast-return-from
          :make-ast-tagbody :make-ast-go :make-ast-setq :make-ast-multiple-value-call
          :make-ast-multiple-value-prog1 :make-ast-call :make-ast-quote :make-ast-the
          :make-ast-catch :make-ast-throw :make-ast-unwind-protect
          :ast-to-sexp :lower-sexp-to-ast
          :ast-int-value :ast-var-name :ast-binop-op :ast-binop-lhs :ast-binop-rhs
          :ast-if-cond :ast-if-then :ast-if-else :ast-progn-forms :ast-print-expr
          :ast-let-bindings :ast-let-body :ast-lambda-params :ast-lambda-body
          :ast-function-name :ast-flet-bindings :ast-flet-body :ast-labels-bindings
          :ast-labels-body :ast-block-name :ast-block-body :ast-return-from-name
          :ast-return-from-value :ast-tagbody-tags :ast-go-tag :ast-setq-var
          :ast-setq-value :ast-mv-call-func :ast-mv-call-args :ast-mv-prog1-first
          :ast-mv-prog1-forms :ast-catch-tag :ast-catch-body :ast-throw-tag
          :ast-throw-value :ast-unwind-protected :ast-unwind-cleanup :ast-call-func
          :ast-call-args :ast-quote-value :ast-the-type :ast-the-value
          :cps-transform
          :vm-state :vm-heap-counter :vm-cons-cell :vm-closure-object
          :vm-closure-entry-label :vm-closure-params :vm-closure-captured-values
          :vm-const :vm-move :vm-add :vm-sub :vm-mul :vm-cons :vm-car :vm-cdr
          :vm-rplaca :vm-rplacd :vm-make-closure :vm-closure-ref-idx
          :make-vm-add :make-vm-car :make-vm-cdr :make-vm-closure-ref-idx
          :make-vm-cons :make-vm-const :make-vm-make-closure :make-vm-move
          :make-vm-mul :make-vm-rplaca :make-vm-rplacd :make-vm-sub
          :vm-reg-get :vm-reg-set :vm-heap-get :execute-instruction
          :instruction->sexp :sexp->instruction :vm-dst :vm-src :vm-lhs :vm-rhs
          :vm-value :vm-car-reg :vm-cdr-reg :vm-cons-reg :vm-val-reg
          :vm-closure-reg :vm-closure-index :unify :unify-failed-p
          :substitute-variables :logic-var-p))

  (setf cl-user::*cl-cc-pbt-exports*
        '(#:defproperty #:defgenerator #:for-all #:check
          #:*test-count* #:*max-list-length* #:*max-string-length*
          #:*max-type-depth* #:*size* #:*pbt-rng-override*
          #:gen-integer #:gen-boolean #:gen-symbol #:gen-list #:gen-string
          #:gen-ast-node #:gen-expr #:gen-float #:gen-character #:gen-cons #:gen-vector
          #:gen-one-of #:gen-tuple #:gen-list-of #:gen-alist #:gen-map #:gen-bind
          #:gen-fmap #:gen-such-that #:gen-resize #:gen-scale
          #:shrink #:shrink-integer #:shrink-list
          #:run-property-tests #:report-failure
          #:generator #:generate #:gen-fn #:shrink-value #:make-generator
          #:gen-primitive-type #:gen-type-variable #:gen-simple-compound-type
          #:gen-values-type #:gen-fn-type-args #:gen-fn-type #:gen-array-type
          #:gen-cons-type #:gen-type-expr
          #:typed-ast #:make-typed-ast-raw #:typed-ast-node-type #:typed-ast-source-node
          #:typed-ast-int #:make-typed-ast-int-raw #:typed-ast-int-value
          #:typed-ast-float #:make-typed-ast-float-raw #:typed-ast-float-value
          #:typed-ast-string #:make-typed-ast-string-raw #:typed-ast-string-value
          #:typed-ast-boolean #:make-typed-ast-boolean-raw #:typed-ast-boolean-value
          #:typed-ast-var #:make-typed-ast-var-raw #:typed-ast-var-name
          #:typed-ast-binop #:make-typed-ast-binop-raw #:typed-ast-binop-op
          #:typed-ast-binop-lhs #:typed-ast-binop-rhs
          #:typed-ast-if #:make-typed-ast-if-raw #:typed-ast-if-cond
          #:typed-ast-if-then #:typed-ast-if-else
          #:typed-ast-lambda #:make-typed-ast-lambda-raw #:typed-ast-lambda-params
          #:typed-ast-lambda-body
          #:typed-ast-call #:make-typed-ast-call-raw #:typed-ast-call-func
          #:typed-ast-call-func-type #:typed-ast-call-args
          #:typed-ast-let #:make-typed-ast-let-raw #:typed-ast-let-bindings #:typed-ast-let-body
          #:gen-typed-primitive-value #:gen-typed-terminal #:gen-typed-binop #:gen-typed-if
          #:gen-typed-param #:gen-typed-lambda #:gen-typed-call #:gen-typed-let
          #:gen-typed-ast-node #:typed-ast-to-sexp #:extract-type-from-ast)))

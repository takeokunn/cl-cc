;;; Test script to verify PBT generators integration
(require :asdf)
(asdf:load-asd (truename (merge-pathnames "cl-cc.asd" *default-pathname-defaults*)))
(asdf:load-system :cl-cc/test)

(format t "~%=== Package Check ===~%")
(format t "cl-cc/pbt package exists: ~a~%" (if (find-package :cl-cc/pbt) "YES" "NO"))

(when (find-package :cl-cc/pbt)
  (format t "~%=== Generator Symbol Exports ===~%")
  (let ((symbols '(gen-primitive-type
                   gen-type-variable
                   gen-type-expr
                   gen-function-type
                   gen-mach-header
                   gen-segment-command
                   gen-section
                   gen-typed-ast-int
                   gen-typed-ast-binop
                   gen-typed-ast-expr
                   gen-typed-ast-node)))
    (dolist (sym symbols)
      (let ((found (find-symbol (symbol-name sym) :cl-cc/pbt)))
        (format t "  ~a: ~a~%" sym (if found "FOUND" "NOT FOUND")))))

  (format t "~%=== Test Generator Functionality ===~%")
  (let ((*random-state* (make-random-state t)))
    ;; Test gen-primitive-type
    (handler-case
        (let ((result (cl-cc/pbt:generate (cl-cc/pbt:gen-primitive-type))))
          (format t "  gen-primitive-type sample: ~a~%" result))
      (error (e) (format t "  gen-primitive-type ERROR: ~a~%" e)))

    ;; Test gen-type-expr
    (handler-case
        (let ((result (cl-cc/pbt:generate (cl-cc/pbt:gen-type-expr :depth 1))))
          (format t "  gen-type-expr sample: ~a~%" result))
      (error (e) (format t "  gen-type-expr ERROR: ~a~%" e)))

    ;; Test gen-mach-header
    (handler-case
        (let ((result (cl-cc/pbt:generate (cl-cc/pbt:gen-mach-header))))
          (format t "  gen-mach-header sample: ~a~%" result))
      (error (e) (format t "  gen-mach-header ERROR: ~a~%" e)))

    ;; Test gen-typed-ast-node
    (handler-case
        (let ((result (cl-cc/pbt:generate (cl-cc/pbt:gen-typed-ast-node :depth 1))))
          (format t "  gen-typed-ast-node sample: ~a~%" (type-of result)))
      (error (e) (format t "  gen-typed-ast-node ERROR: ~a~%" e))))

  (format t "~%=== Run PBT Suite ===~%")
  (handler-case
      (progn
        (fiveam:run! 'cl-cc/pbt::cl-cc-pbt-suite)
        (format t "~%PBT Suite completed successfully!~%"))
    (error (e) (format t "~%PBT Suite ERROR: ~a~%" e))))

(format t "~%=== Done ===~%")
(sb-ext:exit :code 0)

;;;; packages/javascript/src/runtime-method-resolver.lisp — JS prototype method dispatch
;;;;
;;;; Loaded last so every %js-array-*/%js-string-*/etc. helper is already defined.
;;;; %js-get-prop delegates to *js-method-resolver* (declared in runtime.lisp) via the
;;;; hook installed at the bottom of this file.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Prototype method tables — alist of (method-name . host-helper)
;;; -----------------------------------------------------------------------
;;;
;;; Each table's entries are (string . function) pairs.  %js-bound-method
;;; looks up the name and returns a closure that prepends the receiver as the
;;; first argument, so  arr.push(v)  becomes  (%js-array-push arr v).

(defparameter *js-array-method-table*
  (list (cons "push" #'%js-array-push)         (cons "pop" #'%js-array-pop)
        (cons "shift" #'%js-array-shift)        (cons "unshift" #'%js-array-unshift)
        (cons "map" #'%js-array-map)            (cons "forEach" #'%js-array-for-each)
        (cons "filter" #'%js-array-filter)      (cons "reduce" #'%js-array-reduce)
        (cons "reduceRight" #'%js-array-reduce-right)
        (cons "find" #'%js-array-find)          (cons "findIndex" #'%js-array-find-index)
        (cons "some" #'%js-array-some)          (cons "every" #'%js-array-every)
        (cons "includes" #'%js-array-includes)  (cons "indexOf" #'%js-array-index-of)
        (cons "lastIndexOf" #'%js-array-last-index-of)
        (cons "join" #'%js-array-join)          (cons "slice" #'%js-array-slice)
        (cons "splice" #'%js-array-splice)      (cons "concat" #'%js-array-concat)
        (cons "reverse" #'%js-array-reverse)    (cons "sort" #'%js-array-sort)
        (cons "flat" #'%js-array-flat)          (cons "flatMap" #'%js-array-flat-map)
        (cons "fill" #'%js-array-fill)          (cons "copyWithin" #'%js-array-copy-within)
        (cons "entries" #'%js-array-entries)    (cons "keys" #'%js-array-keys)
        (cons "values"
              (lambda (arr)
                (let ((i (list 0)))
                  (%js-make-generator (lambda ()
                    (loop while (< (car i) (length arr))
                          do (%js-yield (aref arr (car i)))
                             (incf (car i))))))))
        (cons "@@iterator"
              (lambda (arr)
                (let ((i (list 0)))
                  (%js-make-object
                   "next" (lambda ()
                            (if (< (car i) (length arr))
                                (prog1 (%js-make-object "value" (aref arr (car i)) "done" nil)
                                  (incf (car i)))
                                (%js-make-object "value" +js-undefined+ "done" t)))
                   "@@iterator" (lambda () (gethash "@@iterator" arr))))))
        (cons "group"          #'%js-array-group)
        (cons "groupToMap"     #'%js-array-group-to-map)
        (cons "toReversed"     #'%js-array-to-reversed)
        (cons "toSorted"       #'%js-array-to-sorted)
        (cons "toSpliced"      #'%js-array-to-spliced)
        (cons "with"           #'%js-array-with)
        (cons "findLast"       #'%js-array-find-last)
        (cons "findLastIndex"  #'%js-array-find-last-index)
        (cons "at"             #'%js-array-at))
  "Alist: JS Array.prototype method name -> host helper (receiver = first arg).")

(defparameter *js-string-method-table*
  (list (cons "slice" #'%js-string-slice)         (cons "indexOf" #'%js-string-index-of)
        (cons "lastIndexOf" #'%js-string-last-index-of)
        (cons "includes" #'%js-string-includes)   (cons "startsWith" #'%js-string-starts-with)
        (cons "endsWith" #'%js-string-ends-with)  (cons "split" #'%js-string-split)
        (cons "replace" #'%js-string-replace)     (cons "replaceAll" #'%js-string-replace-all)
        (cons "padStart" #'%js-string-pad-start)  (cons "padEnd" #'%js-string-pad-end)
        (cons "at" #'%js-string-at)               (cons "repeat" #'%js-string-repeat)
        (cons "charAt" #'%js-string-char-at)      (cons "charCodeAt" #'%js-string-char-code-at)
        (cons "concat" #'%js-string-concat)       (cons "match" #'%js-string-match)
        (cons "matchAll" #'%js-string-match-all)  (cons "search" #'%js-string-search)
        (cons "toUpperCase" #'%js-string-to-upper-case)
        (cons "toLowerCase" #'%js-string-to-lower-case)
        (cons "trim" #'%js-string-trim)           (cons "trimStart" #'%js-string-trim-start)
        (cons "trimEnd" #'%js-string-trim-end)
        (cons "codePointAt" #'%js-string-code-point-at)
        (cons "normalize" #'%js-string-normalize)
        (cons "substr" #'%js-string-slice)         ; deprecated alias
        (cons "substring" #'%js-string-substring)
        (cons "valueOf" (lambda (s) s))
        (cons "toString" (lambda (s) s))
        (cons "toWellFormed"       #'%js-string-to-well-formed)
        (cons "isWellFormed"       #'%js-string-is-well-formed)
        (cons "toLocaleLowerCase"  #'%js-string-to-locale-lower-case)
        (cons "toLocaleUpperCase"  #'%js-string-to-locale-upper-case)
        (cons "localeCompare"      #'%js-string-locale-compare)
        (cons "@@iterator"
              (lambda (s)
                (let ((i (list 0)))
                  (%js-make-object
                   "next" (lambda ()
                            (if (< (car i) (length s))
                                (prog1 (%js-make-object "value" (string (char s (car i))) "done" nil)
                                  (incf (car i)))
                                (%js-make-object "value" +js-undefined+ "done" t))))))))
  "Alist: JS String.prototype method name -> host helper (receiver = first arg).")

(defparameter *js-set-method-table*
  (list (cons "add"                #'%js-set-add)
        (cons "delete"             #'%js-set-delete)
        (cons "has"                #'%js-set-has)
        (cons "clear"              #'%js-set-clear)
        (cons "forEach"            #'%js-set-for-each)
        (cons "keys"               (lambda (s) (%js-set-keys s)))
        (cons "values"             (lambda (s) (%js-set-keys s)))   ; Set keys = values
        (cons "entries"            #'%js-set-entries)
        (cons "union"              #'%js-set-union)
        (cons "intersection"       #'%js-set-intersection)
        (cons "difference"         #'%js-set-difference)
        (cons "symmetricDifference" #'%js-set-symmetric-difference)
        (cons "isSubsetOf"         #'%js-set-is-subset-of)
        (cons "isSupersetOf"       #'%js-set-is-superset-of)
        (cons "isDisjointFrom"     #'%js-set-is-disjoint-from))
  "Alist: JS Set.prototype method name -> host helper.")

(defparameter *js-map-method-table*
  (list (cons "set"     #'%js-map-set)
        (cons "get"     #'%js-map-get)
        (cons "has"     #'%js-map-has)
        (cons "delete"  #'%js-map-delete)
        (cons "clear"   #'%js-map-clear)
        (cons "forEach" #'%js-map-for-each)
        (cons "keys"    (lambda (m) (%js-map-keys m)))
        (cons "values"  (lambda (m) (%js-map-values m)))
        (cons "entries" (lambda (m) (%js-map-entries m))))
  "Alist: JS Map.prototype method name -> host helper.")

(defparameter *js-weak-map-method-table*
  (list (cons "set"    #'%js-weak-map-set)
        (cons "get"    #'%js-weak-map-get)
        (cons "has"    #'%js-weak-map-has)
        (cons "delete" #'%js-weak-map-delete))
  "Alist: JS WeakMap.prototype method name -> host helper.")

(defparameter *js-weak-set-method-table*
  (list (cons "add"    #'%js-weak-set-add)
        (cons "has"    #'%js-weak-set-has)
        (cons "delete" #'%js-weak-set-delete))
  "Alist: JS WeakSet.prototype method name -> host helper.")

;;; -----------------------------------------------------------------------
;;;  Number.prototype helpers
;;; -----------------------------------------------------------------------

(defun %js-strip-trailing-dot (s)
  "Drop the trailing '.' that CL's ~,0F format directive adds (\"8.\" -> \"8\")."
  (if (and (plusp (length s)) (char= (char s (1- (length s))) #\.))
      (subseq s 0 (1- (length s)))
      s))

(defun %js-strip-pre-exp-dot (s)
  "Remove a lone '.' immediately before the exponent marker: '1.e+2' -> '1e+2'.
CL's ~,0E always emits the decimal point even when d=0; JS omits it."
  (let ((pos (or (search ".e" s) (search ".E" s))))
    (if pos
        (concatenate 'string (subseq s 0 pos) (subseq s (1+ pos)))
        s)))

(defun %js-number-to-precision (x p)
  "JS Number.prototype.toPrecision(P): render X to P significant digits.
Uses fixed notation for exponents in [-6, P), exponential otherwise."
  (cond
    ((or (< p 1) (> p 100)) (format nil "~A" x))
    ((zerop x)
     (if (= p 1) "0" (format nil "0.~A" (make-string (1- p) :initial-element #\0))))
    (t
     (let* ((neg (minusp x))
            (ax  (abs (coerce x 'double-float)))
            (e   (floor (log ax 10d0))))
       (loop while (>= (/ ax (expt 10d0 e)) 10d0) do (incf e))
       (loop while (<  (/ ax (expt 10d0 e)) 1d0)  do (decf e))
       (let ((body (if (or (< e -6) (>= e p))
                       (%js-strip-pre-exp-dot (format nil "~,v,,,,,'eE" (1- p) ax))
                       (%js-strip-trailing-dot (format nil "~,vF" (max 0 (- p 1 e)) ax)))))
         (if neg (concatenate 'string "-" body) body))))))

(defparameter *js-number-method-table*
  (list (cons "toFixed"
              (lambda (n digits)
                (let ((d (if (eq digits +js-undefined+) 0 (truncate (%js-to-number digits)))))
                  (%js-strip-trailing-dot (format nil "~,vF" d (%js-to-number n))))))
        (cons "toString"
              (lambda (n &optional (radix 10))
                (let ((r  (if (eq radix +js-undefined+) 10 (truncate (%js-to-number radix))))
                      (ni (truncate (%js-to-number n))))
                  (if (= r 10)
                      (format nil "~A" (%js-to-number n))
                      (string-downcase (format nil "~vR" r ni))))))
        (cons "toPrecision"
              (lambda (n prec)
                (if (eq prec +js-undefined+)
                    (format nil "~A" (%js-to-number n))
                    (%js-number-to-precision (%js-to-number n) (truncate (%js-to-number prec))))))
        (cons "toExponential"
              (lambda (n &optional digits)
                (let* ((v (%js-to-number n))
                       (d (if (or (null digits) (eq digits +js-undefined+))
                              6 (truncate (%js-to-number digits)))))
                  (format nil "~,v,,,,,'eE" d (coerce v 'double-float)))))
        (cons "valueOf"       (lambda (n) (%js-to-number n)))
        (cons "toLocaleString" (lambda (n &rest _) (declare (ignore _))
                                 (format nil "~A" (%js-to-number n)))))
  "Alist: Number.prototype method name -> (lambda (num &rest args)) helpers.")

(defparameter *js-symbol-method-table*
  (list (cons "toString" #'%js-symbol-to-string)
        (cons "valueOf"  (lambda (s) s)))
  "Alist: Symbol.prototype method name -> helpers.
Note: `description' is a property accessor, not a method — handled in %js-resolve-method.")

;;; -----------------------------------------------------------------------
;;;  %js-bound-method — generic method lookup + receiver currying
;;; -----------------------------------------------------------------------

(defun %js-bound-method (table receiver name)
  "Look NAME up in TABLE; return a closure (helper RECEIVER . args) or +js-undefined+."
  (let ((entry (assoc name table :test #'string=)))
    (if entry
        (let ((fn (cdr entry)))
          (lambda (&rest args) (apply fn receiver args)))
        +js-undefined+)))

;;; -----------------------------------------------------------------------
;;;  Per-type resolver functions
;;; -----------------------------------------------------------------------
;;;
;;; Each resolver handles one JS type. %js-resolve-method delegates to these.

(defmacro define-js-type-resolver (name method-table &rest special-props)
  "Generate a resolver defun for a JS type.
SPECIAL-PROPS are alternating (key-string value-form) pairs handled before
the table lookup. OBJ and KEY are bound in value-forms."
  `(defun ,name (obj key)
     ,(if special-props
          `(cond
             ,@(loop for (k v) on special-props by #'cddr
                     collect `((string= key ,k) ,v))
             (t (%js-bound-method ,method-table obj key)))
          `(%js-bound-method ,method-table obj key))))

;;; Simple resolvers: special property + table lookup
(define-js-type-resolver %js-resolve-array-method *js-array-method-table*
  "length" (coerce (length obj) 'double-float))

(define-js-type-resolver %js-resolve-string-method *js-string-method-table*
  "length" (coerce (length obj) 'double-float))

(define-js-type-resolver %js-resolve-map-method *js-map-method-table*
  "size" (coerce (%js-map-size obj) 'double-float))

(define-js-type-resolver %js-resolve-set-method *js-set-method-table*
  "size" (coerce (%js-set-size obj) 'double-float))

(define-js-type-resolver %js-resolve-weak-map-method *js-weak-map-method-table*)
(define-js-type-resolver %js-resolve-weak-set-method *js-weak-set-method-table*)
(define-js-type-resolver %js-resolve-date-method     *js-date-method-table*)

;;; Complex resolvers: types with multiple special properties or unique logic

(defun %js-resolve-promise-method (obj key)
  (cond
    ((string= key "then")
     (lambda (on-fulfilled &optional on-rejected)
       (%js-promise-then obj on-fulfilled on-rejected)))
    ((string= key "catch")
     (lambda (on-rejected)
       (%js-promise-then obj +js-undefined+ on-rejected)))
    ((string= key "finally")
     (lambda (fn)
       (%js-promise-then obj
        (lambda (v) (%js-funcall fn) (%js-promise-resolve v))
        (lambda (r) (%js-funcall fn) (%js-promise-reject r)))))
    (t +js-undefined+)))

(defun %js-resolve-typed-array-method (obj key)
  (cond
    ((string= key "length")
     (coerce (js-ta-length obj) 'double-float))
    ((string= key "byteLength")
     (coerce (* (js-ta-length obj) (js-ta-element-size obj)) 'double-float))
    ((string= key "byteOffset")
     (coerce (js-ta-byte-offset obj) 'double-float))
    ((string= key "buffer")
     (%js-make-object "byteLength" (* (js-ta-length obj) (js-ta-element-size obj))))
    (t (%js-bound-method *js-typed-array-method-table* obj key))))

(defun %js-resolve-regexp-method (obj key)
  (cond
    ((string= key "source")     (js-regexp-source obj))
    ((string= key "flags")      (js-regexp-flags obj))
    ((string= key "global")     (js-regexp-global-p obj))
    ((string= key "ignoreCase") (js-regexp-ignore-case-p obj))
    ((string= key "multiline")  (js-regexp-multiline-p obj))
    ((string= key "lastIndex")  (coerce (js-regexp-last-index obj) 'double-float))
    ((string= key "test")       (let ((re obj)) (lambda (str) (%js-regex-test re str))))
    ((string= key "exec")       (let ((re obj)) (lambda (str) (%js-regex-exec re str 0))))
    (t +js-undefined+)))

(defun %js-resolve-object-method (obj key)
  (multiple-value-bind (stored found) (gethash key obj)
    (if found
        stored
        (cond
          ((string= key "hasOwnProperty")
           (lambda (k) (nth-value 1 (gethash (%js-to-string k) obj))))
          ((string= key "toString")          (lambda () "[object Object]"))
          ((string= key "valueOf")           (lambda () obj))
          ((string= key "isPrototypeOf")
           (lambda (_proto) (declare (ignore _proto)) nil))
          ((string= key "propertyIsEnumerable")
           (lambda (k) (nth-value 1 (gethash (%js-to-string k) obj))))
          ((string= key "constructor")
           (lambda (&rest _) (declare (ignore _)) obj))
          (t +js-undefined+)))))

(defun %js-resolve-number-method (obj key)
  (let ((entry (assoc key *js-number-method-table* :test #'string=)))
    (when entry
      (let ((fn (cdr entry)))
        (lambda (&rest args) (apply fn obj args))))))

(defun %js-resolve-symbol-method (obj key)
  (let ((entry (assoc key *js-symbol-method-table* :test #'string=)))
    (cond
      (entry (let ((fn (cdr entry)))
               (lambda (&rest args) (apply fn obj args))))
      ((string= key "description") (%js-symbol-description obj))
      (t +js-undefined+))))

(defun %js-resolve-weak-ref-method (obj key)
  (if (string= key "deref")
      (lambda () (%js-weak-ref-deref obj))
      +js-undefined+))

(defun %js-resolve-function-method (obj key)
  (cond
    ((string= key "bind")
     (lambda (this-arg &rest partial-args)
       (lambda (&rest args)
         (apply #'%js-funcall obj (list* this-arg (append partial-args args))))))
    ((string= key "call")
     (lambda (this-arg &rest args)
       (apply #'%js-funcall obj (list* this-arg args))))
    ((string= key "apply")
     (lambda (this-arg args-array)
       (apply #'%js-funcall obj
              (list* this-arg
                     (if (%js-vec-p args-array) (coerce args-array 'list) nil)))))
    ((string= key "name")     "")
    ((string= key "length")   0.0d0)
    ((string= key "toString") (lambda () "function() { [native code] }"))
    (t +js-undefined+)))

(defun %js-resolve-bigint-method (obj key)
  (cond
    ((string= key "toString")
     (lambda (&optional radix)
       (%js-bigint-to-string obj
        (if (eq radix +js-undefined+) 10 (truncate (%js-to-number radix))))))
    ((string= key "valueOf")
     (lambda () obj))
    ((string= key "toLocaleString")
     (lambda (&rest _) (declare (ignore _)) (%js-bigint-to-string obj)))
    (t +js-undefined+)))

;;; -----------------------------------------------------------------------
;;;  %js-resolve-method — the *js-method-resolver* implementation
;;; -----------------------------------------------------------------------

(defun %js-resolve-method (obj key)
  "Resolve OBJ.KEY to a bound method closure, or +js-undefined+.
Installed as *js-method-resolver* so %js-get-prop can offer prototype methods."
  (cond
    ((js-promise-p obj)         (%js-resolve-promise-method obj key))
    ((%js-vec-p obj)            (%js-resolve-array-method obj key))
    ((stringp obj)              (%js-resolve-string-method obj key))
    ((js-map-p obj)             (%js-resolve-map-method obj key))
    ((js-date-p obj)            (%js-resolve-date-method obj key))
    ((js-typed-array-p obj)     (%js-resolve-typed-array-method obj key))
    ((js-regexp-p obj)          (%js-resolve-regexp-method obj key))
    ((js-set-p obj)             (%js-resolve-set-method obj key))
    ((js-weak-map-p obj)        (%js-resolve-weak-map-method obj key))
    ((js-weak-set-p obj)        (%js-resolve-weak-set-method obj key))
    ((hash-table-p obj)         (%js-resolve-object-method obj key))
    ((numberp obj)              (or (%js-resolve-number-method obj key) +js-undefined+))
    ((js-symbol-p obj)          (%js-resolve-symbol-method obj key))
    ((typep obj 'js-weak-ref)   (%js-resolve-weak-ref-method obj key))
    ((functionp obj)            (%js-resolve-function-method obj key))
    ((js-bigint-p obj)          (%js-resolve-bigint-method obj key))
    (t +js-undefined+)))

(setf *js-method-resolver* #'%js-resolve-method)

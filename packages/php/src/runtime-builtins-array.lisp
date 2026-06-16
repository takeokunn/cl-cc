;;;; Array PHP builtin helpers.

(in-package :cl-cc/php)

(defun %php-copy-array (array)
  "Return a shallow copy of PHP ARRAY preserving insertion order."
  (let ((copy (%php-array)))
    (dolist (pair (%php-array-pairs array) copy)
      (%php-array-set copy (car pair) (cdr pair)))))

(defun %php-callable-function (callback)
  "Resolve CALLBACK to a Common Lisp function, or NIL.

A compiled-PHP closure (Closure object / arrow fn) is a vm-closure-object, which
host CL:FUNCALL cannot call. Wrap it in a trampoline that routes back into the VM
via %vm-call-closure-sync, using *vm-state* (dynamically bound around VM
execution for exactly this host-runtime -> VM-closure inverse bridge). Mirrors the
JS *js-apply-fn* installer. Without this, array_map/array_filter/array_reduce/
usort with a closure callback silently no-op (the resolver returned NIL)."
  (cond ((functionp callback) callback)
        ((cl-cc/vm::%vm-closure-object-p callback)
         (lambda (&rest args)
           (cl-cc/vm::%vm-call-closure-sync callback cl-cc/vm:*vm-state* args)))
        ((and (symbolp callback) (fboundp callback)) (symbol-function callback))
        ((stringp callback)
         ;; A string callable names a global function.  Try builtins first, then
         ;; the VM function registry for USER functions (function foo(){...}).
         (or (and (fboundp '%php-lookup-builtin)
                  (funcall (symbol-function '%php-lookup-builtin) callback))
             (%php-callable-user-function callback)))
        (t nil)))

(defun %php-callable-user-function (name)
  "Resolve PHP user-function NAME (a string) to a callable via the VM function
registry, or NIL.  PHP function names are case-insensitive; the compiler interns
each into whatever *package* was current at parse time, so matching by EQ symbol
is fragile across the host/VM boundary.  Match by SYMBOL-NAME (upcased) instead,
which is package-independent.  A registry hit that is a vm-closure-object is
wrapped in the same %vm-call-closure-sync trampoline used for Closure args."
  (let ((state cl-cc/vm:*vm-state*))
    (when (and state (stringp name))
      (let ((target (string-upcase name))
            (registry (cl-cc/vm:vm-function-registry state)))
        (block found
          (maphash (lambda (sym fn)
                     (when (and (symbolp sym)
                                (string= (symbol-name sym) target))
                       (return-from found
                         (if (cl-cc/vm::%vm-closure-object-p fn)
                             (lambda (&rest args)
                               (cl-cc/vm::%vm-call-closure-sync fn state args))
                             fn))))
                   registry)
          nil)))))

(defun %php-call-user-func (callback &rest args)
  "PHP call_user_func: invoke CALLBACK (Closure, function-name string, or a host
function) with ARGS and return the result."
  (let ((fn (%php-callable-function callback)))
    (if fn
        (apply fn args)
        (%php-throw 'type-error
                    "call_user_func(): Argument #1 ($callback) must be a valid callback"))))

(defun %php-call-user-func-array (callback args-array)
  "PHP call_user_func_array: invoke CALLBACK with the values of PHP ARGS-ARRAY
spread as positional arguments."
  (let ((fn (%php-callable-function callback)))
    (if fn
        (apply fn (if (hash-table-p args-array)
                      (%php-array-values-list args-array)
                      nil))
        (%php-throw 'type-error
                    "call_user_func_array(): Argument #1 ($callback) must be a valid callback"))))

(defun %php-array-merge (&rest arrays)
  "Merge PHP ARRAYS into a new array."
  ;; String keys are overwritten; integer keys are appended and reindexed.
  ;; (%php-count (%php-array-merge (%php-list-to-array '(1)) (%php-list-to-array '(2)))) => 2
  (let ((result (%php-array)))
    (dolist (array arrays result)
      (when (hash-table-p array)
        (dolist (pair (%php-array-pairs array))
          (if (integerp (car pair))
              (%php-array-set result (%php-array-next-auto-index result) (cdr pair))
              (%php-array-set result (car pair) (cdr pair))))))))

(defun %php-array-keys (array &optional (filter-value nil filter-supplied-p) strict)
  "Return ARRAY keys as a PHP array, optionally filtered by value."
  ;; (%php-array-values-list (%php-array-keys (%php-array (list t "a" 1)))) => ("a")
  (let ((result (%php-array)))
    (when (hash-table-p array)
      (dolist (pair (%php-array-pairs array) result)
        (when (or (not filter-supplied-p)
                  (if (%php-truthy strict)
                      (%php-eq-strict filter-value (cdr pair))
                      (%php-eq-loose filter-value (cdr pair))))
          (%php-array-set result (%php-array-next-auto-index result) (car pair)))))))

(defun %php-array-values (array)
  "Return ARRAY values as a reindexed PHP array."
  (%php-list-to-array (%php-array-values-list array)))

(defun %php-array-push (array &rest values)
  "Append VALUES to ARRAY and return the new count."
  ;; (let ((a (%php-array))) (%php-array-push a "x")) => 1
  (check-type array hash-table)
  (dolist (value values (%php-count array))
    (%php-array-set array (%php-array-next-auto-index array) value)))

(defun %php-array-append-target (&rest _)
  "Stub for the $a[] append marker reached in read context, which is a PHP fatal
error (\"Cannot use [] for reading\"). Valid code consumes the marker at assignment
time, so this only fires on misuse."
  (declare (ignore _))
  (error "PHP fatal error: Cannot use [] for reading"))

(defun %php-array-pop (array)
  "Remove and return ARRAY's last value, or PHP null for an empty array."
  (check-type array hash-table)
  (let ((keys (%php-array-ordered-keys array)))
    (if keys
        (let* ((key (car (last keys)))
               (value (gethash key array)))
          (%php-array-unset array key)
          value)
        +php-null+)))

(defun %php-array-shift (array)
  "Remove and return ARRAY's first value, or PHP null for an empty array."
  (check-type array hash-table)
  (let ((keys (%php-array-ordered-keys array)))
    (if keys
        (let* ((key (first keys))
               (value (gethash key array)))
          (%php-array-unset array key)
          value)
        +php-null+)))

(defun %php-array-unshift (array &rest values)
  "Prepend VALUES to ARRAY and reindex numeric keys."
  ;; (let ((a (%php-list-to-array '(2)))) (%php-array-unshift a 1)) => 2
  (check-type array hash-table)
  (let ((all-values (append values (%php-array-values-list array))))
    (clrhash array)
    (setf (gethash +php-array-order-key+ array) nil
          (gethash +php-array-next-index-key+ array) 0)
    (dolist (value all-values (%php-count array))
      (%php-array-set array (%php-array-next-auto-index array) value))))

(defun %php-in-array (needle haystack &optional strict)
  "Return true when NEEDLE appears in HAYSTACK."
  ;; (%php-in-array 2 (%php-list-to-array '(1 2 3))) => T
  (and (hash-table-p haystack)
       (some (lambda (value)
               (if (%php-truthy strict)
                   (%php-eq-strict needle value)
                   (%php-eq-loose needle value)))
             (%php-array-values-list haystack))))

(defun %php-array-search (needle haystack &optional strict)
  "Return NEEDLE's key in HAYSTACK, or NIL when absent."
  (when (hash-table-p haystack)
    (loop for pair in (%php-array-pairs haystack)
          when (if (%php-truthy strict)
                   (%php-eq-strict needle (cdr pair))
                   (%php-eq-loose needle (cdr pair)))
            return (car pair))))

(defun %php-array-copy-with-key-policy (result key value preserve-keys)
  "Copy VALUE into RESULT using PHP's numeric-key reindexing policy."
  (%php-array-set result
                  (if (or (%php-truthy preserve-keys) (not (integerp key)))
                      key
                      (%php-array-next-auto-index result))
                  value))

(defun %php-array-reverse (array &optional preserve-keys)
  "Return ARRAY values in reverse order."
  ;; (%php-array-values-list (%php-array-reverse (%php-list-to-array '(1 2)))) => (2 1)
  (let ((result (%php-array)))
    (dolist (pair (reverse (%php-array-pairs array)) result)
      (%php-array-copy-with-key-policy result (car pair) (cdr pair) preserve-keys))))

(defun %php-array-slice (array offset &optional length preserve-keys)
  "Return a slice of ARRAY as a PHP array."
  (let* ((pairs (%php-array-pairs array))
         (size (length pairs))
         (start (if (minusp offset) (max 0 (+ size offset)) (min offset size)))
         (end (cond ((or (null length) (%php-null-p length)) size)
                    ((minusp length) (max start (+ size length)))
                    (t (min size (+ start length)))))
         (slice (subseq pairs start end))
         (result (%php-array)))
    (dolist (pair slice result)
      (%php-array-copy-with-key-policy result (car pair) (cdr pair) preserve-keys))))

(defun %php-array-unique (array)
  "Return ARRAY with duplicate values removed, preserving first keys."
  (let ((result (%php-array))
        (seen nil))
    (dolist (pair (%php-array-pairs array) result)
      (unless (some (lambda (value) (%php-array-value-string= value (cdr pair))) seen)
        (push (cdr pair) seen)
        (%php-array-set result (car pair) (cdr pair))))))

(defun %php-array-map (callback array &rest more-arrays)
  "Apply CALLBACK across one or more PHP arrays.
PHP preserves keys only when exactly one array is supplied; multiple arrays are
zipped by position and reindexed. A null callback returns the original values
for one array and row arrays for multiple arrays."
  (let* ((arrays (cons array more-arrays))
         (fn (and (not (%php-null-p callback))
                  (%php-callable-function callback)))
         (result (%php-array)))
    (cond
      ((some (lambda (arr) (not (hash-table-p arr))) arrays)
       result)
      ((null more-arrays)
       (dolist (pair (%php-array-pairs array) result)
         (%php-array-set result (car pair)
                         (if fn (funcall fn (cdr pair)) (cdr pair)))))
      (t
       (let* ((lists (mapcar #'%php-array-values-list arrays))
              (limit (if lists (apply #'max (mapcar #'length lists)) 0)))
         (loop for i below limit
               for args = (mapcar (lambda (lst)
                                     (if (< i (length lst)) (nth i lst) +php-null+))
                                   lists)
               do (if fn
                      (%php-array-set result (%php-array-next-auto-index result)
                                      (apply fn args))
                      (let ((row (%php-array)))
                        (dolist (arg args)
                          (%php-array-set row (%php-array-next-auto-index row) arg))
                        (%php-array-set result (%php-array-next-auto-index result) row))))
         result)))))

(defun %php-array-filter (array &optional callback (mode 0))
  "Filter ARRAY values by CALLBACK or PHP truthiness.

MODE follows PHP's array_filter flags:
0 = pass value, ARRAY_FILTER_USE_BOTH(1) = pass value and key,
ARRAY_FILTER_USE_KEY(2) = pass key."
  (let ((fn (and callback (not (%php-null-p callback)) (%php-callable-function callback)))
        (result (%php-array)))
    (when (hash-table-p array)
      (dolist (pair (%php-array-pairs array))
        (let ((keep (if fn
                        (case mode
                          (1 (funcall fn (cdr pair) (car pair)))
                          (2 (funcall fn (car pair)))
                          (otherwise (funcall fn (cdr pair))))
                        (%php-truthy (cdr pair)))))
          (when (%php-truthy keep)
            (%php-array-set result (car pair) (cdr pair))))))
    result))

(defun %php-array-reduce (array callback &optional initial)
  "Reduce ARRAY values with CALLBACK and optional INITIAL accumulator."
  (let ((fn (%php-callable-function callback))
        (acc (if (or (null initial) (%php-null-p initial)) +php-null+ initial)))
    (when (and fn (hash-table-p array))
      (dolist (value (%php-array-values-list array))
        (setf acc (funcall fn acc value))))
    acc))

(defun %php-sort-flag-type (sort-flags)
  "Return the comparison mode selected by PHP SORT_* flags."
  (case sort-flags
    (1 :numeric)                         ; SORT_NUMERIC
    (2 :string)                          ; SORT_STRING
    (otherwise :regular)))               ; SORT_REGULAR/default

(defun %php-sort-compare-values (a b sort-type)
  "Return -1, 0, or 1 comparing A and B for PHP array sorting."
  (case sort-type
    (:numeric
     (let ((na (%php-numeric a))
           (nb (%php-numeric b)))
       (cond ((< na nb) -1) ((> na nb) 1) (t 0))))
    (:string
     (let ((sa (%php-stringify a))
           (sb (%php-stringify b)))
       (cond ((string< sa sb) -1) ((string> sa sb) 1) (t 0))))
    (otherwise
     (cond
       ((and (numberp a) (numberp b))
        (cond ((< a b) -1) ((> a b) 1) (t 0)))
       (t (%php-sort-compare-values a b :string))))))

(defun %php-sort-pairs-by (array key-fn &key descending preserve-keys sort-flags)
  "Sort ARRAY by KEY-FN and return T after mutating ARRAY."
  (let* ((sort-type (%php-sort-flag-type sort-flags))
         (pairs (stable-sort
                 (copy-list (%php-array-pairs array))
                 (lambda (left right)
                   (let ((comparison (%php-sort-compare-values
                                      (funcall key-fn left)
                                      (funcall key-fn right)
                                      sort-type)))
                     (if descending
                         (> comparison 0)
                         (< comparison 0)))))))
    (clrhash array)
    (setf (gethash +php-array-order-key+ array) nil
          (gethash +php-array-next-index-key+ array) 0)
    (dolist (pair pairs t)
      (%php-array-set array
                      (if preserve-keys (car pair) (%php-array-next-auto-index array))
                      (cdr pair)))))

(defun %php-sort (array &optional sort-flags)
  "Sort ARRAY values ascending, reindexing numeric keys."
  (%php-sort-pairs-by array #'cdr :sort-flags sort-flags))

(defun %php-rsort (array &optional sort-flags)
  "Sort ARRAY values descending, reindexing numeric keys."
  (%php-sort-pairs-by array #'cdr :descending t :sort-flags sort-flags))

(defun %php-asort (array &optional sort-flags)
  "Sort ARRAY by value ascending, preserving keys."
  (%php-sort-pairs-by array #'cdr :preserve-keys t :sort-flags sort-flags))

(defun %php-ksort (array &optional sort-flags)
  "Sort ARRAY by key ascending, preserving key/value associations."
  (%php-sort-pairs-by array #'car :preserve-keys t :sort-flags sort-flags))

(defun %php-range (start end &optional (step 1))
  "Return a PHP array containing values from START to END by STEP."
  ;; (%php-array-values-list (%php-range 1 3)) => (1 2 3)
  (let ((result (%php-array))
        (actual-step (if (or (null step) (zerop step)) 1 (abs step))))
    (if (and (numberp start) (numberp end))
        (if (<= start end)
            (loop for value from start to end by actual-step
                  do (%php-array-set result (%php-array-next-auto-index result) value))
            (loop for value from start downto end by actual-step
                  do (%php-array-set result (%php-array-next-auto-index result) value)))
        (let ((s (char-code (char (%php-stringify start) 0)))
              (e (char-code (char (%php-stringify end) 0))))
          (if (<= s e)
              (loop for code from s to e by actual-step
                    do (%php-array-set result (%php-array-next-auto-index result)
                                       (string (code-char code))))
              (loop for code from s downto e by actual-step
                    do (%php-array-set result (%php-array-next-auto-index result)
                                       (string (code-char code)))))))
    result))

;;; ─── User-defined comparison sorts ───────────────────────────────────────────

(defun %php-user-sort-in-place (array compare-fn select-fn preserve-keys)
  "Sort ARRAY IN PLACE: PHP's usort/uasort/uksort take the array BY REFERENCE
and mutate it (they do not return a sorted copy).  COMPARE-FN is applied to
(funcall SELECT-FN pair) of each pair — #'cdr for value sorts, #'car for key
sorts.  Numeric keys are re-indexed unless PRESERVE-KEYS.  Uses stable-sort to
match PHP 8.0+ stable-sort semantics.  Returns T (the PHP return value).

The earlier implementations built a fresh result array and returned it, so the
caller's variable — which still held the original hash-table — was never
updated and the sort appeared to do nothing."
  (let ((fn (%php-callable-function compare-fn)))
    (when fn
      (let ((sorted (stable-sort (copy-list (%php-array-pairs array))
                                 (lambda (a b)
                                   (< (%php-numeric
                                       (funcall fn (funcall select-fn a) (funcall select-fn b)))
                                      0)))))
        (clrhash array)
        (setf (gethash +php-array-order-key+ array) nil
              (gethash +php-array-next-index-key+ array) 0)
        (dolist (pair sorted)
          (%php-array-set array
                          (if preserve-keys (car pair) (%php-array-next-auto-index array))
                          (cdr pair)))))
    t))

(defun %php-usort (array compare-fn)
  "PHP usort: sort ARRAY in place by user COMPARE-FN on values, re-indexing keys."
  (%php-user-sort-in-place array compare-fn #'cdr nil))

(defun %php-uasort (array compare-fn)
  "PHP uasort: sort ARRAY in place by user COMPARE-FN on values, preserving keys."
  (%php-user-sort-in-place array compare-fn #'cdr t))

(defun %php-uksort (array compare-fn)
  "PHP uksort: sort ARRAY in place by user COMPARE-FN on keys, preserving keys."
  (%php-user-sort-in-place array compare-fn #'car t))

;;; ─── array_walk / array_chunk / array_pad ────────────────────────────────────

(defun %php-array-walk (array callback &optional extra-data)
  "PHP array_walk: call CALLBACK(value, key, extra?) for each element."
  (let ((fn (%php-callable-function callback)))
    (when fn
      (dolist (pair (%php-array-pairs array))
        (if extra-data
            (funcall fn (cdr pair) (car pair) extra-data)
            (funcall fn (cdr pair) (car pair))))))
  t)

(defun %php-array-chunk (array size &optional preserve-keys)
  "PHP array_chunk: split ARRAY into chunks of SIZE."
  (let* ((pairs (%php-array-pairs array))
         (result (%php-make-array))
         (chunk nil)
         (chunk-idx 0)
         (key-idx 0))
    (dolist (pair pairs)
      (when (and chunk (= (length chunk) size))
        (let ((c (%php-make-array)))
          (loop for i from 0 for p in (nreverse chunk)
                do (%php-array-set c (if preserve-keys (car p) i) (cdr p)))
          (%php-array-set result chunk-idx c)
          (incf chunk-idx)
          (setf chunk nil key-idx 0)))
      (push pair chunk)
      (incf key-idx))
    (when chunk
      (let ((c (%php-make-array)))
        (loop for i from 0 for p in (nreverse chunk)
              do (%php-array-set c (if preserve-keys (car p) i) (cdr p)))
        (%php-array-set result chunk-idx c)))
    result))

(defun %php-array-pad (array size value)
  "PHP array_pad: pad array to SIZE with VALUE."
  (let* ((pairs (%php-array-pairs array))
         (current-size (length pairs))
         (target (abs size))
         (result (%php-make-array)))
    (if (>= current-size target)
        ;; array_pad returns a copy; integer keys are reindexed, string keys survive.
        (dolist (pair pairs result)
          (%php-array-copy-with-key-policy result (car pair) (cdr pair) nil))
        (let ((pad-count (- target current-size)))
          (if (< size 0)
              (progn
                (dotimes (_ pad-count)
                  (%php-array-set result (%php-array-next-auto-index result) value))
                (dolist (pair pairs)
                  (%php-array-copy-with-key-policy result (car pair) (cdr pair) nil)))
              (progn
                (dolist (pair pairs)
                  (%php-array-copy-with-key-policy result (car pair) (cdr pair) nil))
                (dotimes (_ pad-count)
                  (%php-array-set result (%php-array-next-auto-index result) value))))
          result))))

(defun %php-array-count-values (array)
  "PHP array_count_values: count occurrences of each value."
  (let ((result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (let ((value (cdr pair)))
        (when (or (integerp value) (stringp value))
          (let* ((v (%php-stringify value))
                 (current (%php-array-ref result v)))
            (%php-array-set result v (if (%php-null-p current) 1 (1+ current)))))))
    result))

(defun %php-array-sum (array)
  "PHP array_sum: sum all numeric values."
  (let ((sum 0))
    (dolist (pair (%php-array-pairs array))
      (let ((v (cdr pair)))
        (when (numberp v) (incf sum v))))
    sum))

(defun %php-array-product (array)
  "PHP array_product: multiply all numeric values."
  (let ((product 1))
    (dolist (pair (%php-array-pairs array))
      (let ((v (cdr pair)))
        (when (numberp v) (setf product (* product v)))))
    product))

(defun %php-array-value-string= (a b)
  "Compare array values using PHP's string representation rule."
  (string= (%php-stringify a) (%php-stringify b)))

(defun %php-array-assoc-pair-match-p (array key value)
  "Return true when ARRAY contains KEY with a PHP array-comparable VALUE."
  (and (%php-array-key-exists array key)
       (%php-array-value-string= (%php-array-ref array key) value)))

(defun %php-array-diff (array &rest arrays)
  "PHP array_diff: elements in first not in others (by value)."
  (let* ((others (loop for a in arrays append (mapcar #'cdr (%php-array-pairs a))))
         (result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (unless (member (cdr pair) others :test #'%php-array-value-string=)
        (%php-array-set result (car pair) (cdr pair))))
    result))

(defun %php-array-intersect (array &rest arrays)
  "PHP array_intersect: elements in first also in all others."
  (let* ((others (loop for a in arrays collect (mapcar #'cdr (%php-array-pairs a))))
         (result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (when (every (lambda (other)
                     (member (cdr pair) other :test #'%php-array-value-string=))
                   others)
        (%php-array-set result (car pair) (cdr pair))))
    result))

(defun %php-callback-equal-p (cb value items)
  "True when VALUE compares equal (callback returns 0) to some element of the
CL list ITEMS, using the user comparison callback CB."
  (and cb (some (lambda (o) (zerop (%php-numeric (funcall cb value o)))) items)))

(defun %php-array-udiff (array &rest rest)
  "PHP array_udiff: elements of ARRAY not found in the other arrays, compared by
the LAST argument — a callback ($a,$b) -> negative/0/positive (0 means equal)."
  (let* ((cb (%php-callable-function (car (last rest))))
         (others (loop for a in (butlast rest)
                       when (hash-table-p a) append (mapcar #'cdr (%php-array-pairs a))))
         (result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (unless (%php-callback-equal-p cb (cdr pair) others)
        (%php-array-set result (car pair) (cdr pair))))
    result))

(defun %php-array-uintersect (array &rest rest)
  "PHP array_uintersect: elements of ARRAY present in ALL other arrays, compared
by the last argument (a user comparison callback)."
  (let* ((cb (%php-callable-function (car (last rest))))
         (arrays (remove-if-not #'hash-table-p (butlast rest)))
         (result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (when (and cb (every (lambda (a)
                             (%php-callback-equal-p cb (cdr pair) (mapcar #'cdr (%php-array-pairs a))))
                           arrays))
        (%php-array-set result (car pair) (cdr pair))))
    result))

;;; ─── array_column / array_combine / array_flip ───────────────────────────────

(defun %php-array-column (input column-key &optional index-key)
  "PHP array_column: extract values from COLUMN-KEY across rows, optionally indexed by INDEX-KEY."
  (let ((result (%php-make-array))
        (all-columns-p (or (null column-key) (%php-null-p column-key)))
        (indexed-p (and index-key (not (%php-null-p index-key)))))
    (dolist (pair (%php-array-pairs input))
      (let ((row (cdr pair)))
        (when (and (hash-table-p row)
                   (or all-columns-p (%php-array-key-present-p row column-key)))
          (let ((value (if all-columns-p row (%php-array-ref row column-key)))
                (has-index-p (and indexed-p (%php-array-key-present-p row index-key))))
            (%php-array-set result
                            (if has-index-p
                                (%php-array-ref row index-key)
                                (%php-array-next-auto-index result))
                            value)))))
    result))

(defun %php-array-combine (keys values)
  "PHP array_combine: create array from KEYS and VALUES arrays."
  (let* ((ks (%php-array-values-list keys))
         (vs (%php-array-values-list values))
         (result (%php-make-array)))
    (loop for k in ks for v in vs
          do (%php-array-set result k v))
    result))

(defun %php-array-flip (array)
  "PHP array_flip: exchange keys and values."
  (let ((result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (let ((value (cdr pair)))
        (when (or (integerp value) (stringp value))
          (%php-array-set result value (car pair)))))
    result))

;;; ─── array_fill / array_fill_keys ────────────────────────────────────────────

(defun %php-array-fill (start-index num value)
  "PHP array_fill: fill array with VALUE starting at START-INDEX."
  (let ((result (%php-make-array)))
    (dotimes (i num)
      (%php-array-set result (+ start-index i) value))
    result))

(defun %php-array-fill-keys (keys value)
  "PHP array_fill_keys: create array using KEYS with VALUE."
  (let ((result (%php-make-array)))
    (dolist (k (%php-array-values-list keys))
      (%php-array-set result k value))
    result))

;;; ─── array_splice / array_replace ───────────────────────────────────────────

(defun %php-array-splice (array offset &optional length replacement)
  "PHP array_splice: remove LENGTH elements at OFFSET, optionally insert REPLACEMENT."
  (let* ((pairs (%php-array-pairs array))
         (size (length pairs))
         (start (if (minusp offset) (max 0 (+ size offset)) (min offset size)))
         (end (cond ((or (null length) (%php-null-p length)) size)
                    ((minusp length) (max start (+ size length)))
                    (t (min size (+ start length)))))
         (before (subseq pairs 0 start))
         (removed (subseq pairs start end))
         (after (subseq pairs end))
         (inserts (when (and replacement (hash-table-p replacement))
                    (mapcar #'cdr (%php-array-pairs replacement))))
         (new-pairs (append before
                            (mapcar (lambda (v) (cons 0 v)) inserts)
                            after)))
    (clrhash array)
    (setf (gethash +php-array-order-key+ array) nil
          (gethash +php-array-next-index-key+ array) 0)
    (loop for pair in new-pairs
          do (%php-array-set array (%php-array-next-auto-index array) (cdr pair)))
    (%php-list-to-array (mapcar #'cdr removed))))

(defun %php-array-replace (array &rest replacements)
  "PHP array_replace: replace values in ARRAY with values from REPLACEMENTS."
  (let ((result (%php-copy-array array)))
    (dolist (r replacements)
      (when (hash-table-p r)
        (dolist (pair (%php-array-pairs r))
          (%php-array-set result (car pair) (cdr pair)))))
    result))

;;; ─── Additional sorts ────────────────────────────────────────────────────────

(defun %php-krsort (array &optional sort-flags)
  "PHP krsort: sort ARRAY by key descending."
  (%php-sort-pairs-by array #'car
                      :preserve-keys t
                      :descending t
                      :sort-flags sort-flags))

(defun %php-arsort (array &optional sort-flags)
  "PHP arsort: sort ARRAY by value descending, preserving keys."
  (%php-sort-pairs-by array #'cdr
                      :preserve-keys t
                      :descending t
                      :sort-flags sort-flags))

;;; ─── array_diff_key / array_intersect_key ────────────────────────────────────

(defun %php-array-diff-key (array &rest arrays)
  "PHP array_diff_key: keys in ARRAY not present in any of ARRAYS."
  (let* ((other-keys (loop for a in arrays
                           append (mapcar #'car (%php-array-pairs a))))
         (result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (unless (member (car pair) other-keys :test #'equal)
        (%php-array-set result (car pair) (cdr pair))))
    result))

(defun %php-array-intersect-key (array &rest arrays)
  "PHP array_intersect_key: keys in ARRAY present in all of ARRAYS."
  (let* ((other-key-sets (loop for a in arrays
                               collect (mapcar #'car (%php-array-pairs a))))
         (result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (when (every (lambda (ks) (member (car pair) ks :test #'equal)) other-key-sets)
        (%php-array-set result (car pair) (cdr pair))))
    result))

(defun %php-array-diff-assoc (array &rest arrays)
  "PHP array_diff_assoc: key+value pairs in ARRAY not in others."
  (let ((result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (unless (some (lambda (a)
                      (%php-array-assoc-pair-match-p a (car pair) (cdr pair)))
                    arrays)
        (%php-array-set result (car pair) (cdr pair))))
    result))

(defun %php-array-intersect-assoc (array &rest arrays)
  "PHP array_intersect_assoc: key+value pairs in ARRAY also in all others."
  (let ((result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (when (every (lambda (a)
                     (%php-array-assoc-pair-match-p a (car pair) (cdr pair)))
                   arrays)
        (%php-array-set result (car pair) (cdr pair))))
    result))

(defun %php-array-merge-recursive (&rest arrays)
  "PHP array_merge_recursive: merge arrays, combining duplicate keys recursively."
  (let ((result (%php-make-array)))
    (dolist (array arrays)
      (when (hash-table-p array)
        (dolist (pair (%php-array-pairs array))
          (let ((key (car pair)) (val (cdr pair)))
            (cond
              ;; Integer keys are ALWAYS appended (renumbered), never merged by
              ;; key — this is what array_merge (and the recursive descent into a
              ;; list) does.  Previously the key-0 collision of two lists merged
              ;; by key and wrapped, so [[1]]+[[2]] gave [[1,2]] not [1,2].
              ((integerp key)
               (%php-array-set result (%php-array-next-auto-index result) val))
              (t
               (let ((existing (%php-array-ref result key)))
                 (if (%php-null-p existing)
                     (%php-array-set result key val)
                     ;; String-key collision: combine both sides as arrays and
                     ;; merge recursively (a scalar is first wrapped in [x]).
                     (let ((ex (if (hash-table-p existing) existing (%php-list-to-array (list existing))))
                           (nw (if (hash-table-p val) val (%php-list-to-array (list val)))))
                       (%php-array-set result key (%php-array-merge-recursive ex nw)))))))))))
    result))

(defun %php-shuffle (array)
  "PHP shuffle: randomize order of ARRAY elements."
  (let* ((values (%php-array-values-list array))
         (vec (coerce values 'vector))
         (n (length vec)))
    (loop for i from (1- n) downto 1
          do (let* ((j (random (1+ i)))
                    (tmp (aref vec i)))
               (setf (aref vec i) (aref vec j)
                     (aref vec j) tmp)))
    (clrhash array)
    (setf (gethash +php-array-order-key+ array) nil
          (gethash +php-array-next-index-key+ array) 0)
    (loop for v across vec
          do (%php-array-set array (%php-array-next-auto-index array) v))
    t))

(defun %php-array-key-first (array)
  "PHP array_key_first: return the first key of ARRAY."
  (let ((pairs (%php-array-pairs array)))
    (if pairs (car (first pairs)) +php-null+)))

(defun %php-array-key-last (array)
  "PHP array_key_last: return the last key of ARRAY."
  (let ((pairs (%php-array-pairs array)))
    (if pairs (car (car (last pairs))) +php-null+)))

(defun %php-array-first (array)
  "PHP array_first: return the first value of ARRAY."
  (let ((pairs (%php-array-pairs array)))
    (if pairs (cdr (first pairs)) +php-null+)))

(defun %php-array-last (array)
  "PHP array_last: return the last value of ARRAY."
  (let ((pairs (%php-array-pairs array)))
    (if pairs (cdr (car (last pairs))) +php-null+)))

(defun %php-array-find (arr callback)
  "Return the first element of ARR for which CALLBACK returns true, or PHP null.
PHP 8.4: array_find()."
  (let ((fn (%php-callable-function callback)))
    (when (and fn (hash-table-p arr))
      (dolist (pair (%php-array-pairs arr))
        (when (%php-truthy (funcall fn (cdr pair)))
          (return-from %php-array-find (cdr pair)))))
    +php-null+))

(defun %php-array-find-key (arr callback)
  "Return the key of the first element for which CALLBACK returns true, or PHP null.
PHP 8.4: array_find_key()."
  (let ((fn (%php-callable-function callback)))
    (when (and fn (hash-table-p arr))
      (dolist (pair (%php-array-pairs arr))
        (when (%php-truthy (funcall fn (cdr pair)))
          (return-from %php-array-find-key (car pair)))))
    +php-null+))

(defun %php-array-any (arr callback)
  "Return true when CALLBACK returns true for at least one element of ARR.
PHP 8.4: array_any()."
  (let ((fn (%php-callable-function callback)))
    (and fn
         (hash-table-p arr)
         (some (lambda (pair) (%php-truthy (funcall fn (cdr pair))))
               (%php-array-pairs arr)))))

(defun %php-array-all (arr callback)
  "Return true when CALLBACK returns true for every element of ARR.
PHP 8.4: array_all(). Returns true for an empty array."
  (let ((fn (%php-callable-function callback)))
    (or (not fn)
        (not (hash-table-p arr))
        (every (lambda (pair) (%php-truthy (funcall fn (cdr pair))))
               (%php-array-pairs arr)))))

;;; ─── array_map with multiple arrays ────────────────────────────────────────

(defun %php-array-map-multi (callback &rest arrays)
  "PHP array_map with multiple arrays: maps CALLBACK over parallel elements."
  (if arrays
      (apply #'%php-array-map callback arrays)
      (%php-array)))

;;; ─── array_multisort ────────────────────────────────────────────────────────

(defun %php-array-multisort-spec (array)
  "Build an internal array_multisort spec for ARRAY."
  (vector array nil :regular))

(defun %php-array-multisort-apply-flag (spec flag)
  "Apply an array_multisort sort/order FLAG to SPEC."
  (case flag
    (3 (setf (aref spec 1) t))          ; SORT_DESC
    (4 (setf (aref spec 1) nil))        ; SORT_ASC
    (0 (setf (aref spec 2) :regular))   ; SORT_REGULAR
    (1 (setf (aref spec 2) :numeric))   ; SORT_NUMERIC
    (2 (setf (aref spec 2) :string))    ; SORT_STRING
    (otherwise spec))
  spec)

(defun %php-array-multisort-parse-specs (array rest)
  "Parse PHP's array_multisort argument stream into array specs."
  (let ((specs (list (%php-array-multisort-spec array))))
    (dolist (arg rest (nreverse specs))
      (cond
        ((hash-table-p arg)
         (push (%php-array-multisort-spec arg) specs))
        ((and (integerp arg) specs)
         (%php-array-multisort-apply-flag (first specs) arg))))))

(defun %php-array-multisort-compare-values (a b sort-type)
  "Return -1, 0, or 1 comparing A and B for array_multisort SORT-TYPE."
  (%php-sort-compare-values a b sort-type))

(defun %php-array-multisort-pair-value (pairs index)
  "Return PAIRS[INDEX]'s value, or PHP null when absent."
  (let ((pair (nth index pairs)))
    (if pair (cdr pair) +php-null+)))

(defun %php-array-multisort (array &rest rest)
  "PHP array_multisort: lexicographically sort arrays together in place."
  (let* ((specs (%php-array-multisort-parse-specs array rest))
         (pairs-by-array (mapcar (lambda (spec) (%php-array-pairs (aref spec 0))) specs))
         (lengths (mapcar #'length pairs-by-array)))
    (unless (and specs (every (lambda (len) (= len (first lengths))) lengths))
      (return-from %php-array-multisort nil))
    (labels ((index< (left right)
               (loop for spec in specs
                     for pairs in pairs-by-array
                     for comparison = (%php-array-multisort-compare-values
                                        (%php-array-multisort-pair-value pairs left)
                                        (%php-array-multisort-pair-value pairs right)
                                        (aref spec 2))
                     when (/= comparison 0)
                       return (if (aref spec 1)
                                  (> comparison 0)
                                  (< comparison 0))
                     finally (return nil))))
      (let ((indices (stable-sort (loop for i below (first lengths) collect i)
                                  #'index<)))
        (loop for spec in specs
              for pairs in pairs-by-array
              for target = (aref spec 0)
              do (progn
                   (clrhash target)
                   (setf (gethash +php-array-order-key+ target) nil
                         (gethash +php-array-next-index-key+ target) 0)
                   (dolist (index indices)
                     (let* ((pair (nth index pairs))
                            (key (car pair)))
                       (%php-array-set target
                                       (if (integerp key)
                                           (%php-array-next-auto-index target)
                                           key)
                                       (cdr pair)))))))
        t)))

;;; ─── array_key_exists alias + in_array strict ──────────────────────────────

(defun %php-in-array-strict (needle haystack)
  "PHP in_array with strict=true: uses === comparison."
  (when (hash-table-p haystack)
    (some (lambda (pair) (equal (cdr pair) needle)) (%php-array-pairs haystack))))

(defun %php-array-search-strict (needle haystack)
  "PHP array_search with strict=true."
  (when (hash-table-p haystack)
    (let ((pair (find-if (lambda (p) (equal (cdr p) needle)) (%php-array-pairs haystack))))
      (if pair (car pair) nil))))

;;; ─── Miscellaneous PHP 8.x array helpers ────────────────────────────────────

(defun %php-array-is-list (array)
  "PHP array_is_list: check if array has sequential integer keys from 0."
  (when (hash-table-p array)
    (loop for i from 0 for k in (%php-array-ordered-keys array)
          always (eql k i))))

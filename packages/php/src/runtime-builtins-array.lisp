;;;; Array PHP builtin helpers.

(in-package :cl-cc/php)

(defun %php-copy-array (array)
  "Return a shallow copy of PHP ARRAY preserving insertion order."
  (let ((copy (%php-array)))
    (dolist (pair (%php-array-pairs array) copy)
      (%php-array-set copy (car pair) (cdr pair)))))

(defun %php-callable-function (callback)
  "Resolve CALLBACK to a Common Lisp function, or NIL."
  (cond ((functionp callback) callback)
        ((and (symbolp callback) (fboundp callback)) (symbol-function callback))
        ((and (stringp callback) (fboundp '%php-lookup-builtin))
         (funcall (symbol-function '%php-lookup-builtin) callback))
        (t nil)))

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

(defun %php-array-keys (array)
  "Return ARRAY keys as a PHP array."
  ;; (%php-array-values-list (%php-array-keys (%php-array (list t "a" 1)))) => ("a")
  (%php-list-to-array (%php-array-ordered-keys array)))

(defun %php-array-values (array)
  "Return ARRAY values as a reindexed PHP array."
  (%php-list-to-array (%php-array-values-list array)))

(defun %php-array-push (array &rest values)
  "Append VALUES to ARRAY and return the new count."
  ;; (let ((a (%php-array))) (%php-array-push a "x")) => 1
  (check-type array hash-table)
  (dolist (value values (%php-count array))
    (%php-array-set array (%php-array-next-auto-index array) value)))

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

(defun %php-array-reverse (array &optional preserve-keys)
  "Return ARRAY values in reverse order."
  ;; (%php-array-values-list (%php-array-reverse (%php-list-to-array '(1 2)))) => (2 1)
  (let ((result (%php-array)))
    (dolist (pair (reverse (%php-array-pairs array)) result)
      (if (%php-truthy preserve-keys)
          (%php-array-set result (car pair) (cdr pair))
          (%php-array-set result (%php-array-next-auto-index result) (cdr pair))))))

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
      (if (%php-truthy preserve-keys)
          (%php-array-set result (car pair) (cdr pair))
          (%php-array-set result (%php-array-next-auto-index result) (cdr pair))))))

(defun %php-array-unique (array)
  "Return ARRAY with duplicate values removed, preserving first keys."
  (let ((result (%php-array))
        (seen nil))
    (dolist (pair (%php-array-pairs array) result)
      (unless (some (lambda (value) (%php-eq-loose value (cdr pair))) seen)
        (push (cdr pair) seen)
        (%php-array-set result (car pair) (cdr pair))))))

(defun %php-array-map (callback array)
  "Apply CALLBACK to each ARRAY value and return a reindexed PHP array."
  (let ((fn (%php-callable-function callback))
        (result (%php-array)))
    (when (and fn (hash-table-p array))
      (dolist (value (%php-array-values-list array))
        (%php-array-set result (%php-array-next-auto-index result)
                        (funcall fn value))))
    result))

(defun %php-array-filter (array &optional callback)
  "Filter ARRAY values by CALLBACK or PHP truthiness."
  (let ((fn (and callback (not (%php-null-p callback)) (%php-callable-function callback)))
        (result (%php-array)))
    (when (hash-table-p array)
      (dolist (pair (%php-array-pairs array))
        (let ((keep (if fn (funcall fn (cdr pair)) (%php-truthy (cdr pair)))))
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

(defun %php-sort-pairs-by (array key-fn &key descending preserve-keys)
  "Sort ARRAY by KEY-FN and return T after mutating ARRAY."
  (let ((pairs (stable-sort (copy-list (%php-array-pairs array))
                            (if descending #'string> #'string<)
                            :key (lambda (pair) (%php-stringify (funcall key-fn pair))))))
    (clrhash array)
    (setf (gethash +php-array-order-key+ array) nil
          (gethash +php-array-next-index-key+ array) 0)
    (dolist (pair pairs t)
      (%php-array-set array
                      (if preserve-keys (car pair) (%php-array-next-auto-index array))
                      (cdr pair)))))

(defun %php-sort (array)
  "Sort ARRAY values ascending, reindexing numeric keys."
  (%php-sort-pairs-by array #'cdr))

(defun %php-rsort (array)
  "Sort ARRAY values descending, reindexing numeric keys."
  (%php-sort-pairs-by array #'cdr :descending t))

(defun %php-asort (array)
  "Sort ARRAY by value ascending, preserving keys."
  (%php-sort-pairs-by array #'cdr :preserve-keys t))

(defun %php-ksort (array)
  "Sort ARRAY by key ascending, preserving key/value associations."
  (%php-sort-pairs-by array #'car :preserve-keys t))

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

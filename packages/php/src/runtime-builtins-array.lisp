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

;;; ─── User-defined comparison sorts ───────────────────────────────────────────

(defun %php-usort (array compare-fn)
  "PHP usort: sort ARRAY by user-defined COMPARE-FN, re-indexing keys."
  (let* ((fn (%php-callable-function compare-fn))
         (pairs (%php-array-pairs array))
         (sorted (if fn
                     (sort pairs (lambda (a b)
                                   (< (funcall fn (cdr a) (cdr b)) 0)))
                     pairs))
         (result (%php-make-array)))
    (loop for i from 0 for pair in sorted
          do (%php-array-set result i (cdr pair)))
    result))

(defun %php-uasort (array compare-fn)
  "PHP uasort: sort by user compare, preserving key associations."
  (let* ((fn (%php-callable-function compare-fn))
         (pairs (%php-array-pairs array))
         (sorted (if fn
                     (sort pairs (lambda (a b)
                                   (< (funcall fn (cdr a) (cdr b)) 0)))
                     pairs))
         (result (%php-make-array)))
    (dolist (pair sorted)
      (%php-array-set result (car pair) (cdr pair)))
    result))

(defun %php-uksort (array compare-fn)
  "PHP uksort: sort by key using user compare."
  (let* ((fn (%php-callable-function compare-fn))
         (pairs (%php-array-pairs array))
         (sorted (if fn
                     (sort pairs (lambda (a b)
                                   (< (funcall fn (car a) (car b)) 0)))
                     pairs))
         (result (%php-make-array)))
    (dolist (pair sorted)
      (%php-array-set result (car pair) (cdr pair)))
    result))

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
        ;; No padding needed — copy as-is
        (dolist (pair pairs result)
          (%php-array-set result (car pair) (cdr pair)))
        (let ((pad-count (- target current-size)))
          (if (< size 0)
              ;; Pad at beginning
              (progn
                (dotimes (i pad-count)
                  (%php-array-set result i value))
                (loop for i from pad-count for pair in pairs
                      do (%php-array-set result i (cdr pair))))
              ;; Pad at end
              (progn
                (loop for i from 0 for pair in pairs
                      do (%php-array-set result i (cdr pair)))
                (dotimes (i pad-count)
                  (%php-array-set result (+ current-size i) value))))
          result))))

(defun %php-array-count-values (array)
  "PHP array_count_values: count occurrences of each value."
  (let ((result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (let* ((v (%php-stringify (cdr pair)))
             (current (%php-array-ref result v)))
        (%php-array-set result v (if (%php-null-p current) 1 (1+ current)))))
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

(defun %php-array-diff (array &rest arrays)
  "PHP array_diff: elements in first not in others (by value)."
  (let* ((others (loop for a in arrays append (mapcar #'cdr (%php-array-pairs a))))
         (result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (unless (member (cdr pair) others :test #'equal)
        (%php-array-set result (car pair) (cdr pair))))
    result))

(defun %php-array-intersect (array &rest arrays)
  "PHP array_intersect: elements in first also in all others."
  (let* ((others (loop for a in arrays collect (mapcar #'cdr (%php-array-pairs a))))
         (result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (when (every (lambda (other) (member (cdr pair) other :test #'equal)) others)
        (%php-array-set result (car pair) (cdr pair))))
    result))

;;; ─── array_column / array_combine / array_flip ───────────────────────────────

(defun %php-array-column (input column-key &optional index-key)
  "PHP array_column: extract values from COLUMN-KEY across rows, optionally indexed by INDEX-KEY."
  (let ((result (%php-make-array)))
    (dolist (pair (%php-array-pairs input))
      (let* ((row (cdr pair))
             (val (if (hash-table-p row)
                      (if (or (null column-key) (%php-null-p column-key))
                          row
                          (%php-array-ref row column-key))
                      row))
             (key (if (and index-key (not (%php-null-p index-key)) (hash-table-p row))
                      (%php-array-ref row index-key)
                      nil)))
        (if key
            (%php-array-set result key val)
            (%php-array-set result (%php-array-next-auto-index result) val))))
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
      (%php-array-set result (cdr pair) (car pair)))
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

(defun %php-krsort (array)
  "PHP krsort: sort ARRAY by key descending."
  (%php-sort-pairs-by array #'car :preserve-keys t :descending t))

(defun %php-arsort (array)
  "PHP arsort: sort ARRAY by value descending, preserving keys."
  (%php-sort-pairs-by array #'cdr :preserve-keys t :descending t))

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
                      (let ((v (%php-array-ref a (car pair))))
                        (and (not (%php-null-p v)) (equal v (cdr pair)))))
                    arrays)
        (%php-array-set result (car pair) (cdr pair))))
    result))

(defun %php-array-intersect-assoc (array &rest arrays)
  "PHP array_intersect_assoc: key+value pairs in ARRAY also in all others."
  (let ((result (%php-make-array)))
    (dolist (pair (%php-array-pairs array))
      (when (every (lambda (a) (equal (%php-array-ref a (car pair)) (cdr pair))) arrays)
        (%php-array-set result (car pair) (cdr pair))))
    result))

(defun %php-array-merge-recursive (&rest arrays)
  "PHP array_merge_recursive: merge arrays, combining duplicate keys recursively."
  (let ((result (%php-make-array)))
    (dolist (array arrays)
      (when (hash-table-p array)
        (dolist (pair (%php-array-pairs array))
          (let ((existing (%php-array-ref result (car pair))))
            (cond ((and (not (%php-null-p existing)) (hash-table-p existing) (hash-table-p (cdr pair)))
                   (%php-array-set result (car pair) (apply #'%php-array-merge-recursive (list existing (cdr pair)))))
                  ((not (%php-null-p existing))
                   (let ((merged (%php-make-array)))
                     (%php-array-set merged 0 existing)
                     (%php-array-set merged 1 (cdr pair))
                     (%php-array-set result (car pair) merged)))
                  ((integerp (car pair))
                   (%php-array-set result (%php-array-next-auto-index result) (cdr pair)))
                  (t (%php-array-set result (car pair) (cdr pair))))))))
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

(defun %php-compact (&rest var-names)
  "PHP compact: create array from variable names (returns empty — context-dependent)."
  (%php-make-array))

(defun %php-array-key-first (array)
  "PHP array_key_first: return the first key of ARRAY."
  (let ((pairs (%php-array-pairs array)))
    (if pairs (car (first pairs)) +php-null+)))

(defun %php-array-key-last (array)
  "PHP array_key_last: return the last key of ARRAY."
  (let ((pairs (%php-array-pairs array)))
    (if pairs (car (car (last pairs))) +php-null+)))

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
  (if (null callback)
      ;; null callback: zip arrays into sub-arrays
      (let ((result (%php-make-array))
            (lists (mapcar #'%php-array-values-list arrays)))
        (loop for i from 0 below (apply #'min (mapcar #'length lists))
              do (let ((row (%php-make-array)))
                   (dolist (lst lists)
                     (%php-array-set row (%php-array-next-auto-index row) (nth i lst)))
                   (%php-array-set result i row)))
        result)
      ;; normal multi-array map
      (let ((result (%php-make-array))
            (fn (%php-callable-function callback))
            (lists (mapcar #'%php-array-values-list arrays)))
        (when fn
          (loop for i from 0 below (apply #'min (mapcar #'length lists))
                do (%php-array-set result i (apply fn (mapcar (lambda (lst) (nth i lst)) lists)))))
        result)))

;;; ─── array_multisort (simplified) ──────────────────────────────────────────

(defun %php-array-multisort (array &rest rest)
  "PHP array_multisort: sort array (simplified, ignores extra flags)."
  (declare (ignore rest))
  (%php-sort array))

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

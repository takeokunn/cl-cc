(in-package :cl-cc/vm)

;;; FR-748 / FR-749: simple persistent map/vector facades.
;;; These intentionally copy on write at the public boundary; FR-824 transients
;;; below provide the mutable construction path for bulk updates.

(defstruct (persistent-map
             (:constructor %make-persistent-map (table count test)))
  (table (make-hash-table) :type hash-table)
  (count 0 :type fixnum)
  (test 'eql))

(defun %persistent-map-test-name (test)
  (cond
    ((or (eq test 'eq) (eq test #'eq)) 'eq)
    ((or (eq test 'eql) (eq test #'eql)) 'eql)
    ((or (eq test 'equal) (eq test #'equal)) 'equal)
    ((or (eq test 'equalp) (eq test #'equalp)) 'equalp)
    (t test)))

(defun %parse-persistent-map-args (args)
  (if (and args (eq (first args) :test))
      (values (%persistent-map-test-name (second args)) (cddr args))
      (values 'eql args)))

(defun %ensure-even-key-values (operation entries)
  (when (oddp (length entries))
    (error "~A: expected an even number of key/value entries, got ~D"
           operation
           (length entries)))
  entries)

(defun %ensure-persistent-map (object operation)
  (unless (persistent-map-p object)
    (error "~A: expected persistent-map, got ~S" operation object))
  object)

(defun %copy-hash-table (table &key test)
  (let ((copy (make-hash-table :test (or test (hash-table-test table)))))
    (loop for key being the hash-keys of table using (hash-value value)
          do (setf (gethash key copy) value))
    copy))

(defun persistent-map (&rest args)
  "Create a persistent hash map. Accepts optional leading :TEST TEST."
  (multiple-value-bind (test entries) (%parse-persistent-map-args args)
    (%ensure-even-key-values "persistent-map" entries)
    (let ((table (make-hash-table :test test)))
      (loop for (key value) on entries by #'cddr
            do (setf (gethash key table) value))
      (%make-persistent-map table (hash-table-count table) test))))

(defun pmap-assoc (m k v)
  "Persistent-map assoc: (PMAP-ASSOC MAP KEY VALUE) returns new map with association."
  (%ensure-persistent-map m "pmap-assoc")
  (let ((table (%copy-hash-table (persistent-map-table m)
                                 :test (persistent-map-test m))))
    (setf (gethash k table) v)
    (%make-persistent-map table
                          (hash-table-count table)
                          (persistent-map-test m))))

(defun dissoc (m k)
  "Return a persistent map like M without K."
  (%ensure-persistent-map m "dissoc")
  (let ((table (%copy-hash-table (persistent-map-table m)
                                 :test (persistent-map-test m))))
    (remhash k table)
    (%make-persistent-map table (hash-table-count table)
                          (persistent-map-test m))))

(defun pmap-get (m k &optional default)
  "Persistent-map lookup for key K in M."
  (%ensure-persistent-map m "pmap-get")
  (multiple-value-bind (value found-p) (gethash k (persistent-map-table m))
    (if found-p
        (values value t)
        (values default nil))))

(defun pget (m k &optional default)
  (pmap-get m k default))

(defun pvec (&rest elts)
  "Create a persistent vector from ELTS."
  (let ((data (make-array (length elts) :initial-contents elts)))
    (cons data (length elts))))

(defun pvec-count (v) (cdr v))

(defun pvec-get (v i &optional default)
  (if (and (integerp i) (<= 0 i) (< i (pvec-count v)))
      (aref (car v) i)
      default))

(defun pvec->list (v)
  (loop for i below (pvec-count v) collect (pvec-get v i)))

(defun pvec-assoc (v i value)
  "Return a vector like V with VALUE at I. I equal to count appends."
  (let ((count (pvec-count v)))
    (cond
      ((and (integerp i) (<= 0 i) (< i count))
       (let ((data (copy-seq (car v))))
         (setf (aref data i) value)
         (cons data count)))
      ((eql i count) (pvec-conj v value))
      (t (error "pvec-assoc: index ~S out of bounds for count ~D" i count)))))

(defun pvec-conj (v value)
  "Return a vector with VALUE appended."
  (let* ((count (pvec-count v))
         (data (make-array (1+ count))))
    (replace data (car v))
    (setf (aref data count) value)
    (cons data (1+ count))))

;;; FR-824: Transient collections.

(defstruct (transient (:constructor %make-transient (buffer frozen-p)))
  (buffer (make-array 0 :adjustable t :fill-pointer 0))
  (frozen-p nil :type boolean))

(defstruct (transient-hash (:constructor %make-transient-hash (table test frozen-p)))
  (table (make-hash-table) :type hash-table)
  (test 'eql)
  (frozen-p nil :type boolean))

(defun %ensure-unfrozen-transient (obj operation)
  (when (or (and (transient-p obj) (transient-frozen-p obj))
            (and (transient-hash-p obj) (transient-hash-frozen-p obj)))
    (error "~A: transient collection has already been made persistent" operation))
  obj)

(defun %pvec->mutable-buffer (v)
  (let* ((count (pvec-count v))
         (buffer (make-array (max 8 count)
                             :adjustable t
                             :fill-pointer count)))
    (replace buffer (car v) :end1 count :end2 count)
    buffer))

(defun transient (collection)
  "Return a mutable transient copy of a persistent vector or map."
  (cond
    ((persistent-map-p collection)
     (%make-transient-hash
      (%copy-hash-table (persistent-map-table collection)
                        :test (persistent-map-test collection))
      (persistent-map-test collection)
      nil))
    ((and (consp collection) (vectorp (car collection)) (integerp (cdr collection)))
     (%make-transient (%pvec->mutable-buffer collection) nil))
    (t (error "transient: unsupported collection ~S" collection))))

(defun transient! (collection &optional index value)
  "With one argument, make a transient vector. With three, update it in place."
  (if (and (transient-p collection) index)
      (progn
        (%ensure-unfrozen-transient collection 'transient!)
        (unless (and (integerp index) (<= 0 index) (< index (fill-pointer (transient-buffer collection))))
          (error "transient!: index ~S out of bounds" index))
        (setf (aref (transient-buffer collection) index) value)
        collection)
      (transient collection)))

(defun conj! (tv value)
  "Append VALUE to transient vector TV in place."
  (%ensure-unfrozen-transient tv 'conj!)
  (unless (transient-p tv)
    (error "conj!: expected transient vector, got ~S" tv))
  (vector-push-extend value (transient-buffer tv))
  tv)

(defun assoc! (tm key value)
  "Set KEY to VALUE in transient map TM in place."
  (%ensure-unfrozen-transient tm 'assoc!)
  (unless (transient-hash-p tm)
    (error "assoc!: expected transient map, got ~S" tm))
  (setf (gethash key (transient-hash-table tm)) value)
  tm)

(defun dissoc! (tm key)
  "Remove KEY from transient map TM in place."
  (%ensure-unfrozen-transient tm 'dissoc!)
  (unless (transient-hash-p tm)
    (error "dissoc!: expected transient map, got ~S" tm))
  (remhash key (transient-hash-table tm))
  tm)

(defun persistent! (tc)
  "Freeze transient collection TC and return its persistent value."
  (cond
    ((transient-p tc)
     (%ensure-unfrozen-transient tc 'persistent!)
     (let* ((buffer (transient-buffer tc))
            (count (fill-pointer buffer))
            (data (make-array count)))
       (replace data buffer :end1 count :end2 count)
       (setf (transient-frozen-p tc) t)
       (cons data count)))
    ((transient-hash-p tc)
     (%ensure-unfrozen-transient tc 'persistent!)
     (let* ((table (%copy-hash-table (transient-hash-table tc)
                                     :test (transient-hash-test tc)))
            (result (%make-persistent-map table (hash-table-count table)
                                          (transient-hash-test tc))))
       (setf (transient-hash-frozen-p tc) t)
       result))
    (t (error "persistent!: expected transient collection, got ~S" tc))))

(defstruct (lazy-seq (:constructor make-lazy-seq (&key thunk)))
  (thunk (lambda () nil) :type function)
  (realized nil :type boolean)
  cached-value)

(defmacro lazy-seq (&body body)
  (let ((thunk (if (and (= (length body) 1)
                        (consp (first body))
                        (eq (first (first body)) 'lambda))
                   (first body)
                   `(lambda () ,@body))))
    `(make-lazy-seq :thunk ,thunk)))

(defun %lazy-realize (ls)
  (unless (lazy-seq-realized ls)
    (setf (lazy-seq-cached-value ls) (funcall (lazy-seq-thunk ls))
          (lazy-seq-realized ls) t))
  (lazy-seq-cached-value ls))

(defun lazy-force (ls)
  (when ls
    (let ((value (%lazy-realize ls)))
      (when (consp value)
        (cons (car value) (cdr value))))))

(defun lazy-take-seq (n ls)
  (loop repeat n
        for pair = (lazy-force ls)
        while pair
        collect (car pair)
        do (setf ls (cdr pair))))

(defun lazy-map (f ls)
  (lazy-seq
    (let ((pair (lazy-force ls)))
      (when pair
        (cons (funcall f (car pair))
              (lazy-map f (cdr pair)))))))

(defun lazy-filter (pred ls)
  (lazy-seq
    (loop with cursor = ls
          for pair = (lazy-force cursor)
          while pair
          when (funcall pred (car pair))
            return (cons (car pair)
                         (lazy-filter pred (cdr pair)))
          do (setf cursor (cdr pair)))))

(defun iterate (f init)
  (lazy-seq
    (cons init (iterate f (funcall f init)))))

(defun %lazy-range-finished-p (start end step)
  (and end
       (if (plusp step)
           (>= start end)
           (<= start end))))

(defun lazy-range (&key (start 0) end (step 1))
  (when (zerop step)
    (error "lazy-range: step must not be zero"))
  (unless (%lazy-range-finished-p start end step)
    (lazy-seq
      (cons start
            (lazy-range :start (+ start step)
                        :end end
                        :step step)))))

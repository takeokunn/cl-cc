(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Test selection and dependency helpers
;;; ------------------------------------------------------------

(defvar *suite-fixture-cache* nil)
(defvar *suite-parallel-cache* nil)

(defun %test-tags-match-p (test-tags tags)
  "Return t if TEST-TAGS intersects TAGS (inclusion filter)."
  (or (null tags)
      (and test-tags (some (lambda (tag) (member tag test-tags)) tags))))

(defun %test-tags-excluded-p (test-tags exclude-tags)
  "Return t if TEST-TAGS intersects EXCLUDE-TAGS."
  (and exclude-tags test-tags
       (some (lambda (tag) (member tag test-tags)) exclude-tags)))

(defun %build-suite-test-index (tags exclude-tags)
  "Build an index from suite name to filtered test plists."
  (let ((index (make-hash-table :test #'eq)))
    (persist-each *test-registry*
                  (lambda (test-sym plist)
                    (declare (ignore test-sym))
                    (let ((test-tags (getf plist :tags)))
                      (when (and (%test-tags-match-p test-tags tags)
                                 (not (%test-tags-excluded-p test-tags exclude-tags)))
                        (push plist (gethash (getf plist :suite) index))))))
    index))

(defun %build-suite-child-index ()
  "Build an index from suite name to direct child suite names."
  (let ((index (make-hash-table :test #'eq)))
    (persist-each *suite-registry*
                  (lambda (child-name child-plist)
                    (let ((parent (getf child-plist :parent)))
                      (when parent
                        (push child-name (gethash parent index))))))
    (maphash (lambda (suite children)
               (setf (gethash suite index) (nreverse children)))
             index)
    index))

(defun %collect-suite-tests (suite-name tags &optional exclude-tags)
  "Collect all tests belonging to SUITE-NAME (not children), filtered by tags."
  (let ((result '()))
    (persist-each *test-registry*
                  (lambda (test-sym plist)
                    (declare (ignore test-sym))
                    (let ((suite (getf plist :suite))
                          (test-tags (getf plist :tags)))
                      (when (and (eq suite suite-name)
                                 (%test-tags-match-p test-tags tags)
                                 (not (%test-tags-excluded-p test-tags exclude-tags)))
                        (push plist result)))))
    result))

(defun %collect-all-suite-tests (suite-name tags &optional exclude-tags exclude-suites seen-suites)
  "Collect tests from SUITE-NAME and all descendant suites."
  (let ((test-index (%build-suite-test-index tags exclude-tags))
        (child-index (%build-suite-child-index)))
    (labels ((collect (current-suite seen)
               (cond
                 ((or (member current-suite exclude-suites :test #'eq)
                      (member current-suite seen :test #'eq))
                  '())
                 (t
                  (let ((result (copy-list (gethash current-suite test-index)))
                        (next-seen (cons current-suite seen)))
                    (dolist (child-name (gethash current-suite child-index))
                      (setf result
                            (nconc result (collect child-name next-seen))))
                    result)))))
      (collect suite-name seen-suites))))

(defun %fisher-yates-shuffle (vec)
  "In-place Fisher-Yates shuffle of a vector."
  (let ((n (length vec)))
    (loop for i from (1- n) downto 1
          do (let ((j (random (1+ i))))
               (rotatef (aref vec i) (aref vec j)))))
  vec)

(defun %compute-suite-fixtures (suite-name)
  "Return inherited before/after fixture chains for SUITE-NAME."
  (let ((before-chain '())
        (after-chain '())
        (current suite-name)
        (seen '()))
    (loop while current
          do (when (member current seen :test #'eq)
               (return))
             (push current seen)
             (let ((entry (persist-lookup *suite-registry* current)))
               (unless entry
                 (return))
               (setf before-chain (append (getf entry :before-each) before-chain))
               (setf after-chain  (append after-chain (getf entry :after-each)))
               (setf current (getf entry :parent))))
    (values before-chain after-chain)))

(defun %get-suite-fixtures (suite-name)
  "Return inherited before/after fixture chains for SUITE-NAME."
  (if *suite-fixture-cache*
      (multiple-value-bind (cached present-p)
          (gethash suite-name *suite-fixture-cache*)
        (if present-p
            (values (car cached) (cdr cached))
            (multiple-value-bind (before-chain after-chain)
                (%compute-suite-fixtures suite-name)
              (setf (gethash suite-name *suite-fixture-cache*)
                    (cons before-chain after-chain))
              (values before-chain after-chain))))
      (%compute-suite-fixtures suite-name)))

(defun %compute-suite-parallel-p (suite-name)
  "Return T when SUITE-NAME and all of its ancestors allow parallel execution."
  (loop with current = suite-name
        with seen = '()
        while current
        do (when (member current seen :test #'eq)
             (return-from %compute-suite-parallel-p nil))
           (push current seen)
           (let ((entry (persist-lookup *suite-registry* current)))
             (unless entry
               (return-from %compute-suite-parallel-p t))
             (when (null (getf entry :parallel))
               (return-from %compute-suite-parallel-p nil))
             (setf current (getf entry :parent)))
        finally (return t)))

(defun %suite-parallel-p (suite-name)
  "Return T when SUITE-NAME and all of its ancestors allow parallel execution."
  (if *suite-parallel-cache*
      (multiple-value-bind (cached present-p)
          (gethash suite-name *suite-parallel-cache*)
        (if present-p
            cached
            (setf (gethash suite-name *suite-parallel-cache*)
                  (%compute-suite-parallel-p suite-name))))
      (%compute-suite-parallel-p suite-name)))

(defun %test-parallel-safe-p (test-plist)
  "Return T when TEST-PLIST can safely run in the parallel worker pool."
  (and (null (getf test-plist :depends-on))
       (%suite-parallel-p (getf test-plist :suite))))

(defun %order-tests-for-dependencies (tests)
  "Return TESTS reordered so in-list dependencies run before dependents."
  (let* ((all-names (mapcar (lambda (test) (getf test :name)) tests))
         (emitted-names '())
         (pending tests)
         (ordered '()))
    (flet ((dependency-ready-p (test)
             (let ((dep (getf test :depends-on)))
               (or (null dep)
                   (member dep emitted-names :test #'eq)
                   (not (member dep all-names :test #'eq))))))
      (loop while pending
            for ready = (find-if #'dependency-ready-p pending)
            do (if ready
                   (progn
                     (setf ordered (append ordered (list ready)))
                     (push (getf ready :name) emitted-names)
                     (setf pending (remove ready pending :count 1 :test #'eq)))
                   (progn
                     (setf ordered (append ordered pending))
                     (return))))
      ordered)))

(defun %check-dependency (test-plist all-results)
  "Return nil if dependency failed (test should be skipped), t otherwise."
  (let ((dep (getf test-plist :depends-on)))
    (if (null dep)
        t
        (let ((dep-result (find dep all-results :key (lambda (r) (getf r :name)))))
          (if dep-result
              (eq (getf dep-result :status) :pass)
              t)))))

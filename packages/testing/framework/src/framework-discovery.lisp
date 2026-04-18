(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Test selection and dependency helpers
;;; ------------------------------------------------------------

(defun %test-tags-match-p (test-tags tags)
  "Return t if TEST-TAGS intersects TAGS (inclusion filter)."
  (or (null tags)
      (and test-tags (some (lambda (tag) (member tag test-tags)) tags))))

(defun %test-tags-excluded-p (test-tags exclude-tags)
  "Return t if TEST-TAGS intersects EXCLUDE-TAGS."
  (and exclude-tags test-tags
       (some (lambda (tag) (member tag test-tags)) exclude-tags)))

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

(defun %collect-all-suite-tests (suite-name tags &optional exclude-tags exclude-suites)
  "Collect tests from SUITE-NAME and all descendant suites."
  (if (member suite-name exclude-suites)
      '()
      (let ((result (%collect-suite-tests suite-name tags exclude-tags)))
        (persist-each *suite-registry*
                      (lambda (child-name child-plist)
                        (when (eq (getf child-plist :parent) suite-name)
                          (setf result
                                (append result
                                        (%collect-all-suite-tests
                                         child-name tags exclude-tags exclude-suites))))))
        result)))

(defun %fisher-yates-shuffle (vec)
  "In-place Fisher-Yates shuffle of a vector."
  (let ((n (length vec)))
    (loop for i from (1- n) downto 1
          do (let ((j (random (1+ i))))
               (rotatef (aref vec i) (aref vec j)))))
  vec)

(defun %get-suite-fixtures (suite-name)
  "Return inherited before/after fixture chains for SUITE-NAME."
  (let ((before-chain '())
        (after-chain '())
        (current suite-name))
    (loop while current
          for entry = (persist-lookup *suite-registry* current)
          while entry
          do (setf before-chain (append (getf entry :before-each) before-chain))
             (setf after-chain  (append after-chain (getf entry :after-each)))
             (setf current (getf entry :parent)))
    (values before-chain after-chain)))

(defun %suite-parallel-p (suite-name)
  "Return T when SUITE-NAME and all of its ancestors allow parallel execution."
  (loop with current = suite-name
        while current
        for entry = (persist-lookup *suite-registry* current)
        while entry
        do (when (null (getf entry :parallel))
             (return-from %suite-parallel-p nil))
           (setf current (getf entry :parent))
        finally (return t)))

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
    (labels ((dependency-ready-p (test)
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

(defun %default-test-timeout ()
  "Return the default per-test timeout in seconds."
  (let ((raw (uiop:getenv "timeout")))
    (or (and raw
             (ignore-errors
               (let ((parsed (parse-integer raw)))
                 (and (plusp parsed) parsed))))
        10)))

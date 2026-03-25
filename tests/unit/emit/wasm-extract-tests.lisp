;;;; tests/unit/emit/wasm-extract-tests.lisp — WASM Function Extraction Tests
;;;;
;;;; Tests for src/emit/wasm-extract.lisp:
;;;; collect-entry-labels, segment-instructions

(in-package :cl-cc/test)

(defsuite wasm-extract-suite :description "WASM function extraction pass tests")

;;; ─── collect-entry-labels ─────────────────────────────────────────────────────

(deftest extract-entry-labels-empty
  "collect-entry-labels on empty list returns empty hash table."
  (let ((ht (cl-cc::collect-entry-labels nil)))
    (assert-true (hash-table-p ht))
    (assert-equal 0 (hash-table-count ht))))

(deftest extract-entry-labels-no-closures
  "collect-entry-labels with no closure/func-ref returns empty."
  (let* ((instrs (list (make-vm-const :dst :r0 :value 42)
                       (make-vm-ret :reg :r0)))
         (ht (cl-cc::collect-entry-labels instrs)))
    (assert-equal 0 (hash-table-count ht))))

(deftest extract-entry-labels-closure
  "collect-entry-labels finds vm-closure entry labels."
  (let* ((instrs (list (make-vm-closure :dst :r0
                                        :label "fn1"
                                        :params '(:r1)
                                        :captured nil)
                       (make-vm-ret :reg :r0)))
         (ht (cl-cc::collect-entry-labels instrs)))
    (assert-equal 1 (hash-table-count ht))
    (assert-true (gethash "fn1" ht))))

(deftest extract-entry-labels-func-ref
  "collect-entry-labels finds vm-func-ref entry labels."
  (let* ((instrs (list (make-vm-func-ref :dst :r0 :label "fn2")))
         (ht (cl-cc::collect-entry-labels instrs)))
    (assert-equal 1 (hash-table-count ht))
    (assert-true (gethash "fn2" ht))))

(deftest extract-entry-labels-mixed
  "collect-entry-labels finds both closure and func-ref labels."
  (let* ((instrs (list (make-vm-closure :dst :r0 :label "fn-a"
                                        :params '(:r1) :captured nil)
                       (make-vm-func-ref :dst :r1 :label "fn-b")))
         (ht (cl-cc::collect-entry-labels instrs)))
    (assert-equal 2 (hash-table-count ht))
    (assert-true (gethash "fn-a" ht))
    (assert-true (gethash "fn-b" ht))))

(deftest extract-entry-labels-dedup
  "collect-entry-labels deduplicates same label from closure and func-ref."
  (let* ((instrs (list (make-vm-closure :dst :r0 :label "same"
                                        :params nil :captured nil)
                       (make-vm-func-ref :dst :r1 :label "same")))
         (ht (cl-cc::collect-entry-labels instrs)))
    (assert-equal 1 (hash-table-count ht))))

(deftest extract-entry-labels-ignores-register-function
  "collect-entry-labels ignores vm-register-function (not a label)."
  (let* ((instrs (list (cl-cc::make-vm-register-function :name 'foo :src :r0)))
         (ht (cl-cc::collect-entry-labels instrs)))
    (assert-equal 0 (hash-table-count ht))))

;;; ─── segment-instructions ─────────────────────────────────────────────────────

(deftest segment-empty-instructions
  "segment-instructions on empty list returns empty."
  (let ((entry-labels (make-hash-table :test #'equal)))
    (assert-null (cl-cc::segment-instructions nil entry-labels))))

(deftest segment-all-toplevel
  "Instructions with no function entries are all :toplevel."
  (let ((entry-labels (make-hash-table :test #'equal))
        (instrs (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-ret :reg :r0))))
    (let ((segs (cl-cc::segment-instructions instrs entry-labels)))
      (assert-equal 1 (length segs))
      (assert-eq :toplevel (car (first segs)))
      (assert-equal 2 (length (cdr (first segs)))))))

(deftest segment-one-function
  "A function body between entry-label and vm-ret produces a :function segment."
  (let ((entry-labels (make-hash-table :test #'equal)))
    (setf (gethash "fn1" entry-labels) t)
    (let* ((lbl (make-vm-label :name "fn1"))
           (body-inst (make-vm-const :dst :r0 :value 99))
           (ret (make-vm-ret :reg :r0))
           (instrs (list lbl body-inst ret)))
      (let ((segs (cl-cc::segment-instructions instrs entry-labels)))
        (assert-equal 1 (length segs))
        (let ((seg (first segs)))
          (assert-eq :function (first seg))
          (assert-equal "fn1" (second seg))
          ;; Body includes label + body-inst + ret
          (assert-equal 3 (length (third seg))))))))

(deftest segment-toplevel-then-function
  "Toplevel code before a function produces two segments."
  (let ((entry-labels (make-hash-table :test #'equal)))
    (setf (gethash "fn1" entry-labels) t)
    (let* ((top (make-vm-const :dst :r0 :value 0))
           (lbl (make-vm-label :name "fn1"))
           (ret (make-vm-ret :reg :r0))
           (instrs (list top lbl ret)))
      (let ((segs (cl-cc::segment-instructions instrs entry-labels)))
        (assert-equal 2 (length segs))
        (assert-eq :toplevel (car (first segs)))
        (assert-eq :function (car (second segs)))))))

(deftest segment-function-then-toplevel
  "A function body followed by toplevel code produces two segments."
  (let ((entry-labels (make-hash-table :test #'equal)))
    (setf (gethash "fn1" entry-labels) t)
    (let* ((lbl (make-vm-label :name "fn1"))
           (ret (make-vm-ret :reg :r0))
           (top (make-vm-const :dst :r0 :value 0))
           (instrs (list lbl ret top)))
      (let ((segs (cl-cc::segment-instructions instrs entry-labels)))
        (assert-equal 2 (length segs))
        (assert-eq :function (car (first segs)))
        (assert-eq :toplevel (car (second segs)))))))

(deftest segment-two-functions
  "Two consecutive function bodies produce two :function segments."
  (let ((entry-labels (make-hash-table :test #'equal)))
    (setf (gethash "fn-a" entry-labels) t)
    (setf (gethash "fn-b" entry-labels) t)
    (let* ((lbl-a (make-vm-label :name "fn-a"))
           (ret-a (make-vm-ret :reg :r0))
           (lbl-b (make-vm-label :name "fn-b"))
           (ret-b (make-vm-ret :reg :r1))
           (instrs (list lbl-a ret-a lbl-b ret-b)))
      (let ((segs (cl-cc::segment-instructions instrs entry-labels)))
        (assert-equal 2 (length segs))
        (assert-eq :function (car (first segs)))
        (assert-equal "fn-a" (second (first segs)))
        (assert-eq :function (car (second segs)))
        (assert-equal "fn-b" (second (second segs)))))))

(deftest segment-non-entry-label-stays-toplevel
  "A vm-label not in entry-labels stays in the :toplevel segment."
  (let ((entry-labels (make-hash-table :test #'equal)))
    ;; "loop" is NOT an entry label
    (let* ((lbl (make-vm-label :name "loop"))
           (inst (make-vm-const :dst :r0 :value 1))
           (instrs (list lbl inst)))
      (let ((segs (cl-cc::segment-instructions instrs entry-labels)))
        (assert-equal 1 (length segs))
        (assert-eq :toplevel (car (first segs)))))))

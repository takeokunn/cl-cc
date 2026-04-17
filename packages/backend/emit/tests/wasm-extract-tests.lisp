;;;; tests/unit/emit/wasm-extract-tests.lisp — WASM Function Extraction Tests
;;;;
;;;; Tests for src/emit/wasm-extract.lisp:
;;;; collect-entry-labels, segment-instructions

(in-package :cl-cc/test)

(defsuite wasm-extract-suite :description "WASM function extraction pass tests"
  :parent cl-cc-unit-suite)


(in-suite wasm-extract-suite)
(defun make-entry-labels (&rest labels)
  (let ((entry-labels (make-hash-table :test #'equal)))
    (dolist (label labels entry-labels)
      (setf (gethash label entry-labels) t))))

(defun make-function-instructions (label &rest body)
  (cons (make-vm-label :name label) body))

(defun assert-segment (segment kind &key label body-length)
  (assert-eq kind (car segment))
  (when label
    (assert-equal label (second segment)))
  (when body-length
    (assert-equal body-length (length (third segment)))))

;;; ─── collect-entry-labels ─────────────────────────────────────────────────────

(deftest extract-entry-labels-empty-cases
  "collect-entry-labels returns empty hash table for empty list or instructions with no closure/func-ref."
  (let ((ht-nil (cl-cc/emit::collect-entry-labels nil)))
    (assert-true (hash-table-p ht-nil))
    (assert-equal 0 (hash-table-count ht-nil)))
  (let* ((instrs (list (make-vm-const :dst :r0 :value 42)
                       (make-vm-ret :reg :r0)))
         (ht (cl-cc/emit::collect-entry-labels instrs)))
    (assert-equal 0 (hash-table-count ht))))

(deftest-each extract-entry-labels-single-entry
  "collect-entry-labels finds entry labels from vm-closure and vm-func-ref instructions."
  :cases (("closure"  (list (make-vm-closure :dst :r0 :label "fn1" :params '(:r1) :captured nil)
                            (make-vm-ret :reg :r0))
                      "fn1")
          ("func-ref" (list (make-vm-func-ref :dst :r0 :label "fn2"))
                      "fn2"))
  (instrs label)
  (let ((ht (cl-cc/emit::collect-entry-labels instrs)))
    (assert-equal 1 (hash-table-count ht))
    (assert-true (gethash label ht))))

(deftest extract-entry-labels-mixed
  "collect-entry-labels finds both closure and func-ref labels."
  (let* ((instrs (list (make-vm-closure :dst :r0 :label "fn-a"
                                        :params '(:r1) :captured nil)
                       (make-vm-func-ref :dst :r1 :label "fn-b")))
         (ht (cl-cc/emit::collect-entry-labels instrs)))
    (assert-equal 2 (hash-table-count ht))
    (assert-true (gethash "fn-a" ht))
    (assert-true (gethash "fn-b" ht))))

(deftest extract-entry-labels-dedup
  "collect-entry-labels deduplicates same label from closure and func-ref."
  (let* ((instrs (list (make-vm-closure :dst :r0 :label "same"
                                        :params nil :captured nil)
                       (make-vm-func-ref :dst :r1 :label "same")))
         (ht (cl-cc/emit::collect-entry-labels instrs)))
    (assert-equal 1 (hash-table-count ht))))

(deftest extract-entry-labels-ignores-register-function
  "collect-entry-labels ignores vm-register-function (not a label)."
  (let* ((instrs (list (cl-cc::make-vm-register-function :name 'foo :src :r0)))
         (ht (cl-cc/emit::collect-entry-labels instrs)))
    (assert-equal 0 (hash-table-count ht))))

;;; ─── segment-instructions ─────────────────────────────────────────────────────

(deftest segment-empty-instructions
  "segment-instructions on empty list returns empty."
  (let ((entry-labels (make-entry-labels)))
    (assert-null (cl-cc/emit::segment-instructions nil entry-labels))))

(deftest segment-all-toplevel
  "Instructions with no function entries are all :toplevel."
  (let ((entry-labels (make-entry-labels))
        (instrs (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-ret :reg :r0))))
    (let ((segs (cl-cc/emit::segment-instructions instrs entry-labels)))
      (assert-equal 1 (length segs))
      (assert-segment (first segs) :toplevel)
      (assert-equal 2 (length (cdr (first segs)))))))

(deftest segment-one-function
  "A function body between entry-label and vm-ret produces a :function segment."
  (let ((entry-labels (make-entry-labels "fn1")))
    (let* ((body-inst (make-vm-const :dst :r0 :value 99))
           (ret (make-vm-ret :reg :r0))
           (instrs (make-function-instructions "fn1" body-inst ret)))
      (let ((segs (cl-cc/emit::segment-instructions instrs entry-labels)))
        (assert-equal 1 (length segs))
        (assert-segment (first segs) :function :label "fn1" :body-length 3)))))

(deftest-each segment-mixed-ordering
  "Mixed toplevel + function code produces 2 segments; first-segment type depends on order."
  :cases (("toplevel-first" t   :toplevel :function)
          ("function-first" nil :function :toplevel))
  (toplevel-first expected-car1 expected-car2)
  (let ((entry-labels (make-entry-labels "fn1")))
    (let* ((top (make-vm-const :dst :r0 :value 0))
           (lbl (make-vm-label :name "fn1"))
           (ret (make-vm-ret :reg :r0))
           (instrs (if toplevel-first (list top lbl ret) (list lbl ret top))))
      (let ((segs (cl-cc/emit::segment-instructions instrs entry-labels)))
        (assert-equal 2 (length segs))
        (assert-segment (first segs) expected-car1)
        (assert-segment (second segs) expected-car2)))))

(deftest segment-two-functions
  "Two consecutive function bodies produce two :function segments."
  (let ((entry-labels (make-entry-labels "fn-a" "fn-b")))
    (let* ((ret-a (make-vm-ret :reg :r0))
           (ret-b (make-vm-ret :reg :r1))
           (instrs (append (make-function-instructions "fn-a" ret-a)
                           (make-function-instructions "fn-b" ret-b))))
      (let ((segs (cl-cc/emit::segment-instructions instrs entry-labels)))
        (assert-equal 2 (length segs))
        (assert-segment (first segs) :function :label "fn-a")
        (assert-segment (second segs) :function :label "fn-b")))))

(deftest segment-non-entry-label-stays-toplevel
  "A vm-label not in entry-labels stays in the :toplevel segment."
  (let ((entry-labels (make-entry-labels)))
    ;; "loop" is NOT an entry label
    (let* ((lbl (make-vm-label :name "loop"))
           (inst (make-vm-const :dst :r0 :value 1))
           (instrs (list lbl inst)))
      (let ((segs (cl-cc/emit::segment-instructions instrs entry-labels)))
        (assert-equal 1 (length segs))
        (assert-segment (first segs) :toplevel)))))

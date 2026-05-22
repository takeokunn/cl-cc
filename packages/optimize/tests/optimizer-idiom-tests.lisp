(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun idiom-zero-fill-loop ()
  (list (make-vm-const :dst :rzero :value 0)
        (make-vm-array-length :dst :rlen :src :rvec)
        (make-vm-const :dst :ri :value 0)
        (make-vm-label :name "Lmemset")
        (make-vm-lt :dst :rcond :lhs :ri :rhs :rlen)
        (make-vm-jump-zero :reg :rcond :label "Lmemset-exit")
        (make-vm-aset :array-reg :rvec :index-reg :ri :val-reg :rzero)
        (make-vm-const :dst :rone :value 1)
        (make-vm-add :dst :rnext :lhs :ri :rhs :rone)
        (make-vm-move :dst :ri :src :rnext)
        (make-vm-jump :label "Lmemset")
        (make-vm-label :name "Lmemset-exit")))

(defun idiom-copy-loop (&key strided-p)
  (list (make-vm-const :dst :rsize :value 4)
        (make-vm-make-array :dst :rdst :size-reg :rsize)
        (make-vm-make-array :dst :rsrc :size-reg :rsize)
        (make-vm-array-length :dst :rlen :src :rsrc)
        (make-vm-const :dst :ri :value 0)
        (make-vm-label :name "Lmemcpy")
        (make-vm-lt :dst :rcond :lhs :ri :rhs :rlen)
        (make-vm-jump-zero :reg :rcond :label "Lmemcpy-exit")
        (make-vm-aref :dst :rtmp :array-reg :rsrc :index-reg :ri)
        (make-vm-aset :array-reg :rdst :index-reg :ri :val-reg :rtmp)
        (make-vm-const :dst :rone :value (if strided-p 2 1))
        (make-vm-add :dst :rnext :lhs :ri :rhs :rone)
        (make-vm-move :dst :ri :src :rnext)
        (make-vm-jump :label "Lmemcpy")
        (make-vm-label :name "Lmemcpy-exit")))

(defun idiom-popcount-loop ()
  (list (make-vm-const :dst :rcount :value 0)
        (make-vm-label :name "Lpop")
        (make-vm-jump-zero :reg :rx :label "Lpop-exit")
        (make-vm-const :dst :rone :value 1)
        (make-vm-sub :dst :rdec :lhs :rx :rhs :rone)
        (make-vm-logand :dst :rnext :lhs :rx :rhs :rdec)
        (make-vm-add :dst :rcount-next :lhs :rcount :rhs :rone)
        (make-vm-move :dst :rx :src :rnext)
        (make-vm-jump :label "Lpop")
        (make-vm-label :name "Lpop-exit")))

(defun idiom-strlen-loop ()
  (list (make-vm-const :dst :ri :value 0)
        (make-vm-label :name "Lstrlen")
        (make-vm-char :dst :rch :string :rstr :index :ri)
        (make-vm-const :dst :rnul :value #\Nul)
        (make-vm-char= :dst :rcond :char1 :rch :char2 :rnul)
        (make-vm-jump-zero :reg :rcond :label "Lstrlen-body")
        (make-vm-jump :label "Lstrlen-exit")
        (make-vm-label :name "Lstrlen-body")
        (make-vm-const :dst :rone :value 1)
        (make-vm-add :dst :rnext :lhs :ri :rhs :rone)
        (make-vm-move :dst :ri :src :rnext)
        (make-vm-jump :label "Lstrlen")
        (make-vm-label :name "Lstrlen-exit")))

(deftest fr-684-memset-recognition-collapses-zero-fill-loop
  "FR-684: canonical dotimes zero fill is recognized as a memset/fill idiom."
  (let ((result (cl-cc/optimize:opt-pass-idiom-recognition (idiom-zero-fill-loop))))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-fill)) result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result))))

(deftest fr-684-memcpy-recognition-collapses-unit-stride-copy-loop
  "FR-684: canonical unit-stride aref/aset copy is recognized as memcpy."
  (let ((result (cl-cc/optimize:opt-pass-idiom-recognition (idiom-copy-loop))))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-copy-vector)) result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result))))

(deftest fr-684-memcpy-recognition-skips-strided-copy-loop
  "FR-684: strided copy loops are not rewritten as memcpy."
  (let ((result (cl-cc/optimize:opt-pass-idiom-recognition (idiom-copy-loop :strided-p t))))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-copy-vector)) result))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result))))

(deftest fr-684-popcount-recognition-emits-logcount
  "FR-684: Kernighan bit-counting loop becomes vm-logcount for POPCNT/CNT lowering."
  (let ((result (cl-cc/optimize:opt-pass-idiom-recognition (idiom-popcount-loop))))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-logcount)) result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-logand)) result))))

(deftest fr-684-strlen-recognition-emits-string-length
  "FR-684: loop counting until #\\Nul becomes vm-string-length."
  (let ((result (cl-cc/optimize:opt-pass-idiom-recognition (idiom-strlen-loop))))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-string-length)) result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-char)) result))))

(in-package :cl-cc/codegen)

;;;; FR-174: Native-level post-emit peephole optimization.
;;;;
;;;; This pass intentionally runs after byte emission.  x86-64 is decoded into
;;;; instruction records first so transformations never split variable-length
;;;; instructions.  Length-changing rewrites also remap relative branch
;;;; displacements so existing label targets remain valid.

(defstruct native-peephole-inst
  start
  end
  bytes
  kind
  dst
  src
  branch-size
  target)

(defun native-rewrite-inst (inst &key (bytes nil bytes-p)
                                      (kind nil kind-p)
                                      (dst nil dst-p)
                                      (src nil src-p)
                                      (branch-size nil branch-size-p)
                                      (target nil target-p))
  "Return a copy of INST with supplied slots replaced."
  (let ((copy (copy-native-peephole-inst inst)))
    (when bytes-p (setf (native-peephole-inst-bytes copy) bytes))
    (when kind-p (setf (native-peephole-inst-kind copy) kind))
    (when dst-p (setf (native-peephole-inst-dst copy) dst))
    (when src-p (setf (native-peephole-inst-src copy) src))
    (when branch-size-p (setf (native-peephole-inst-branch-size copy) branch-size))
    (when target-p (setf (native-peephole-inst-target copy) target))
    copy))

(defun peephole-copy-byte-vector (bytes &key (start 0) end)
  "Return a fresh unsigned-byte vector copied from BYTES."
  (let* ((limit (or end (length bytes)))
         (out (make-array (- limit start) :element-type '(unsigned-byte 8))))
    (loop for i from start below limit
          for j from 0
          do (setf (aref out j) (aref bytes i)))
    out))

(defun peephole-byte-list-vector (bytes)
  "Return an unsigned-byte vector containing BYTES."
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun x86-rex-byte-p (byte)
  (<= #x40 byte #x4F))

(defun x86-legacy-prefix-byte-p (byte)
  (member byte '(#x2E #x36 #x3E #x26 #x64 #x65 #x66 #x67 #xF0 #xF2 #xF3)
          :test #'=))

(defun x86-modrm-mod (modrm)
  (ldb (byte 2 6) modrm))

(defun x86-modrm-reg (modrm)
  (ldb (byte 3 3) modrm))

(defun x86-modrm-rm (modrm)
  (ldb (byte 3 0) modrm))

(defun x86-rex-bit (rex bit)
  (if rex (ldb (byte 1 bit) rex) 0))

(defun x86-modrm-reg-full (modrm rex)
  (+ (x86-modrm-reg modrm) (ash (x86-rex-bit rex 2) 3)))

(defun x86-modrm-rm-full (modrm rex)
  (+ (x86-modrm-rm modrm) (ash (x86-rex-bit rex 0) 3)))

(defun x86-read-s32 (bytes pos)
  (let ((value (logior (aref bytes pos)
                       (ash (aref bytes (+ pos 1)) 8)
                       (ash (aref bytes (+ pos 2)) 16)
                       (ash (aref bytes (+ pos 3)) 24))))
    (if (logbitp 31 value) (- value #x100000000) value)))

(defun x86-read-s8 (bytes pos)
  (let ((value (aref bytes pos)))
    (if (logbitp 7 value) (- value #x100) value)))

(defun x86-write-s32 (bytes pos value)
  (let ((u (logand value #xFFFFFFFF)))
    (setf (aref bytes pos) (logand u #xFF)
          (aref bytes (+ pos 1)) (logand (ash u -8) #xFF)
          (aref bytes (+ pos 2)) (logand (ash u -16) #xFF)
          (aref bytes (+ pos 3)) (logand (ash u -24) #xFF))))

(defun x86-write-s8 (bytes pos value)
  (setf (aref bytes pos) (logand value #xFF)))

(defun x86-modrm-instruction-end (bytes pos modrm-pos)
  "Return end offset for an instruction whose ModR/M byte starts at MODRM-POS."
  (let* ((modrm (aref bytes modrm-pos))
         (mod (x86-modrm-mod modrm))
         (rm (x86-modrm-rm modrm))
         (cursor (1+ modrm-pos)))
    (when (and (/= mod 3) (= rm 4))
      (incf cursor))
    (incf cursor (case mod
                   (0 (if (= rm 5) 4 0))
                   (1 1)
                   (2 4)
                   (otherwise 0)))
    (max cursor (1+ pos))))

(defun x86-decode-one (bytes pos)
  "Decode one emitted x86-64 instruction at POS into a native-peephole-inst."
  (let* ((len (length bytes))
         (cursor pos)
         (rex nil))
    (loop while (and (< cursor len)
                     (x86-legacy-prefix-byte-p (aref bytes cursor)))
          do (incf cursor))
    (when (and (< cursor len) (x86-rex-byte-p (aref bytes cursor)))
      (setf rex (aref bytes cursor))
      (incf cursor))
    (when (>= cursor len)
      (return-from x86-decode-one
        (make-native-peephole-inst
         :start pos :end len :bytes (peephole-copy-byte-vector bytes :start pos :end len))))
    (let* ((opcode (aref bytes cursor))
           (op-pos cursor))
      (labels ((finish (end &key kind dst src branch-size target replacement)
                 (make-native-peephole-inst
                  :start pos
                  :end (min end len)
                  :bytes (or replacement
                             (peephole-copy-byte-vector bytes :start pos :end (min end len)))
                  :kind kind
                  :dst dst
                  :src src
                  :branch-size branch-size
                  :target target))
               (modrm-finish (modrm-pos &key kind dst src imm-bytes)
                 (finish (+ (x86-modrm-instruction-end bytes pos modrm-pos) (or imm-bytes 0))
                         :kind kind :dst dst :src src)))
        (cond
          ;; VEX-encoded instructions currently emitted as fixed 5-byte forms.
          ((= opcode #xC4)
           (finish (+ op-pos 5)))
          ((and (= opcode #x0F) (< (1+ cursor) len))
           (let ((op2 (aref bytes (1+ cursor))))
             (cond
               ((member op2 '(#x80 #x81 #x84 #x85) :test #'=)
                (let* ((end (+ cursor 6))
                       (target (+ end (x86-read-s32 bytes (+ cursor 2)))))
                  (finish end :kind :branch :branch-size 4 :target target)))
               ((and (= op2 #x1E) (<= (+ cursor 4) len))
                (finish (+ cursor 4)))
               ((= op2 #x0B)
                (finish (+ cursor 2)))
               ((member op2 '(#xAF #xB8 #xBD #xAE #x10 #x58 #x5C #x59 #x5E #x51)
                        :test #'=)
                (modrm-finish (+ cursor 2)))
               (t
                (finish (min len (+ cursor 2)))))))
          ((member opcode '(#x89 #x8B #x31 #x33 #x01 #x29 #x39 #x85) :test #'=)
           (let* ((modrm-pos (1+ cursor))
                  (modrm (and (< modrm-pos len) (aref bytes modrm-pos))))
             (if (and modrm (= (x86-modrm-mod modrm) 3))
                 (let* ((reg (x86-modrm-reg-full modrm rex))
                        (rm (x86-modrm-rm-full modrm rex)))
                   (case opcode
                     (#x89 (modrm-finish modrm-pos :kind :mov-rr :dst rm :src reg))
                     (#x8B (modrm-finish modrm-pos :kind :mov-rr :dst reg :src rm))
                     (#x31 (modrm-finish modrm-pos :kind :xor-rr :dst rm :src reg))
                     (#x33 (modrm-finish modrm-pos :kind :xor-rr :dst reg :src rm))
                     (#x39 (modrm-finish modrm-pos :kind :cmp-rr :dst rm :src reg))
                     (#x85 (modrm-finish modrm-pos :kind :test-rr :dst rm :src reg))
                     (otherwise (modrm-finish modrm-pos))))
                 (modrm-finish modrm-pos))))
          ((= opcode #x8D)
           (modrm-finish (1+ cursor)))
          ((= opcode #x81)
           (let* ((modrm-pos (1+ cursor))
                  (modrm (and (< modrm-pos len) (aref bytes modrm-pos)))
                  (rm (and modrm (x86-modrm-rm-full modrm rex)))
                  (reg (and modrm (x86-modrm-reg modrm)))
                  (imm-pos (and modrm (x86-modrm-instruction-end bytes pos modrm-pos))))
             (finish (+ (or imm-pos (+ cursor 2)) 4)
                     :kind (and (= reg 7) (= (x86-read-s32 bytes imm-pos) 0) :cmp-ri0)
                     :dst rm)))
          ((= opcode #x83)
           (let* ((modrm-pos (1+ cursor))
                  (imm-pos (x86-modrm-instruction-end bytes pos modrm-pos)))
             (finish (1+ imm-pos))))
          ((member opcode '(#xF7 #xFF) :test #'=)
           (modrm-finish (1+ cursor)))
          ((<= #xB8 opcode #xBF)
           (let ((reg (+ (- opcode #xB8) (ash (x86-rex-bit rex 0) 3))))
             (finish (+ cursor 9)
                     :kind (and rex (= (x86-rex-bit rex 3) 1)
                                (zerop (loop for i from (+ cursor 1) below (+ cursor 9)
                                             sum (aref bytes i)))
                                :mov-ri0)
                     :dst reg)))
          ((<= #x50 opcode #x57)
           (finish (1+ cursor) :kind :push :dst (+ (- opcode #x50) (ash (x86-rex-bit rex 0) 3))))
          ((<= #x58 opcode #x5F)
           (finish (1+ cursor) :kind :pop :dst (+ (- opcode #x58) (ash (x86-rex-bit rex 0) 3))))
          ((member opcode '(#xE8 #xE9) :test #'=)
           (let* ((end (+ cursor 5))
                  (target (+ end (x86-read-s32 bytes (1+ cursor)))))
             (finish end :kind :branch :branch-size 4 :target target)))
          ((member opcode '(#xEB #x74 #x75 #x79 #x7D) :test #'=)
           (let* ((end (+ cursor 2))
                  (target (+ end (x86-read-s8 bytes (1+ cursor)))))
             (finish end :kind :branch :branch-size 1 :target target)))
          ((member opcode '(#xC3 #x90) :test #'=)
           (finish (1+ cursor) :kind (and (= opcode #x90) :nop)))
          (t
           (finish (1+ cursor))))))))

(defun x86-decode-instructions (bytes)
  "Decode BYTES into x86-64 instruction records."
  (let ((result '())
        (pos 0)
        (len (length bytes)))
    (loop while (< pos len)
          for inst = (x86-decode-one bytes pos)
          do (progn
               (push inst result)
               (setf pos (max (1+ pos) (native-peephole-inst-end inst)))))
    (nreverse result)))

(defun x86-encode-xor-rr64 (reg)
  "Return XOR REG, REG as a short x86-64 byte vector."
  (peephole-byte-list-vector
   (list (rex-prefix :w 1 :r (ash reg -3) :b (ash reg -3))
         #x31
         (modrm 3 reg reg))))

(defun x86-encode-test-rr64 (reg)
  "Return TEST REG, REG as a short x86-64 byte vector."
  (peephole-byte-list-vector
   (list (rex-prefix :w 1 :r (ash reg -3) :b (ash reg -3))
         #x85
         (modrm 3 reg reg))))

(defun native-safe-identical-dedup-p (inst)
  "Return true when adjacent duplicate INST is semantically idempotent."
  (member (native-peephole-inst-kind inst) '(:mov-rr :cmp-rr :test-rr :nop)
          :test #'eq))

(defun x86-64-peephole-rewrite (insts)
  "Return optimized x86-64 instruction records with local peephole rewrites."
  (let ((out '())
        (rest insts))
    (loop while rest
          do (let* ((a (first rest))
                    (b (second rest)))
               (cond
                 ((and (eq (native-peephole-inst-kind a) :mov-rr)
                       (= (native-peephole-inst-dst a) (native-peephole-inst-src a)))
                  (setf rest (rest rest)))
                 ((and b
                       (eq (native-peephole-inst-kind a) :push)
                       (eq (native-peephole-inst-kind b) :pop)
                       (= (native-peephole-inst-dst a) (native-peephole-inst-dst b)))
                  (setf rest (cddr rest)))
                 ((and b
                       (eq (native-peephole-inst-kind a) :mov-rr)
                       (eq (native-peephole-inst-kind b) :mov-rr)
                       (= (native-peephole-inst-dst a) (native-peephole-inst-src b))
                       (= (native-peephole-inst-src a) (native-peephole-inst-dst b)))
                  (push a out)
                  (setf rest (cddr rest)))
                 ((eq (native-peephole-inst-kind a) :mov-ri0)
                  (push (native-rewrite-inst a
                                             :bytes (x86-encode-xor-rr64
                                                     (native-peephole-inst-dst a))
                                             :kind :xor-rr
                                             :src (native-peephole-inst-dst a))
                        out)
                  (setf rest (rest rest)))
                 ((and b
                       (eq (native-peephole-inst-kind a) :cmp-ri0)
                       (eq (native-peephole-inst-kind b) :branch))
                  (push (native-rewrite-inst a
                                             :bytes (x86-encode-test-rr64
                                                     (native-peephole-inst-dst a))
                                             :kind :test-rr
                                             :src (native-peephole-inst-dst a))
                        out)
                  (setf rest (rest rest)))
                 ((and b
                       (native-safe-identical-dedup-p a)
                       (= (length (native-peephole-inst-bytes a))
                          (length (native-peephole-inst-bytes b)))
                       (equalp (native-peephole-inst-bytes a)
                               (native-peephole-inst-bytes b)))
                  (push a out)
                  (setf rest (cddr rest)))
                 (t
                  (push a out)
                  (setf rest (rest rest))))))
    (nreverse out)))

(defun x86-patch-branch-displacement (inst offset-map new-start new-end)
  "Patch branch displacement in INST for its remapped target."
  (declare (ignore new-start))
  (let* ((target (native-peephole-inst-target inst))
         (branch-size (native-peephole-inst-branch-size inst))
         (bytes (peephole-copy-byte-vector (native-peephole-inst-bytes inst)))
         (mapped-target (and target (<= 0 target (1- (length offset-map)))
                             (aref offset-map target))))
    (when (and mapped-target branch-size)
      (let ((disp (- mapped-target new-end)))
        (if (= branch-size 4)
            (x86-write-s32 bytes (- (length bytes) 4) disp)
            (when (<= -128 disp 127)
              (x86-write-s8 bytes (- (length bytes) 1) disp)))))
    bytes))

(defun native-build-offset-map (original-length insts)
  "Map every original byte position to its new byte position after INSTS."
  (let ((map (make-array (1+ original-length) :initial-element 0))
        (new-pos 0)
        (old-pos 0))
    (dolist (inst insts)
      (loop while (< old-pos (native-peephole-inst-start inst))
            do (progn (setf (aref map old-pos) new-pos)
                      (incf old-pos)))
      (loop while (<= old-pos (native-peephole-inst-start inst))
            do (progn (setf (aref map old-pos) new-pos)
                      (incf old-pos)))
      (incf new-pos (length (native-peephole-inst-bytes inst))))
    (loop while (<= old-pos original-length)
          do (progn (setf (aref map old-pos) new-pos)
                    (incf old-pos)))
    map))

(defun native-emit-optimized-insts (insts original-length &key branch-patcher)
  "Emit optimized INSTS to a byte vector, patching relative branches when needed."
  (let* ((offset-map (native-build-offset-map original-length insts))
         (out '())
         (new-pos 0))
    (dolist (inst insts)
      (let* ((bytes (if (and branch-patcher (native-peephole-inst-branch-size inst))
                        (funcall branch-patcher inst offset-map new-pos
                                 (+ new-pos (length (native-peephole-inst-bytes inst))))
                        (native-peephole-inst-bytes inst))))
        (loop for i from 0 below (length bytes)
              do (push (aref bytes i) out))
        (incf new-pos (length bytes))))
    (peephole-byte-list-vector (nreverse out))))

(defun x86-64-peephole-optimize (bytes)
  "Optimize emitted x86-64 machine-code BYTES and return a fresh byte vector."
  (let* ((decoded (x86-decode-instructions bytes))
         (optimized (x86-64-peephole-rewrite decoded)))
    (native-emit-optimized-insts optimized (length bytes)
                                :branch-patcher #'x86-patch-branch-displacement)))

;;; AArch64 post-emit peephole.  Fixed-width instructions make boundary handling
;;; simple, but branch displacement remapping is still required after removals.

(defun a64-read-u32 (bytes pos)
  (logior (aref bytes pos)
          (ash (aref bytes (+ pos 1)) 8)
          (ash (aref bytes (+ pos 2)) 16)
          (ash (aref bytes (+ pos 3)) 24)))

(defun a64-write-u32 (bytes pos word)
  (setf (aref bytes pos) (logand word #xFF)
        (aref bytes (+ pos 1)) (logand (ash word -8) #xFF)
        (aref bytes (+ pos 2)) (logand (ash word -16) #xFF)
        (aref bytes (+ pos 3)) (logand (ash word -24) #xFF)))

(defun a64-sign-extend (value bits)
  (let ((sign (ash 1 (1- bits))))
    (if (logtest value sign)
        (- value (ash 1 bits))
        value)))

(defun a64-mov-rr-word-p (word)
  (= (logand word #xFFE0FFE0) #xAA0003E0))

(defun a64-mov-rd (word) (ldb (byte 5 0) word))
(defun a64-mov-rn (word) (ldb (byte 5 16) word))

(defun a64-cmp-zr-word-p (word)
  (and (= (logand word #xFFE0FC1F) #xEB00001F)
       (= (ldb (byte 5 16) word) 31)))

(defun a64-bcond-word-p (word)
  (= (logand word #xFF000010) #x54000000))

(defun a64-cbz-word-p (word)
  (= (logand word #xFF000000) #xB4000000))

(defun a64-branch-target (word end-pos)
  (cond
    ((or (a64-bcond-word-p word) (a64-cbz-word-p word))
     (+ (- end-pos 4)
        (* 4 (a64-sign-extend (ldb (byte 19 5) word) 19))))
    ((= (logand word #xFC000000) #x14000000)
     (+ (- end-pos 4)
        (* 4 (a64-sign-extend (ldb (byte 26 0) word) 26))))
    (t nil)))

(defun a64-decode-instructions (bytes)
  "Decode fixed-width AArch64 BYTES into instruction records."
  (let ((result '()))
    (loop for pos from 0 below (length bytes) by 4
          for end = (min (+ pos 4) (length bytes))
          for word = (and (= (- end pos) 4) (a64-read-u32 bytes pos))
          do (push (make-native-peephole-inst
                    :start pos
                    :end end
                    :bytes (peephole-copy-byte-vector bytes :start pos :end end)
                    :kind (cond
                            ((and word (a64-mov-rr-word-p word)) :mov-rr)
                            ((and word (a64-cmp-zr-word-p word)) :cmp-zero)
                            ((and word (or (a64-bcond-word-p word)
                                           (a64-cbz-word-p word)
                                           (= (logand word #xFC000000) #x14000000)))
                             :branch)
                            (t nil))
                    :dst (and word (a64-mov-rr-word-p word) (a64-mov-rd word))
                    :src (and word (a64-mov-rr-word-p word) (a64-mov-rn word))
                    :branch-size (and word (a64-branch-target word end) 4)
                    :target (and word (a64-branch-target word end)))
                   result))
    (nreverse result)))

(defun a64-encode-cbz-word (rn target-pos inst-pos)
  (let ((imm19 (logand (ash (- target-pos inst-pos) -2) #x7FFFF)))
    (logior #xB4000000
            (ash imm19 5)
            (logand rn #x1F))))

(defun a64-peephole-rewrite (insts)
  "Return optimized AArch64 instruction records."
  (let ((out '())
        (rest insts))
    (loop while rest
          do (let* ((a (first rest))
                    (b (second rest)))
               (cond
                 ((and (eq (native-peephole-inst-kind a) :mov-rr)
                       (= (native-peephole-inst-dst a) (native-peephole-inst-src a)))
                  (setf rest (rest rest)))
                 ((and b
                       (eq (native-peephole-inst-kind a) :mov-rr)
                       (eq (native-peephole-inst-kind b) :mov-rr)
                       (= (native-peephole-inst-dst a) (native-peephole-inst-src b))
                       (= (native-peephole-inst-src a) (native-peephole-inst-dst b)))
                  (push a out)
                  (setf rest (cddr rest)))
                 ((and b
                       (eq (native-peephole-inst-kind a) :cmp-zero)
                       (eq (native-peephole-inst-kind b) :branch)
                       (a64-bcond-word-p (a64-read-u32 (native-peephole-inst-bytes b) 0))
                       (= (ldb (byte 4 0) (a64-read-u32 (native-peephole-inst-bytes b) 0)) 0))
                  (let* ((word (a64-read-u32 (native-peephole-inst-bytes a) 0))
                         (rn (ldb (byte 5 5) word))
                         (placeholder (peephole-byte-list-vector '(0 0 0 0))))
                    (push (native-rewrite-inst a
                                               :bytes placeholder
                                               :kind :branch
                                               :dst rn
                                               :branch-size 4
                                               :target (native-peephole-inst-target b))
                          out)
                    (setf rest (cddr rest))))
                 ((and b
                       (native-safe-identical-dedup-p a)
                       (equalp (native-peephole-inst-bytes a)
                               (native-peephole-inst-bytes b)))
                  (push a out)
                  (setf rest (cddr rest)))
                 (t
                  (push a out)
                  (setf rest (rest rest))))))
    (nreverse out)))

(defun a64-patch-branch-displacement (inst offset-map new-start new-end)
  "Patch AArch64 branch displacement in INST for its remapped target."
  (declare (ignore new-end))
  (let* ((bytes (peephole-copy-byte-vector (native-peephole-inst-bytes inst)))
         (word (a64-read-u32 bytes 0))
         (target (native-peephole-inst-target inst))
         (mapped-target (and target (<= 0 target (1- (length offset-map)))
                             (aref offset-map target))))
    (when mapped-target
      (cond
        ((and (eq (native-peephole-inst-kind inst) :branch)
              (native-peephole-inst-dst inst)
              (zerop word))
         (a64-write-u32 bytes 0
                        (a64-encode-cbz-word (native-peephole-inst-dst inst)
                                             mapped-target new-start)))
        ((or (a64-bcond-word-p word) (a64-cbz-word-p word))
         (let ((imm19 (logand (ash (- mapped-target new-start) -2) #x7FFFF)))
           (setf word (logior (logand word (lognot (ash #x7FFFF 5)))
                              (ash imm19 5)))
           (a64-write-u32 bytes 0 word)))
        ((= (logand word #xFC000000) #x14000000)
         (let ((imm26 (logand (ash (- mapped-target new-start) -2) #x3FFFFFF)))
           (setf word (logior (logand word #xFC000000) imm26))
           (a64-write-u32 bytes 0 word)))))
    bytes))

(defun aarch64-peephole-optimize (bytes)
  "Optimize emitted AArch64 machine-code BYTES and return a fresh byte vector."
  (let* ((decoded (a64-decode-instructions bytes))
         (optimized (a64-peephole-rewrite decoded)))
    (native-emit-optimized-insts optimized (length bytes)
                                :branch-patcher #'a64-patch-branch-displacement)))

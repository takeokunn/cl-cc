;;; ─── Phase 138 String Builder / Rope ──────────────────────────────────
;;; FR-787: String Builder with O(1) amortized append
;;; FR-788: Rope data structure with O(log n) concat/split

(in-package :cl-cc/vm)

;; ── FR-787 String Builder ─────────────────────────────────────────────

(defstruct (string-builder (:constructor %make-string-builder)
                           (:conc-name sb-))
  "Variable-capacity character buffer for efficient string construction.
Avoids O(n^2) repeated copying that happens with CONCATENATE."
  (buffer (make-array 64 :element-type 'character :adjustable t :fill-pointer 0)
          :type (vector character))
  (capacity 64 :type fixnum))

(defconstant +rope-inline-threshold+ 1024
  "Strings shorter than this many characters remain as flat strings.")

(defun make-string-builder (&key (capacity 64))
  "Create a fresh string builder with CAPACITY (default 64)."
  (check-type capacity (integer 0 *))
  (setf capacity (max 1 capacity))
  (let ((buf (make-array capacity :element-type 'character
                         :adjustable t :fill-pointer 0)))
    (%make-string-builder :buffer buf :capacity capacity)))

(defun string-builder-append! (sb value)
  "Append VALUE (string, character, number, or symbol) to the string builder SB.
Value is converted via PRINC-TO-STRING. Returns SB for chaining."
  (etypecase value
    (string (string-builder-append-string! sb value))
    (character (vector-push-extend value (sb-buffer sb) (sb-capacity sb)))
    (number (string-builder-append! sb (princ-to-string value)))
    (symbol (string-builder-append! sb (string-downcase (symbol-name value)))))
  sb)

(defun string-builder-length (sb)
  "Return the number of characters currently accumulated in SB."
  (fill-pointer (sb-buffer sb)))

(defun string-builder-ensure-capacity! (sb capacity)
  "Ensure SB can hold at least CAPACITY characters without another growth step.
Returns SB.  Existing contents are preserved."
  (check-type capacity (integer 0 *))
  (let ((current (sb-capacity sb)))
    (when (> capacity current)
      (let ((new-capacity current))
        (loop while (< new-capacity capacity)
              do (setf new-capacity (max 1 (* 2 new-capacity))))
        (setf (sb-buffer sb) (adjust-array (sb-buffer sb) new-capacity
                                           :element-type 'character
                                           :fill-pointer (fill-pointer (sb-buffer sb)))
              (sb-capacity sb) new-capacity))))
  sb)

(defun string-builder-append-string! (sb string)
  "Append STRING to SB with one bulk copy. Returns SB for chaining."
  (check-type string string)
  (let* ((buffer (sb-buffer sb))
         (old-length (fill-pointer buffer))
         (new-length (+ old-length (length string))))
    (string-builder-ensure-capacity! sb new-length)
    (let ((buffer (sb-buffer sb)))
      (setf (fill-pointer buffer) new-length)
      (replace buffer string :start1 old-length :end1 new-length)))
  sb)

(defun string-builder-clear! (sb)
  "Reset SB to empty while retaining its allocated buffer for reuse. Returns SB."
  (setf (fill-pointer (sb-buffer sb)) 0)
  sb)

(defun string-builder-finish (sb)
  "Return the final string from SB. Performs exactly ONE copy (O(n))."
  (copy-seq (sb-buffer sb)))

(defmacro with-string-builder ((var &optional (initial-capacity 64)) &body body)
  "Execute BODY with VAR bound to a fresh string builder.
Returns the finished string."
  (let ((sb-var (gensym "SB")))
    `(let* ((,sb-var (make-string-builder :capacity ,initial-capacity))
            (,var ,sb-var))
       ,@body
       (string-builder-finish ,sb-var))))

;; ── FR-788 Rope Data Structure ────────────────────────────────────────

(defstruct (rope-node (:constructor %make-rope-node))
  "Internal node of the rope binary tree."
  (weight 0 :type fixnum)
  (left nil :type (or null rope-node string))
  (right nil :type (or null rope-node string)))

(defstruct rope
  "Rope: a binary tree of strings for O(log n) concat/split and
O(k) substring extraction. Short strings (< 1024 chars) stored inline."
  (root nil :type (or null rope-node string))
  (length 0 :type fixnum))

(defun rope (string)
  "Create a new rope from STRING."
  (check-type string string)
  (make-rope :root string :length (length string)))

(defun %rope-root-length (root)
  (etypecase root
    (null 0)
    (string (length root))
    (rope-node (+ (rope-node-weight root)
                  (%rope-root-length (rope-node-right root))))))

(defun %rope-flatten-root (root)
  (let ((builder (make-string-builder :capacity (%rope-root-length root))))
    (labels ((%flatten (node)
               (etypecase node
                 (null nil)
                 (string (string-builder-append-string! builder node))
                 (rope-node
                  (%flatten (rope-node-left node))
                  (%flatten (rope-node-right node))))))
      (%flatten root)
      (string-builder-finish builder))))

(defun %make-rope-from-root (root)
  "Create a rope, flattening sub-threshold roots to honor the hybrid invariant."
  (let ((length (%rope-root-length root)))
    (make-rope :root (if (and root (< length +rope-inline-threshold+))
                         (%rope-flatten-root root)
                         root)
               :length length)))

(defun %rope-coerce (x)
  "Coerce X to a rope if it is a plain string."
  (if (stringp x) (rope x) x))

(defun rope-concat (r1 r2)
  "Concatenate two ropes (or plain strings).
Short results (< +ROPE-INLINE-THRESHOLD+) remain flat strings for Hyrum's law.
Long results are O(1): create one new concat root without copying children.
R1 and R2 are left unchanged (persistent)."
  (let* ((r1 (%rope-coerce r1))
         (r2 (%rope-coerce r2))
         (len1 (rope-length r1))
         (len2 (rope-length r2))
         (total (+ len1 len2)))
    (cond
      ((zerop len1) r2)
      ((zerop len2) r1)
      ((< total +rope-inline-threshold+)
       (rope (concatenate 'string (rope-to-string r1) (rope-to-string r2))))
      (t
       (make-rope :root (%make-rope-node :weight len1
                                         :left (rope-root r1)
                                         :right (rope-root r2))
                  :length total)))))

(defun rope-split (r position)
  "Split rope R at POSITION. O(log n).
Returns (values left-rope right-rope)."
  (let ((position (max 0 (min position (rope-length r)))))
    (labels ((%split (node pos)
              ;; Returns (values left-node right-node); nodes are null/string/rope-node only
              (etypecase node
               (null (values nil nil))
               (string
                (let ((len (length node)))
                  (cond ((<= pos 0) (values nil node))
                        ((>= pos len) (values node nil))
                        (t (values (subseq node 0 pos) (subseq node pos))))))
               (rope-node
                (let ((left-weight (rope-node-weight node)))
                  (if (<= pos left-weight)
                      (multiple-value-bind (ll lr)
                          (%split (rope-node-left node) pos)
                        (values ll (if lr
                                       (%make-rope-node
                                        :weight (rope-length* lr)
                                        :left lr
                                        :right (rope-node-right node))
                                       (rope-node-right node))))
                      (multiple-value-bind (rl rr)
                          (%split (rope-node-right node) (- pos left-weight))
                        (values (if rl
                                    (%make-rope-node
                                     :weight left-weight
                                     :left (rope-node-left node)
                                     :right rl)
                                    (rope-node-left node))
                                rr))))))))
      (multiple-value-bind (left right)
          (%split (rope-root r) position)
        (values (%make-rope-from-root left)
                (%make-rope-from-root right))))))

(defun rope-length* (node)
  "Compute the total string length of a rope NODE tree."
  (etypecase node
    (null 0)
    (string (length node))
    (rope-node (+ (rope-length* (rope-node-left node))
                  (rope-length* (rope-node-right node))))))

(defun rope-to-string (r)
  "Flatten rope R to a plain string. O(n). Left-to-right traversal."
  (let ((result (make-array (rope-length r) :element-type 'character
                            :fill-pointer 0)))
    (labels ((%flatten (node)
               (etypecase node
                 (null)
                 (string (loop for c across node do (vector-push c result)))
                 (rope-node (%flatten (rope-node-left node))
                            (%flatten (rope-node-right node))))))
      (%flatten (rope-root r))
      result)))

(defun rope-insert (r i string)
  "Return a new rope with STRING inserted into R at character index I."
  (check-type string string)
  (let* ((r (%rope-coerce r))
         (index (max 0 (min i (rope-length r)))))
    (multiple-value-bind (left right) (rope-split r index)
      (rope-concat (rope-concat left string) right))))

(defun rope-delete (r start end)
  "Return a new rope with the half-open range [START, END) removed."
  (let* ((r (%rope-coerce r))
         (length (rope-length r))
         (from (max 0 (min start length)))
         (to (max from (min end length))))
    (multiple-value-bind (left rest) (rope-split r from)
      (multiple-value-bind (_deleted right) (rope-split rest (- to from))
        (declare (ignore _deleted))
        (rope-concat left right)))))

(defun rope-substring (r start end)
  "Extract the half-open range [START, END) from R as a new rope."
  (let* ((r (%rope-coerce r))
         (length (rope-length r))
         (from (max 0 (min start length)))
         (to (max from (min end length))))
    (multiple-value-bind (_left rest) (rope-split r from)
      (declare (ignore _left))
      (multiple-value-bind (middle _right) (rope-split rest (- to from))
        (declare (ignore _right))
        middle))))

(defun test-string-builder-performance (&key (iterations 10000) (chunk "x"))
  "Demonstrate O(1) amortized append by reporting linear growth metrics.
Returns a plist containing append count, final length, capacity, and average
allocated capacity per appended character."
  (let ((builder (make-string-builder :capacity 1)))
    (dotimes (_ iterations)
      (declare (ignore _))
      (string-builder-append-string! builder chunk))
    (let ((length (string-builder-length builder)))
      (list :iterations iterations
            :length length
            :capacity (sb-capacity builder)
            :capacity-per-character (if (zerop length)
                                        0
                                        (/ (sb-capacity builder) length))))))

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

(defun make-string-builder (&optional (initial-capacity 64))
  "Create a fresh string builder with INITIAL-CAPACITY (default 64)."
  (let ((buf (make-array initial-capacity :element-type 'character
                         :adjustable t :fill-pointer 0)))
    (%make-string-builder :buffer buf :capacity initial-capacity)))

(defun string-builder-append! (sb value)
  "Append VALUE (string, character, or number) to the string builder SB.
Value is converted via PRINC-TO-STRING. Returns SB for chaining."
  (etypecase value
    (string (loop for c across value do
              (vector-push-extend c (sb-buffer sb) (sb-capacity sb))))
    (character (vector-push-extend value (sb-buffer sb) (sb-capacity sb)))
    (number (let ((s (princ-to-string value)))
              (string-builder-append! sb s))))
  sb)

(defun string-builder-finish (sb)
  "Return the final string from SB. Performs exactly ONE copy (O(n))."
  (copy-seq (sb-buffer sb)))

(defmacro with-string-builder ((var &optional (initial-capacity 64)) &body body)
  "Execute BODY with VAR bound to a fresh string builder.
Returns the finished string."
  (let ((sb-var (gensym "SB")))
    `(let* ((,sb-var (make-string-builder ,initial-capacity))
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
  (make-rope :root string :length (length string)))

(defun rope-concat (r1 r2)
  "Concatenate two ropes. O(1): creates a new concat node only.
R1 and R2 are left unchanged (persistent)."
  (let* ((len1 (rope-length r1))
         (len2 (rope-length r2))
         (node (%make-rope-node :weight len1
                                :left (rope-root r1)
                                :right (rope-root r2))))
    (make-rope :root node :length (+ len1 len2))))

(defun rope-split (r position)
  "Split rope R at POSITION. O(log n).
Returns (values left-rope right-rope)."
  (labels ((%split (node pos)
             (etypecase node
               (string
                (let ((len (length node)))
                  (if (<= pos 0)
                      (values nil node)
                      (if (>= pos len)
                          (values node nil)
                          (values (subseq node 0 pos)
                                  (subseq node pos))))))
               (rope-node
                (let ((left-weight (rope-node-weight node)))
                  (if (<= pos left-weight)
                      (multiple-value-bind (ll lr)
                          (%split (rope-node-left node) pos)
                        (values ll (if lr
                                       (make-rope :root
                                                  (%make-rope-node
                                                   :weight (if (typep lr 'string)
                                                               (length lr)
                                                               (rope-length* lr))
                                                   :left lr
                                                   :right (rope-node-right node)))
                                       (make-rope :root (rope-node-right node)))))
                      (multiple-value-bind (rl rr)
                          (%split (rope-node-right node) (- pos left-weight))
                        (values (make-rope :root
                                           (%make-rope-node
                                            :weight left-weight
                                            :left (rope-node-left node)
                                            :right rl))
                                rr))))))))
    (multiple-value-bind (left right)
        (%split (rope-root r) position)
      (values (make-rope :root left :length (rope-length* left))
              (make-rope :root right :length (rope-length* right))))))

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

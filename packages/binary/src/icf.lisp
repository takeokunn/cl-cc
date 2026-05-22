;;;; packages/binary/src/icf.lisp — FR-607 Identical Code Folding helpers

(in-package :cl-cc/binary)

(defparameter *icf-enabled* nil
  "When true, identical function-sized code sections may be folded by ICF.")

(defstruct (icf-function-section (:constructor make-icf-function-section
                                      (&key name bytes references linkable-distinct-p)))
  "A function-sized code section considered by Identical Code Folding.

NAME is the externally visible symbol name, BYTES is the final machine-code byte
sequence for that function section, REFERENCES is optional relocation/reference
metadata that must match for safe folding, and LINKABLE-DISTINCT-P prevents
folding symbols that must remain distinct for external linking or debugging."
  name
  (bytes #() :type vector)
  (references nil :type list)
  (linkable-distinct-p nil :type boolean))

(defparameter +icf-sha256-initial-state+
  #(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
    #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

(defparameter +icf-sha256-k+
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
    #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
    #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(defun %icf-u32 (x) (ldb (byte 32 0) x))

(defun %icf-rotr32 (x n)
  (%icf-u32 (logior (ash x (- n)) (ash x (- 32 n)))))

(defun %icf-sha256-pad (bytes)
  (let* ((len (length bytes))
         (bit-len (* len 8))
         (pad-len (mod (- 56 (+ len 1)) 64))
         (out (make-array (+ len 1 pad-len 8) :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (replace out bytes)
    (setf (aref out len) #x80)
    (loop for i below 8
          do (setf (aref out (+ len 1 pad-len i))
                   (ldb (byte 8 (* 8 (- 7 i))) bit-len)))
    out))

(defun icf-sha256 (bytes)
  "Return the SHA-256 digest of BYTES as a 32-byte vector."
  (let ((h (copy-seq +icf-sha256-initial-state+))
        (msg (%icf-sha256-pad bytes)))
    (loop for chunk from 0 below (length msg) by 64 do
      (let ((w (make-array 64 :initial-element 0)))
        (loop for i below 16
              for j = (+ chunk (* i 4))
              do (setf (aref w i)
                       (logior (ash (aref msg j) 24)
                               (ash (aref msg (+ j 1)) 16)
                               (ash (aref msg (+ j 2)) 8)
                               (aref msg (+ j 3)))))
        (loop for i from 16 below 64 do
          (let* ((s0 (logxor (%icf-rotr32 (aref w (- i 15)) 7)
                             (%icf-rotr32 (aref w (- i 15)) 18)
                             (ash (aref w (- i 15)) -3)))
                 (s1 (logxor (%icf-rotr32 (aref w (- i 2)) 17)
                             (%icf-rotr32 (aref w (- i 2)) 19)
                             (ash (aref w (- i 2)) -10))))
            (setf (aref w i) (%icf-u32 (+ (aref w (- i 16)) s0
                                          (aref w (- i 7)) s1)))))
        (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
              (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
          (loop for i below 64 do
            (let* ((s1 (logxor (%icf-rotr32 e 6) (%icf-rotr32 e 11) (%icf-rotr32 e 25)))
                   (ch (logxor (logand e f) (logand (lognot e) g)))
                   (temp1 (%icf-u32 (+ hh s1 ch (aref +icf-sha256-k+ i) (aref w i))))
                   (s0 (logxor (%icf-rotr32 a 2) (%icf-rotr32 a 13) (%icf-rotr32 a 22)))
                   (maj (logxor (logand a b) (logand a c) (logand b c)))
                   (temp2 (%icf-u32 (+ s0 maj))))
              (setf hh g g f f e e (%icf-u32 (+ d temp1)) d c c b b a a (%icf-u32 (+ temp1 temp2)))))
          (loop for val in (list a b c d e f g hh)
                for i below 8
                do (setf (aref h i) (%icf-u32 (+ (aref h i) val)))))))
    (let ((out (make-array 32 :element-type '(unsigned-byte 8))))
      (loop for word across h
            for j from 0 by 4 do
              (setf (aref out j) (ldb (byte 8 24) word)
                    (aref out (+ j 1)) (ldb (byte 8 16) word)
                    (aref out (+ j 2)) (ldb (byte 8 8) word)
                    (aref out (+ j 3)) (ldb (byte 8 0) word)))
      out)))

(defun icf-code-hash (bytes &optional references)
  "Return a SHA-256 digest for machine-code BYTES and relocation REFERENCES."
  (let* ((ref-string (prin1-to-string references))
         (ref-bytes (map 'vector #'char-code ref-string))
         (joined (make-array (+ (length bytes) 1 (length ref-bytes))
                             :element-type '(unsigned-byte 8))))
    (replace joined bytes)
    (setf (aref joined (length bytes)) 0)
    (replace joined ref-bytes :start1 (1+ (length bytes)))
    (icf-sha256 joined)))

(defun %icf-digest-key (digest)
  (coerce digest 'list))

(defun icf-fold-functions (sections)
  "Fold identical function SECTIONS.

Returns three values: kept sections, a NAME->CANONICAL-NAME hash table, and the
number of folded functions.  Sections marked LINKABLE-DISTINCT-P are never folded
even if their machine code is byte-identical."
  (let ((seen (make-hash-table :test #'equal))
        (redirects (make-hash-table :test #'equal))
        (kept '())
        (folded 0))
    (dolist (section sections)
      (let ((name (icf-function-section-name section)))
        (if (icf-function-section-linkable-distinct-p section)
            (progn
              (setf (gethash name redirects) name)
              (push section kept))
            (let* ((key (%icf-digest-key
                         (icf-code-hash (icf-function-section-bytes section)
                                        (icf-function-section-references section))))
                   (canonical (gethash key seen)))
              (if canonical
                  (progn
                    (setf (gethash name redirects) (icf-function-section-name canonical))
                    (incf folded))
                  (progn
                    (setf (gethash key seen) section
                          (gethash name redirects) name)
                    (push section kept)))))))
    (values (nreverse kept) redirects folded)))

(defun icf-merge-identical-functions (functions &key (enabled *icf-enabled*))
  "Merge byte-identical FUNCTIONS and return kept entries plus redirections.

FUNCTIONS may be ICF-FUNCTION-SECTION instances or plist/alist entries carrying
:NAME and :BYTES.  Hash buckets are formed from SXHASH-compatible byte/reference
keys and byte equality is rechecked before merging so collisions do not fold
non-identical code.  Three values are returned: kept functions, a NAME->CANONICAL
hash table, and the number of merged functions."
  (if (not enabled)
      (let ((redirects (make-hash-table :test #'equal)))
        (dolist (function functions)
          (let ((name (etypecase function
                        (icf-function-section (icf-function-section-name function))
                        (list (or (getf function :name) (cdr (assoc :name function)))))))
            (setf (gethash name redirects) name)))
        (values functions redirects 0))
      (let ((buckets (make-hash-table :test #'eql))
            (redirects (make-hash-table :test #'equal))
            (kept '())
            (folded 0))
        (labels ((name-of (function)
                   (etypecase function
                     (icf-function-section (icf-function-section-name function))
                     (list (or (getf function :name) (cdr (assoc :name function))))))
                 (bytes-of (function)
                   (etypecase function
                     (icf-function-section (icf-function-section-bytes function))
                     (list (or (getf function :bytes) (cdr (assoc :bytes function))))))
                 (refs-of (function)
                   (etypecase function
                     (icf-function-section (icf-function-section-references function))
                     (list (or (getf function :references)
                               (cdr (assoc :references function))))))
                 (distinct-p (function)
                   (etypecase function
                     (icf-function-section (icf-function-section-linkable-distinct-p function))
                     (list (or (getf function :linkable-distinct-p)
                               (cdr (assoc :linkable-distinct-p function))))))
                 (byte-hash (bytes references)
                   (sxhash (list (length bytes) (coerce bytes 'list) references)))
                 (same-code-p (left right)
                   (and (equalp (bytes-of left) (bytes-of right))
                        (equal (refs-of left) (refs-of right)))))
          (dolist (function functions)
            (let ((name (name-of function)))
              (if (distinct-p function)
                  (progn
                    (setf (gethash name redirects) name)
                    (push function kept))
                  (let* ((hash (byte-hash (bytes-of function) (refs-of function)))
                         (bucket (gethash hash buckets))
                         (canonical (find function bucket :test #'same-code-p)))
                    (if canonical
                        (progn
                          (setf (gethash name redirects) (name-of canonical))
                          (incf folded))
                        (progn
                          (push function (gethash hash buckets))
                          (setf (gethash name redirects) name)
                          (push function kept)))))))
          (values (nreverse kept) redirects folded)))))

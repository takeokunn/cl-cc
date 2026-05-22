(in-package :cl-cc/cli)

;;; FR-702 — direct Brendan Gregg-style flame graph SVG generation.

(defparameter *svg-char-entities*
  '((#\& . "&amp;") (#\< . "&lt;") (#\> . "&gt;") (#\" . "&quot;"))
  "Alist mapping special SVG/XML characters to their entity references.")

(defun %svg-escape (text)
  (with-output-to-string (out)
    (loop for ch across (princ-to-string text)
          do (let ((entity (cdr (assoc ch *svg-char-entities*))))
               (if entity (princ entity out) (write-char ch out))))))

(defparameter *flamegraph-color-overrides*
  '(("gc"  . "rgb(90,140,255)")
    ("jit" . "rgb(255,165,0)"))
  "Special-case frame colors keyed by substring match (first match wins).")

(defun %flamegraph-color (name)
  (let ((s (string-downcase (princ-to-string name))))
    (or (cdr (assoc-if (lambda (k) (search k s)) *flamegraph-color-overrides*))
        (format nil "hsl(8,75%,~D%)" (+ 45 (mod (sxhash s) 20))))))

(defun %flamegraph-build-tree (samples)
  (let ((root (list :name "root" :count 0 :children (make-hash-table :test #'equal))))
    (maphash
     (lambda (stack count)
       (incf (getf root :count) count)
       (let ((node root))
         (dolist (name (uiop:split-string stack :separator '(#\;)))
           (unless (string= name "")
             (let* ((children (getf node :children))
                    (child (or (gethash name children)
                               (setf (gethash name children)
                                     (list :name name :count 0 :children (make-hash-table :test #'equal))))))
               (incf (getf child :count) count)
               (setf node child))))))
     samples)
    root))

(defun %flamegraph-children-list (node)
  (let (children)
    (let ((table (getf node :children)))
      (when table
        (maphash (lambda (_ child) (declare (ignore _)) (push child children)) table)))
    (sort children #'string< :key (lambda (c) (princ-to-string (getf c :name))))))

(defun %flamegraph-depth-of (node depth max-depth-cell)
  "Compute max nesting depth by DFS; store result in (car MAX-DEPTH-CELL)."
  (setf (car max-depth-cell) (max (car max-depth-cell) depth))
  (dolist (child (%flamegraph-children-list node))
    (%flamegraph-depth-of child (1+ depth) max-depth-cell)))

(defun %flamegraph-emit-node (stream node depth x scale max-depth frame-height)
  "Recursively emit SVG rect+text for NODE and its children."
  (let ((cursor x))
    (dolist (child (%flamegraph-children-list node))
      (let* ((count (getf child :count))
             (w (* count scale))
             (y (- (+ 20 (* max-depth frame-height)) (* depth frame-height))))
        (format stream "<g><title>~A (~D samples)</title><rect x='~,2f' y='~,2f' width='~,2f' height='~D' fill='~A' stroke='white' stroke-width='0.5'/><text x='~,2f' y='~,2f' font-size='12' fill='black'>~A</text></g>~%"
                (%svg-escape (getf child :name)) count cursor y w (- frame-height 2)
                (%flamegraph-color (getf child :name)) (+ cursor 3) (+ y 13)
                (%svg-escape (getf child :name)))
        (%flamegraph-emit-node stream child (1+ depth) cursor scale max-depth frame-height)
        (incf cursor w)))))

(defun %write-flamegraph-svg (path samples)
  (let* ((tree          (%flamegraph-build-tree samples))
         (total         (max 1 (getf tree :count)))
         (frame-height  18)
         (width         1200)
         (max-depth-cell (list 0)))
    (%flamegraph-depth-of tree 0 max-depth-cell)
    (let ((max-depth (car max-depth-cell)))
      (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format out "<svg xmlns='http://www.w3.org/2000/svg' width='~D' height='~D'>~%" width (+ 40 (* (1+ max-depth) frame-height)))
        (format out "<rect width='100%' height='100%' fill='rgb(250,250,250)'/>~%")
        (%flamegraph-emit-node out tree 0 0 (/ width total) max-depth frame-height)
        (format out "</svg>~%")))
    path))

(defun %flamegraph-inc-sample (samples stack &optional (count 1))
  (incf (gethash stack samples 0) count)
  samples)

(defun %perf-map-line->sample (line samples)
  "Add one sample from a perf-map line to SAMPLES when LINE is valid."
  (let ((parts (uiop:split-string line :separator '(#\Space #\Tab))))
    (when (>= (length parts) 3)
      (let ((size (ignore-errors (parse-integer (second parts) :radix 16)))
            (name (third parts)))
        (when (and size name)
          (%flamegraph-inc-sample samples (format nil "jit;~A" name) (max 1 size))))))
  samples)

(defun %perf-script-line->sample (line samples)
  "Add one folded-stack sample from a simple perf script line to SAMPLES."
  (cond
    ((search ";" line)
     (%flamegraph-inc-sample samples (string-trim '(#\Space #\Tab) line)))
    ((and (> (length line) 0) (not (find (char line 0) " \t#")))
     (%flamegraph-inc-sample samples (format nil "cpu;~A" (string-trim '(#\Space #\Tab) line)))))
  samples)

(defun %read-flamegraph-samples-from-file (path)
  "Read perf map/script/collapsed data from PATH and return collapsed samples."
  (let ((samples (make-hash-table :test #'equal)))
    (when (and path (probe-file path))
      (with-open-file (in path :direction :input)
        (loop for line = (read-line in nil nil)
              while line
              for parts = (uiop:split-string line :separator '(#\Space #\Tab))
              do (if (and parts (ignore-errors (parse-integer (first parts) :radix 16)))
                     (%perf-map-line->sample line samples)
                     (%perf-script-line->sample line samples)))))
    samples))

(defun %default-perf-map-path ()
  (parse-namestring
   (format nil "/tmp/perf-~D.map"
           (or #+sbcl
               (ignore-errors
                 (require :sb-posix)
                 (let* ((pkg (find-package "SB-POSIX"))
                        (sym (and pkg (find-symbol "GETPID" pkg))))
                   (and sym (funcall sym))))
               #-sbcl nil
               0))))

(defun %write-flamegraph-from-perf-data (output-path &key input-path)
  "Generate OUTPUT-PATH SVG from INPUT-PATH or /tmp/perf-<pid>.map."
  (%write-flamegraph-svg output-path
                         (%read-flamegraph-samples-from-file
                          (or input-path (%default-perf-map-path)))))

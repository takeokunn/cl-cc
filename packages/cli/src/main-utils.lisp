;;;; cli/src/main-utils.lisp — CLI utilities and shared rendering helpers
;;;
;;; Contains shared CLI helpers that are reused by the command handlers and
;;; dump/compile-option support modules:
;;;   - %read-file, %detect-language — I/O and language detection helpers
;;;   - %dump-phase-label, %parse-ir-phase — IR phase helpers
;;;   - %ensure-list, %source-location-comment, %print-source-comment
;;;   - %call-with-optional-output-file
;;;   - %svg-escape, %flamegraph-color, %flamegraph-build-tree,
;;;     %flamegraph-children-list, %write-flamegraph-svg — flamegraph rendering
;;;   - %ssa-block-name
;;;
;;; Dump-specific helpers and compile-option parsing live in main-dump.lisp.
;;; Help system (%print-global-help, %print-command-help, %print-help)
;;; lives in main.lisp (loads before).
;;;
;;; Load order: after cli/main.lisp.
(in-package :cl-cc/cli)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Utilities
;;; ─────────────────────────────────────────────────────────────────────────

(defun %read-file (path)
  "Read the entire contents of PATH as a string.
Signals an error when the file does not exist."
  (with-open-file (in path :direction :input
                       :element-type 'character
                       :if-does-not-exist nil)
    (unless in
      (error "File not found: ~A" path))
    (let* ((buf (make-string (file-length in)))
           (n   (read-sequence buf in)))
      (subseq buf 0 n))))

(defun %detect-language (file lang-flag)
  "Determine source language from LANG-FLAG string or FILE extension.
Returns :lisp or :php."
  (cond
    ((string= lang-flag "php")  :php)
    ((string= lang-flag "lisp") :lisp)
    ((let ((ext (and file (pathname-type file))))
       (and ext (string= ext "php"))) :php)
    (t :lisp)))

(defun %dump-phase-label (phase)
  (string-downcase (string phase)))

(defun %parse-ir-phase (phase-str)
  (let ((phase (string-downcase phase-str)))
    (case (intern (string-upcase phase) :keyword)
      (:ast :ast)
      (:cps :cps)
      (:ssa :ssa)
      (:vm  :vm)
      (:opt :opt)
      (:asm :asm)
      (t nil))))

(defun %ensure-list (thing)
  (cond
    ((null thing) nil)
    ((listp thing) thing)
    (t (list thing))))

(defun %source-location-comment (node)
  (when (typep node 'cl-cc:ast-node)
    (let ((loc (cl-cc:ast-location-string node)))
      (unless (string= loc "<unknown location>")
        loc))))

(defun %print-source-comment (stream loc)
  (when loc
    (format stream "; source: ~A~%" loc)))

(defun %call-with-optional-output-file (path thunk)
  "Call THUNK with an output stream for PATH, or NIL when PATH is NIL."
  (if path
      (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
        (funcall thunk out))
      (funcall thunk nil)))

(defun %svg-escape (text)
  (with-output-to-string (out)
    (loop for ch across (princ-to-string text)
          do (case ch
               (#\& (princ "&amp;" out))
               (#\< (princ "&lt;" out))
               (#\> (princ "&gt;" out))
               (#\" (princ "&quot;" out))
               (t (write-char ch out))))))

(defun %flamegraph-color (name)
  (let ((s (string-downcase (princ-to-string name))))
    (cond ((search "gc" s) "rgb(90,140,255)")
          ((search "jit" s) "rgb(255,165,0)")
          (t (format nil "hsl(8,75%,~D%)" (+ 45 (mod (sxhash s) 20)))))))

(defun %flamegraph-build-tree (samples)
  (let ((root (list :name "root" :count 0 :children (make-hash-table :test #'equal))))
    (maphash
     (lambda (stack count)
       (incf (getf root :count) count)
       (let ((node root))
         (dolist (name (uiop:split-string stack :separator '(#\;)))
           (let* ((children (getf node :children))
                  (child (or (gethash name children)
                             (setf (gethash name children)
                                   (list :name name :count 0 :children (make-hash-table :test #'equal))))))
             (incf (getf child :count) count)
             (setf node child)))))
     samples)
    root))

(defun %flamegraph-children-list (node)
  (let (children)
    (maphash (lambda (_ child) (declare (ignore _)) (push child children)) (getf node :children))
    (sort children #'string< :key (lambda (c) (princ-to-string (getf c :name))))))

(defun %write-flamegraph-svg (path samples)
  (let* ((tree (%flamegraph-build-tree samples))
         (total (max 1 (getf tree :count)))
         (frame-height 18)
         (width 1200)
         (max-depth 0))
    (labels ((depth-of (node depth)
               (setf max-depth (max max-depth depth))
               (dolist (child (%flamegraph-children-list node))
                 (depth-of child (1+ depth))))
             (emit-node (stream node depth x scale)
               (let ((children (%flamegraph-children-list node))
                     (cursor x))
                 (dolist (child children)
                   (let* ((count (getf child :count))
                          (w (* count scale))
                          (y (- (+ 20 (* max-depth frame-height)) (* depth frame-height))))
                     (format stream "<g><title>~A (~D samples)</title><rect x='~,2f' y='~,2f' width='~,2f' height='~D' fill='~A' stroke='white' stroke-width='0.5'/><text x='~,2f' y='~,2f' font-size='12' fill='black'>~A</text></g>~%"
                             (%svg-escape (getf child :name)) count cursor y w (- frame-height 2)
                             (%flamegraph-color (getf child :name)) (+ cursor 3) (+ y 13)
                             (%svg-escape (getf child :name)))
                     (emit-node stream child (1+ depth) cursor scale)
                     (incf cursor w))))))
      (depth-of tree 0)
      (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format out "<svg xmlns='http://www.w3.org/2000/svg' width='~D' height='~D'>~%" width (+ 40 (* (1+ max-depth) frame-height)))
        (format out "<rect width='100%' height='100%' fill='rgb(250,250,250)'/>~%")
        (emit-node out tree 0 0 (/ width total))
        (format out "</svg>~%")))
    path))

(defun %ssa-block-name (blk)
  (let ((label (cl-cc/optimize:bb-label blk)))
    (string-downcase
     (format nil "~A"
             (or (and label (cl-cc:vm-name label))
                 (format nil "block-~D" (cl-cc/optimize:bb-id blk)))))))

;; +ansi-*+ colors, %dump-*-phase, %dump-ir-phase, %trace-emit-stages,
;; %arch-keyword, %compile-target-keyword, %parse-opt-remarks-mode,
;; compile-opts struct + %parse-compile-opts + %compile-opts-kwargs
;; are in main-dump.lisp (loaded next).

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

;; `compile-opts` is defined later in main-dump.lisp. Keep early accessor calls
;; non-inline so compile-file does not warn before the defstruct is seen.
(declaim (notinline compile-opts-flamegraph-path))

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

(defparameter *lang-flag-map*
  '(("php" . :php) ("lisp" . :lisp))
  "Alist mapping --lang flag strings to language keywords.")

(defparameter *lang-ext-map*
  '(("php" . :php))
  "Alist mapping file extensions to non-default language keywords.")

(defun %detect-language (file lang-flag)
  "Determine source language from LANG-FLAG string or FILE extension.
Returns :lisp or :php."
  (or (cdr (assoc lang-flag *lang-flag-map* :test #'string=))
      (let ((ext (and file (pathname-type file))))
        (and ext (cdr (assoc ext *lang-ext-map* :test #'string=))))
      :lisp))

(defun %dump-phase-label (phase)
  (string-downcase (string phase)))

(defparameter *ir-phases* '(:ast :cps :ssa :vm :opt :asm)
  "All recognized IR dump phases.")

(defun %parse-ir-phase (phase-str)
  (let ((kw (intern (string-upcase (string-downcase phase-str)) :keyword)))
    (when (member kw *ir-phases*)
      kw)))

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

(defun %quit-command-usage-error (command message)
  "Print MESSAGE, show command help, and exit with usage status 2."
  (format *error-output* "Error: ~A~%" message)
  (%print-help command)
  (uiop:quit 2))

(defun %required-file-arg (parsed command)
  "Return the first positional file argument or exit with command usage help."
  (or (car (parsed-args-positional parsed))
      (%quit-command-usage-error command
                                 (format nil "'~A' requires a file argument." command))))

(defun %read-command-source (file)
  "Read FILE or print a consistent CLI read error and exit with status 1."
  (handler-case (%read-file file)
    (error (e)
      (format *error-output* "Error reading ~A: ~A~%" file e)
      (uiop:quit 1))))

(defun %maybe-make-profiled-vm-state (opts)
  "Create a profiled VM state when flamegraph output is requested."
  (when (compile-opts-flamegraph-path opts)
    (let ((vm-state (cl-cc/vm::make-vm-state
                                   :output-stream *standard-output*)))
      (setf (cl-cc/vm::vm-profile-enabled-p vm-state) t
            (cl-cc/vm::vm-profile-call-stack vm-state) (list "<toplevel>"))
      vm-state)))

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

(defun %ssa-block-name (blk)
  (let ((label (cl-cc/optimize:bb-label blk)))
    (string-downcase
     (format nil "~A"
             (or (and label (cl-cc:vm-name label))
                 (format nil "block-~D" (cl-cc/optimize:bb-id blk)))))))

;; +ansi-*+ colors, %dump-*-phase, %dump-ir-phase, %trace-emit-stages,
;; %quit-command-usage-error, %required-file-arg, %read-command-source,
;; %maybe-make-profiled-vm-state,
;; %arch-keyword, %compile-target-keyword, %parse-opt-remarks-mode,
;; compile-opts struct + %parse-compile-opts + %compile-opts-kwargs
;; are in main-dump.lisp (loaded next).

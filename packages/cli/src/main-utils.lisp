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

(defun %shebang-line-p (line)
  "Return T when LINE is a POSIX shebang line."
  (and (stringp line)
       (>= (length line) 2)
       (char= (char line 0) #\#)
       (char= (char line 1) #\!)))

(defun %strip-shebang-line (source)
  "Return SOURCE with a leading #! interpreter line removed."
  (let* ((text (or source ""))
         (newline (position #\Newline text))
         (first-line (if newline (subseq text 0 newline) text)))
    (if (%shebang-line-p first-line)
        (if newline (subseq text (1+ newline)) "")
        text)))

(defparameter *lang-flag-map*
  '(("php" . :php) ("lisp" . :lisp)
    ("js" . :javascript) ("javascript" . :javascript))
  "Alist mapping --lang flag strings to language keywords.")

(defparameter *lang-ext-map*
  '(("php" . :php) ("js" . :javascript) ("mjs" . :javascript))
  "Alist mapping file extensions to non-default language keywords.")

(defun %detect-language (file lang-flag)
  "Determine source language from LANG-FLAG string or FILE extension.
Returns :lisp, :php, or :javascript."
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
  (handler-case (%strip-shebang-line (%read-file file))
    (error (e)
      (format *error-output* "Error reading ~A: ~A~%" file e)
      (uiop:quit 1))))

(defun %maybe-make-profiled-vm-state (opts)
  "Create a profiled VM state when profiling outputs are requested."
  (when (or (compile-opts-flamegraph-path opts)
            (compile-opts-profile opts)
            (compile-opts-pgo-generate-path opts))
    (let ((vm-state (cl-cc/vm:make-vm-state
                                    :output-stream *standard-output*)))
      (setf (cl-cc/vm:vm-profile-enabled-p vm-state) t
            (cl-cc/vm:vm-profile-call-stack vm-state) (list "<toplevel>"))
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

;;; ─────────────────────────────────────────────────────────────────────────
;;; Timeout support
;;; ─────────────────────────────────────────────────────────────────────────

(defun %parse-timeout-seconds (timeout-str)
  "Parse TIMEOUT-STR as a positive integer number of seconds."
  (let ((seconds (handler-case
                     (let ((n (parse-integer timeout-str :junk-allowed nil)))
                       (unless n
                         (error 'arg-parse-error
                                :message (format nil "Timeout must be a positive integer, got: ~S" timeout-str)))
                       n)
                   (error (c)
                     (if (typep c 'arg-parse-error)
                         (error c)
                         (error 'arg-parse-error
                                :message (format nil "Timeout must be a positive integer, got: ~S" timeout-str)))))))
    (unless (and (typep seconds 'integer) (plusp seconds))
      (error 'arg-parse-error
             :message (format nil "Timeout must be a positive integer, got: ~S" timeout-str)))
    seconds))

(defun %get-timeout (parsed)
  (unless (flag parsed "--no-timeout")
    (let ((raw (flag parsed "--timeout")))
      (if raw
          (%parse-timeout-seconds raw)
          30))))

(defun %call-with-cli-timeout (seconds thunk command-name)
  (if seconds
      (handler-case (sb-ext:with-timeout seconds (funcall thunk))
        (sb-ext:timeout (c) (declare (ignore c))
          (format *error-output* "~&Error: ~A timed out after ~A second~:P~%" command-name seconds)
          (uiop:quit 124)))
      (funcall thunk)))

;;; FR-808: Script mode / shebang support stub
(defun parse-cli-args (argv)
  "FR-808: Parse CLI arguments and script-mode argv conventions.
Returns a parsed-args structure."
  (parse-args argv))

;;; FR-809: Command-line arguments API stub
(defvar *command-line-arguments* nil
  "Stable CLI-level view of script arguments after the script/expression.")

(defun cl-cc-argv ()
  "FR-809: Return the command-line arguments as a list of strings."
  (copy-list *command-line-arguments*))

(defun %script-argv-from-parsed (parsed)
  "Return script-visible argv: positional arguments after file/expression."
  (copy-list (cdr (parsed-args-positional parsed))))

(defun %set-vm-global-if-package (package-name symbol-name value &optional (state nil state-p))
  (let ((package (find-package package-name)))
    (when package
      (multiple-value-bind (symbol status) (find-symbol symbol-name package)
        (declare (ignore status))
        (when symbol
          (set symbol value)
          ;; STATE-P is true whenever a state argument was *passed*, even if it
          ;; is NIL (callers forward a possibly-NIL profiled vm-state). Guard on
          ;; the value too, or (vm-global-vars NIL) signals no-applicable-method.
          (when (and state-p state)
            (setf (gethash symbol (cl-cc/vm:vm-global-vars state)) value)))))))

(defun %bind-command-line-arguments (argv &optional vm-state)
  "Bind FR-809 command-line arguments in CLI, runtime, and VM state."
  (let ((args (copy-list argv)))
    (setf *command-line-arguments* args)
    (dolist (name '("*COMMAND-LINE-ARGUMENTS*" "*COMMAND-LINE-ARGS*"))
      (%set-vm-global-if-package :cl-cc/vm name args vm-state)
      (%set-vm-global-if-package :cl-cc name args vm-state))
    args))

(defun getopt (_program option-spec argv)
  "FR-809: Minimal script getopt helper. Returns option plist and positionals."
  (declare (ignore _program))
  (let ((opts nil)
        (positionals nil)
        (rest (copy-list argv)))
    (labels ((spec-for-short (ch)
               (find ch option-spec :key #'second :test #'char=))
             (spec-for-long (name)
               (find name option-spec :key #'third :test #'string=)))
      (loop while rest do
        (let ((arg (pop rest)))
          (cond
            ((string= arg "--")
             (setf positionals (append positionals rest)
                   rest nil))
            ((and (> (length arg) 2) (string= arg "--" :end1 2))
             (let ((spec (spec-for-long (subseq arg 2))))
               (if spec
                   (setf (getf opts (first spec)) t)
                   (push arg positionals))))
            ((and (= (length arg) 2) (char= (char arg 0) #\-))
             (let ((spec (spec-for-short (char arg 1))))
               (if spec
                   (setf (getf opts (first spec)) t)
                   (push arg positionals))))
            (t (push arg positionals))))))
    (values opts (nreverse positionals))))

(defun %source-with-script-bindings (source argv)
  "Wrap SOURCE with script argv initialization and optional CL-CC:MAIN call."
  (format nil "(progn~%  (defparameter cl-cc:*command-line-arguments* '~S)~%  ~A~%  (when (fboundp 'cl-cc:main) (funcall #'cl-cc:main)))~%"
          argv source))

;;; FR-917: Reproducible build support stub
(defun cl-cc-deterministic-build-p ()
  "FR-917: Return T when building in reproducible/deterministic mode."
  (let ((env (or (ignore-errors (uiop:getenv "CLCC_DETERMINISTIC"))
                 (ignore-errors (uiop:getenv "SOURCE_DATE_EPOCH")))))
    (and env (not (member (string-downcase env) '("" "0" "false" "no") :test #'string=)))))

(defun configure-reproducible-build (&key (epoch "0") (seed 0))
  "Enable deterministic build defaults for this process and return metadata."
  (setf (uiop:getenv "CLCC_DETERMINISTIC") "1")
  (unless (uiop:getenv "SOURCE_DATE_EPOCH")
    (setf (uiop:getenv "SOURCE_DATE_EPOCH") epoch))
  (let ((apply-seed (find-symbol "APPLY-BUILD-SEED" :cl-cc/vm)))
    (when (and apply-seed (fboundp apply-seed))
      (funcall apply-seed seed)))
  (list :format :cl-cc-reproducible-build-v1
        :source-date-epoch (uiop:getenv "SOURCE_DATE_EPOCH")
        :seed seed
        :deterministic t))

(defun %reproducible-metadata-path (path)
  (namestring (make-pathname :defaults (pathname path) :type "build-metadata.sexp")))

(defun %apply-reproducible-build-options (output parsed)
  "Write deterministic sidecar metadata for OUTPUT when reproducible mode is on."
  (when (and output (or (flag parsed "--deterministic")
                       (flag parsed "--reproducible")
                       (flag parsed "--build-id")))
    (let* ((path (%reproducible-metadata-path output))
           (metadata (append (configure-reproducible-build)
                             (list :output (file-namestring (pathname output))
                                   :build-id (or (flag parsed "--build-id") "")
                                   :timestamp-stripped t))))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write metadata :stream out :pretty t)
        (terpri out))
      path)))

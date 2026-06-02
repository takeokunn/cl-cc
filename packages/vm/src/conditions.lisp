(in-package :cl-cc/vm)

;;; VM Condition System — Protocol Layer
;;;
;;; Provides condition classes, handler stack management, and restart bindings.
;;; VM instruction definitions, execute-instruction methods, and condition constructors
;;; are in conditions-instructions.lisp (loaded immediately after this file).

;;; ─── Standard/VM Condition Classes ───────────────────────────────────────────

;;; FR-204/252/255 Wasm EH mapping: VM condition classes remain the semantic
;;; source of truth; the Wasm backend lowers handler ranges to tag/try_table/
;;; exnref metadata and uses finalization hooks only when the corresponding
;;; experimental feature flag is explicitly enabled.

(defvar *active-restarts* nil
  "VM-visible list of active restart records.

This mirrors the self-hosted stdlib's restart registry while host-side VM code
continues to interoperate with Common Lisp's native restart protocol.")

(defparameter *vm-error-output-format* :text
  "Error output format.  Supported values are :TEXT and :JSON.")

(defun %source-location-file (location)
  (when location
    (or (ignore-errors (source-location-file location))
        (ignore-errors (source-location-pathname location))
        (ignore-errors (vm-source-location-pathname location))
        (getf location :file)
        (getf location :pathname))))

(defun %source-location-line (location)
  (when location
    (or (ignore-errors (source-location-line location))
        (ignore-errors (vm-source-location-line location))
        (getf location :line))))

(defun %source-location-column (location)
  (when location
    (or (ignore-errors (source-location-column location))
        (ignore-errors (vm-source-location-column location))
        (getf location :column))))

(defun %source-location-string (location)
  (when location
    (let ((file (%source-location-file location))
          (line (%source-location-line location))
          (column (%source-location-column location)))
      (when (or file line column)
        (format nil "~A~@[:~D~]~@[:~D~]"
                (or file "<unknown>") line column)))))

(defun %condition-location (condition)
  (or (ignore-errors (%vm-condition-source-location condition))
      (and (boundp '*current-source-location*) *current-source-location*)))

(defun %condition-source-text (condition)
  (ignore-errors (%vm-condition-source-text condition)))

(defun %read-source-lines (condition location)
  (let ((source (%condition-source-text condition))
        (file (%source-location-file location)))
    (cond
      ((stringp source)
       (loop with start = 0
             for pos = (position #\Newline source :start start)
             collect (subseq source start (or pos (length source)))
             do (setf start (and pos (1+ pos)))
             while start))
      ((and file (ignore-errors (probe-file file)))
       (with-open-file (in file :direction :input)
         (loop for line = (read-line in nil nil)
               while line collect line)))
      (t nil))))

(defun format-source-context (condition stream &key (context 3))
  "Print source context for CONDITION, if source text or file is available."
  (let* ((location (%condition-location condition))
         (line (%source-location-line location))
         (column (or (%source-location-column location) 1))
         (lines (and line (%read-source-lines condition location))))
    (when (and lines line)
      (let ((start (max 1 (- line context)))
            (end (min (length lines) (+ line context))))
        (loop for n from start to end
              for text = (nth (1- n) lines)
              do (format stream "~&~4D | ~A~%" n text)
                 (when (= n line)
                   (format stream "     | ~A^~%"
                           (make-string (max 0 (1- column)) :initial-element #\Space))))))))

(defun vm-levenshtein-distance (s1 s2)
  "Compute Levenshtein edit distance between two string designators."
  (let* ((a (string-downcase (string s1)))
         (b (string-downcase (string s2)))
         (n (length a))
         (m (length b))
         (d (make-array (list (1+ n) (1+ m)) :initial-element 0)))
    (dotimes (i (1+ n)) (setf (aref d i 0) i))
    (dotimes (j (1+ m)) (setf (aref d 0 j) j))
    (dotimes (i n)
      (dotimes (j m)
        (setf (aref d (1+ i) (1+ j))
              (min (1+ (aref d i (1+ j)))
                   (1+ (aref d (1+ i) j))
                   (+ (aref d i j)
                      (if (char= (char a i) (char b j)) 0 1))))))
    (aref d n m)))

(defun vm-did-you-mean (target candidates &key (max-distance 3) (max-results 5))
  "Return closest CANDIDATES to TARGET by edit distance."
  (let ((scored (loop for c in candidates
                      for distance = (vm-levenshtein-distance target c)
                      when (<= distance max-distance)
                        collect (cons c distance))))
    (subseq (mapcar #'car (stable-sort scored #'< :key #'cdr))
            0 (min max-results (length scored)))))

(defun %all-bound-variable-symbols (&optional vm-state)
  (let ((symbols nil))
    (do-all-symbols (sym)
      (when (boundp sym) (pushnew sym symbols :test #'eq)))
    (when vm-state
      (let ((globals (ignore-errors (vm-global-vars vm-state))))
        (when (hash-table-p globals)
          (maphash (lambda (sym value)
                     (declare (ignore value))
                     (pushnew sym symbols :test #'eq))
                   globals))))
    symbols))

(defun %all-function-symbols (&optional vm-state)
  (let ((symbols nil))
    (do-all-symbols (sym)
      (when (fboundp sym) (pushnew sym symbols :test #'eq)))
    (when vm-state
      (let ((functions (ignore-errors (vm-function-registry vm-state))))
        (when (hash-table-p functions)
          (maphash (lambda (sym value)
                     (declare (ignore value))
                     (pushnew sym symbols :test #'eq))
                   functions))))
    symbols))

(defun %condition-suggestions (condition)
  (or (ignore-errors (%vm-condition-suggestions condition))
      (let ((name (and (typep condition 'cell-error)
                       (ignore-errors (cell-error-name condition)))))
        (cond ((and name (typep condition 'undefined-function))
               (vm-did-you-mean name (%all-function-symbols (ignore-errors (vm-condition-state condition)))))
              ((and name (typep condition 'unbound-variable))
               (vm-did-you-mean name (%all-bound-variable-symbols (ignore-errors (vm-condition-state condition)))))
              (t nil)))))

(defun %json-escape-string (string)
  (with-output-to-string (out)
    (loop for ch across (princ-to-string string)
          do (case ch
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (otherwise (write-char ch out))))))

(defun format-condition-json (condition stream)
  "Write CONDITION as a compact JSON diagnostic object."
  (let ((location (%condition-location condition))
        (suggestions (%condition-suggestions condition)))
    (format stream "{\"type\":\"~A\",\"message\":\"~A\""
            (class-name (class-of condition))
            (%json-escape-string (%vm-condition-report-string condition)))
    (when location
      (format stream ",\"location\":{\"file\":\"~A\",\"line\":~D,\"column\":~D}"
              (%json-escape-string (or (%source-location-file location) ""))
              (or (%source-location-line location) 0)
              (or (%source-location-column location) 0)))
    (when suggestions
      (format stream ",\"suggestions\":[~{\"~A\"~^,~}]"
              (mapcar (lambda (s) (%json-escape-string (symbol-name s))) suggestions)))
    (write-string "}" stream)
    (values)))

(defun format-rich-condition (condition stream)
  "Write CONDITION with source location, context, and did-you-mean hints."
  (if (eq *vm-error-output-format* :json)
      (format-condition-json condition stream)
      (let ((location (%condition-location condition))
            (suggestions (%condition-suggestions condition)))
        (if location
            (format stream "error at ~A: ~A" (%source-location-string location)
                    (%vm-condition-report-string condition))
            (format stream "~A" (%vm-condition-report-string condition)))
        (when suggestions
          (format stream "~%Did you mean: ~{~S~^, ~}?" suggestions))
        (format-source-context condition stream)
        (values))))

(defun format-rich-condition-report (stream control condition &rest arguments)
  "Condition :REPORT helper that avoids recursive condition printing."
  (let ((location (%condition-location condition))
        (suggestions (%condition-suggestions condition)))
    (when location
      (format stream "error at ~A: " (%source-location-string location)))
    (apply #'format stream control arguments)
    (when suggestions
      (format stream "~%Did you mean: ~{~S~^, ~}?" suggestions))
    (format-source-context condition stream)
    (values)))

(defun format-stack-trace (condition stream &key (depth 10))
  "Format a VM stack trace attached to CONDITION's VM state."
  (let ((state (ignore-errors (vm-condition-state condition))))
    (format stream "~&Stack trace:~%")
    (cond ((and state (ignore-errors (vm-call-stack state)))
           (loop for frame in (vm-call-stack state)
                 for index from 0 below depth
                 do (destructuring-bind (return-pc dst-reg _old-env saved-regs &rest _more) frame
                      (declare (ignore _old-env saved-regs _more))
                      (format stream "  ~D: return-pc=~D dst=~A~%" index return-pc dst-reg))))
          (t (format stream "  <empty>~%")))
    (values)))

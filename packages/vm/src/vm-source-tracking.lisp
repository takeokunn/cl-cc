;;; vm-source-tracking.lisp — FR-1082: Source location tracking
;;;
;;; Real implementation: reads source files for context lines,
;;; stores object→location mappings in weak hash table.

(in-package :cl-cc/vm)

(defvar *source-location-map* (make-hash-table :test #'eq :weakness :key))

(defstruct (vm-source-location (:include source-location)
                               (:constructor make-vm-source-location
                                 (&key pathname line column form)))
  (compilation-unit nil :type (or null string))
  (macro-context nil :type list))

(defun vm-source-location-string (loc)
  "Return 'file.lisp:42:10' for LOC."
  (when loc
    (let ((pn (source-location-pathname loc))
          (ln (source-location-line loc))
          (cn (source-location-column loc)))
      (cond ((and pn ln cn) (format nil "~A:~D:~D" (namestring pn) ln cn))
            ((and pn ln) (format nil "~A:~D" (namestring pn) ln))
            (pn (namestring pn))
            (t "<unknown-location>")))))

(defun vm-source-context-lines (loc &key (context 3))
  "Read actual source file and extract CONTEXT lines around LOC.
   Returns (values lines-string caret-column) or (values nil nil) on error."
  (let ((pn (and loc (source-location-pathname loc)))
        (target-line (and loc (source-location-line loc)))
        (target-col (and loc (source-location-column loc))))
    (unless (and pn target-line (probe-file pn))
      (return-from vm-source-context-lines (values nil nil)))
    (ignore-errors
      (with-open-file (in pn :direction :input :if-does-not-exist nil)
        (when in
          (let* ((start (max 1 (- target-line context)))
                 (end (+ target-line context))
                 (lines nil) (line-no 0) (caret-col nil))
            (loop for line = (read-line in nil nil)
                  while line do (incf line-no)
                  when (<= start line-no end) do
                  (push (format nil "~4D: ~A" line-no line) lines)
                  when (= line-no target-line)
                  do (setf caret-col (+ 5 (or target-col 0))))
            (when caret-col
              (let ((caret-str (make-string caret-col :initial-element #\Space)))
                (setf (char caret-str (1- caret-col)) #\^)
                (push caret-str lines)))
            (values (format nil "~{~A~^~%~}" (nreverse lines)) caret-col))
          (values nil nil))))))

(defun vm-attach-source-location (object location)
  "Store source LOCATION for OBJECT in the weak hash map."
  (setf (gethash object *source-location-map*) location)
  object)

(defun vm-get-source-location (object)
  "Retrieve source-location for OBJECT, or NIL."
  (gethash object *source-location-map*))

(defvar *current-source-location* nil)

(defun vm-condition-source-location (condition)
  (declare (ignore condition))
  *current-source-location*)

(defun vm-signal-with-location (condition location)
  (let ((*current-source-location* location))
    (error condition)))

(export '(vm-source-location vm-source-location-p make-vm-source-location
          vm-source-location-pathname vm-source-location-line
          vm-source-location-column vm-source-location-form
          vm-source-location-compilation-unit vm-source-location-macro-context
          vm-source-location-string vm-source-context-lines
          *current-source-location* vm-attach-source-location
          vm-get-source-location vm-condition-source-location
          vm-signal-with-location *source-location-map*))

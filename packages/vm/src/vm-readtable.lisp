(in-package :cl-cc/vm)

;;; ─── FR-607: Multiple Readtables ─────────────────────────────────────────────

(defun %make-vm-readtable (&key (case :upcase) host macro-chars dispatch-chars)
  "Create the VM readtable representation used by reader bridges."
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash :readtable table) t
          (gethash :case table) case
          (gethash :host table) (or host (cl:copy-readtable nil))
          (gethash :macro-chars table) (or macro-chars (make-hash-table :test #'eql))
          (gethash :dispatch-chars table) (or dispatch-chars (make-hash-table :test #'equal)))
    table))

(defun %vm-readtable-p (object)
  (and (hash-table-p object) (gethash :readtable object)))

(defun %vm-readtable-host (readtable)
  (cond
    ((null readtable) (cl:copy-readtable nil))
    ((%vm-readtable-p readtable) (gethash :host readtable))
    (t readtable)))

(defun %copy-hash-table (table &key (test #'eql))
  "Return a shallow copy of TABLE using TEST."
  (let ((copy (make-hash-table :test test)))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defvar *readtable* (%make-vm-readtable)
  "Current VM readtable.")

(defun make-readtable (&key (case :upcase))
  "Return a fresh VM readtable with READTABLE-CASE set to CASE."
  (let ((readtable (%make-vm-readtable :case case)))
    (setf (cl:readtable-case (gethash :host readtable)) case)
    readtable))

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  "Copy FROM-READTABLE into TO-READTABLE or a fresh VM readtable."
  (let* ((source (or from-readtable *readtable*))
         (source-host (%vm-readtable-host source))
         (target (or to-readtable (%make-vm-readtable)))
         (target-host (cl:copy-readtable source-host (%vm-readtable-host target))))
    (when (%vm-readtable-p target)
      (setf (gethash :host target) target-host
            (gethash :case target) (cl:readtable-case target-host)
            (gethash :macro-chars target)
            (if (%vm-readtable-p source)
                (%copy-hash-table (gethash :macro-chars source) :test #'eql)
                (make-hash-table :test #'eql))
            (gethash :dispatch-chars target)
            (if (%vm-readtable-p source)
                (%copy-hash-table (gethash :dispatch-chars source) :test #'equal)
                (make-hash-table :test #'equal))))
    target))

(defun readtable-case (readtable)
  "Return READTABLE's case conversion mode."
  (if (%vm-readtable-p readtable)
      (gethash :case readtable)
      (cl:readtable-case readtable)))

(defun (setf readtable-case) (case readtable)
  "Set READTABLE's case conversion mode."
  (check-type case (member :upcase :downcase :preserve :invert))
  (if (%vm-readtable-p readtable)
      (setf (gethash :case readtable) case
            (cl:readtable-case (gethash :host readtable)) case)
      (setf (cl:readtable-case readtable) case))
  case)

(defun set-macro-character (char function &optional non-terminating-p
                                  (readtable *readtable*))
  "Install FUNCTION as CHAR's reader macro in READTABLE."
  (let ((char (character char)))
    (when (%vm-readtable-p readtable)
      (setf (gethash char (gethash :macro-chars readtable))
            (cons function non-terminating-p)))
    (when (functionp function)
      (cl:set-macro-character char function non-terminating-p
                              (%vm-readtable-host readtable))))
  t)

(defun get-macro-character (char &optional (readtable *readtable*))
  "Return the reader macro function and non-terminating flag for CHAR."
  (let ((char (character char)))
    (if (%vm-readtable-p readtable)
        (let ((entry (gethash char (gethash :macro-chars readtable))))
          (if entry
              (values (car entry) (cdr entry))
              (cl:get-macro-character char (gethash :host readtable))))
        (cl:get-macro-character char readtable))))

(defun %dispatch-key (disp-char sub-char)
  (cons (char-upcase (character disp-char))
        (char-upcase (character sub-char))))

(defun set-dispatch-macro-character (disp-char sub-char function
                                     &optional (readtable *readtable*))
  "Install FUNCTION for DISP-CHAR/SUB-CHAR in READTABLE."
  (let ((key (%dispatch-key disp-char sub-char)))
    (when (%vm-readtable-p readtable)
      (setf (gethash key (gethash :dispatch-chars readtable)) function))
    (when (functionp function)
      (cl:set-dispatch-macro-character (car key) (cdr key) function
                                       (%vm-readtable-host readtable))))
  t)

(defun get-dispatch-macro-character (disp-char sub-char
                                     &optional (readtable *readtable*))
  "Return the dispatch macro function for DISP-CHAR/SUB-CHAR."
  (let ((key (%dispatch-key disp-char sub-char)))
    (if (%vm-readtable-p readtable)
        (or (gethash key (gethash :dispatch-chars readtable))
            (cl:get-dispatch-macro-character (car key) (cdr key)
                                             (gethash :host readtable)))
        (cl:get-dispatch-macro-character (car key) (cdr key) readtable))))

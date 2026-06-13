;;;; packages/javascript/src/runtime-json.lisp — JSON.stringify / JSON.parse
;;;;
;;;; Depends on runtime-object.lisp (%js-internal-key-p) and runtime.lisp
;;;; (JS value constants, %js-make-ht, %js-vec-p, %js-ht-p).

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  JSON serialization
;;; -----------------------------------------------------------------------

(defun %js-json-stringify-value (val depth)
  "Recursively convert JS value VAL to a JSON string (depth limited to 50)."
  (when (> depth 50) (return-from %js-json-stringify-value "null"))
  (cond
    ((or (eq val +js-null+) (eq val +js-undefined+) (null val)) "null")
    ((eq val t)  "true")
    ((eq val nil) "false")
    ((%js-float-nan-p val) "null")
    ((%js-float-infinity-p val) "null")
    ((numberp val)
     (let ((n (coerce val 'double-float)))
       (if (= n (floor n))
           (format nil "~D" (floor n))
           (format nil "~F" n))))
    ((stringp val)
     (with-output-to-string (s)
       (write-char #\" s)
       (loop for ch across val do
         (cond ((char= ch #\") (write-string "\\\"" s))
               ((char= ch #\\) (write-string "\\\\" s))
               ((char= ch #\Newline) (write-string "\\n" s))
               ((char= ch #\Return) (write-string "\\r" s))
               ((char= ch #\Tab) (write-string "\\t" s))
               (t (write-char ch s))))
       (write-char #\" s)))
    ((%js-vec-p val)
     (format nil "[~{~A~^,~}]"
             (loop for i below (length val)
                   collect (%js-json-stringify-value (aref val i) (1+ depth)))))
    ((%js-ht-p val)
     (let ((pairs nil))
       (maphash (lambda (k v)
                  (unless (%js-internal-key-p k)
                    (let ((vs (%js-json-stringify-value v (1+ depth))))
                      (unless (string= vs "undefined")
                        (push (format nil "~A:~A" (%js-json-stringify-value k 0) vs) pairs)))))
                val)
       (format nil "{~{~A~^,~}}" (nreverse pairs))))
    (t "null")))

(defun %js-json-stringify (val)
  (%js-json-stringify-value val 0))

;;; -----------------------------------------------------------------------
;;;  JSON deserialization
;;; -----------------------------------------------------------------------

(defun %js-json-parse (str)
  "Minimal JSON parser: handles null, true, false, numbers, strings, arrays, objects."
  (handler-case
      (%js-json-parse-value (string-trim '(#\Space #\Tab #\Newline #\Return) str) 0)
    (error () +js-undefined+)))

(defun %js-json-skip-ws (str pos)
  (loop while (and (< pos (length str))
                   (member (char str pos) '(#\Space #\Tab #\Newline #\Return)))
        do (incf pos))
  pos)

(defun %js-json-parse-value (str pos)
  "Parse JSON value at POS in STR, returning (values parsed-value new-pos)."
  (let ((c (and (< pos (length str)) (char str pos))))
    (cond
      ((null c) (values +js-undefined+ pos))
      ((char= c #\") (%js-json-parse-string str (1+ pos)))
      ((char= c #\{) (%js-json-parse-object str (1+ pos)))
      ((char= c #\[) (%js-json-parse-array str (1+ pos)))
      ((and (>= (length str) (+ pos 4)) (string= str "null" :start1 pos :end1 (+ pos 4)))
       (values +js-null+ (+ pos 4)))
      ((and (>= (length str) (+ pos 4)) (string= str "true" :start1 pos :end1 (+ pos 4)))
       (values t (+ pos 4)))
      ((and (>= (length str) (+ pos 5)) (string= str "false" :start1 pos :end1 (+ pos 5)))
       (values nil (+ pos 5)))
      ((or (digit-char-p c) (char= c #\-))
       (%js-json-parse-number str pos))
      (t (values +js-undefined+ pos)))))

(defun %js-json-parse-string (str pos)
  ;; Use an explicit stream — WITH-OUTPUT-TO-STRING returns the string and
  ;; discards the body's (values …), losing the new position.
  (let ((buf (make-string-output-stream)))
    (loop
      (when (>= pos (length str)) (return))
      (let ((c (char str pos)))
        (when (char= c #\") (return))
        (incf pos)
        (if (char= c #\\)
            (let ((esc (char str pos)))
              (incf pos)
              (write-char (case esc (#\" #\") (#\\ #\\) (#\/ #\/) (#\n #\Newline)
                                    (#\r #\Return) (#\t #\Tab) (t esc)) buf))
            (write-char c buf))))
    (values (get-output-stream-string buf) (1+ pos))))

(defun %js-json-parse-number (str pos)
  (let ((end pos))
    (when (char= (char str end) #\-) (incf end))
    (loop while (and (< end (length str))
                     (or (digit-char-p (char str end))
                         (char= (char str end) #\.)
                         (member (char str end) '(#\e #\E #\+ #\-))))
          do (incf end))
    (values (handler-case (coerce (read-from-string (subseq str pos end)) 'double-float)
              (error () *js-nan-float*))
            end)))

(defun %js-json-parse-array (str pos)
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (setf pos (%js-json-skip-ws str pos))
    (when (and (< pos (length str)) (char= (char str pos) #\]))
      (return-from %js-json-parse-array (values result (1+ pos))))
    (loop
      (multiple-value-bind (val new-pos) (%js-json-parse-value str (%js-json-skip-ws str pos))
        (vector-push-extend val result)
        (setf pos (%js-json-skip-ws str new-pos))
        (cond ((and (< pos (length str)) (char= (char str pos) #\,)) (incf pos))
              ((and (< pos (length str)) (char= (char str pos) #\])) (return (values result (1+ pos))))
              (t (return (values result pos))))))))

(defun %js-json-parse-object (str pos)
  (let ((ht (%js-make-ht)))
    (setf pos (%js-json-skip-ws str pos))
    (when (and (< pos (length str)) (char= (char str pos) #\}))
      (return-from %js-json-parse-object (values ht (1+ pos))))
    (loop
      (setf pos (%js-json-skip-ws str pos))
      (unless (and (< pos (length str)) (char= (char str pos) #\")) (return (values ht pos)))
      (multiple-value-bind (key new-pos) (%js-json-parse-string str (1+ pos))
        (setf pos (%js-json-skip-ws str new-pos))
        (when (and (< pos (length str)) (char= (char str pos) #\:)) (incf pos))
        (multiple-value-bind (val new-pos2) (%js-json-parse-value str (%js-json-skip-ws str pos))
          (setf (gethash key ht) val
                pos (%js-json-skip-ws str new-pos2))
          (cond ((and (< pos (length str)) (char= (char str pos) #\,)) (incf pos))
                ((and (< pos (length str)) (char= (char str pos) #\})) (return (values ht (1+ pos))))
                (t (return (values ht pos)))))))))

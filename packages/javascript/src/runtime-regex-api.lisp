;;;; packages/javascript/src/runtime-regex-api.lisp — JS RegExp public API
;;;;
;;;; Public matching API: %js-make-regex, %js-regex-exec, %js-regex-test,
;;;; %js-string-match-regex, %js-string-search-regex, %js-string-replace-regex,
;;;; regex-replace-placeholders, %js-string-replace-all-regex, %js-string-split-regex.
;;;;
;;;; Load order: after runtime-regex.lisp (needs js-regexp struct, compiled slot,
;;;; %js-compile-pattern, and js-regexp-* accessors).

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Public API — matching
;;; -----------------------------------------------------------------------

(defun %js-make-regex (pattern &optional (flags ""))
  "Create a JS RegExp from PATTERN and FLAGS strings."
  (let* ((ic (find #\i flags))
         (ml (find #\m flags))
         (gl (find #\g flags))
         (st (find #\y flags))
         (compiled (handler-case (%js-compile-pattern pattern :ignore-case-p ic :multiline-p ml)
                     (error () nil))))
    (make-js-regexp
     :source pattern :flags flags
     :compiled compiled
     :global-p (not (null gl))
     :ignore-case-p (not (null ic))
     :multiline-p (not (null ml))
     :sticky-p (not (null st))
     :last-index 0)))

(defun %js-regex-exec (re str &optional (start 0))
  "Execute RE against STR starting at START. Returns [match, groups...] or null."
  (let ((fn (js-regexp-compiled re))
        (n (length str)))
    (unless fn (return-from %js-regex-exec +js-null+))
    (loop for i from start to n
          for end = (funcall fn str i nil)
          when end
            do (let ((match-str (subseq str i end)))
                 (when (js-regexp-global-p re)
                   (setf (js-regexp-last-index re) end))
                 (return (%js-make-object "index" (coerce i 'double-float)
                                          "input" str
                                          "0" match-str)))
          finally (progn
                    (when (js-regexp-global-p re)
                      (setf (js-regexp-last-index re) 0))
                    (return +js-null+)))))

(defun %js-regex-test (re str)
  "RegExp.prototype.test(str) — return t if match found."
  (not (eq +js-null+ (%js-regex-exec re str 0))))

(defun %js-string-match-regex (str re)
  "String.prototype.match(regexp)."
  (if (js-regexp-p re)
      (if (js-regexp-global-p re)
          ;; /g flag: collect all matches
          (let ((results (make-array 0 :element-type t :adjustable t :fill-pointer 0))
                (pos 0))
            (loop
              (let ((m (%js-regex-exec re str pos)))
                (when (eq m +js-null+) (return))
                (let ((match (gethash "0" m)))
                  (vector-push-extend match results)
                  (setf pos (max (1+ pos) (truncate (gethash "index" m) ))))))
            (if (zerop (length results)) +js-null+ results))
          ;; No /g: return first match object
          (%js-regex-exec re str 0))
      ;; String pattern: use substring search
      (%js-string-match str (%js-to-string re))))

(defun %js-string-search-regex (str re)
  "String.prototype.search(regexp) — return index or -1."
  (if (js-regexp-p re)
      (let ((m (%js-regex-exec re str 0)))
        (if (eq m +js-null+) -1 (truncate (gethash "index" m))))
      (%js-string-search str (%js-to-string re))))

(defun %js-string-replace-regex (str re replacement)
  "String.prototype.replace(regexp, replacement)."
  (if (js-regexp-p re)
      (let ((fn (js-regexp-compiled re))
            (result (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character))
            (pos 0))
        (if fn
            (loop
              (let ((end (funcall fn str pos nil)))
                (when (or (null end) (> pos (length str)))
                  (loop for i from pos below (length str)
                        do (vector-push-extend (char str i) result))
                  (return (coerce result 'string)))
                (loop for i from pos below (or (gethash "index" (%js-regex-exec re str pos)) pos)
                      do (vector-push-extend (char str i) result))
                ;; Find actual match start
                (let ((match-start nil))
                  (loop for i from pos to (length str)
                        for end2 = (funcall fn str i nil)
                        when end2
                          do (setf match-start i)
                             (return))
                  (unless match-start
                    (loop for i from pos below (length str)
                          do (vector-push-extend (char str i) result))
                    (return (coerce result 'string)))
                  (let* ((match-str (subseq str match-start end))
                         (repl (if (functionp replacement)
                                   (%js-to-string (%js-funcall replacement match-str match-start str))
                                   (let ((r (%js-to-string replacement)))
                                     (regex-replace-placeholders r match-str)))))
                    (loop for c across repl do (vector-push-extend c result))
                    (setf pos end)
                    (unless (js-regexp-global-p re) (return nil))))))
            (%js-string-replace str (%js-to-string (js-regexp-source re)) replacement)))
      (%js-string-replace str (%js-to-string re) replacement)))

(defun regex-replace-placeholders (template match-str)
  "Replace $& with the match in TEMPLATE."
  (let ((out (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
    (loop for i below (length template)
          do (if (and (char= (char template i) #\$)
                      (< (1+ i) (length template))
                      (char= (char template (1+ i)) #\&))
                 (progn (loop for c across match-str do (vector-push-extend c out))
                        (incf i))
                 (vector-push-extend (char template i) out)))
    (coerce out 'string)))

(defun %js-string-replace-all-regex (str re replacement)
  "String.prototype.replaceAll with regex (must have /g flag)."
  (if (js-regexp-p re)
      (%js-string-replace-regex str re replacement)
      (%js-string-replace-all str (%js-to-string re) replacement)))

(defun %js-string-split-regex (str re &optional limit)
  "String.prototype.split with regex separator."
  (if (js-regexp-p re)
      (let* ((fn (js-regexp-compiled re))
             (results (make-array 0 :element-type t :adjustable t :fill-pointer 0))
             (max-count (if (and limit (not (eq limit +js-undefined+))) (truncate limit) most-positive-fixnum))
             (pos 0))
        (when (or (null fn) (string= str ""))
          (return-from %js-string-split-regex (%js-string-split str (%js-to-string (js-regexp-source re)) limit)))
        (loop while (< pos (length str))
              do (let ((end (funcall fn str pos nil)))
                   (if end
                       (progn
                         (vector-push-extend (subseq str pos (max pos end)) results)
                         (when (>= (length results) max-count) (return))
                         (setf pos (if (= end pos) (1+ pos) end)))
                       (progn
                         (vector-push-extend (subseq str pos) results)
                         (return)))))
        results)
      (%js-string-split str (%js-to-string re) limit)))

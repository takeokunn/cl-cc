;;;; persistent-api.lisp — Persistent Map Public API
;;;;
;;;; persist-empty, persist-count, persist-assoc, persist-lookup,
;;;; persist-contains-p, persist-remove, persist-each, persist-keys,
;;;; persist-values, persist-to-alist, persist-from-alist.
;;;;
;;;; Internal tree/bucket helpers are in persistent.lisp (loads before).
;;;; Load order: after persistent.lisp.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Public API
;;; ------------------------------------------------------------

(defun persist-empty (&key (test 'eql))
  "Return a fresh empty persistent-map using TEST (a symbol) for key equality."
  (%make-persistent-map nil 0 test))

(defun persist-count (pm)
  "Number of entries in PM."
  (persistent-map-count pm))

(defun persist-assoc (pm key value)
  "Return a new persistent-map equal to PM with KEY mapped to VALUE.
   Does not mutate PM. If KEY is present, the mapping is replaced and
   the count is unchanged."
  (let ((test (persistent-map-test pm)))
    (multiple-value-bind (new-root added-p)
        (%tree-insert (persistent-map-root pm) (%hash key) key value test)
      (%make-persistent-map new-root
                            (if added-p
                                (1+ (persistent-map-count pm))
                                (persistent-map-count pm))
                            test))))

(defun persist-lookup (pm key &optional default)
  "Return (values value found-p). If KEY is absent, VALUE is DEFAULT and
   FOUND-P is NIL."
  (multiple-value-bind (v found-p)
      (%tree-lookup (persistent-map-root pm)
                    (%hash key)
                    key
                    (persistent-map-test pm))
    (if found-p
        (values v t)
        (values default nil))))

(defun persist-contains-p (pm key)
  "True iff KEY is a key in PM."
  (multiple-value-bind (v found-p) (persist-lookup pm key)
    (declare (ignore v))
    found-p))

(defun persist-remove (pm key)
  "Return a new persistent-map equal to PM without KEY. If KEY is
   absent, returns a map equivalent to PM with count unchanged."
  (let ((test (persistent-map-test pm)))
    (multiple-value-bind (new-root removed-p)
        (%tree-remove (persistent-map-root pm) (%hash key) key test)
      (%make-persistent-map new-root
                            (if removed-p
                                (1- (persistent-map-count pm))
                                (persistent-map-count pm))
                            test))))

(defun persist-each (pm fn)
  "Call (funcall FN key value) for every entry of PM. Returns NIL."
  (%tree-walk (persistent-map-root pm) fn)
  nil)

(defun persist-keys (pm)
  "Return a list of every key in PM. Order is unspecified."
  (let ((acc '()))
    (persist-each pm (lambda (k v) (declare (ignore v)) (push k acc)))
    acc))

(defun persist-values (pm)
  "Return a list of every value in PM. Order is unspecified."
  (let ((acc '()))
    (persist-each pm (lambda (k v) (declare (ignore k)) (push v acc)))
    acc))

(defun persist-to-alist (pm)
  "Return an alist ((key . value) ...) with every entry of PM."
  (let ((acc '()))
    (persist-each pm (lambda (k v) (push (cons k v) acc)))
    acc))

(defun persist-from-alist (alist &key (test 'eql))
  "Return a persistent-map populated from ALIST. If ALIST contains
   duplicate keys, the leftmost binding wins — matching CL `assoc'
   semantics."
  (let ((pm (persist-empty :test test)))
    (dolist (cell (reverse alist))
      (setf pm (persist-assoc pm (car cell) (cdr cell))))
    pm))

(in-package :cl-cc/expand)
;;;; DEFSTRUCT BOA lambda-list and constructor helpers.

;;; ── BOA (By-Order-of-Arguments) lambda list helpers ──────────────────────

(defun %defstruct-extract-boa-parts (boa-args)
  "Split a BOA lambda list into (normal-params . aux-bindings)."
  (let ((tail boa-args)
        (arg nil)
        (normal nil)
        (aux nil)
        (in-aux nil))
    (tagbody
     scan
       (if (null tail) (go done))
       (setq arg (car tail))
       (if (eq arg '&aux)
           (setq in-aux t)
           (if in-aux
               (if (consp arg)
                   (setq aux (cons arg aux))
                   (setq aux (cons (%expander-form arg nil) aux)))
               (setq normal (cons arg normal))))
       (setq tail (cdr tail))
       (go scan)
     done)
    (cons (nreverse normal) (nreverse aux))))

(defun %defstruct-boa-param-names (normal-params)
  "Extract bound parameter names from a BOA lambda list (excludes &aux)."
  (let ((tail normal-params)
        (p nil)
        (names nil)
        (skip nil))
    (tagbody
     scan
       (if (null tail) (go done))
       (setq p (car tail))
       (setq skip nil)
       (if (eq p '&key) (setq skip t))
       (if (eq p '&optional) (setq skip t))
       (if (eq p '&rest) (setq skip t))
       (if (eq p '&body) (setq skip t))
       (if (eq p '&allow-other-keys) (setq skip t))
       (if skip
           nil
           (if (consp p)
               (if (consp (first p))
                   (setq names (cons (second (first p)) names))
                   (setq names (cons (first p) names)))
               (if (symbolp p)
                   (setq names (cons p names)))))
       (setq tail (cdr tail))
       (go scan)
     done)
    (nreverse names)))

;;; ── Constructor generation ───────────────────────────────────────────────

(defun %defstruct-build-constructor (ctor-name boa-args all-slots body-fn)
  "Build a DEFUN constructor form. BODY-FN receives the effective slot-value forms."
  (if boa-args
      (let ((boa-data nil)
            (normal-params nil)
            (bound-names nil)
            (aux-lets nil))
        (setq boa-data (%defstruct-boa-bindings boa-args))
        (setq normal-params (first boa-data))
        (setq bound-names (car (cdddr boa-data)))
        (setq aux-lets (car (cddddr boa-data)))
        (%expander-form 'defun ctor-name normal-params
                        (%expander-form
                         'let* aux-lets
                         (funcall body-fn
                                  (%defstruct-resolve-slot-values all-slots bound-names)))))
      (let ((tail all-slots)
            (slot nil)
            (key-bindings nil)
            (slot-values nil))
        (tagbody
         scan
           (if (null tail) (go done))
           (setq slot (car tail))
           (setq key-bindings
                 (cons (%expander-form (first slot) (second slot)) key-bindings))
           (setq slot-values (cons (first slot) slot-values))
           (setq tail (cdr tail))
           (go scan)
         done)
        (setq key-bindings (nreverse key-bindings))
        (setq slot-values (nreverse slot-values))
        (%expander-form 'defun ctor-name
                        (cons '&key key-bindings)
                        (funcall body-fn slot-values)))))

(defun %defstruct-make-constructor (ctor-name class-name boa-args all-slots)
  "Generate a DEFUN form for a defstruct constructor using CLOS make-instance."
  (%defstruct-build-constructor
   ctor-name boa-args all-slots
   (lambda (slot-values)
     (let ((slot-tail all-slots)
           (value-tail slot-values)
           (slot nil)
           (args nil))
       (tagbody
        scan
          (if (null slot-tail) (go done))
          (setq slot (car slot-tail))
          (setq args (cons (%defstruct-make-keyword (first slot)) args))
          (setq args (cons (car value-tail) args))
          (setq slot-tail (cdr slot-tail))
          (setq value-tail (cdr value-tail))
          (go scan)
        done)
       (cons 'make-instance
             (cons (%expander-form 'quote class-name)
                   (nreverse args)))))))

;;;; packages/javascript/src/runtime-symbol.lisp — JS Symbol primitive (ES2015+)
;;;;
;;;; Symbol is a unique, immutable primitive. Each call to Symbol() returns a
;;;; fresh value that is never equal to any other value (even with the same
;;;; description). In CL we represent symbols as a struct wrapping a gensym so
;;;; identity equality works naturally.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Symbol struct
;;; -----------------------------------------------------------------------

(defstruct (js-symbol (:conc-name js-symbol-))
  description    ; string or undefined
  key)           ; a CL gensym — provides unique identity

(defun %js-symbol-p (x) (js-symbol-p x))

(defun %js-make-symbol (&optional (description +js-undefined+))
  "JS Symbol([description]) — always returns a fresh unique symbol."
  (make-js-symbol
   :description (if (eq description +js-undefined+) +js-undefined+
                    (%js-to-string description))
   :key (gensym "JS-SYMBOL-")))

(defun %js-symbol-to-string (sym)
  "Symbol.prototype.toString → 'Symbol(desc)'."
  (let ((desc (js-symbol-description sym)))
    (if (eq desc +js-undefined+)
        "Symbol()"
        (format nil "Symbol(~A)" desc))))

(defun %js-symbol-description (sym)
  "Symbol.prototype.description getter."
  (js-symbol-description sym))

;;; -----------------------------------------------------------------------
;;;  Global symbol registry — Symbol.for / Symbol.keyFor
;;; -----------------------------------------------------------------------

(defparameter *js-symbol-registry* (make-hash-table :test #'equal)
  "Global symbol registry mapping string keys to js-symbol instances.")

(defun %js-symbol-for (key)
  "Symbol.for(key) — returns the registered symbol for KEY, creating it if absent."
  (let ((k (%js-to-string key)))
    (multiple-value-bind (sym found) (gethash k *js-symbol-registry*)
      (if found sym
          (let ((new-sym (make-js-symbol :description k :key (gensym "JS-GLOBAL-SYM-"))))
            (setf (gethash k *js-symbol-registry*) new-sym)
            new-sym)))))

(defun %js-symbol-key-for (sym)
  "Symbol.keyFor(sym) — return the registry key for SYM, or undefined."
  (if (js-symbol-p sym)
      (block found
        (maphash (lambda (k v)
                   (when (eq v sym) (return-from found k)))
                 *js-symbol-registry*)
        +js-undefined+)
      +js-undefined+))

;;; -----------------------------------------------------------------------
;;;  Well-known symbols (@@iterator, @@toPrimitive, @@toStringTag, etc.)
;;; -----------------------------------------------------------------------
;;;
;;; Stored as CL defparameters so every reference uses the same object.
;;; The parser stores them under "@@name" keys in objects.

(defparameter %js-symbol-iterator
  (%js-symbol-for "Symbol.iterator")
  "Symbol.iterator — used by for...of and spread.")

(defparameter %js-symbol-to-primitive
  (%js-symbol-for "Symbol.toPrimitive")
  "Symbol.toPrimitive — type coercion hook.")

(defparameter %js-symbol-to-string-tag
  (%js-symbol-for "Symbol.toStringTag")
  "Symbol.toStringTag — Object.prototype.toString tag.")

(defparameter %js-symbol-has-instance
  (%js-symbol-for "Symbol.hasInstance")
  "Symbol.hasInstance — instanceof hook.")

(defparameter %js-symbol-species
  (%js-symbol-for "Symbol.species")
  "Symbol.species — constructor for derived objects.")

(defparameter %js-symbol-async-iterator
  (%js-symbol-for "Symbol.asyncIterator")
  "Symbol.asyncIterator — async iteration protocol.")

(defparameter %js-symbol-match
  (%js-symbol-for "Symbol.match")
  "Symbol.match — String.prototype.match hook.")

(defparameter %js-symbol-replace
  (%js-symbol-for "Symbol.replace")
  "Symbol.replace — String.prototype.replace hook.")

(defparameter %js-symbol-search
  (%js-symbol-for "Symbol.search")
  "Symbol.search — String.prototype.search hook.")

(defparameter %js-symbol-split
  (%js-symbol-for "Symbol.split")
  "Symbol.split — String.prototype.split hook.")

;;; -----------------------------------------------------------------------
;;;  Symbol as property key — ht uses the js-symbol struct as key (eq lookup)
;;; -----------------------------------------------------------------------
;;;
;;; Symbols as property keys use the struct identity directly (stored in the HT
;;; under the symbol struct itself, not a string). %js-get-prop / %js-set-prop
;;; already use gethash which works for struct keys under #'equal when the structs
;;; are the SAME object — but since #'equal doesn't compare structs by identity,
;;; we need #'eq. The current ht uses #'equal which handles string keys well but
;;; would not find symbol keys by identity.
;;;
;;; Workaround: convert symbol keys to their string representation for storage.
;;; Full Symbol-as-property-key support requires an #'eq hash-table alongside
;;; the existing #'equal one — deferred to a later implementation.

(defun %js-symbol-as-key (sym)
  "Convert a JS symbol to its hash-table storage key (string form)."
  (format nil "__sym_~A__" (js-symbol-key sym)))

;;; -----------------------------------------------------------------------
;;;  Make the Symbol global object (callable + static methods)
;;; -----------------------------------------------------------------------

(defun %js-make-symbol-global ()
  "Build the Symbol global object with static methods and well-known symbols."
  (let ((sym-obj (%js-make-ht)))
    ;; Well-known symbols as properties
    (setf (gethash "iterator"    sym-obj) %js-symbol-iterator
          (gethash "toPrimitive" sym-obj) %js-symbol-to-primitive
          (gethash "toStringTag" sym-obj) %js-symbol-to-string-tag
          (gethash "hasInstance" sym-obj) %js-symbol-has-instance
          (gethash "species"     sym-obj) %js-symbol-species
          (gethash "asyncIterator" sym-obj) %js-symbol-async-iterator
          (gethash "match"       sym-obj) %js-symbol-match
          (gethash "replace"     sym-obj) %js-symbol-replace
          (gethash "search"      sym-obj) %js-symbol-search
          (gethash "split"       sym-obj) %js-symbol-split
          ;; Static methods
          (gethash "for"         sym-obj) #'%js-symbol-for
          (gethash "keyFor"      sym-obj) #'%js-symbol-key-for
          ;; Make Symbol() callable via __call__
          (gethash "__call__"    sym-obj) #'%js-make-symbol)
    sym-obj))

(defparameter *js-symbol-global* (%js-make-symbol-global)
  "The global Symbol object (callable + static methods).")

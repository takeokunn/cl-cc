;;;; packages/engine/vm/src/package.lisp - VM module package marker
;;;;
;;;; VM sources live in the :cl-cc/vm package defined by
;;;; facade-package-defpackage.lisp. This file intentionally contains only the
;;;; package switch so ASDF can establish ordering without hiding logic here.

(in-package :cl-cc/vm)

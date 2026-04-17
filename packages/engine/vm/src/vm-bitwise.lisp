(in-package :cl-cc/vm)

;;; VM — Bitwise Integer Operations (FR-303)
;;;
;;; Contains: ash, rotate, logand, logior, logxor, logeqv, lognot, bswap, logtest,
;;;           logbitp, logcount, integer-length.
;;;
;;; Load order: after primitives.lisp, before vm-transcendental.lisp.

;;; ─── Bit shift and bitwise binary ────────────────────────────────────────────
;; define-vm-binary-instruction / define-vm-unary-instruction are defined in vm.lisp.

(define-vm-binary-instruction vm-ash    :ash    "Arithmetic shift: (ash integer count). Positive count = left shift.")
(define-vm-binary-instruction vm-rotate  :rotate "Rotate bits right by COUNT within the native word width.")
(define-vm-binary-instruction vm-logand :logand "Bitwise AND.")
(define-vm-binary-instruction vm-logior :logior "Bitwise inclusive OR.")
(define-vm-binary-instruction vm-logxor :logxor "Bitwise exclusive OR.")
(define-vm-binary-instruction vm-logeqv :logeqv "Bitwise equivalence (XNOR).")

(define-simple-instruction vm-ash    :binary ash)
(defun rotate-right (x count &optional (width 64))
  "Rotate X right by COUNT within WIDTH bits."
  (let* ((width (max 1 width))
         (count (mod count width))
         (mask (1- (ash 1 width)))
         (x (logand x mask)))
    (logand mask
            (logior (ash x (- count))
                    (ash x (- width count))))))

(define-simple-instruction vm-rotate :binary rotate-right)
(define-simple-instruction vm-logand :binary logand)
(define-simple-instruction vm-logior :binary logior)
(define-simple-instruction vm-logxor :binary logxor)
(define-simple-instruction vm-logeqv :binary logeqv)

;;; ─── Bitwise unary ───────────────────────────────────────────────────────────

(define-vm-unary-instruction vm-lognot         :lognot          "Bitwise complement.")
(define-vm-unary-instruction vm-bswap          :bswap           "Reverse the byte order of the low 32 bits.")
(define-vm-unary-instruction vm-logcount       :logcount        "Count set bits in integer.")
(define-vm-unary-instruction vm-integer-length :integer-length  "Number of bits needed to represent integer.")

(defun bswap (x)
  "Reverse the byte order of the low 32 bits of X, returning a zero-extended integer."
  (let ((x (logand x #xFFFFFFFF)))
    (logior (ash (logand x #xFF) 24)
            (ash (logand x #xFF00) 8)
            (ash (logand x #xFF0000) -8)
            (ash (logand x #xFF000000) -24))))

(define-simple-instruction vm-lognot         :unary lognot)
(define-simple-instruction vm-bswap          :unary bswap)
(define-simple-instruction vm-logcount       :unary logcount)
(define-simple-instruction vm-integer-length :unary integer-length)

;;; ─── Bitwise predicates ──────────────────────────────────────────────────────

(define-vm-binary-instruction vm-logtest :logtest "Test if any bits are set in common: (logtest j k) => t/nil.")
(define-vm-binary-instruction vm-logbitp :logbitp "Test if bit INDEX is set in INTEGER: (logbitp index integer) => t/nil.")

(define-simple-instruction vm-logtest :pred2 logtest)
(define-simple-instruction vm-logbitp :pred2 logbitp)
